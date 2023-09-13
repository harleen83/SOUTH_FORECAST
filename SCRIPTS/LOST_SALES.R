cat("Cleaning enviornment", "/n")
rm(list = ls())
gc()

#importing libraries
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(R.utils)
library(stringr)
library(readxl)
library("arrow")

#path of the project
path_proj<-"D:/products/SOUTH_FORECASTS"


#calculating the ASP for each co-art-month
ASP_CALCULATION<-function(store_qty,store_revenue,end_date)
{
  
  #convert dataframe to datatable
  store_revenue<-setDT(store_revenue)
  store_qty<-setDT(store_qty)
  #picking only the article_code and the sales-val corresponding to the latest month
  store_revenue<-subset(store_revenue,select=c("article_code",
                                               colnames(store_revenue)[grep(end_date,colnames(store_revenue)):
                                                                         grep(end_date,colnames(store_revenue))]))
  
  names(store_revenue)[colnames(store_revenue)==end_date]<- "latest_rev"
  #grouping by on article_code level
  store_revenue<-store_revenue[order(article_code),.("latest_rev" = sum(`latest_rev`)),.(article_code)]
  
  #picking only the article_code and sales-qty corresponding to the latest month
  store_qty<-subset(store_qty,select=c("article_code",
                                       colnames(store_qty)[grep(end_date,colnames(store_qty)):
                                                             grep(end_date,colnames(store_qty))]))
  
  names(store_qty)[colnames(store_qty)==end_date]<-"latest_qty"
  #grouping by on article_code level
  store_qty<-store_qty[order(article_code),.("latest_qty" = sum(`latest_qty`)),.(article_code)]
  
  
  #mapping the store_revenue and store-qty by article-code 
  store_revenue<-inner_join(store_revenue,store_qty,by=c("article_code"))
  
  #calculating the asp for the latest month
  store_revenue$asp<-store_revenue$latest_rev/store_revenue$latest_qty
  store_revenue[is.infinite(store_revenue$asp),]<-0
  
  #retaining only the asp and the article code
  store_revenue<-subset(store_revenue,select=c("article_code","asp"))
  return(store_revenue)
}




is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


add.months<-function(date,n) seq(date, by = "month", length.out = n)[n]

add.month<-function(date) add.months(date,2)

##function to bind multiple lists for lost-sale-demand
CBINDLIST_DEMAND<-function(ls)
{
  df<-ls[[1]]
  if(length(ls)>1)
  {
    for(ind in 2:length(ls))
    {
      df<-full_join(df,ls[[ind]],by=c("section_code","dept_code",
                                       "site_code","article_code"))
    }
  }
  return(df)
}

#getting the missing-data captured
MISSING_DATA<- function(str_sales, str_demand, fcst_months)
{
  
  #getting the months for which due to absence of model-with budget files
  #the lost sales weren't calculated
  cols_store_sales<- colnames(str_sales)
  cols_sale_demand<-  colnames(str_demand)
  remaining_months<- cols_store_sales[!(cols_store_sales %in% cols_sale_demand) ]
  
  
  #joining the sales data for such months, to the demand data captured in the for-loop above
  sale_remaining_months<- subset(str_sales, select=c("site_code","article_code","dept_code","section_code",remaining_months))
  str_sales_full<- right_join(sale_remaining_months, str_demand, by=c("site_code","article_code","dept_code","section_code"))
  #ensuring that the columns are arranged in appropriate order 
  str_sales_full<- subset(str_sales_full, select=c("site_code","article_code","dept_code","section_code",fcst_months))
  
  
  #there might be some items whose budget allocation is not present in the budget files
  #due to some internal-issue, we have to capture that as well
  str_sales[,"concat"]<- paste0(str_sales$site_code,"_", str_sales$article_code)
  str_demand[,"concat"]<- paste0(str_demand$site_code,"_",str_demand$article_code)
  
  str_revenue_missing<-str_sales[!(str_sales$concat %in% str_demand$concat)]
  setDT(str_revenue_missing)
  
  str_revenue_missing<- str_revenue_missing[,-c("concat")]
  
  str_art_demand<- rbind(str_sales_full, str_revenue_missing)
  
  
  return(str_art_demand)
  
  
  
}

#remove duplicates both from the store-article revenue demand & store-article quantitu demand
REMOVE_DUPLICATES<- function(str_sales)
{
  str_sales$concat<- paste0(str_sales$site_code,"_", str_sales$article_code)
  str_sales<- str_sales[!duplicated(str_sales$concat),]
  
  str_sales<- str_sales[,-c("concat")]
  
  return(str_sales)
}


#------------path variable deceleration-------------------------------
path_stores_sales<-file.path(path_proj,"input")
store_files <- list.files(path_stores_sales, full.names = TRUE)
#reading the revenue and quantity data

store_revenue<- fread(list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA", "P1",
                                           "P1_SALES"), full.names = TRUE )[grep("P1_SALES_REV"
                ,list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA","P1","P1_SALES")))])


                                                                                                                      
store_qty<- fread(list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA", "P1",
                                         "P1_SALES"), full.names = TRUE )[grep("P1_SALES_QTY"
                                                                               ,list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA","P1","P1_SALES")))])

user_input<- read_excel(list.files(file.path(path_proj, "USER_INPUT"), full.names=TRUE))

# getting sequence of fcst_months 
fcst_months <- format(seq.Date(as.Date(user_input$START_PERIOD),as.Date(user_input$END_PERIOD), by = "month"),
                      format = "%b-%Y")

colnames(store_revenue)<-c("site_code","article_code","dept_code","section_code",fcst_months)
colnames(store_qty)<-c("site_code","article_code","dept_code","section_code",fcst_months)


#getting the start and the end year from the user
start_yr<-user_input$START_PERIOD
end_date<-user_input$END_PERIOD



##Count the number of CSV files in the folder, and names of each file in list
#yr_folders <- list.files("D:/budget_files_weekly",full.names=TRUE)
yr_folders<- list.files(file.path(path_proj, "MODEL_BUDGET_FILES"), full.names=TRUE)
yr_ind<-1
ls_demand_rev<-list()
ls_demand_qty<-list()


for(yr_files in yr_folders)
{
  
  
  
  ##Read each csv file as a separate dataframe
  yr_files<-list.files(yr_files,pattern="*parquet",full.names=TRUE)
  
  ##Convert all month columns to standardized columns & append into one big dataframe
  ##standardization is being done in this was month_1, month_2, month_3......
  ls_resultant_files<-list()
  index<-1
  for(val in 1:length(yr_files)){
    
    #reading the file in temp- variable
    temp<-read_parquet(yr_files[val])
    
    #column names transfromed so that the first letter capitalized
    colnames(temp)<-tolower(colnames(temp))
    colnames(temp)<-capitalize(colnames(temp))
    
    #replace dash in budget files by /
    temp$Date<-str_replace_all(temp$Date, "-", "/")
    
    temp[is.na(temp)]<- 0
    
    
    if((any(grepl((format(as.Date(unique(temp$Date),"%d/%m/%Y"),"%b")),colnames(temp)))==TRUE) & 
       (any(grepl((format(add.month(as.Date(unique(temp$Date),"%d/%m/%Y")),"%b")),colnames(temp)))==TRUE))
    {
      colnames(temp)[grep(format(as.Date(unique(temp$Date),"%d/%m/%Y"),"%b"),colnames(temp))]<-"month_1"
      colnames(temp)[grep(format(add.month(as.Date(unique(temp$Date),"%d/%m/%Y")),"%b"),colnames(temp))]<-"month_2";
    }else {
      next
    }
    
    colnames(temp)<-c("date","store","division","section","department","article_name","site_code",
                      "article_code","available_stock","GIT",
                      "assortment_factor",
                      colnames(temp)[(grep("Assortment",colnames(temp))+1):length(colnames(temp))])
    
    
    
    temp<-subset(temp,select=c("date","store",
                               "site_code",
                               "article_code","available_stock","GIT",
                               "assortment_factor","month_1","month_2"))
    ls_resultant_files[[index]]<-temp
    index<-index+1
    print(index)
  }
  
  df_lost<-rbindlist(ls_resultant_files, fill=TRUE, idcol=NULL)
  df_lost<-as.data.frame(df_lost)
  #getting all the columns as lower case
  present_colnames<-capitalize(colnames(df_lost))
  
  colnames(df_lost)<-c("date","store","site_code",
                       "article_code","available_stock","GIT",
                       "assortment_factor",
                       present_colnames[(grep("Assortment",present_colnames)+1):length(present_colnames)])
  

  
  #Make changes to data_frame for lost sales calculations
  df_lost['SOH']<-df_lost['available_stock']+df_lost['GIT']
  df_lost['forward_cover_days']<-df_lost['assortment_factor']*15/2
  df_lost['forward_cover_qty']<-0
  df_lost['forward_cover_qty'][which(df_lost$'forward_cover_days'<=30),]<-((df_lost['forward_cover_days'][which(df_lost$'forward_cover_days'<=30),])/30)*df_lost['Month_1'][which(df_lost$'forward_cover_days'<=30),]
  df_lost['forward_cover_qty'][which(df_lost$'forward_cover_days'>30),]<-(((df_lost['forward_cover_days'][which(df_lost$'forward_cover_days'>30),]-30)/30)*df_lost['Month_2'][which(df_lost$'forward_cover_days'>30),])+df_lost['Month_1'][which(df_lost$'forward_cover_days'>30),]
  df_lost['fill_rate']<-df_lost$SOH/df_lost$forward_cover_qty
  df_lost['lost_sale_status']<-"No Lost Sale"
  df_lost['lost_sale_status'][which(df_lost$`fill_rate`<1),]<-"Lost sale"
  df_lost[is.nan(df_lost$fill_rate),]$fill_rate <- 0
  df_lost[is.infinite(df_lost$fill_rate),]$fill_rate<-0
  df_lost['lost_sale_qty']<-0
  #50% of confidence that the customer will come
  df_lost['lost_sale_qty'][which(df_lost$fill_rate<1),]<-(1-df_lost['fill_rate'][which(df_lost$`fill_rate`<1),])*0.5*(df_lost['Month_1'][which(df_lost$`fill_rate`<1),])/4
  df_lost$date<-as.Date(df_lost$date,"%d/%m/%Y")
  df_lost<-df_lost[order(df_lost$date),]
  df_lost$month_year<-format(as.Date(df_lost$date, format = "%Y-%m-%d"),"%b-%Y")
  
  
  #getting the current stock 
  store_qty<-data.table(store_qty)
  store_revenue<-data.table(store_revenue)
  
  #getting the sequence of years from start to end yr
  end_date<-format(as.Date(user_input$END_PERIOD,format="%d-%b-%Y"),format="%b-%Y")
  #calling the ASP for stores
  df_lost_asp<-ASP_CALCULATION(store_qty,store_revenue,end_date)
  df_lost<-setDT(df_lost)
  df_lost<-df_lost[order(site_code),.("lost_sale_qty" = sum(`lost_sale_qty`)),.(site_code,article_code,month_year)]
  

  
  #mapping the df_lost and df_lost_asp
  df_lost<-left_join(df_lost,df_lost_asp,by=c("article_code"))
  #in case of of negative(un-explainable-lost-sales convert to 0s)
  df_lost[which(df_lost$lost_sale_qty<0),]<-0
  
  df_lost$asp[is.na(df_lost$asp)] <- 0
  df_lost$`lost_sale_value`<-df_lost$`lost_sale_qty`*df_lost$asp
  df_lost[is.na(df_lost)]<- 0
  
  ##Convert into pivot table for output
  pv_lost_sale_rev <- pivot_wider(data = df_lost, id_cols = c("site_code", "article_code"),names_from = "month_year", values_from =  "lost_sale_value",values_fn = sum)
  pv_lost_sale_rev[is.na(pv_lost_sale_rev)]<-0
  pv_lost_sale_qty <- pivot_wider(data = df_lost, id_cols = c("site_code", "article_code"),names_from = "month_year", values_from =  "lost_sale_qty",values_fn = sum)
  pv_lost_sale_qty[is.na(pv_lost_sale_qty)]<-0
  
  
  
  pv_lost_sale_rev<-pivot_longer(data=pv_lost_sale_rev,!c(site_code,article_code), names_to = "month", values_to = "lost_sales_rev")
  
  pv_lost_sale_qty<-pivot_longer(data=pv_lost_sale_qty,!c(site_code,article_code), names_to = "month", values_to = "lost_sales_qty")
  
  store_revenue_temp<-pivot_longer(data=store_revenue,!c(site_code,article_code,dept_code,section_code), names_to = "month", values_to = "sales")
  
  store_qty_temp<-pivot_longer(data=store_qty,!c(site_code,article_code,dept_code,section_code), names_to = "month", values_to = "qty")
  
  
  #map the lost_sale and revenue data
  pv_lost_sale_rev<-inner_join(pv_lost_sale_rev,store_revenue_temp,by=c("site_code","article_code",
                                                                        "month"))
  #adding the lost-sales value to actual sales
  pv_lost_sale_rev$sales<-pv_lost_sale_rev$sales+pv_lost_sale_rev$lost_sales_rev
  
  
  #map the lost_qty and revenue data
  pv_lost_sale_qty<-inner_join(pv_lost_sale_qty,store_qty_temp,by=c("site_code","article_code",
                                                                      "month"))
  #adding the lost-sales value to actual sales
  pv_lost_sale_qty$qty<-pv_lost_sale_qty$qty+pv_lost_sale_qty$lost_sales_qty
  
  
  
  
  #converting the sales_rev to demand
  df_lost_demand<-pivot_wider(data = pv_lost_sale_rev, 
                              id_cols = c("site_code","article_code","dept_code","section_code"
                                                                   ),names_from = "month", 
                              values_from =  "sales", values_fn = sum)
  ls_demand_rev[[yr_ind]]<-df_lost_demand
  
  
  #converting the sales_qty to demand
  df_lost_qty<-pivot_wider(data = pv_lost_sale_qty, 
                           id_cols = c("site_code","article_code","dept_code","section_code"
                                                                ),names_from = "month", 
                           values_from =  "qty", values_fn = sum)
  ls_demand_qty[[yr_ind]]<-df_lost_qty
  
  yr_ind<-yr_ind+1
  
  
}



#combing the demand for all the years
sale_rev_demand<-CBINDLIST_DEMAND(ls_demand_rev)
sale_qty_demand<-CBINDLIST_DEMAND(ls_demand_qty)
sale_rev_demand[is.na(sale_rev_demand)]<-0
sale_qty_demand[is.na(sale_qty_demand)]<-0

#imputing the missing str-articles & month for sales_rev_demand using store_revenue (actual store sales revenue)
sale_rev_demand<- MISSING_DATA(store_revenue, sale_rev_demand, fcst_months)

#imputing the missing str-articles & month for sale_qty_demand using store_qty (actual store sales quantity)
sale_qty_demand<- MISSING_DATA(store_qty, sale_qty_demand, fcst_months)


#removing duplicate rows if any from the sale_rev_demand & sale_qty_demand
sale_rev_demand<- REMOVE_DUPLICATES(sale_rev_demand)
sale_qty_deamnd<- REMOVE_DUPLICATES(sale_qty_demand)



#writing the sale_rev_demand and sale_qty_demand in the output folder
fwrite(sale_rev_demand,file.path(path_proj,"PREPROCESSED_OPS","P1","ARTICLE_SALES",
                                  "P1_DEMAND_REV.csv"))
fwrite(sale_qty_demand,file.path(path_proj,"PREPROCESSED_OPS","P1","ARTICLE_SALES",
                                   "P1_DEMAND_QTY.csv"))








  
  
  
























