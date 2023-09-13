cat("Cleaning enviornment", "/n")
rm(list = ls())
gc()

#path of the project
path_proj<-"D:/products/SOUTH_FORECASTS"


#import the required libraries
library(dplyr)
library(readxl)
library(data.table)
library(tidyr)
library("stringr")
library(readr)


#tags available
article_tags<-c("ECO","REGULAR","POPULAR","PREMIUM","EXCLUSIVE")

#---------------------------------------Declaring Local-Functions-----------------------------------------------------------

##aggregate data at store article level
AGGREGATE_DATA_STORE_DEPT_LEVEL<-function(df_store_art)
{
  df_store_dept<-df_store_art[,lapply(.SD, sum, na.rm = TRUE),.(site_code,dept_code)]
  df_store_dept[,"article_code"]<-"article"
  df_store_dept<-df_store_dept %>% relocate(article_code,.before = dept_code)
  
  return(df_store_dept)
  
}

##function to bind multiple lists
CBINDLIST<-function(ls)
{
  df<-ls[[1]]
  if(length(ls)>1)
  {
    for(ind in 2:length(ls))
    {
      df<-left_join(df,ls[[ind]],by=c("article_code","dept_code"))
    }
  }
  return(df)
}


#this function is to check whether there are regional matches along the three regional parameters
#regional_parameters<-region,tier,festival
##variable desc: store:"new-store", df_new_store_mstr:"store_mstr_table", p1_stores:"old_stores"
CHECK_REGION_MATCHES<-function(store_code,nso_stores,p1_stores)
{

  ##new_store for which proxy calculation needs to be performed
  df_new_store<-nso_stores[which(nso_stores$site_code==store_code),]
  print(df_new_store)
 
  #here we are checking whether there are any stores which has matching regional parameters with the new store
  if(is.na(df_new_store$festival))
  {
    df_regional_proxy<-p1_stores[which(df_new_store$region==p1_stores$region  
                                       & df_new_store$tier==p1_stores$tier),]
  }
  else
  {
    
    df_regional_proxy<-p1_stores[which(df_new_store$region==p1_stores$region  
                                       & df_new_store$tier==p1_stores$tier
                                       & df_new_store$festival==p1_stores$festival),]
  }  
  
  
  return(df_regional_proxy)
}


##function to extract article-tags
EXTRACT_ARTICLE_TAG<-function(df_sales,article_tags)
{
  
  df_sales<-as.data.frame(df_sales)
  df_sales$article_type<-"NA"
  
  for(atag in article_tags)
  {
    df_sales[grep(atag,df_sales$article_name),]$article_type<-atag
  }
  
  df_sales<-df_sales %>% relocate(article_type, .after=article_code)
  return(df_sales)
}


##Common-store match parameters both for NSO-and P2 stores
COMMON_STORE_MATCHES<-function(df_proxy_stores,df_new_store)
{
  
  match_count<-as.integer(df_proxy_stores$`fdu/non-fdu`==df_new_store$`fdu/non-fdu`)+
    
                as.integer(df_proxy_stores$store_format==df_new_store$store_format)+
                as.integer((df_proxy_stores$apparel_area==df_new_store$apparel_area)|
                             ((df_proxy_stores$apparel_area<=df_new_store$apparel_area*(1+sqft_allowance)) 
                              & df_proxy_stores$apparel_area>=df_new_store$apparel_area)|
                             (df_proxy_stores$apparel_area>=df_new_store$apparel_area*(1-sqft_allowance) 
                              &  df_proxy_stores$apparel_area<df_new_store$apparel_area))
                
  return(match_count)
}
  
##Function to Check if there are greater than 4 matches for the NSO stores with filtered-stores with regional-matches
NSO_STORE_MATCHES<-function(df_new_store,df_proxy_stores,df_str_mstr)
{
  ##if none of the stores have all the regional parameters matching
  ##find which are the stores having maximum number of matches
  if(length(df_proxy_stores)==0)
  {
    df_proxy_stores<-MAXIMUM_NSO_STORE_MATCHES(df_str_mstr,df_new_store)
  }
  else
  { 
    df_proxy_stores$match_count<-COMMON_STORE_MATCHES(df_proxy_stores,df_new_store)
    ##checking if there are greater than 4 matches for NSO-stores
    df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count>=2),]
    ##if the none of the stores satisfy the matching criteria, check which are the stores which have maximum overall-matches
    if(nrow(df_proxy_stores)==0)
    {
      df_proxy_stores<-MAXIMUM_NSO_STORE_MATCHES(df_str_mstr,df_new_store)
    }
    else
    {
      df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count==max(df_proxy_stores$match_count)),]
    }
  }
  
  
  return(df_proxy_stores)
}

##function to check which are the stores which have maximum number of matches with NSO-stores
MAXIMUM_NSO_STORE_MATCHES<-function(df_proxy_stores,df_new_store)
{
  
  df_proxy_stores$match_count<-COMMON_STORE_MATCHES(df_proxy_stores,df_new_store)+
                               as.integer(df_proxy_stores$region==df_new_store$region)+
                               as.integer(df_proxy_stores$tier==df_new_store$tier)
  ##if festival for the NSO stores is available
  if(!is.na(df_new_store$festival))
  {
    df_proxy_stores$match_count<-df_proxy_stores$match_count+as.integer(df_proxy_stores$festival==df_new_store$festival)
  }
  ##we choose those stores as proxy store which has maximum of parameter matches
  df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count==max(df_proxy_stores$match_count)),]
  
  return(df_proxy_stores)
  
}

#perform the calculation for NSO stores
NSO_CALCULATION<-function(df_proxy,df_new_store,p1_sales_rev,nso_sales_rev,start_date,end_date)
{
  ##filtering out the sales data for the proxy stores
  df_proxy_sales<-p1_sales_rev[which(p1_sales_rev$site_code %in% df_proxy$site_code),]
  
  ##calculating the turnover-multiplier
  multiplier<-(df_new_store$sqft)/(sum(df_proxy$sqft)/nrow(df_proxy))
  ##checking if the NSO store has already being opened
  if(nrow(nso_sales_rev)>0 & df_new_store$site_code %in% unique(nso_sales_rev$site_code))
  {
    #getting the store revenue details for the nso-store
    df_missing_data<-nso_sales_rev[which(nso_sales_rev$site_code==df_new_store$site_code),]
    #checking which are the months for which the data is available
    avai_data_ind<-which(colSums(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))])!=0)
    avai_data<-colnames(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))])[avai_data_ind]
    #capturing column names in the variable cols
    missing_months<-colnames(df_missing_data[,grep(start_date,colnames(df_missing_data)):(grep(end_date,colnames(df_missing_data))-length(avai_data))])
    #articles currently available in the nso-stores
    avai_articles<-unique(df_missing_data$article_code)
  }
  else
  {
    #if the nso-store isn't open yet then data missing for all the months
    missing_months<-colnames(nso_sales_rev[,grep(start_date,colnames(nso_sales_rev)):grep(end_date,colnames(nso_sales_rev))])
  }
  ##keeping only missing_months for which the proxy sales calculation
  df_proxy_sales<-subset(df_proxy_sales,select=c("dept_code","article_code",missing_months))
  df_proxy_sales[is.na(df_proxy_sales)]<-0
  df_proxy_sales<-setDT(df_proxy_sales)
  ##grouping by on the basis of dept-article
  df_proxy_sales<-df_proxy_sales[,lapply(.SD, sum, na.rm = TRUE),.(dept_code,article_code)]
  ##multiplying the month-on month avg-sale with the multiplier
  df_proxy_sales[,3:ncol(df_proxy_sales)]<-df_proxy_sales[,3:ncol(df_proxy_sales)]*multiplier/nrow(df_proxy)
  #checking if the new-store is available in the store-master 
  if(df_new_store$site_code %in% unique(nso_sales_rev$site_code))
  {
    df_missing_data<-setDT(df_missing_data)
    #selecting the col with available data for a particular store
    df_missing_data<-select(df_missing_data,c("article_code","dept_code",all_of(avai_data)))
    #picking only those articles which are available for NSO-STORES
    df_proxy_sales<-df_proxy_sales[which(df_proxy_sales$article_code %in% avai_articles),]
    #join  the calculated-proxy sales for the store and the actual available data
    df_missing_data<-left_join(df_proxy_sales,df_missing_data,by=c("article_code","dept_code"))
  }
  else
  {
    #if the store is about to open then similarly allocating the proxy-sales to all the months
    df_missing_data<-df_proxy_sales
  }
  #allocating a site_code col which contains the site_code number for the particular store
  df_missing_data$site_code<-df_new_store$site_code
  df_missing_data<-df_missing_data %>% relocate(site_code,.before = dept_code)
  
  return(df_missing_data)
  
}

##calculating the ASP for each department
ASP_CALCULATION<-function(df_sales_qty,df_sales_val,start_period,end_period)
{
  ##converting the dataframe to datatable
  df_sales_qty<-setDT(df_sales_qty)
  df_sales_val<-setDT(df_sales_val)
  #calculating the total_qty on store_dept_article level 
  df_sales_qty$total_qty<-rowSums(df_sales_qty[,grep(start_period,colnames(df_sales_qty)):grep(end_period,colnames(df_sales_qty))])
  df_sales_qty<-df_sales_qty[order(site_code),.("total_qty" = sum(`total_qty`)),.(site_code)]
  #calculating the total_sales on store_dept_article level
  df_sales_val$total_sales<-rowSums(df_sales_val[,grep(start_period,colnames(df_sales_val)):grep(end_period,colnames(df_sales_val))])
  df_sales_val<-df_sales_val[order(site_code),.("total_sales" = sum(`total_sales`)),.(site_code)]
  #perform left-join by Store
  df_sales_val<-left_join(df_sales_val,df_sales_qty,by=c("site_code"))
  #calculating the asp
  df_sales_val$asp<-(df_sales_val$total_sales)/(df_sales_val$total_qty)
  
  return(df_sales_val)
}

#function which performs seasonal_share_calculation
#ss yr-is the yr 2019 which happens to be the ss of the yrs avai for forecast(with no covid-impact)
SEASONAL_SHARE_CALCULATION<-function(df_store_sales,
                                     df_dept_season,
                                     start_period,
                                     end_period,
                                     ss_start,
                                     ss_end)
{
  ##converting the data-frame to data-table
  df_store_sales<-setDT(df_store_sales)
  #performing total sales calculation for ss month
  df_store_sales$ss_yr_sales<-rowSums(df_store_sales[,grep(ss_start,colnames(df_store_sales)):grep(ss_end,colnames(df_store_sales))])
  #mapping with the season
  df_store_sales<-left_join(df_store_sales,df_dept_season,by=c("dept_code"))
  #joining the season_tagged_dept with the store associated data
  df_store_sales<-df_store_sales[,c("site_code",
                                    "dept_code",
                                    "article_code",
                                    "season",
                                    "ss_yr_sales")]
  ##getting the seasonal sales for all the seasons in the stores
  df_store_sales<-df_store_sales[order(site_code),.("ss_yr_sales" = sum(`ss_yr_sales`)),.(site_code,season)]
  df_store_sales<-df_store_sales %>% pivot_wider(names_from = "season", values_from = "ss_yr_sales", values_fn = sum)
  df_store_sales[is.na(df_store_sales)]<-0
  ##calculating the share of each season
  df_store_sales[,2:ncol(df_store_sales)][which(rowSums(df_store_sales[,2:ncol(df_store_sales)])!=0),]<-df_store_sales[,2:ncol(df_store_sales)][which(rowSums(df_store_sales[,2:ncol(df_store_sales)])!=0),]/rowSums(df_store_sales[,2:ncol(df_store_sales)][which(rowSums(df_store_sales[,2:ncol(df_store_sales)])!=0),])
  ##calculating the summer_season share
  df_store_sales$ss_share<-df_store_sales$SUMMER+df_store_sales$BASIC
  
  return(df_store_sales)
  
}


##join seasonal_share and asp calculation
JOIN_SEASON_SALES<-function(df_dept_season,df_sales_val,df_sales_qty,
                            df_store_mstr,start_period,
                            end_period,ss_start,ss_end)
{
  #ASP function called out to calculate the ASP value
  df_sales_asp<-ASP_CALCULATION(df_sales_qty,df_sales_val,start_period,end_period)
  #tagging the season corresponding to stores
  df_store_season_share<-SEASONAL_SHARE_CALCULATION(df_sales_val,df_dept_season,start_period,end_period,ss_start,ss_end)
  #mapping the summer_seasonal share and asp
  df_sales_asp<-left_join(df_sales_asp,df_store_season_share,by=c("site_code"))
  #mapping the asp, season_share with the other attributes
  df_store_info<-left_join(df_store_mstr,df_sales_asp,by=c("site_code"))
  
  return(df_store_info)
}


##Function to Check if there are greater than 4 matches for the NSO stores with filtered-stores with regional-matches
P2_STORE_MATCHES<-function(df_new_store,df_proxy_stores,df_str_mstr)
{
  ##if none of the stores have all the regional parameters matching
  ##find which are the stores having maximum number of matches
  if(length(df_proxy_stores)==0)
  { 
    df_proxy_stores<-MAXIMUM_P2_STORE_MATCHES(df_str_mstr,df_new_store)
  }
  else
  { 
    df_proxy_stores$match_count<-COMMON_STORE_MATCHES(df_proxy_stores,df_new_store)+
                                as.integer((df_proxy_stores$ss_share==df_new_store$ss_share)|
                                             (df_proxy_stores$ss_share<=df_new_store$ss_share*(1+ss_allowance) & df_proxy_stores$ss_share>=df_new_store$ss_share)|
                                             (df_proxy_stores$ss_share>=df_new_store$ss_share*(1-ss_allowance) & df_proxy_stores$ss_share<df_new_store$ss_share))+
                                as.integer((df_proxy_stores$asp==df_new_store$asp)|
                                             (df_proxy_stores$asp<=df_new_store$asp*(1+asp_allowance) & df_proxy_stores$asp>=df_new_store$asp)|
                                             (df_proxy_stores$asp>=df_new_store$asp*(1-asp_allowance) & df_proxy_stores$asp<df_new_store$asp))
    ##checking if there are greater than 5 matches for P2-stores
    df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count>=3),]
    
    ##if the none of the stores satisfy the matching criteria, check which are the stores which have maximum overall-matches
    if(nrow(df_proxy_stores)==0)
    {
      df_proxy_stores<-MAXIMUM_P2_STORE_MATCHES(df_str_mstr,df_new_store)
    }
    else
    {
      df_proxy_stores[which(is.na(df_proxy_stores$match_count)),]<-0
      df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count==max(df_proxy_stores$match_count)),]
    }
  }
  return(df_proxy_stores)
}


##function to check which are the stores which have maximum number of matches with P2-store
MAXIMUM_P2_STORE_MATCHES<-function(df_proxy_stores,df_new_store)
{
  
  
  df_proxy_stores$match_count<-COMMON_STORE_MATCHES(df_proxy_stores,df_new_store)+
                              as.integer(df_proxy_stores$region==df_new_store$region)+
                              as.integer(df_proxy_stores$tier==df_new_store$tier)+
                              as.integer((df_proxy_stores$ss_share==df_new_store$ss_share)|
                                           (df_proxy_stores$ss_share<=df_new_store$ss_share*(1+ss_allowance) & df_proxy_stores$ss_share>=df_new_store$ss_share)|
                                           (df_proxy_stores$ss_share>=df_new_store$ss_share*(1-ss_allowance) & df_proxy_stores$ss_share<df_new_store$ss_share))+
                              as.integer((df_proxy_stores$asp==df_new_store$asp)|
                                           (df_proxy_stores$asp<=df_new_store$asp*(1+asp_allowance) & df_proxy_stores$asp>=df_new_store$asp)|
                                           (df_proxy_stores$asp>=df_new_store$asp*(1-asp_allowance) & df_proxy_stores$asp<df_new_store$asp))+
                             as.integer(df_proxy_stores$festival==df_new_store$festival)
                                           
  
  
  df_proxy_stores[which(is.na(df_proxy_stores$match_count)),]<-0
  
  #we choose those stores as proxy store which has maximum of parameter matches
  df_proxy_stores<-df_proxy_stores[which(df_proxy_stores$match_count==max(df_proxy_stores$match_count)),]
  
 
  
  
 return(df_proxy_stores)
  
}

#P2_store calculation
P2_PROXY_SECTION_SHARE<-function(sales_rev,start_date,end_date,df_store)
{
  #extract article
  sales_rev<-EXTRACT_ARTICLE_TAG(sales_rev,article_tags)
  sales_tag<-colnames(sales_rev)
  sales_rev$total_sales<-rowSums(sales_rev[,grep(start_date,colnames(sales_rev)):grep(end_date,colnames(sales_rev))])
  sales_sec<-pivot_wider(data = sales_rev, id_cols = c("section_code"),names_from = "article_type", values_from =  "total_sales", values_fn = sum)
  sales_sec[is.na(sales_sec)]<-0
  sales_sec$total_sales<-rowSums(sales_sec[,2:ncol(sales_sec)])
  sales_sec[,2:(ncol(sales_sec)-1)]<-sales_sec[,2:(ncol(sales_sec)-1)]/ sales_sec$total_sales
  sales_sec$total_sales<-sales_sec$total_sales/nrow(df_store)
  return(sales_sec)
  
}

P2_PROXY_CALCULATION<-function(p1_sales_rev,p2_sales_rev,start_date,end_date,df_proxy_store,df_new_store)
{
  
  #getting the store revenue details for the nso-store
  df_missing_data<-p2_sales_rev
  #checking which are the months for which the data is available
  avai_data_ind<-which(colSums(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))])!=0)
  avai_months<-colnames(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))])[avai_data_ind]
  #capturing column names in the variable cols
  cols<-colnames(df_missing_data)
  #getting the months for which data is unavailable
  missing_months<-colnames(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))])[!colnames(df_missing_data[,grep(start_date,colnames(df_missing_data)):grep(end_date,colnames(df_missing_data))]) %in% avai_months]
  #getting the section share for p1 stores
  p2_section_share<-P2_PROXY_SECTION_SHARE(p2_sales_rev,avai_months[1],avai_months[length(avai_months)],df_new_store)
  colnames(p2_section_share)[ncol(p2_section_share)]<-paste0("p2",colnames(p2_section_share)[ncol(p2_section_share)])

  
  #getting the section share for p2 stores
  p1_section_share<-P2_PROXY_SECTION_SHARE(p1_sales_rev,avai_months[1],avai_months[length(avai_months)],df_proxy_store)
  colnames(p1_section_share)[ncol(p1_section_share)]<-paste0("p1",colnames(p1_section_share)[ncol(p1_section_share)])
 
  #getting the article-type share for section-article_name
  p1_section_share<-pivot_longer(data=p1_section_share,!c(section_code,p1total_sales), names_to = "article_type", values_to = "p1_art_type_share")
  p2_section_share<-pivot_longer(data=p2_section_share,!c(section_code,p2total_sales), names_to = "article_type", values_to = "p2_art_type_share")
  
  #calculating the section-share for both p1 and p2 stores and joining the two for calculating multiplier
  section_share<-left_join(p2_section_share, p1_section_share, by=c("section_code","article_type"))
  section_share[is.na(section_share)]<-0
  
  #calculating the turnover_multiplier and multiplier
  section_share$turnover_multiplier<-section_share$p2total_sales/section_share$p1total_sales
  section_share$multiplier<-section_share$p2_art_type_share/section_share$p1_art_type_share

  #in case the turnover_multiplier or the multiplier is infinity then replace them by 0s
  section_share[which(is.infinite(section_share$turnover_multiplier)),]$turnover_multiplier=0
  section_share[which(is.infinite(section_share$multiplier)),]$multiplier=0
    
  
   #getting the avg-sales for all the proxy-stores 
   p1_sales_rev<-setDT(p1_sales_rev)
   #extract article tags from the article-name on p1-sales-rev
   p1_sales_rev<-EXTRACT_ARTICLE_TAG(p1_sales_rev,article_tags)
   #converting p1-sales revenue into a data-table
   p1_sales_rev<-setDT(p1_sales_rev)
   #aggregating the sales data of all the proxy-stores
   p1_sales_rev<-subset(p1_sales_rev,select=c("section_code","dept_code","article_code","article_name","article_type",missing_months))[,lapply(.SD, sum, na.rm = TRUE),.(section_code,dept_code,article_code,article_name,article_type)]
   #taking the final average of the sales-calculation for proxy-stores
   p1_sales_rev[,grep(missing_months[1],colnames(p1_sales_rev)):grep(missing_months[length(missing_months)],colnames(p1_sales_rev))]<-p1_sales_rev[,grep(missing_months[1],colnames(p1_sales_rev)):grep(missing_months[length(missing_months)],colnames(p1_sales_rev))]/nrow(df_proxy_store)
   #mapping the sale_data of proxy_stores with multipliers
   p1_sales_rev<-inner_join(p1_sales_rev,section_share[,c("section_code","article_type",
                                                        "turnover_multiplier","multiplier")],by=c("section_code",
                                                                                                 "article_type"))
  
  #multiplying the average sales with the turnover-multiplier and the multiplier to make the averaged sales appropriate for p2-store  
  p1_sales_rev[,grep(missing_months[1],colnames(p1_sales_rev)):                        
                  grep(missing_months[length(missing_months)],colnames(p1_sales_rev))]<-p1_sales_rev[,grep(missing_months[1],colnames(p1_sales_rev)):
                                                                                                     grep(missing_months[length(missing_months)],colnames(p1_sales_rev))]*p1_sales_rev$turnover_multiplier*p1_sales_rev$multiplier
  
  #choosing the section, dept_code, article_code and data of the available months 
  p2_sales_rev<-subset(p2_sales_rev,select=c("section_code","dept_code","article_code",avai_months))
  p1_sales_rev<-subset(p1_sales_rev,select=c("section_code","dept_code","article_code",missing_months))
  
  ##mapping the missing_months calculation with the available_data
  p2_sales_rev<-inner_join(p1_sales_rev,p2_sales_rev,by=c("section_code","dept_code","article_code"))
  p2_sales_rev$site_code<-df_new_store$site_code
  p2_sales_rev<-p2_sales_rev %>% relocate(site_code,.before = dept_code)

  return(p2_sales_rev)

  
}

##NSO_stores calculation for all stores
NSO_CALCULATION_ALL_STORES<-function(nso_stores,p1_stores,p1_sales_rev,nso_sales_rev,start_date,end_date,path_proj)
{
 
 #cleaning the site_codes so that it doesn't have any ambiguous site-codes    
 nso_stores$site_code<-str_replace_all(nso_stores$site_code ,"/", "-")
 p1_stores$site_code<-str_replace_all(p1_stores$site_code ,"/", "-")
 ls_nso<-list()
 ind_nso<-1
  
  for(str in 1:length(unique(nso_stores$site_code)))
  {
    ##filtered-stores details after regional matches
    df_region<-CHECK_REGION_MATCHES(nso_stores$site_code[[str]],nso_stores,p1_stores)
    
    print(df_region)
    
    ##proxy-stores which have maximum matches
    df_proxy<-NSO_STORE_MATCHES(nso_stores[str,],df_region,p1_stores)
    
    print(df_proxy)
    
    ##performing sales calculation-for new stores
    df_new_store<-NSO_CALCULATION(df_proxy,nso_stores[str,],p1_sales_rev,nso_sales_rev,start_date,end_date)
    #taking the newly calculation for the particular nso-store in the list
    ls_nso[[ind_nso]]<-df_new_store
    
    ind_nso<-ind_nso+1
    
   
    
  } 
 #row-binding the calculation for all the nso-stores
 nso_proxy_sales<-rbindlist(ls_nso)
 return(nso_proxy_sales) 
  
}

P2_CALCULATION_ALL_STORES<-function(df_dept_season,p1_sales_rev,p2_sales_rev,p1_sales_qty,
                                    p2_sales_qty,p2_stores,p1_stores,mapping_article_code,start_date,end_date,
                                    ss_start_date,ss_end_date,fcst_months)
{
  #mapping the asp and the seasonal share to the current store mstr
  p2_stores<-JOIN_SEASON_SALES(df_dept_season,p2_sales_rev,p2_sales_qty,p2_stores,start_date,end_date,ss_start_date,ss_end_date)
  p1_stores<-JOIN_SEASON_SALES(df_dept_season,p1_sales_rev,p1_sales_qty,p1_stores,start_date,end_date,ss_start_date,ss_end_date)
  p2_stores<-p2_stores[which(p2_stores$site_code %in% p2_sales_rev$site_code),]
  ls_p2<-list()
  ls_proxy_store<-list()
  ind_p2<-1
  for(str in 1:length(p2_stores$site_code))
  {
  
  #performing the regional level filtering
  df_region<-CHECK_REGION_MATCHES(p2_stores$site_code[[str]],p2_stores,p1_stores)
  ##finding the p1-stores(proxies) with max matches with the p2-store
  df_p2_proxy<-P2_STORE_MATCHES(p2_stores[str,],df_region,p1_stores)
  ##allocating proxy_sales to the store
  p2_sales_rev_temp<-left_join(p2_sales_rev[which(p2_sales_rev$site_code==p2_stores$site_code[[str]]),],mapping_article_code,by=c("article_code"))
  p1_sales_rev_temp<-left_join(p1_sales_rev[which(p1_sales_rev$site_code %in% df_p2_proxy$site_code),],mapping_article_code,by=c("article_code"))
  
  p2_sales<-P2_PROXY_CALCULATION(p1_sales_rev_temp,p2_sales_rev_temp,start_date,end_date,df_p2_proxy,p2_stores[which(p2_stores$site_code==p2_stores$site_code[[str]]),])
  #writing the file in an appropriate folder
  p2_sales<-subset(p2_sales,select=c("section_code","site_code","dept_code",
                                     "article_code",fcst_months
                                     ))
  ls_p2[[ind_p2]]<-p2_sales
  
  ind_p2<-ind_p2+1
  

  }
  
  p2_proxy_sales<-rbindlist(ls_p2)
  
  return(p2_proxy_sales)
}


#renaming the columns of store-mstr
RENAME_STR_MSTR<- function(str_mstr)
{
  colnames(str_mstr)<- c("store", "site_code", "st_type", "tier", "op_date", "sqft",
                          "store_format", "region", "zone", "fdu/non-fdu", "apparel_area", "festival")
  
  return(str_mstr)
}

#renaming the columns of sales-rev files
RENAME_SALES_REV<- function(sales_data, ip_months)
{
  
  colnames(sales_data)<- c("site_code","article_code","dept_code","section_code",ip_months)
  
  return(sales_data)
}


#reading the required files
user_input<- read_excel(list.files(file.path(path_proj,"ALLOWANCE_INPUT"), full.names= TRUE))
mt_user<- read_excel(list.files(file.path(path_proj,"USER_INPUT"), full.name=TRUE))

#getting the allowances as input from the user
asp_allowance<-user_input[grep("asp_allowance",user_input$parameters),]$allowances
sqft_allowance<-user_input[grep("sqft_allowance",user_input$parameters),]$allowances
seasonal_cut_off<-user_input[grep("seasonal_cut_off",user_input$parameters),]$allowances
ss_allowance<-user_input[grep("ss_allowance",user_input$parameters),]$allowances

#reading the respective data of p1, p2 and nso
p1_sales_rev<- fread(list.files(file.path(path_proj, 
                               "PREPROCESSED_OPS", 
                               "P1", "ARTICLE_SALES"),full.names=TRUE)[
                                grep("P1_DEMAND_REV",
                                list.files(file.path(path_proj, 
                                "PREPROCESSED_OPS", 
                                "P1", "ARTICLE_SALES"),full.names=TRUE))])

p1_sales_qty<- fread(list.files(file.path(path_proj, 
                                "PREPROCESSED_OPS", 
                                "P1", "ARTICLE_SALES"),full.names=TRUE)[
                                grep("P1_DEMAND_QTY",
                                list.files(file.path(path_proj, 
                                "PREPROCESSED_OPS", 
                                "P1", "ARTICLE_SALES"),full.names=TRUE))])



p2_sales_rev<-fread(list.files(file.path(path_proj,
                              "COMPANY_LEVEL_SEPARATED_DATA",
                              "P2","P2_SALES"),full.names=TRUE)
                              [grep("P2_SALES_REV",
                              list.files(file.path(path_proj,
                              "COMPANY_LEVEL_SEPARATED_DATA",
                              "P2","P2_SALES")))])

p2_sales_qty<-fread(list.files(file.path(path_proj,
                               "COMPANY_LEVEL_SEPARATED_DATA",
                                "P2","P2_SALES"),full.names=TRUE)
                                [grep("P2_SALES_QTY",
                                list.files(file.path(path_proj,
                                "COMPANY_LEVEL_SEPARATED_DATA",
                                 "P2","P2_SALES")))])

nso_sales_rev<-fread(list.files(file.path(path_proj,
                               "COMPANY_LEVEL_SEPARATED_DATA",
                               "NSO","NSO_SALES"),full.names=TRUE)
                               [grep("NSO_SALES_REV",
                               list.files(file.path(path_proj,
                               "COMPANY_LEVEL_SEPARATED_DATA",
                               "NSO","NSO_SALES")))])
  
                    
nso_sales_qty<-fread(list.files(file.path(path_proj,
                              "COMPANY_LEVEL_SEPARATED_DATA",
                               "NSO","NSO_SALES"),full.names=TRUE)
                               [grep("NSO_SALES_QTY",
                                list.files(file.path(path_proj,
                                "COMPANY_LEVEL_SEPARATED_DATA",
                                "NSO","NSO_SALES")))])


#replacing-all the nas if present by 0s
p1_sales_qty[is.na(p1_sales_qty)]<-0
p1_sales_rev[is.na(p1_sales_rev)]<-0
p2_sales_qty[is.na(p2_sales_qty)]<-0
p2_sales_rev[is.na(p2_sales_rev)]<-0
nso_sales_qty[is.na(nso_sales_qty)]<-0
nso_sales_rev[is.na(nso_sales_rev)]<-0



#reading the p1-p2 and nso-stores masters
p1_stores<-fread(list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA","P1","P1_STR_MSTR"), full.names=TRUE))

p2_stores<-fread(list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA","P2","P2_STR_MSTR"), full.names=TRUE))

nso_stores<-fread(list.files(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA","NSO","NSO_STR_MSTR"), full.names=TRUE))

#nso_stores<- fread("D:/products/FORECAST_FINAL_PROJECT - SOUTH/COMPANY_LEVEL_SEPARATED_DATA/NSO/NSO_STR_MSTR/NSO_STR_MSTR_V3.csv")
#renaming the columns in the store-mstr
p1_stores<- RENAME_STR_MSTR(p1_stores)
p2_stores<- RENAME_STR_MSTR(p2_stores)
nso_stores<- RENAME_STR_MSTR(nso_stores)

#making sure the unique stores are only retained in the store_mstr
p1_stores$concat<- paste0(p1_stores$store,"_",p1_stores$site_code)
p2_stores$concat<- paste0(p2_stores$store,"_",p2_stores$site_code)
nso_stores$concat<- paste0(nso_stores$store,"_",nso_stores$site_code)
#retaining only the unique elements of the series
p1_stores<- p1_stores[!duplicated(p1_stores$concat)]
p2_stores<- p2_stores[!duplicated(p2_stores$concat)]
nso_stores<-nso_stores[!duplicated(nso_stores$concat)]

p1_stores<- p1_stores[,-c("concat")]
p2_stores<- p2_stores[,-c("concat")]
nso_stores<- nso_stores[,-c("concat")]


#getting the sequence of i/p months
ip_months <- format(seq.Date(as.Date(mt_user$START_PERIOD),as.Date(mt_user$END_PERIOD), by = "month"),
                    format = "%b-%Y")

#renaming the columns in standardized-form
p2_sales_rev<- RENAME_SALES_REV(p2_sales_rev, ip_months)
p2_sales_qty<- RENAME_SALES_REV(p2_sales_qty, ip_months)
nso_sales_rev<- RENAME_SALES_REV(nso_sales_rev, ip_months)
nso_sales_qty<- RENAME_SALES_REV(nso_sales_qty, ip_months)
p1_sales_rev<- RENAME_SALES_REV(p1_sales_rev, ip_months)
p1_sales_qty<- RENAME_SALES_REV(p1_sales_qty, ip_months)

#reading the file with dept-season mapping
df_dept_season<-fread(list.files(file.path(path_proj, "MAPPINGS"),full.names=TRUE)[grep("DEPT_INFO",
                                                                                        list.files(file.path(path_proj, 
                                                                                        "MAPPINGS")))])
df_dept_season<- df_dept_season[,c("dept_code","season")]

#reading the article to code mapping
mapping_article_code<-fread(list.files(file.path(path_proj, "MAPPINGS"),full.names=TRUE)[grep("ARTICLE_MASTER",
                                                                                              list.files(file.path(path_proj, 
                                                                                              "MAPPINGS")))])

colnames(mapping_article_code)<-c("article_name","article_code")


#checking if there are any stores marked as nso in the store-master
if(nrow(nso_stores)!=0)
{  
#function which does the proxy-sales calculation for all the nso-stores
nso_proxy_sales<-NSO_CALCULATION_ALL_STORES(nso_stores,p1_stores,p1_sales_rev[,-c("section_code")],
                           nso_sales_rev[,-c("section_code")],format(mt_user$START_PERIOD,"%b-%Y"),format(mt_user$END_PERIOD,"%b-%Y"),
                           path_proj)
nso_str_dept<-AGGREGATE_DATA_STORE_DEPT_LEVEL(subset(nso_proxy_sales,select=-c(article_code)))

}

#month used for forecasting
start_date<-format(as.Date(mt_user$START_PERIOD),"%b-%Y")
end_date<-format(as.Date(mt_user$END_PERIOD),"%b-%Y")
rem_start_date<-format(as.Date(mt_user$REM_START_PERIOD),"%b-%Y")
rem_end_date<-format(as.Date(mt_user$REM_END_PERIOD),"%b-%Y")
# getting sequence of fcst_ip_months 
fcst_months <- format(seq.Date(as.Date(mt_user$START_PERIOD),as.Date(mt_user$END_PERIOD), by = "month"),
                      format = "%b-%Y")
#function which does the proxy-sales calculation for all the p2-stores
ss_start_date<-format(as.Date(mt_user$SS_START_DATE),"%b-%Y")
ss_end_date<-format(as.Date(mt_user$SS_END_DATE),"%b-%Y")

p1_stores$site_code<-as.character(p1_stores$site_code)
p2_stores$site_code<-as.character(p2_stores$site_code)
p1_sales_rev$site_code<-as.character(p1_sales_rev$site_code)
p2_sales_rev$site_code<-as.character(p2_sales_rev$site_code)
p1_sales_qty$site_code<-as.character(p1_sales_qty$site_code)
p2_sales_qty$site_code<-as.character(p2_sales_qty$site_code)



#checking if there are any stores marked as p2 in the data
if(nrow(p2_stores)!=0)
{  
p2_proxy_sales<-P2_CALCULATION_ALL_STORES(df_dept_season,p1_sales_rev,p2_sales_rev,p1_sales_qty,p2_sales_qty,p2_stores,p1_stores,mapping_article_code,
                          start_date,end_date,ss_start_date,ss_end_date,fcst_months)

#if p2-stores present then assembling them at store-dept level
p2_str_dept<-AGGREGATE_DATA_STORE_DEPT_LEVEL(subset(p2_proxy_sales,select=-c(section_code,article_code)))

fwrite(p2_str_dept, file.path(path_proj, "PREPROCESSED_OPS","P2", "DEPT_SALES", "P2_STR_DEPT.csv"))
fwrite(p2_proxy_sales, file.path(path_proj, "PREPROCESSED_OPS","P2", "ARTICLE_SALES", "P2_STR_ART.csv"))
}


#assembling the p1-store article data at store-dept level
p1_str_dept<-AGGREGATE_DATA_STORE_DEPT_LEVEL(subset(p1_sales_rev,select=-c(section_code,article_code)))


#combining all the strs dept
if(nrow(p2_stores)!=0 & nrow(nso_stores)!=0)
{
all_str_dept<- rbind(p1_str_dept, p2_str_dept, nso_str_dept)
}else if(nrow(p2_stores)==0 & nrow(nso_stores)!=0)
{
  all_str_dept<- rbind(p1_str_dept, nso_str_dept)
}else if(nrow(p2_stores)!=0 & nrow(nso_stores)==0)
{
  all_str_dept<- rbind(p1_str_dept, p2_str_dept)
}


#writing the str_dept files in appropriate locations
fwrite(p1_str_dept, file.path(path_proj, "PREPROCESSED_OPS","P1", "DEPT_SALES", "P1_STR_DEPT.csv"))

if(nrow(nso_stores)!=0)
{
fwrite(nso_str_dept, file.path(path_proj, "PREPROCESSED_OPS","NSO", "DEPT_SALES", "NSO_STR_DEPT.csv"))
}

if(nrow(p2_stores)!=0)
{
fwrite(nso_proxy_sales, file.path(path_proj, "PREPROCESSED_OPS","P2", "ARTICLE_SALES", "P2_STR_DEPT.csv"))
}

fwrite(all_str_dept, file.path(path_proj, "PREPROCESSED_OPS","ALL", "DEPT_SALES", "ALL_STR_DEPT.csv"))







