#install.packages("writexl")

#path of the project
path_proj<-"D:/products/SOUTH_FORECASTS"

#---- Sourcing Libraries and external files ----

#import the required libraries
library(dplyr)
library(data.table)
library(tidyr)
library("stringr")
library(readr)
library(lubridate)
library(stringr)
library("writexl")
library(readxl)



#getting the n months subsequent to the current month
ADD.MONTHS<-function(date,n) seq(date, by = "month", length.out = n)[n]
#getting the first month subsequent to the current month
ADD.MONTH<-function(date) ADD.MONTHS(date,2)
#function to calculate moving average
MOVING_AVERAGE<-function(dt_sales,dt_wt)
{
  #list to capture all the set of moving averages
  ma<-list()
  
  #getting all the unique_site_codes available in sales-data
  unique_site_codes<-unique(dt_sales$site_code)
  
  #getting the list of feature tables
  ls_features<-list()
  
  #index incremented over every iteration
  str_ind<-1
  
  for(str_code in unique_site_codes)
  {
    data<-dt_sales[which(dt_sales$site_code==str_code),]
    
    #for loop calculating the moving-average for all the months 
    for(sales in 2:(nrow(data)-1))
    {
      data[sales,"moving_average"]<-(data[(sales-1),'sales_rev']+data[sales,'sales_rev']+data[(sales+1),'sales_rev'])/3
    }
    
    #creating a feature table comprising of weights allocated to each month along with the moving average
    data<-left_join(data[2:(nrow(data)-1),],dt_wt,by=c("month_yr"))
    
    ls_features[[str_ind]]<-data
    
    str_ind<-str_ind+1
  }
  
  #creating a common data-frame consisting of the features
  dt_features<-rbindlist(ls_features)
  
  return(dt_features)
  
}
#prepare input table with features and output
INPUT_TABLE<-function(dt_festival,dt_str_past_sales,start_date,end_date)
{
  
  #getting the set of training months
  month_start_yr<-start_date
  month_end_yr<-end_date
  month_yrs<-format(seq.Date((month_start_yr-months(1)),(month_end_yr+months(1)), by = "month"),format = "%b-%Y")
  
  #getting the total-sales and the weight age allocated to festival-months
  dt_fest<-dt_festival[which(dt_festival$month_yr %in% month_yrs),]
  dt_monthly_sales<-dt_str_past_sales[which(dt_str_past_sales$month_yr %in% month_yrs),]
  
  dt_features<-MOVING_AVERAGE(dt_monthly_sales,dt_fest)
  
  
  
  return(dt_features)  
  
}


#function to get the sequence of dates
GET_SEQUENCE_OF_DATES<- function(start_date, end_date) 
{
  date_seq<-format(seq.Date(as.Date(start_date),as.Date(end_date), by = "month"),
                   format = "%b-%Y")
  
  return(date_seq)
}

#function to allocate weight as per days
#for the first 5 days: wt = 100, for the next 5 days (5+5=10 days): wt=110,  
#for the next 5 days (5+10=15 days): wt=121, for the last 5 days (15+5=20 days): wt=133.1
ALLOCATING_WTS_AS_PER_DAYS<- function(dt_fest, dt_festival, ls_wt)
{
  
  
  
  #festival based data-frame
  dt_fest<-pivot_longer(data=dt_fest,!c(Festival), names_to = "yr", values_to = "Date")
  dt_fest$Date<-as.Date(dt_fest$Date)
  
  
  
  #finding the months which lies 20-days before, 15-days before, 10 days before and 5-days before
  dt_fest[,"20_days_before"]<-format(dt_fest$Date-17,"%b-%Y")
  dt_fest[,"15_days_before"]<-format(dt_fest$Date-12,"%b-%Y")
  dt_fest[,"10_days_before"]<-format(dt_fest$Date-7,"%b-%Y")
  dt_fest[,"5_days_before"]<-format(dt_fest$Date-2,"%b-%Y")
  
  print(dt_fest)
  
  
  #converting the festival-names into lower-case
  dt_fest$Festival<- tolower(dt_fest$Festival)
  
  #create a weight table for  all the years-across all the months
  main_festivals<-unique(dt_fest$Festival)
  
  #converting dt_festival to data-frame
  dt_festival<- data.frame(dt_festival)
  
  
  for(fest in 1:length(main_festivals))
  {
    
    dt_fest_yr<-dt_fest[grep(main_festivals[[fest]],dt_fest$Festival),]
    
    
    dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`20_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]<-
      dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`20_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]+ls_wt[[1]]
    
    dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`15_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]<-
      dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`15_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]+ls_wt[[2]]
    
    dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`10_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]<-
      dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`10_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]+ls_wt[[3]]
    
    dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`5_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]<-
      dt_festival[which(dt_festival$month_yr %in% dt_fest_yr$`5_days_before`),grep(main_festivals[[fest]],colnames(dt_festival))]+ls_wt[[4]]
    
    
  }
  
  return(dt_festival)
  
}


#calculating the coefficients on using the features of the training table
#the features being taken are: the weights corresponding to each festival, weighted-moving average & sales-revenue for
#the store corresponding to that particular month
COEFFICIENTS_CALCULATE<- function(dt_training_features)
{
  #getting the unique store_codes in V-Mart
  unique_site_codes<-unique(dt_training_features$site_code)
  #list to capture the coefficients corresponding to all the stores
  ls_coeff<- list()
  ind<- 1
  
  #training the multiple linear regression model for all the stores
  #and getting the coefficients(multipliers) corresponding to each store
  for(store_code in unique_site_codes)
  {
    
    #getting the feature corresponding to each store
    dt_site_features<-dt_training_features[which(dt_training_features$site_code==store_code),]
    
    #training the multiple-linear regression model
    LR<-lm(formula = formula(dt_site_features[,3:ncol(dt_site_features)]), data = dt_site_features[,3:ncol(dt_site_features)])
    
    #capturing the intercept, coefficients for features and R_square and adjusted_R
    feature_names<- c("site_code","intercept", c(colnames(dt_site_features[,4:ncol(dt_site_features)])), "adjusted_R^2")
    valList<- c(store_code, c(unlist(LR$coefficients)), summary(LR)$adj.r.squared)
    valList[is.na(valList)]<- 0
    names(valList)<- feature_names
    
    ls_coeff[[ind]]<- valList
    
    
    ind<- ind+1
    
    
    
  }  
  
  
  str_coeff<-as.data.frame(do.call(rbind, ls_coeff))
  str_coeff[,2:ncol(str_coeff)]<-lapply(str_coeff[,2:ncol(str_coeff)],as.numeric)
  
  return(str_coeff)
  
}

#function to rename the column names for the data_frames with coefficients associated with different festivals
RENAME_FESTIVAL_COEFF_TBL<- function(fest_str_coeff)
{
  colnames(fest_str_coeff)[(grep("moving_average",colnames(fest_str_coeff)):(grep("adjusted",colnames(fest_str_coeff))-1))]<- paste0(colnames(fest_str_coeff)[grep("moving_average",colnames(fest_str_coeff)):
                                                                                                                                                                ((grep("adjusted",colnames(fest_str_coeff)))-1)],"_coeff")
  
  return(fest_str_coeff)
  
}

#function to calculate store-level p2p adjusted forecasted-sales
P2P_STR_ADJUSTED_SALES<- function(main_festivals, fest_str_features, fest_str_coeff)
{
  
  fest_str_features<-setDT(fest_str_features)
  
  #giving the impact status to each month-yr
  fest_str_features[,"fest_wts"]<- rowSums(fest_str_features[,(grep("moving_average",colnames(fest_str_features))+1):ncol(fest_str_features)])
  
  
  fest_str_features[which(fest_str_features$fest_wts!=0),"impact_status"]<- "impacted"
  fest_str_features[which(fest_str_features$fest_wts==0),"impact_status"]<- "not_impacted"
  
  
  #joining the table with the test-features and the table with calculated coefficients for the features(on training the features on one of the prev-yr) 
  fest_str_features<- inner_join(fest_str_features, fest_str_coeff, by=c("site_code"))
  
  
  #y= X*X_coeff
  fest_str_features[,'p2p_adjusted_sales']<- fest_str_features$moving_average+fest_str_features$moving_average_coeff
  
  
  #iterating through all the main festivals corresponding to the set of stores
  for(fest in main_festivals)
  {
    #getting the col_indices with festival names
    fest_ind<- grep(fest,colnames(fest_str_features))
    
    
    #if there are two such columns one with festival wt and other with festival coeff we perform calculation
    # y = X*X_coeff
    if(length(fest_ind)==2)
    {
      fest_str_features[,'p2p_adjusted_sales']<- fest_str_features[,'p2p_adjusted_sales']+fest_str_features[,fest_ind[[1]]:fest_ind[[1]]]*fest_str_features[,fest_ind[[2]]:fest_ind[[2]]]
    }
    
    
  }
  
  #y =y + intercept
  fest_str_features$p2p_adjusted_sales<- fest_str_features$p2p_adjusted_sales+fest_str_features$intercept
  
  fest_str_features[which(fest_str_features$impact_status=="impacted" & fest_str_features$p2p_adjusted_sales<=0),"p2p_adjusted_sales"]<-fest_str_features[which(fest_str_features$impact_status=="impacted" & fest_str_features$p2p_adjusted_sales<=0),"sales_rev"]
  
  fest_str_features[which(fest_str_features$impact_status=="not_impacted"),"p2p_adjusted_sales"]<-pmin(pmax(fest_str_features[which(fest_str_features$impact_status=="not_impacted"),"p2p_adjusted_sales"],fest_str_features[which(fest_str_features$impact_status=="not_impacted"),"sales_rev"])
                                                                                                       ,fest_str_features[which(fest_str_features$impact_status=="not_impacted"),"sales_rev"])
  
  #return(fest_str_features)
  return(fest_str_features[,c("site_code","month_yr","sales_rev","p2p_adjusted_sales","impact_status")])
  
  
}


#getting the store-wise multipliers
STR_MULTIPLIERS<-function(str_fcst)
{
  #calculating the multipliers for each of the store
  #getting the unique_site_codes present in str_fcst
  unique_site_codes<-unique(str_fcst$site_code)
  str_fcst$sales_disagg<-str_fcst$sales_rev
  str_fcst$p2p_adj_disagg<-str_fcst$p2p_adjusted_sales
  ls_str_multipliers<-list()

  
  #calculating the store-level multipliers
  for(site in 1:length(unique_site_codes))
  {
    
    #capturing a particular site info in the data-frame
    dt_site<-str_fcst[which(str_fcst$site_code==unique_site_codes[site]),]
    #creating an impact_vals vector with intialized value corresponding to all the test month as 0
    impact_vals<-c(numeric(nrow(dt_site)))
    
    for(ind in 1:length(dt_site$month_yr))
    {
      #intialising the prev-ind as the curret_ind number(pointer is pointing towards the current month)
      prev_ind<-ind
      
      #if there is impact in the current-month then enter the if statement
      if(dt_site$impact_status[[ind]]=="impacted")
      {
        #iterate in the while loop to bring the pointer to the contigous month which has festival impact
        while(ind!=nrow(dt_site) & dt_site$impact_status[[ind]]=="impacted" & impact_vals[ind]==0)
        {
          
          impact_vals[ind]<-1
          
          ind<-ind+1
        }
        
        #after finding the bucket of continously impacted months(by festival) sum up their sales(forecast and hwa)
        dt_site[prev_ind:(ind-1),"sales_disagg"]<-sum(dt_site[prev_ind:(ind-1),"sales_rev"])
        
        dt_site[prev_ind:(ind-1),"p2p_adj_disagg"]<-sum(dt_site[prev_ind:(ind-1),"p2p_adjusted_sales"])
        
      }
      
      #finding the multipliers by dividing the contribution of the p2p to the particular month by the contribution of hwa in the particular month
      dt_site[,"multipliers"]<-(dt_site$sales_disagg*dt_site$p2p_adjusted_sales)/(dt_site$sales_rev*dt_site$p2p_adj_disagg)
      
    }
    
    
    #keep capturing the tables ina list
    ls_str_multipliers[[site]]<-dt_site[,c("site_code","month_yr","multipliers")]
    
  }
  
  
  #rbinding the str-multipliers list
  dt_multipliers<-rbindlist(ls_str_multipliers)
  
  return(dt_multipliers)
  
}

#adjust the store-dept sales as per the multipliers
ADJ_STR_DEPT_SALES<- function(str_multipliers, dt_str_dept_fcst)
{
  unique_site_codes<- unique(dt_str_dept_fcst$site_code)
  
  ls_fcst<-list()
  
  for(site in 1:length(unique_site_codes))
  {
    
    dt_str_sales<-pivot_longer(data=dt_str_dept_fcst[which(dt_str_dept_fcst$site_code==unique_site_codes[[site]]),],!c(site_code,dept_code,article_code), names_to = "month_yr", values_to = "sales_rev")
    print(dt_str_sales)
    
    dt_str_sales<-left_join(dt_str_sales,str_multipliers,by=c("site_code","month_yr"))
    
    dt_str_sales$sales_rev<-dt_str_sales$sales_rev*dt_str_sales$multipliers
    
    dt_str_sales<-subset(dt_str_sales,select=c("site_code","article_code","dept_code","sales_rev","month_yr"))
    
    dt_str_sales<-pivot_wider(data = dt_str_sales, id_cols = c("site_code","dept_code","article_code"),names_from = "month_yr", values_from =  "sales_rev")
    
    ls_fcst[[site]]<-dt_str_sales 
    
  }
  
  
  
  dt_str_dept_adj_fcst<- rbindlist(ls_fcst)
  
  return(dt_str_dept_adj_fcst)
  
}




#reading the required files from the excel sheet
dt_fest<- read_excel(list.files(list.files(path_proj,full.names=TRUE)[grep("FESTIVAL_IP",list.files(path_proj,full.names=TRUE))],full.names = TRUE))
#reading the user input from the excel file
mt_user<- read_excel(list.files(file.path(path_proj,"USER_INPUT"),full.names = TRUE)[grep("/USER_INPUT",list.files(file.path(path_proj,"USER_INPUT"),full.names=TRUE))])
#complete str-mstr
store_mstr<-fread(list.files(file.path(path_proj,"STORE_MSTR"),full.names = TRUE)[grep("STORE_MSTR",list.files(file.path(path_proj,"STORE_MSTR")))])

#reading the store-dept i/p and HWA forecast files
#reading the real store-mstr
preprocessed_ip_path<- list.files(file.path(path_proj,"PREPROCESSED_OPS"),full.names=TRUE)
hwa_fcst_path<- list.files(file.path(path_proj,"HWA_FCST_OPS"),full.names=TRUE)
str_mstr_path<- list.files(file.path(path_proj, "COMPANY_LEVEL_SEPARATED_DATA"),full.names = TRUE)
mappings<- list.files(file.path(path_proj, "MAPPINGS"),full.names = TRUE)[grep("DEPT_INFO",list.files(file.path(path_proj, "MAPPINGS")))]


if(mt_user$RUN_AT=="P1"){
  
  str_preproceed_ip_path<- list.files(list.files(preprocessed_ip_path[grep("P1",preprocessed_ip_path)], full.names = TRUE)
                                      [grep("DEPT_SALES",list.files(preprocessed_ip_path[grep("P1",preprocessed_ip_path)]
                                      ))], full.names=TRUE)
  
  str_hwa_fcst_path<-  list.files(list.files(hwa_fcst_path[grep("P1",hwa_fcst_path)], full.names = TRUE)
                                  [grep("P1_STR_DEPT", list.files(hwa_fcst_path[grep("P1",hwa_fcst_path)] 
                                  ))], full.names = TRUE)
  
  store_mstr_path<- list.files(list.files(str_mstr_path[grep("P1",str_mstr_path)], full.names = TRUE)
                               [grep("P1_STR_MSTR", list.files(str_mstr_path[grep("P1",str_mstr_path)]))], full.names = TRUE)
  
  output_path<- file.path(path_proj, "P2P_ADJUSTED_OPS", "P1","P1_STR_DEPT_ADJ_FCST.xlsx")
  
}else if(mt_user$RUN_AT=="P2"){
  
  str_preproceed_ip_path<- list.files(list.files(preprocessed_ip_path[grep("P2",preprocessed_ip_path)], full.names = TRUE)
                                      [grep("DEPT_SALES",list.files(preprocessed_ip_path[grep("P2",preprocessed_ip_path)]
                                      ))], full.names=TRUE)
  
  str_hwa_fcst_path<-  list.files(list.files(hwa_fcst_path[grep("P2",hwa_fcst_path)], full.names = TRUE)
                                  [grep("P2_STR_DEPT", list.files(hwa_fcst_path[grep("P2",hwa_fcst_path)] 
                                  ))], full.names = TRUE)
  store_mstr_path<- list.files(list.files(str_mstr_path[grep("P2",str_mstr_path)], full.names = TRUE)
                               [grep("P2_STR_MSTR", list.files(str_mstr_path[grep("P2",str_mstr_path)]))], full.names = TRUE)
  
  output_path<- file.path(path_proj, "P2P_ADJUSTED_OPS", "P2","P2_STR_DEPT_ADJ_FCST.xlsx")
  
}else if(mt_user$RUN_AT=="NSO"){
  str_preproceed_ip_path<- list.files(list.files(preprocessed_ip_path[grep("NSO",preprocessed_ip_path)], full.names = TRUE)
                                      [grep("DEPT_SALES",list.files(preprocessed_ip_path[grep("NSO",preprocessed_ip_path)]
                                      ))], full.names=TRUE)
  
  str_hwa_fcst_path<-  list.files(list.files(hwa_fcst_path[grep("NSO",hwa_fcst_path)], full.names = TRUE)
                                  [grep("NSO_STR_DEPT", list.files(hwa_fcst_path[grep("NSO",hwa_fcst_path)] 
                                  ))], full.names = TRUE)
  store_mstr_path<- list.files(list.files(str_mstr_path[grep("NSO",str_mstr_path)], full.names = TRUE)
                               [grep("NSO_STR_MSTR", list.files(str_mstr_path[grep("NSO",str_mstr_path)]))], full.names = TRUE)
  
  output_path<- file.path(path_proj, "P2P_ADJUSTED_OPS", "NSO","NSO_STR_DEPT_ADJ_FCST.xlsx")
  
  
}else{
  str_preproceed_ip_path<- list.files(list.files(preprocessed_ip_path[grep("ALL",preprocessed_ip_path)], full.names = TRUE)
                                      [grep("DEPT_SALES",list.files(preprocessed_ip_path[grep("ALL",preprocessed_ip_path)]
                                      ))], full.names=TRUE)
  
  str_hwa_fcst_path<-  list.files(list.files(hwa_fcst_path[grep("ALL",hwa_fcst_path)], full.names = TRUE)
                                  [grep("ALL_STR_DEPT", list.files(hwa_fcst_path[grep("ALL",hwa_fcst_path)] 
                                  ))], full.names = TRUE)
  store_mstr_path<- list.files(list.files(str_mstr_path[grep("ALL",str_mstr_path)], full.names = TRUE)
                               [grep("ALL_STR_MSTR", list.files(str_mstr_path[grep("ALL",str_mstr_path)]))], full.names = TRUE)
  
  output_path<- file.path(path_proj, "P2P_ADJUSTED_OPS", "ALL","ALL_STR_DEPT_ADJ_FCST.xlsx")
  
}

#reading the i/p file
dt_str_dept_ip<- fread(str_preproceed_ip_path)
#reading the fcst file
dt_str_dept_fcst<- fread(str_hwa_fcst_path)
#reading the store-mstr
dt_str_mstr<- fread(store_mstr_path)
#read the mappings table
dt_mappings<- fread(mappings)

#renaming the columns of the data-frames in a specific format
ip_months<- GET_SEQUENCE_OF_DATES(mt_user$START_PERIOD, mt_user$END_PERIOD)

#getting the store-dept input preprocessed tables columns in an appropriate form
colnames(dt_str_dept_ip)<- c("site_code","dept_code","article_code",ip_months)

#getting the forecast-months
fcst_months<- GET_SEQUENCE_OF_DATES(mt_user$FORECAST_START_DATE, ADD.MONTHS(mt_user$FORECAST_END_DATE,5))

#getting the store-dept forecasts tables columns
colnames(dt_str_dept_fcst)<- c("site_code","dept_code","article_code",fcst_months)



#months for which weight calculation needs to take place
weighted_months <- GET_SEQUENCE_OF_DATES(mt_user$START_PERIOD, ADD.MONTHS(mt_user$FORECAST_END_DATE,4))

#getting the list of festivals
festival_list <- tolower(dt_fest$Festival)

#initializing the weights in such a way that the weights are incremented by 10% after every 5-days of interval
#it is assumed that each festival has 20 days of impact
intial_wt<-100
percent_age_growth<-10
ls_wt<-list()
ls_wt[[1]]<-intial_wt
for(interval in 2:4)
{
  ls_wt[[interval]]<-ls_wt[[interval-1]]*(1+percent_age_growth/100)
}


#creating a data-frame with weights initialized as 0 corresponding to festivals
col_names<- c("month_yr", festival_list)
colList<- list(weighted_months,c(numeric(length(weighted_months))),c(numeric(length(weighted_months))),c(numeric(length(weighted_months))),
               c(numeric(length(weighted_months))),c(numeric(length(weighted_months))))

#creating the data-frame with festivals and weights initialized as 0s
dt_festival <- do.call(data.table,colList)
colnames(dt_festival)<- col_names


#getting the festival-dataframe with weightage allocated
dt_festival<- ALLOCATING_WTS_AS_PER_DAYS(dt_fest, dt_festival, ls_wt)

main_festivals<- tolower(dt_fest$Festival)

#defining the year in which we want to train our model for getting the coefficients
train_month_start_yr<-as.Date(mt_user$SS_START_DATE)
train_month_end_yr<-as.Date(mt_user$SS_END_DATE)

#getting the store-level total sales corresponding to all the month-year
dt_str_past_sales<- dt_str_dept_ip[,-c(2,3)][,lapply(.SD, sum, na.rm = TRUE),.(site_code)]

#getting the store-level total forecasted sales corresponding to all the month-year
dt_str_future_sales<- dt_str_dept_fcst[,-c(2,3)][,lapply(.SD, sum, na.rm = TRUE),.(site_code)]
dt_str_future_sales<- left_join(subset(dt_str_past_sales,select=c("site_code",format(as.Date(mt_user$END_PERIOD),"%b-%Y"))), dt_str_future_sales, by=c("site_code"))

#assembling the data to a convenient format for feature table generation
dt_str_past_sales<-pivot_longer(data=dt_str_past_sales,!c(site_code), names_to = "month_yr", values_to = "sales_rev")
dt_str_future_sales<-pivot_longer(data=dt_str_future_sales,!c(site_code), names_to = "month_yr", values_to = "sales_rev")

#getting the training-feature table with the moving average and the weights corresponding to each festival
#Here site_code is the store_code, sales_rev is the dependent variable, (moving_average, pujo, diwali, chatth, eid, holi) are the independent variables
dt_training_features<-INPUT_TABLE(dt_festival,dt_str_past_sales,train_month_start_yr,train_month_end_yr)

#getting the test-month start and end period
test_month_start_yr<- as.Date(mt_user$END_PERIOD)
test_month_end_yr<- ADD.MONTHS(as.Date(mt_user$FORECAST_END_DATE),5)
#creating the test-feature table
dt_test_features<-INPUT_TABLE(dt_festival,dt_str_future_sales,test_month_start_yr,test_month_end_yr)


#getting the store-coefficients for pujo stores
if(nrow(dt_str_mstr[which(dt_str_mstr$festival=="purepujo"),])!=0)
{
pujo_coeff<- COEFFICIENTS_CALCULATE(dt_training_features[which(dt_training_features$site_code 
                                                               %in% dt_str_mstr[which(dt_str_mstr$festival=="purepujo"),]$site_code),-c("diwali","chatth")])
#rename the columns to more understandable format
pujo_coeff<- RENAME_FESTIVAL_COEFF_TBL(pujo_coeff)
pujo_strs_fcst<-  P2P_STR_ADJUSTED_SALES(main_festivals, dt_test_features, pujo_coeff)
pujo_strs_fcst[is.na(pujo_strs_fcst$p2p_adjusted_sales),"p2p_adjusted_sales"]<- pujo_strs_fcst[is.na(pujo_strs_fcst$p2p_adjusted_sales),"sales_rev"]



}
if(nrow(dt_str_mstr[which(dt_str_mstr$festival=="dc"),])!=0)
{
#getting the store-coefficients for dc stores
dc_coeff<- COEFFICIENTS_CALCULATE(dt_training_features[which(dt_training_features$site_code 
                                                             %in% (dt_str_mstr[which(dt_str_mstr$festival=="dc"),]$site_code)),-c("pujo")])

#rename the columns to more understandable format
dc_coeff<- RENAME_FESTIVAL_COEFF_TBL(dc_coeff)
dc_strs_fcst<-  P2P_STR_ADJUSTED_SALES(main_festivals, dt_test_features, dc_coeff)
dc_strs_fcst[is.na(dc_strs_fcst$p2p_adjusted_sales),"p2p_adjusted_sales"]<- dc_strs_fcst[is.na(dc_strs_fcst$p2p_adjusted_sales),"sales_rev"]
}
if(nrow(dt_str_mstr[which(dt_str_mstr$festival=="pdc"),])!=0)
{
#getting the store-coefficients for pdc stores
pdc_coeff<- COEFFICIENTS_CALCULATE(dt_training_features[which(dt_training_features$site_code 
                                                              %in% dt_str_mstr[which(dt_str_mstr$festival=="pdc"),]$site_code),])
#renaming the column to more understandable format
pdc_coeff<- RENAME_FESTIVAL_COEFF_TBL(pdc_coeff)
#performining p2p-adjustment at store levle
pdc_strs_fcst<- P2P_STR_ADJUSTED_SALES(main_festivals, dt_test_features, pdc_coeff)
pdc_strs_fcst[is.na(pdc_strs_fcst$p2p_adjusted_sales),"p2p_adjusted_sales"]<- pdc_strs_fcst[is.na(pdc_strs_fcst$p2p_adjusted_sales),"sales_rev"]


}



#getting the p2p adjusted fcsts for pdc, dc and pujo stores
#rbinding all the str_fcst data-frames
if(exists("pdc_strs_fcst") & exists("dc_strs_fcst") & exists("pujo_strs_fcst"))
{  
strs_fcst<- rbind(pdc_strs_fcst,dc_strs_fcst,pujo_strs_fcst)
}else if(exists("pdc_strs_fcst") & exists("dc_strs_fcst") & !exists("pujo_strs_fcst")){
  strs_fcst<- rbind(pdc_strs_fcst,dc_strs_fcst)
}else if(exists("pdc_strs_fcst") & exists("pujo_strs_fcst") & !exists("dc_strs_fcst")){
  strs_fcst<- rbind(pdc_strs_fcst,pujo_strs_fcst)
}else if(exists("dc_strs_fcst") & exists("pujo_strs_fcst") & !exists("pdc_strs_fcst")){
  strs_fcst<- rbind(dc_strs_fcst,pujo_strs_fcst)
}else if(exists("dc_strs_fcst") & !exists("pujo_strs_fcst") & !exists("pdc_strs_fcst")){
  strs_fcst<- dc_strs_fcst
}else if(!exists("dc_strs_fcst") & exists("pujo_strs_fcst") & !exists("pdc_strs_fcst")){
  strs_fcst<- pujo_strs_fcst
}else if(!exists("dc_strs_fcst") & !exists("pujo_strs_fcst") & exists("pdc_strs_fcst")){
  strs_fcst<- pdc_strs_fcst
}

#getting the multipliers corresponding to each store
str_multipliers<- STR_MULTIPLIERS(strs_fcst)


unique_site_codes<- unique(str_multipliers$site_code)

str_multipliers[is.nan(str_multipliers$multipliers),"multipliers"]<-0
str_multipliers$site_code<-as.character(str_multipliers$site_code)
#removing any duplicate forecasts if any
str_multipliers[,"concat"]<- paste0(str_multipliers$site_code,"_",str_multipliers$month_yr)
str_multipliers<- str_multipliers[!duplicated(str_multipliers$concat),]
str_multipliers<- str_multipliers[,-c("concat")]
str_multipliers<-str_multipliers[which(str_multipliers$site_code!=0),]

dt_str_dept_fcst$site_code<- as.character(dt_str_dept_fcst$site_code)

#using the store wise multipliers to shift the sales corresponding to all the departments
dt_str_dept_adj_fcst<- ADJ_STR_DEPT_SALES(str_multipliers, dt_str_dept_fcst)

dt_str_dept_adj_fcst<- left_join(dt_str_dept_adj_fcst,dt_mappings,by=c("dept_code"))


store_mstr$site_code <- sub(" ", "", store_mstr$site_code)
store_mstr$site_code<- tolower(store_mstr$site_code)
store_mstr$site_code<- sub("[^0-9A-Za-z///' ]", "", store_mstr$site_code, ignore.case = TRUE)

#picking only the relevant columns to display as output
dt_str_dept_adj_fcst<- left_join(dt_str_dept_adj_fcst,store_mstr[,c("store","site_code","tier","region","zone","st_type", "op.date")], by=c("site_code"))



dt_str_dept_adj_fcst<- subset(dt_str_dept_adj_fcst,select=c("store","site_code","tier","region","zone","st_type", "op.date",
                                                            "division","section","season","dept_name",
                                                            "dept_code",
                                                            GET_SEQUENCE_OF_DATES(mt_user$FORECAST_START_DATE, mt_user$FORECAST_END_DATE)))
#getting the non-nso stores
non_nso_strs<- dt_str_dept_adj_fcst[which(dt_str_dept_adj_fcst$st_type!="NSO"),]
nso_strs<- dt_str_dept_adj_fcst[which(dt_str_dept_adj_fcst$st_type=="NSO"),]

nso_strs[,c("month")]<- format(as.Date(nso_strs$op.date),"%b-%Y")

nso_strs<- nso_strs %>% relocate(month, .after = dept_code)

ls_results<- list()
ind<-1

for(date_ind in 2:length(fcst_months))
{
  
  df1<-nso_strs[which(nso_strs$month==fcst_months[date_ind]),]
  if(length(grep(fcst_months[date_ind],colnames(df1)))!=0)
  {
    df1[,grep(format(as.Date(mt_user$FORECAST_START_DATE),"%b-%Y"),colnames(df1)):(grep(fcst_months[date_ind],colnames(df1))-1)]<- 0
    if(nrow(df1)!=0)
    {
      ls_results[[ind]]<- df1
      ind<- ind+1
    }
  }
  print(date_ind)
  
}


df_nso_modified<- rbindlist(ls_results)
df_nso_non_modified<- nso_strs[!(nso_strs$site_code %in% df_nso_modified$site_code),]

nso_strs<- rbind(df_nso_modified, df_nso_non_modified) 

nso_strs<- nso_strs[,-c("month")]

dt_str_dept_adj_fcst<- rbind(nso_strs, non_nso_strs)

#write all the files in the required o/p path
write_xlsx(dt_str_dept_adj_fcst, file.path(path_proj,"P2P_ADJUSTED_OPS","ALL",paste0(format(as.Date(mt_user$FORECAST_START_DATE),"%b-%Y"),"_to_",
                                                                                     paste0(format(as.Date(mt_user$FORECAST_END_DATE),"%b-%Y")),
                                                                                     "_ALL_STR_DEPT_FCST.xlsx")))


p1_site_codes<- dt_str_mstr[which(dt_str_mstr$st_type=="p1"),"site_code"]
p2_site_codes<- dt_str_mstr[which(dt_str_mstr$st_type=="p2"),"site_code"]
nso_site_codes<- dt_str_mstr[which(dt_str_mstr$st_type=="nso"),"site_code"]

#write the p1, p2 and nos files separately
p1_str_dept_adj_fcst<- dt_str_dept_adj_fcst[which(dt_str_dept_adj_fcst$site_code %in% p1_site_codes$site_code),]
p2_str_dept_adj_fcst<- dt_str_dept_adj_fcst[which(dt_str_dept_adj_fcst$site_code %in% p2_site_codes$site_code),]
nso_str_dept_adj_fcst<- dt_str_dept_adj_fcst[which(dt_str_dept_adj_fcst$site_code %in% nso_site_codes$site_code),]


#write the individual p1, p2 and nso files 
write_xlsx(p1_str_dept_adj_fcst, file.path(path_proj,"P2P_ADJUSTED_OPS","P1",paste0(format(as.Date(mt_user$FORECAST_START_DATE),"%b-%Y"),"_to_",
                                                                                    paste0(format(as.Date(mt_user$FORECAST_END_DATE),"%b-%Y")),"_P1_STR_DEPT_FCST.xlsx")))
write_xlsx(p2_str_dept_adj_fcst, file.path(path_proj,"P2P_ADJUSTED_OPS","P2",paste0(format(as.Date(mt_user$FORECAST_START_DATE),"%b-%Y"),"_to_",
                                                                                    paste0(format(as.Date(mt_user$FORECAST_END_DATE),"%b-%Y")),"_P2_STR_DEPT_FCST.xlsx")))
write_xlsx(nso_str_dept_adj_fcst, file.path(path_proj,"P2P_ADJUSTED_OPS","NSO",paste0(format(as.Date(mt_user$FORECAST_START_DATE),"%b-%Y"),"_to_",
                                                                                      paste0(format(as.Date(mt_user$FORECAST_END_DATE),"%b-%Y")),"_NSO_STR_DEPT_FCST.xlsx")))



