## Installation of packages that are critical to run the collation and consolidation code below
#checking for the installed libraries for the given module
#list.of.packages <- c("dplyr", "readxl","readr","tidyr",
#                      "readxl","tidyverse","lubridate",
#                      "data.table","jsonlite","forecast")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)


#declaring the path of project
path_proj<- "D:/products/SOUTH_FORECASTS"


#importing the required libraries
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(data.table)


#path of the store-master
path_store_mstr<- file.path(path_proj, "STORE_MSTR")
#path of the sales-data
path_sales<- file.path(path_proj, "SALES_DATA")
#path of the user-input file
path_user_ip<-  file.path(path_proj, "USER_INPUT")

#-----------declaring the internal functions-------------------------
#calculate the month_difference
MONTH_DIFFERENCE<-function(rem_start_date,rem_end_date,df_store_mstr,end_yr)
{
  print(df_store_mstr$month_diff)
  print(interval(df_store_mstr$`op-date`,rem_start_date) %/% months(1))
  ##calculating the month-difference
  df_store_mstr$month_diff<-interval((as.Date(df_store_mstr$`op-date`)),rem_start_date) %/% months(1)+interval(rem_end_date,(as.Date(end_yr)+months(1))) %/% months(1)
  return(df_store_mstr)
  
}


# function to get the mode of a particular feature
GETMODE <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}


# function to clean store-mstr
CLEAN_STORE_MSTR <- function(store_mstr) {
  
  #a for loop to iterate through all the columns
  for (ind in 1:length(colnames(store_mstr)))
  {
    #identify the column which are of type character
    if (sapply(store_mstr[1, ind], typeof) == "character") {
      
      #remove spaces from the entries in the given column
      store_mstr[, ind] <- sub(" ", "", store_mstr[, ind])
      #converting the entries into lower-case
      store_mstr[, ind] <- tolower(store_mstr[, ind])
      #removing all the special-characters from the entries
      store_mstr[, ind] <- sub("[^0-9A-Za-z///' ]", "", store_mstr[, ind], ignore.case = TRUE)
      
    }
  }
  return(store_mstr)
}


# function to impute the missing values in the data
IMPUTE_MISSING_VALUES <- function(store_mstr, mt_user) {
  
  # imputing the missing-festival
  # getting the store-mstr for the stores with missing-festival
  stores_missing_fest <- store_mstr[which(is.na(store_mstr$festival) | store_mstr$festival == ""), ]
  # getting the store-mstr with available-festival
  stores_avai_fest <- store_mstr[which(!is.na(store_mstr$festival) & store_mstr$festival != ""), ]
  
  #since there is no such entity as fdu/non-fdu present in the south stores
  #we impute them by
  store_mstr[,"fdu/non-fdu"]<- "fdu"
  
    
  if (nrow(stores_missing_fest) != 0) {
    # imputing-the festival with the mode of the festivals in a particular region
    for (str in 1:nrow(stores_missing_fest))
    {
      stores_missing_fest[str, ]$festival <- GETMODE(stores_avai_fest[which(stores_avai_fest$region == stores_missing_fest[str, ]$region), ]$festival)
    }
    
    store_mstr <- rbind(stores_avai_fest, stores_missing_fest)
  }
  
  # assuming this case would only occur for NSO-stores
  # imputing the store-area
  # getting the stores with missing store-areas
  stores_missing_store_area <- store_mstr[which(is.na(store_mstr$sqft) | store_mstr$sqft == 0), ]
  # getting the stores with available store-areas
  stores_avai_store_area <- store_mstr[which(!is.na(store_mstr$sqft) & store_mstr$sqft != 0), ]
  
  if (nrow(stores_missing_store_area)) {
    # imputing-the area with the default store-area assumed by the planning team
    for (str in 1:nrow(stores_missing_store_area))
    {
      stores_missing_store_area[str, ]$sqft <- mt_user$STORE_AREA_DEFAULT
    }
    
    store_mstr <- rbind(stores_missing_store_area, stores_avai_apparel_area)
  }
  
  
  # assuming this case would only occur for NSO-stores
  # imputing the apparel-area
  # getting the stores with missing apparel-areas
  stores_missing_apparel_area <- store_mstr[which(store_mstr$apparel_area == 0), ]
  # getting the stores with available apparel-areas
  stores_avai_apparel_area <- store_mstr[which(!is.na(store_mstr$apparel_area) & store_mstr$apparel_area != 0), ]
  
  if (nrow(stores_missing_apparel_area)) {
    # imputing-the apparel-area with 80% of the store-area
    for (str in 1:nrow(stores_missing_apparel_area))
    {
      stores_missing_apparel_area[str, ]$apparel_area <- stores_missing_apparel_area[str, ]$sqft * 0.8
    }
    
    store_mstr <- rbind(stores_missing_apparel_area, stores_avai_apparel_area)
  }
  
  # imputation of tier-component
  stores_missing_tier <- store_mstr[which(is.na(store_mstr$tier) | store_mstr$tier == ""), ]
  # getting the store-mstr with available-tiers
  stores_avai_tier <- store_mstr[which(!is.na(store_mstr$tier) & store_mstr$tier != ""), ]
  
  if (nrow(stores_missing_tier) != 0  & nrow(stores_avai_tier) != 0) {
    # imputing-the festival with the mode of the tier in a particular region, zone
    for (str in 1:nrow(stores_missing_tier))
    {
      stores_missing_tier[str, ]$tier <- GETMODE(stores_avai_tier[which(stores_avai_tier$region == stores_missing_tier[str, ]$region &
                                                                          stores_avai_tier$zone == stores_missing_tier[str, ]$zone), ]$tier)
    }
    
    store_mstr <- rbind(stores_avai_tier, stores_missing_tier) 
  }else if(nrow(stores_missing_tier) != 0  & nrow(stores_avai_tier) == 0)
  {
    stores_missing_tier$tier = "i"
    store_mstr <- rbind(stores_avai_tier, stores_missing_tier)
  }
  
  
  # imputation of store-format
  stores_missing_str_format <- store_mstr[which(is.na(store_mstr$store_format) | store_mstr$store_format == ""), ]
  # getting the store-mstr with available-festival
  stores_avai_str_format <- store_mstr[which(!is.na(store_mstr$store_format) & store_mstr$store_format != ""), ]
  
  if (nrow(stores_missing_str_format) != 0) {
    # imputing-the festival with the mode of the festivals in a particular region
    for (str in 1:nrow(stores_missing_str_format))
    {
      stores_missing_str_format[str, ]$store_format <- GETMODE(stores_avai_str_format[which(stores_avai_str_format$region == stores_missing_str_format[str, ]$region &
                                                                                              stores_avai_str_format$zone == stores_missing_str_format[str, ]$zone), ]$store_format)
    }
    
    store_mstr <- rbind(stores_avai_str_format, stores_missing_str_format)
  }
  
  return(store_mstr)
}


#send the processed  i/p files into the respective o/p folders
OP_FOLDER_ALLOCATION<-  function(store_rev, store_qty, store_mstr, type_of_store)
{
  
  #output path for store-art-revenue-data
  rev_out_path<- file.path(path_proj, "COMPANY_LEVEL_SEPARATED_DATA", type_of_store,paste0(type_of_store,"_SALES"), paste0(type_of_store,"_SALES_",
                                                                                                                   "REV.csv"))
  #output path for  store-art-quantity-data
  qty_out_path<- file.path(path_proj, "COMPANY_LEVEL_SEPARATED_DATA", type_of_store,paste0(type_of_store,"_SALES"), paste0(type_of_store,"_SALES_",
                                                                                                                   "QTY.csv"))
  #output path for store-master
  mstr_out_path<- file.path(path_proj, "COMPANY_LEVEL_SEPARATED_DATA", type_of_store,paste0(type_of_store,"_STR_","MSTR"), paste0(type_of_store,"_STORE_",
                                                                                                                                   "MSTR.csv"))
  #writing the store-revenue data
  fwrite(store_rev, rev_out_path)
  #writing the store-qty data
  fwrite(store_qty, qty_out_path)
  #writing the store-master
  fwrite(store_mstr, mstr_out_path)
  
  
  
}


#-------------------------------------------- reading the required-files------------------------------------------------------------------------------------

store_art_qty<- fread(list.files(path_sales, full.names = TRUE)[grep("SALES_QTY_DATA",
                list.files(path_sales))])


store_art_rev<- fread(list.files(path_sales, full.names = TRUE)[grep("SALES_REV_DATA",
                                                                     list.files(path_sales))])


store_mstr<- fread(list.files(path_store_mstr, full.names= TRUE)[grep("STORE_MSTR",list.files(path_store_mstr))])

user_ip<- read_excel(list.files(path_user_ip, full.names= TRUE)[grep("USER_INPUT",list.files(path_user_ip))])



#---------------------------------------- renaming the columns in the files into standard format---------------------------------------------------------------------

#getting the sequence of months for which forecasting has been performed
fcst_ip_months <- format(seq.Date(as.Date(user_ip$START_PERIOD),as.Date(user_ip$END_PERIOD), by = "month"),
                      format = "%b-%Y")

period<- length(fcst_ip_months)

colnames(store_art_qty)<- c("site_code", "article_code",
                            "dept_code","section_code", fcst_ip_months)

colnames(store_art_rev)<- c("site_code", "article_code",
                            "dept_code", "section_code", fcst_ip_months)

#converting store-mstr to data-frame for performing cleaning
store_mstr<- data.frame(store_mstr)

colnames(store_mstr)<- c("store","site_code","st_type","tier","op-date","sqft","store_format","region","zone","fdu/non-fdu","apparel_area","festival")


#if any sale is being projected as negative in the i/p data then replace it by zero
store_art_rev[store_art_rev<0]<- 0
store_art_qty[store_art_qty<0]<- 0


# cleaning the store-master
store_mstr <- CLEAN_STORE_MSTR(store_mstr)
# imputing the missing-values with valid-substitutes
store_mstr <- IMPUTE_MISSING_VALUES(store_mstr, mt_user)

#replacing the nas by 0
store_art_rev[is.na(store_art_rev)] <- 0
store_art_qty[is.na(store_art_qty)] <- 0

#v-mart & ul-stores separated based on the zone
ul_stores_rev<- store_art_rev[which(store_art_rev$site_code %in% store_mstr[which(store_mstr$zone == "south"),"site_code"])]
ul_stores_qty<- store_art_qty[which(store_art_qty$site_code %in% store_mstr[which(store_mstr$zone == "south"),"site_code"])]


#separate the p1, p2 and nso stores
p1_stores<- store_mstr[which(store_mstr$st_type=="p1"),]
p2_stores<- store_mstr[which(store_mstr$st_type=="p2"),]
nso_stores<- store_mstr[which(store_mstr$st_type=="nso"),]
all_stores<- rbind(p1_stores, p2_stores, nso_stores)


#revenue table for south stores                                                                                                                                                                                                                    #separate the p1, p2 and nso store revenue
p1_stores_rev<- ul_stores_rev[which(ul_stores_rev$site_code %in% p1_stores$site_code),]
p2_stores_rev<- ul_stores_rev[which(ul_stores_rev$site_code %in% p2_stores$site_code),]
nso_stores_rev<- ul_stores_rev[which(ul_stores_rev$site_code %in% nso_stores$site_code),]


#quantity table for south stores
p1_stores_qty<- ul_stores_qty[which(ul_stores_qty$site_code %in% p1_stores$site_code),]
p2_stores_qty<- ul_stores_qty[which(ul_stores_qty$site_code %in% p2_stores$site_code),]
nso_stores_qty<- ul_stores_qty[which(ul_stores_qty$site_code %in% nso_stores$site_code),]


#remove the articles which are newly introduced on each store
#for p1, p2 stores any article which has been introduced in the last two months is assumed to be new
p1_stores_rev <- p1_stores_rev[which(rowSums(p1_stores_rev[, (ncol(p1_stores_rev) - (period-1)):(ncol(p1_stores_rev) - 1)]) != 0), ]
p1_stores_qty <- p1_stores_qty[which(rowSums(p1_stores_qty[, (ncol(p1_stores_qty) - (period-1)):(ncol(p1_stores_qty) - 1)]) != 0), ]
p2_stores_rev <- p2_stores_rev[which(rowSums(p2_stores_rev[, (ncol(p2_stores_rev) - (period-1)):(ncol(p2_stores_rev) - 1)]) != 0), ]
p2_stores_qty <- p2_stores_qty[which(rowSums(p2_stores_qty[, (ncol(p2_stores_qty) - (period-1)):(ncol(p2_stores_qty) - 1)]) != 0), ]
#in nso-stores the article which hasn't started selling yet is assumed to be new
nso_stores_rev<- nso_stores_rev[which(rowSums(nso_stores_rev[, (ncol(nso_stores_rev)):(ncol(nso_stores_rev))]) != 0), ]
nso_stores_qty<- nso_stores_rev[which(rowSums(nso_stores_qty[, (ncol(nso_stores_qty)):(ncol(nso_stores_qty))]) != 0), ]


#remove the articles which are discontinued in stores (didn't sell since last 1 year)
p1_stores_rev <- p1_stores_rev[rowSums(p1_stores_rev[,(ncol(p1_stores_rev)-11):ncol(p1_stores_rev)])!=0,]
p2_stores_rev <- p2_stores_rev[rowSums(p2_stores_rev[,(ncol(p2_stores_rev)-11):ncol(p2_stores_rev)])!=0,]

p1_stores_qty <- p1_stores_qty[rowSums(p1_stores_qty[,(ncol(p1_stores_qty)-11):ncol(p1_stores_qty)])!=0,]
p2_stores_qty <- p2_stores_qty[rowSums(p2_stores_qty[,(ncol(p2_stores_qty)-11):ncol(p2_stores_qty)])!=0,]

all_stores_rev<- rbind(p1_stores_rev, p2_stores_rev, nso_stores_rev)
all_stores_qty<- rbind(p1_stores_qty, p2_stores_qty, nso_stores_qty)

#the p2-stores for which the payment is not present should be the part of nso-stores
nso_mustbe_stores<- p2_stores[!(p2_stores$site_code %in% p2_stores_rev$site_code),]
nso_stores<- rbind(nso_stores, nso_mustbe_stores)
p2_stores<- p2_stores[(p2_stores$site_code %in% p2_stores_rev$site_code),]

#retaining only the unique_stores from each store-master
p1_stores<- p1_stores[!duplicated(p1_stores$site_code),]
p2_stores<- p2_stores[!duplicated(p2_stores$site_code),]
nso_stores<- nso_stores[!duplicated(nso_stores$site_code),]

#------------------------------------writing the separated files to respective folder---------------------------------------------------------------

#writing the p1-store data in their respective folders
OP_FOLDER_ALLOCATION(p1_stores_rev, p1_stores_qty, p1_stores, "P1")
#writing the p2-store data in their respective folders
OP_FOLDER_ALLOCATION(p2_stores_rev, p2_stores_qty, p2_stores, "P2")
#writing the nso-stores data in their respective folders
OP_FOLDER_ALLOCATION(nso_stores_rev, nso_stores_qty,nso_stores, "NSO")
#writing the all-stores data in their respective folders
OP_FOLDER_ALLOCATION(all_stores_rev, all_stores_qty,all_stores, "ALL")


























