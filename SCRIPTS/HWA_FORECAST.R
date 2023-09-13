#---- Cleaning Environment ----
rm(list = ls())
gc()

#install.packages('forecast', dependencies = TRUE)

#declaring the path of the project
path_proj <- "D:/products/SOUTH_FORECASTS"

#---- Sourcing Libraries and external files ----

# loading libraries
cat("Loading libraries", "/n")
library(dplyr)
library(readxl)
library(data.table)
library(forecast)
library(lubridate)


# user input
path_user<-file.path(path_proj,"USER_INPUT")
#mt_user <- read_excel(list.files(path_user, full.names = TRUE)[grep("USER_INPUT",list.files(path_user))])
mt_user<-read_excel(list.files(path_user, full.names=TRUE))
#---- Local Functions ----

add.months<-function(date,n) seq(date, by = "month", length.out = n)[n]

add.month<-function(date) add.months(date,2)

# function to calculate MAPE for the last two years
CALCULATE_2YR_MAPE <- function(actual, predicted) {
  # retaining predicted values for the last two years
  predicted_length <- length(predicted)
  actual_length <- length(actual)
  pred <- predicted[(predicted_length - 23):(predicted_length)]
  # getting actual values for the last two years
  act <- actual[(actual_length - 23):(actual_length)]
  # weighted mape calculate to avoid infinite condition
  # weighted_mape= summation(minimum(absolute(predicted-actual),actual))/summation(actual)
  mape_2yr <- sum(pmin(abs(pred - act), act)) / sum(act)
  return(mape_2yr)
}


# function to calculate weighted MAPE for 1 year for model comparisons
CALCULATE_WTED_MAPE <- function(actual, predicted) {
  predict_length <- length(predicted)
  actual_length <- length(actual)
  
  # predicted value for the last one year
  pred <- predicted[(predict_length - 11):(predict_length)]
  # actual value for the last one year
  act <- actual[(actual_length - 11):(actual_length)]
  # calculating the weighted_mape
  wted_mape <- sum(pmin(abs(pred - act), act)) / sum(act)
  
  return(wted_mape)
}

# calculate for -ve predictions
# if predicted_values come out to be negative they are being given the actual value
CALCULATE_NEGATIVE_PREDICTED <- function(actual, predicted,degrowth_cap,growth_cap) {
  #print("inside negative predicted")
  predicted_length <- length(predicted)
  actual_length <- length(actual)
 
  for (month in 1:predicted_length)
  {
    if(predicted[month] < 0) {
      
      predicted[month] <- (actual[month]*(1+degrowth_cap))
    }else 
    {if(actual[month]!=0)
    {  
      per_cent_growth<-(predicted[month]-actual[month])/(actual[month])
      if(per_cent_growth>growth_cap){
        predicted[month]<-(actual[month])*(1+growth_cap)
        
      }else if(per_cent_growth<0){
        predicted[month]<-(actual[month])*(1+degrowth_cap)
      }
    }
      else
      {
        predicted[month]<-actual[month]
      }
      
    }
  }
  
  return(predicted)
  
}



# calculate for -ve forecasting (1 year window)
# if forecasted value comes out to be negative they are being imputed by the demand value of the previous year
# the growth and degrowth adjustment has also been done simultaneously
CAPPING_FORECASTED <- function(actual, forecasted,degrowth_cap,growth_cap,forecast_duration) {
  
  actual_duration <- length(actual)
  
  print(forecasted)
  
  for(month in 1:12)
  {
    
    if(forecasted[month] < 0) {
      forecasted[month] <- (actual[((actual_duration - 11) + month-1)])*(1+degrowth_cap)
    }
    else{
      if(actual[(actual_duration-11)+month-1]!=0)
      {  
        if(actual[(actual_duration-11)+month-1]!=0)
        {  
          per_cent_growth<-(((forecasted[month]-(actual[(actual_duration-11)+month-1]))/(actual[(actual_duration-11)+month-1])))
          
          if(per_cent_growth>growth_cap){
            forecasted[month]<-(actual[((actual_duration - 11)+month-1)]*(1+growth_cap))
          }else if(per_cent_growth<0){
            forecasted[month]<-(actual[((actual_duration - 11)+month-1)]*(1+degrowth_cap))
          }
        }
      }else
      {
        forecasted[month]<-actual[((actual_duration - 11)+month-1)]
      }
    }
  }
  
  for(month in 13:(forecast_duration))
  {
    
    if(forecasted[month] < 0) {
      forecasted[month] <- (forecasted[month-12])*(1+degrowth_cap)
    }else
    {
      if(forecasted[month-12]!=0)
      {
        per_cent_growth<-(forecasted[month]-forecasted[month-12])/(forecasted[month-12])
        if(per_cent_growth>growth_cap)
        {
          forecasted[month]<-forecasted[month-12]*(1+growth_cap)
        }else if(per_cent_growth<0){
          forecasted[month]<-forecasted[month-12]*(1+degrowth_cap)
        }
      }else
      {
        forecasted[month]<-forecasted[month-12]
      }
    }
  }
  
  
  return(forecasted)
}

#initializing the Holt winters function
HOLT_WINTERS_FORECAST<-function(mt_user,input_path,output_path)
{
  #read the forecast dataset 
  dt_fcst <- fread(input_path)
  
  # getting sequence of fcst_months 
  fcst_months <- format(seq.Date(as.Date(mt_user$START_PERIOD),as.Date(mt_user$END_PERIOD), by = "month"),
                        format = "%b-%Y")
  
  # sequence of dates starting from Mar'2020 to Feb'2021 are being removed
  #rem_period<-format(seq.Date(as.Date(mt_user$REM_START_PERIOD),as.Date(mt_user$REM_END_PERIOD), by = "month"),
  #                   format = "%b-%Y")
  
  #list maintained to adjust the name of the first,second and the third column
  #fcst_months <- fcst_months[-which(fcst_months %in% rem_period)]
  
  #forecasted start and end period are taken as input from the user
  #generate sequence of forecasted dates
  forecasted_dates<-format(seq.Date(as.Date(mt_user$FORECAST_START_DATE),(add.months(as.Date(mt_user$FORECAST_END_DATE),5)), by = "month"),
                           format = "%b-%Y")
  
  colnames(dt_fcst)<-c("site_code","article_code","dept_code",fcst_months)
  #get the forecast duration
  forecasted_duration<-length(forecasted_dates)
  
  print(forecasted_dates)
  
  print(forecasted_duration)
  
  #---- Initializing the level,trend and seasonality ----
  # level index is being calculated by getting the mean estimate
  # l_nought <- rowMeans(dt_fcst[, grep("Apr-2017", colnames(dt_fcst)):grep("Mar-2018", colnames(dt_fcst))])
  l_nought <- rowMeans(dt_fcst[, grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), 
                                      colnames(dt_fcst)):(grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), colnames(dt_fcst)) + 11)])
  
  # 144: 12 * 12
  # b_nought <- (rowSums(dt_fcst[, grep("Apr-2018", colnames(dt_fcst)):grep("Mar-2019", colnames(dt_fcst))]) - rowSums(dt_fcst[, grep("Apr-2017", colnames(dt_fcst)):grep("Mar-2018", colnames(dt_fcst))]))/ (mt_user$fcst_season*mt_user$fcst_season)
  b_nought <- (rowSums(dt_fcst[, (grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), 
                                       colnames(dt_fcst)) + 12):(grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), colnames(dt_fcst)) + 23)]) - rowSums(dt_fcst[, grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), colnames(dt_fcst)):(grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), colnames(dt_fcst)) + 11)])) / (12 *12)
  
  # seasonality
  # s_nought <- dt_fcst[, grep("Apr-2017", colnames(dt_fcst)):grep("Mar-2018", colnames(dt_fcst))] - l_nought
  s_nought <- dt_fcst[, grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), 
                             colnames(dt_fcst)):(grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"),colnames(dt_fcst)) + 11)] - l_nought
  #declaring the resultant list
  ls_results<-list()
  index<-1
  for (dept in 1:nrow(dt_fcst))
  {
    
    
    # this tryCatch has been used to handle an exception to handle the condition where HoltWinters library doesn't respond
    ls_results[[index]] <- tryCatch(
      {
        
        print(dept)
        cat("step 1: preparing time series data", dt_fcst$dept[dept], "/n")
        # getting the sales volume price between the years 2017 and 2022
        row <- dt_fcst[dept, grep(format(as.Date(mt_user$START_PERIOD),"%b-%Y"), colnames(dt_fcst)):grep(format(as.Date(mt_user$END_PERIOD),"%b-%Y"), colnames(dt_fcst))]
        # taking the transpose of the series to fit it into time series
        df <- as.data.frame(t(row))
        # getting the annual time series
        ss <- ts(df, frequency = 12)
        #intilaizing the model
        ml_hw_int <- stats::HoltWinters(ss,
                                        seasonal = c("additive"),
                                        optim.control = list())                               
        
        
        cat("step 2: calculating performance metrics/n")
        # retaining the actual values
        actual <- ml_hw_int$x
        
        print(actual)
        # getting the predicted values
        # fitted helps us to see the calculated value of level,trend and seasonality and final forecasted values
        fitted <- data.frame(ml_hw_int$fitted)
        # the xhat column of fitted has the predicted value on feeding the data in model
        predicted <- fitted$xhat
        
        cat("step 2.1: calculating performance metrics: mape/n")
        # list1 gives us an estimate for which months the predicted values are coming out to be negative
        # e.g. if the predicted_value is negative the value is that index is set to be "TRUE"
        cat("step 3: handling negative values")
        
        fitted <- data.frame(ml_hw_int$fitted)
        actual_predicted_vals <- fitted$xhat
        
        adjusted_predicted_vals<-CALCULATE_NEGATIVE_PREDICTED(actual,actual_predicted_vals,mt_user$DEGROWTH_CAP,mt_user$GROWTH_CAP)
        # forecasted values of 12 months ahead
        actual_forecasted_vals <- predict(ml_hw_int, n.ahead = (forecasted_duration))
        
        #adjusting the forecasted-values such that there are no negative forecasted-values and no forecasted-value has growth above the threshold given by user  
        adjusted_forecasted_vals <- CAPPING_FORECASTED(actual, actual_forecasted_vals,mt_user$DEGROWTH_CAP,
                                                       mt_user$GROWTH_CAP,(forecasted_duration))
        
        cat("step 4: calculating weighted mape")
        # calculating the weighted MAPE
        wted_mape_actual_vals <- CALCULATE_WTED_MAPE(actual, actual_predicted_vals)
        wted_mape_adjusted_vals<- CALCULATE_WTED_MAPE(actual, adjusted_predicted_vals)
        predicted_length <- length(actual_predicted_vals)
        
        #getting the site_code number or company name
        if(any(grepl("company",colnames(dt_fcst)))==TRUE)
        {
          site<-dt_fcst$company[dept]
        }else
        {
          site<-dt_fcst$site_code[dept]
        }
        
        ls_result_it<-data.frame(list("site_code"=site,
                                      "dept_code"=dt_fcst$dept_code[dept],
                                      "article_code"=dt_fcst$article_code[dept],
                                      "alpha"=ml_hw_int$alpha,
                                      "beta"=ml_hw_int$beta,
                                      "gamma"=ml_hw_int$gamma,
                                      "actual_fore"="-->",
                                      "actual_forecasted"=t(actual_forecasted_vals),
                                      "adjusted_fore"="-->",
                                      "adjusted_forecasted"=t(adjusted_forecasted_vals),
                                      "weighted_mape_actual_predictions"=wted_mape_actual_vals,
                                      "weighted_mape_adjusted_predictions"=wted_mape_adjusted_vals))
        
        
      },
      
      error = function(cond) {
        
        print("it went inside")
        if(any(grepl("company",colnames(dt_fcst)))==TRUE)
        {
          site<-dt_fcst$company[dept]
        }else
        {
          site<-dt_fcst$site_code[dept]
        }
        ls_result_it<-data.frame(list("site_code"=site,
                                      "dept_code"=dt_fcst$dept_code[dept],
                                      "article_code"=dt_fcst$article_code[dept],
                                      "alpha"=" ",
                                      "beta"=" ",
                                      "gamma"=" ",
                                      "actual_fore"="-->",
                                      "actual_forecasted"=c(dt_fcst[dept,(grep(format(as.Date(mt_user$END_PERIOD),"%b-%Y"), colnames(dt_fcst))-(forecasted_duration-1)):
                                                                      (grep(format(as.Date(mt_user$END_PERIOD),"%b-%Y"), colnames(dt_fcst)))]
                                      ),
                                      
                                      
                                      
                                      "adjusted_fore"="-->",
                                      "adjusted_forecasted"=c(dt_fcst[dept,(grep(format(as.Date(mt_user$END_PERIOD),"%b-%Y"), colnames(dt_fcst))-(forecasted_duration-1)):
                                                                        (grep(format(as.Date(mt_user$END_PERIOD),"%b-%Y"), colnames(dt_fcst)))]
                                      ),
                                      
                                      
                                      "weighted_mape_actual_predictions"=0,
                                      "weighted_mape_adjusted_predictions"=0))
        
        
        
        
        return(ls_result_it)
      }
      
    )
    index<-index+1
    
  }
  
  #combining the forecast corresponding to all the departments
  dt_output<-rbindlist(ls_results, use.names = FALSE)
  #omiting the store-articles with 0 sales over the last year- the assumption is that dept would have been discontinued at store level
  dt_output<-na.omit(dt_output)
  
  print(ls_results)
  
  cat("step 5: consolidating generated models/outputs", "/n")
  if(any(grepl("company",colnames(dt_fcst)))==TRUE)
  {
    colnames(dt_output) <- c("company", "dept_code", "article_code", "alpha", "beta", "gamma","actual_forecasted-->", forecasted_dates, "adjusted_forecasted-->",forecasted_dates, "weighted_mape_on_actual_pred","weighted_mape_on_adjusted_pred")
  }else
  {
    colnames(dt_output) <- c("site_code", "dept_code", "article_code", "alpha", "beta", "gamma","actual_forecasted-->", forecasted_dates, "adjusted_forecasted-->",forecasted_dates, "weighted_mape_on_actual_pred","weighted_mape_on_adjusted_pred")
  }
  # write the csv file in the output path
  cat("step 6.1: saving output at following location", "/n")
  cat("filename: ",output_path, "/n")
  
  fwrite(dt_output,output_path)
  
  return(dt_output)
  
  
  
}





if(mt_user$RUN_AT=="P1"){
  
  input_path<- list.files(file.path(path_proj,"PREPROCESSED_OPS","P1","DEPT_SALES"),full.names=TRUE)
  output_path<- file.path(path_proj,"HWA_FCST_OPS","P1", "P1_ANALYSIS","P1_STR_DEPT_FCST.CSV")
  
  
}else if(mt_user$RUN_AT=="P2"){
  
  
  input_path<- list.files(file.path(path_proj,"PREPROCESSED_OPS","P2","DEPT_SALES"),full.names=TRUE)
  output_path<- file.path(path_proj,"HWA_FCST_OPS","P2", "P2_ANALYSIS","P2_STR_DEPT_FCST.CSV")
  
  
  
}else if(mt_user$RUN_AT=="NSO")
{
  input_path<- list.files(file.path(path_proj,"PREPROCESSED_OPS","NSO","DEPT_SALES"),full.names=TRUE)
  output_path<- file.path(path_proj,"HWA_FCST_OPS","NSO", "NSO_ANALYSIS","NSO_STR_DEPT_FCST.CSV")
  

}else if(mt_user$RUN_AT=="ALL"){
 
  
  input_path<- list.files(file.path(path_proj,"PREPROCESSED_OPS","ALL","DEPT_SALES"),full.names=TRUE)
  output_path<- file.path(path_proj,"HWA_FCST_OPS","ALL", "ALL_ANALYSIS","ALL_STR_DEPT_FCST.CSV")
  
  
}


dt_fcst_output<-HOLT_WINTERS_FORECAST(mt_user,input_path,output_path)


#sequence of dates for which the forecast has been generated
forecast_dates<- format(seq.Date(as.Date(mt_user$FORECAST_START_DATE),(add.months(as.Date(mt_user$FORECAST_END_DATE),5)), by = "month"),
                        format = "%b-%Y")

dt_fcst_str_dept<- subset(dt_fcst_output[, .SD, .SDcols = ! duplicated(colnames(dt_fcst_output),fromLast=TRUE)],
                          select=c("site_code","dept_code", "article_code", forecast_dates
                          ))


fwrite(dt_fcst_str_dept,file.path(path_proj,"HWA_FCST_OPS",mt_user$RUN_AT, paste0(mt_user$RUN_AT,"_STR_DEPT"),paste0(mt_user$RUN_AT,"_STR_DEPT_FCST.CSV")))




