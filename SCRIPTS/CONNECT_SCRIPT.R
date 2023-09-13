cat("Cleaning enviornment", "/n")
rm(list = ls())
gc()

path_proj<-"D:/products/SOUTH_FORECASTS"


SEPARATED_DATA<- function(type_of_store)
{
  setwd(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA"))
  dir.create(type_of_store, showWarnings = FALSE, recursive = TRUE)
  setwd(file.path(path_proj,"COMPANY_LEVEL_SEPARATED_DATA",type_of_store))
  dir.create(paste0(type_of_store,"_SALES"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(type_of_store,"_STR_MSTR"), showWarnings = FALSE, recursive = TRUE)
  
}


#create the required output folders for cleaned data
setwd(path_proj)
dir.create("COMPANY_LEVEL_SEPARATED_DATA", showWarnings = FALSE, recursive = FALSE)

SEPARATED_DATA("P1")
SEPARATED_DATA("P2")
SEPARATED_DATA("NSO")
SEPARATED_DATA("ALL")

#calling the clean_data script
source(file.path(path_proj,"SCRIPTS","CLEAN_DATA.R"))

print("cleaning data complete")


PREPROCESS<- function(type_of_store)
{
  setwd(file.path(path_proj,"PREPROCESSED_OPS"))
  dir.create(type_of_store, showWarnings = FALSE, recursive = TRUE)
  setwd(file.path(path_proj,"PREPROCESSED_OPS",type_of_store))
  dir.create("ARTICLE_SALES", showWarnings = FALSE, recursive = TRUE)
  dir.create("DEPT_SALES", showWarnings = FALSE, recursive = TRUE)
}

#create the required output folders for lost data
setwd(path_proj)
dir.create("PREPROCESSED_OPS", showWarnings = FALSE, recursive= FALSE)


#create the o/p folders for lost-sales and proxy-sales
PREPROCESS("P1")
PREPROCESS("P2")
PREPROCESS("NSO")
PREPROCESS("ALL")


#calling the lost_sales script
source(file.path(path_proj,"SCRIPTS","LOST_SALES.R"))

print("lost sales complete")


#calling the proxy-sales script
source(file.path(path_proj,"SCRIPTS","PROXY_SALES.R"))

print("proxy sales complete")


#create the o/p folders for HWA-FCST files
setwd(path_proj)
dir.create("HWA_FCST_OPS", showWarnings = FALSE, recursive= FALSE)


FCST_ANALYSIS_DATA<- function(type_of_store)
{
  setwd(file.path(path_proj,"HWA_FCST_OPS"))
  dir.create(type_of_store, showWarnings = FALSE, recursive = TRUE)
  setwd(file.path(path_proj,"HWA_FCST_OPS",type_of_store))
  dir.create(paste0(type_of_store,"_","ANALYSIS"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(type_of_store,"_","STR_DEPT"), showWarnings = FALSE, recursive = TRUE)
  
}


#create the o/p folders for HWA-FORECAST
FCST_ANALYSIS_DATA("P1")
FCST_ANALYSIS_DATA("P2")
FCST_ANALYSIS_DATA("NSO")
FCST_ANALYSIS_DATA("ALL")

#calling the hwa-fcst scripts
source(file.path(path_proj,"SCRIPTS","HWA_FORECAST.R"))


#create the o/p folders for P2P-ADJUSTMENT
setwd(path_proj)
dir.create("P2P_ADJUSTED_OPS", showWarnings = FALSE, recursive = FALSE)


P2P_ANALYSIS_DATA<-function(type_of_store)
{
  setwd(file.path(path_proj,"P2P_ADJUSTED_OPS"))
  dir.create(type_of_store, showWarnings = FALSE, recursive = TRUE)
}


P2P_ANALYSIS_DATA("P1")
P2P_ANALYSIS_DATA("P2")
P2P_ANALYSIS_DATA("NSO")
P2P_ANALYSIS_DATA("ALL")

source(file.path(path_proj,"SCRIPTS","P2P_ADJUSTMENT_FCSTS.R"))

















