
library("arrow")
library("data.table")
library("dplyr")


months<- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
ls_files<- list.files("D:/database/2017_to_2022_data/2022_day", full.names= TRUE)
ls_result<- list()
mon_ind<-1

for(month in months)
{
 
  mon_files<- ls_files[grep(month, ls_files)]
  ls_mon<- list()
  i<-1
  for(file in mon_files)
  {
    df<- read_parquet(file)
    print(df$Bill_Date)
    df<- df[which(df$Division_Group=="1.Apparels"),]
    print(df)
    df<- df[,c("Site_Code","Article_Code","Net_Amount_SUM")]
    df<-df[,lapply(.SD, sum, na.rm = TRUE),.(Site_Code,Article_Code)]
    ls_mon[[i]]<-df
    i<-i+1
    
  }
  df<-rbindlist(ls_mon)
  df<-df[,lapply(.SD, sum, na.rm = TRUE),.(Site_Code,Article_Code)]
  names(df)[3] <- paste0(month,"-","2022")
  ls_result[[mon_ind]]<-df
  print(mon_ind)
  mon_ind<-mon_ind+1
}
  
  
df_final<-ls_result[[1]]
for(i in 2:12)
{
  df<-ls_result[[i]]
  df_final<-full_join(df_final,df,by=c("Site_Code","Article_Code"))
}
  
df_final[is.na(df_final)]<-0  

fwrite(df_final,"D:/products/FORECAST_FINAL_PROJECT - SOUTH/COMPANY_LEVEL_SEPARATED_DATA/yr_2022_rev.csv")
  
  
  
  df_mon_rev<- subset(df_mon, select=-c(Bill_Qty_SUM))[,lapply(.SD, sum, na.rm = TRUE),.(Site_Code,Article_Code)]
  colnames(df_mon_rev)<- c("site_code","article_code","Apr-2023")
  df_mon_qty<- subset(df_mon, select=-c(Net_Amount_SUM))[,lapply(.SD, sum, na.rm = TRUE),.(Site_Code,Article_Code)]
  colnames(df_mon_qty)<- c("site_code","article_code","Apr-2023")

  
fwrite(df_mon_rev, "D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/APR_SALES_REV_DATA.csv")  
fwrite(df_mon_qty, "D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/APR_SALES_QTY_DATA.csv")  
  
sales_rev<- fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_REV_DATA.csv")
sales_qty<- fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_QTY_DATA.csv")  




sales_rev<- full_join(sales_rev, df_mon_rev, by=c("site_code","article_code"))
sales_rev[is.na(sales_rev)]<-0

sales_qty<- full_join(sales_qty, df_mon_qty, by=c("site_code","article_code"))
sales_qty[is.na(sales_qty)]<-0

fwrite(sales_rev,"D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_REV_DATA1.csv")
fwrite(sales_qty, "D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_QTY_DATA1.csv" )





dt_fcst<-read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/ALL/ALL_STR_DEPT_ADJ_FCST.xlsx")

dt_ip<- fread("D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/ALL/ALL_SALES/ALL_SALES_REV.csv")

dt_dept_ip<- dt_ip[,-c(2,4)][,lapply(.SD, sum, na.rm = TRUE),.(site_code,dept_code)]


str_mstr<- fread("D:/products/FORECAST_FINAL_PROJECT/STORE_MSTR/STORE_MSTR.csv")


dt_fcst<- inner_join(dt_fcst, dt_str, by=c("store"="Store"))
p1_fcst<- dt_fcst[which(dt_fcst$Type=="P1"),]

dt_dept_ip$site_code<- as.character(dt_dept_ip$site_code)


p1_com<- inner_join(dt_dept_ip, p1_fcst, by=c("site_code","dept_code"))

fwrite(p1_com, "D:/products/FORECAST_FINAL_PROJECT/COMPARSION/P1_COMPARSION.csv")


str_mstr[!(str_mstr$store %in% dt_fcst$store),]


nso_strs<- read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/NSO/NSO_STR_DEPT_ADJ_FCST.xlsx")
nso_strs$concat<- paste0(nso_strs$site_code, "_", nso_strs$dept_code)
nso_strs<- nso_strs[!duplicated(nso_strs$concat),]


write_xlsx(nso_strs,"D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/NSO/NSO_STR_DEPT_ADJ_FCST.xlsx" )




str_fcst<- read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/ALL/ALL_STR_DEPT_ADJ_FCST.xlsx")
nso_strs<- read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/NSO/NSO_STR_DEPT_ADJ_FCST.xlsx")
str_fcst<- rbind(str_fcst, nso_strs)

str_fcst$concat<- paste0(str_fcst$store,"_",str_fcst$dept_code)

str_fcst<- str_fcst[!duplicated(str_fcst$concat),]

str_fcst<- subset(str_fcst, select=-c(concat))

write_xlsx(str_fcst, "D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/ALL/ALL_STR_DEPT_ADJ_FCST_all_strs.xlsx")


str_list<- read_excel("D:/products/FORECAST_FINAL_PROJECT/STORE_LIST/ROI P1 P2  NSO Stores List 19 May Apr 2024.xlsx", skip=3)


sales_rev<- fread("D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/ALL/ALL_SALES/ALL_SALES_REV.csv")
sales_qty<- fread("D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/ALL/ALL_SALES/ALL_SALES_QTY.csv")


p2_sales_rev<- sales_rev[which(sales_rev$site_code==101878),]
p2_sales_qty<- sales_qty[which(sales_qty$site_code==101878),]


fwrite(p2_sales_rev,"D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/P2/P2_SALES/P2_SALES_REV.csv")

fwrite(p2_sales_qty,"D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/P2/P2_SALES/P2_SALES_QTY.csv")


dt_fcst<- read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/ALL/ALL_STR_DEPT_ADJ_FCST.xlsx")




dt_hwa<- fread("D:/products/FORECAST_FINAL_PROJECT/HWA_FCST_OPS/ALL/ALL_STR_DEPT/ALL_STR_DEPT_FCST.CSV")
dt_ip<- fread("D:/products/FORECAST_FINAL_PROJECT/PREPROCESSED_OPS/ALL/DEPT_SALES/ALL_STR_DEPT.csv")














df_rev<- fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_REV_DATA.csv")
df_qty<-fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_QTY_DATA.csv")
apr_rev<- fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/APR_SALES_REV_DATA.csv")
apr_qty<- fread("D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/APR_SALES_QTY_DATA.csv")

df_rev<- full_join(df_rev, apr_rev, by=c("site_code","article_code"))
df_qty<- full_join(df_qty, apr_qty, by=c("site_code","article_code"))


fwrite(df_rev, "D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_REV_DATA.csv")
fwrite(df_qty, "D:/products/FORECAST_FINAL_PROJECT/SALES_DATA/SALES_QTY_DATA.csv")


p1_sales<- fread("D:/products/FORECAST_FINAL_PROJECT/PREPROCESSED_OPS/P1/DEPT_SALES/P1_STR_DEPT.csv")


library("readr")

p1_ip<-fread("D:/products/FORECAST_FINAL_PROJECT/COMPANY_LEVEL_SEPARATED_DATA/ALL/ALL_SALES/ALL_SALES_REV.csv")
p1_op<-read_excel("D:/products/FORECAST_FINAL_PROJECT/P2P_ADJUSTED_OPS/ALL/FEB-2024_TO_OCT-2024_FORECAST.xlsx")

dress_ip<- p1_ip[which(p1_ip$dept_code==5883),]
dress_op<- p1_op[which(p1_op$dept_code==5883),]









dt<- fread("D:/products/FORECAST_FINAL_PROJECT - SOUTH/HWA_FCST_OPS/ALL/ALL_STR_DEPT/ALL_STR_DEPT_FCST.CSV")
p1_str_mstr<- fread("D:/products/FORECAST_FINAL_PROJECT - SOUTH/COMPANY_LEVEL_SEPARATED_DATA/P1/P1_STR_MSTR/P1_STORE_MSTR.csv")
p2_str_mstr<- fread("D:/products/FORECAST_FINAL_PROJECT - SOUTH/COMPANY_LEVEL_SEPARATED_DATA/P2/P2_STR_MSTR/P2_STORE_MSTR.csv")
nso_str_mstr<- fread("D:/products/FORECAST_FINAL_PROJECT - SOUTH/COMPANY_LEVEL_SEPARATED_DATA/NSO/NSO_STR_MSTR/NSO_STORE_MSTR.csv")  

dt_str_mstr<-rbind(p1_str_mstr, p2_str_mstr)
dt_str_mstr<-rbind(dt_str_mstr, nso_str_mstr)
































