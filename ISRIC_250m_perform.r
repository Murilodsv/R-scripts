#--- Working space soil DB
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/FE_Costumer_Soil_Analysis/formated")

#--- Read DB
dbs = read.csv(file = "Soil_Analysis_DB_template.csv",header = T)

#--- Working space DBF files
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/ISRIC/ISRIC_250m/costumers")

#--- DBF file list
dbf_list = dir(pattern = "*.dbf")
dbf_df   = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
colnames(dbf_df) = c("tif","m","sl","res","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
dbf_df$filename = dbf_list


length(dbf_df$res)
#--- Load library for DBF import
library(foreign)

#--- Read DBF
utif = unique(dbf_df$tif)

#--- Clay files
dbfcl_ls = dbf_df$filename[dbf_df$tif=="CLYPPT"]

dbfrd = dbfcl_ls

dbf_files_df = lapply(dbfrd,function(x){read.dbf(file = x, as.is = F); print(x)})
combined_dbf = do.call("rbind", lapply(dbf_files_df, as.data.frame))

combined_dbf


length(dbf1$X_mean) + length(dbf2$X_mean)

length(combined_df$X_mean)

help(lapply)

dbf1$X_mean
dbf2$X_mean

dbf

length(dbf_list)

tail(dbf_list)

dbf_list[1000:1607]


read.delim(file = dbf_list, sep = "_")

apropos("read")

help("read.delim")
