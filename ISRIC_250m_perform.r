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

#--- Remove noData dbf files from list
unique(dbf_df$pe_id3)
dbf_df = dbf_df[!(dbf_df$pe_id3=="Horta964917"),]   # Remove Horta from Santa Ernestina
dbf_df = dbf_df[!(dbf_df$pe_id2=="SaoFrancisco"),]  # Remove subfield from Sao Francisco
dbf_df = dbf_df[!(dbf_df$pe_id2=="Rodeio" & dbf_df$pe_id3=="Subfield2976547"),]  # Remove subfield from Rodeio due to field "ZONE"
dbf_df = dbf_df[!(dbf_df$pe_id2==""),]  # T01 02 03 04...

length(dbf_df$res)

#--- Load library for DBF import
library(foreign)

#--- Unique names by tif type
utif = unique(dbf_df$tif)

#--- Read and gather all dbf files
lapply(utif,wcsv)


#-----------------#
#--- Functions ---#
#-----------------#

#--- Gather dbf files and write as .csv
wcsv = function(y){
  dbf_ls = dbf_df$filename[dbf_df$tif==y]
  dbf    = rdbfsev(dbf_ls)
  write.csv(dbf, file = paste(y,".csv",sep=""), quote = F)
  
}

#--- Function Read Several dbf
rdbfsev = function(z){
  dbf_files_df = lapply(z,rdbf)
  rdbfsev = do.call("rbind", lapply(dbf_files_df, as.data.frame))
  rdbfsev
}

#--- Read DBF function
rdbf = function(x) {
  f = read.dbf(file = x, as.is = F)
  f$filename = x
  f$ACRON = NULL #Remove this additional field (added on Qgis only for layout)
  f
}


#Check col names
#cnames = lapply(dbf_files_df, function(x) colnames(x))



