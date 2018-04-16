#--- ISRIC 250m
#--- Grid Resolution 250meter worldwide
#--- 0, 5, 15, 30, 60, 100, 200 cm depth

library(dplyr)
library(foreign) #Load library for DBF import

#--- Working space soil DB
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/FE_Costumer_Soil_Analysis/formated")

#--- Read DB
dbs = read.csv(file = "Soil_Analysis_DB_template.csv",header = T)

#--- Working space DBF files

#--- Centroids
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/ISRIC/ISRIC_250m/costumers/centroids")

#--- DBF file list centroids
dbf_list = dir(pattern = "*.dbf")
dbf_df   = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
dbf_df$V1= NULL #Remove the "NA" col
colnames(dbf_df) = c("pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
#colnames(dbf_df) = c("tif","m","sl","res","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
dbf_df$filename = dbf_list
dbf_df_cen = dbf_df

#--- Zonal Statistics
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/ISRIC/ISRIC_250m/costumers/statistics")

#--- DBF file list centroids
dbf_list = dir(pattern = "*.dbf")
dbf_df   = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
colnames(dbf_df) = c("tif","m","sl","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
dbf_df$filename = dbf_list
dbf_df_zos = dbf_df
rm(dbf_df)

#--- Remove noData dbf files from list
unique(dbf_df_cen$pe_id2)
unique(dbf_df_zos$pe_id2)

dbf_df_cen = dbf_df_cen[!(dbf_df_cen$pe_id3=="Horta964917"),]   # Remove Horta from Santa Ernestina
dbf_df_cen = dbf_df_cen[!(dbf_df_cen$pe_id2=="SaoFrancisco"),]  # Remove Sao Francisco (out of .tif bounds)
dbf_df_cen = dbf_df_cen[!(dbf_df_cen$pe_id2=="Rodeio" & dbf_df_cen$pe_id3=="Subfield2976547"),]  # Remove subfield from Rodeio due to field "ZONE"
dbf_df_cen = dbf_df_cen[!(dbf_df_cen$pe_id2==""),]  # T01 02 03 04...

dbf_df_zos = dbf_df_zos[!(dbf_df_zos$pe_id3=="Horta964917"),]   # Remove Horta from Santa Ernestina
dbf_df_zos = dbf_df_zos[!(dbf_df_zos$pe_id2=="SaoFrancisco"),]  # Remove Sao Francisco (out of .tif bounds)
dbf_df_zos = dbf_df_zos[!(dbf_df_zos$pe_id2=="Rodeio" & dbf_df_zos$pe_id3=="Subfield2976547"),]  # Remove subfield from Rodeio due to field "ZONE"
dbf_df_zos = dbf_df_zos[!(dbf_df_zos$pe_id2==""),]  # T01 02 03 04...

#--- Unique names by tif type
utif = unique(dbf_df_zos$tif)

#--- Read and gather all dbf files and write them in CSVs named as utif
lapply(utif,wcsv)

#--- Import csv files
bldfie = read.csv(file = paste(utif[1],".csv",sep = ""))
cecsol = read.csv(file = paste(utif[2],".csv",sep = ""))
clyppt = read.csv(file = paste(utif[3],".csv",sep = ""))
orcdrc = read.csv(file = paste(utif[4],".csv",sep = ""))
phihox = read.csv(file = paste(utif[5],".csv",sep = ""))
sltppt = read.csv(file = paste(utif[6],".csv",sep = ""))
sndppt = read.csv(file = paste(utif[7],".csv",sep = ""))

#--- Text to column
bldfie = ttc(bldfie)
cecsol = ttc(cecsol)
clyppt = ttc(clyppt)
orcdrc = ttc(orcdrc)
phihox = ttc(phihox)
sltppt = ttc(sltppt)
sndppt = ttc(sndppt)

#--- Unique DB
isric_zs = rbind(bldfie,
                 cecsol,
                 clyppt,
                 orcdrc,
                 phihox,
                 sltppt,
                 sndppt)

#--- Release memory
rm(bldfie,cecsol,clyppt,orcdrc,phihox,sltppt,sndppt)

#--- Create new ID colum
isric_zs$shpname = paste(isric_zs$pe_id1,isric_zs$pe_id2,isric_zs$pe_id3,isric_zs$pe_id4, isric_zs$zband,isric_zs$clustermethod,isric_zs$dateofzoning, sep ="_")

#--- Read IDs 
id_fn = read.csv(file = "C:/Murilo/GIS/BRA_Costumers/list_fn_id.csv")
id_fn$shpname = id_fn$filename

#--- Include IDs in isric
isric_zs = merge(isric_zs,id_fn, by = "shpname")

#isric_zs$zs_mean[(isric_zs$zs_mean=="NA")]
#isric_zs_clean = isric_zs[!(isric_zs$zs_mean=="NA"),]

#--- Rename dbs col to match isric
dbs_m          = dbs
dbs_m$Field    = dbs_m$Field_Name
dbs_m$Subfield = dbs_m$ubfield_Name
dbs_m$ZoneID   = dbs_m$Zone.ID

dbs_00_15 = dbs_m[dbs_m$depth_cm=="0-15",]
dbs_00_20 = dbs_m[dbs_m$depth_cm=="0-20",]
dbs_00_30 = dbs_m[dbs_m$depth_cm=="0-30",]
dbs_15_30 = dbs_m[dbs_m$depth_cm=="15-30",]

#--- Merge isric_zs with dbs_m as function of (Farm, Field, Subfield, ZoneID)
dbs_isric_00_15 = merge(isric_zs, dbs_00_15, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-15 cm
dbs_isric_00_20 = merge(isric_zs, dbs_00_20, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-20 cm
dbs_isric_00_30 = merge(isric_zs, dbs_00_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-30 cm
dbs_isric_15_30 = merge(isric_zs, dbs_15_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #15-30 cm

x = dbs_isric_00_15$Clay_g.kg[dbs_isric_00_15$tif=="CLYPPT"]
y = dbs_isric_00_15$zs_mean[dbs_isric_00_15$tif=="CLYPPT"]


plot(dbs_isric_00_15$zs_mean[dbs_isric_00_15$tif=="CLYPPT"]~dbs_isric_00_15$Clay_g.kg[dbs_isric_00_15$tif=="CLYPPT"])
plot(dbs_isric_00_20$zs_mean[dbs_isric_00_20$tif=="CLYPPT"]~dbs_isric_00_20$Clay_g.kg[dbs_isric_00_20$tif=="CLYPPT"])
plot(dbs_isric_00_30$zs_mean[dbs_isric_00_30$tif=="CLYPPT"]~dbs_isric_00_30$Clay_g.kg[dbs_isric_00_30$tif=="CLYPPT"])
plot(dbs_isric_15_30$zs_mean[dbs_isric_15_30$tif=="CLYPPT"]~dbs_isric_15_30$Clay_g.kg[dbs_isric_15_30$tif=="CLYPPT"])

curvplot(dbs_isric_15_30$zs_mean[dbs_isric_15_30$tif=="CLYPPT"],dbs_isric_15_30$Clay_g.kg[dbs_isric_15_30$tif=="CLYPPT"])

fit = lm(y~x)
xx = seq(0,800,length = 1000)
lines(xx, predict(fit, data.frame(x=xx)),col="red")

curvplot = function(y,x){
  plot(y~x)
  fit = lm(y~x)
  xx = seq(0,800,length = 1000)
  lines(xx, predict(fit, data.frame(x=xx)),col="red")
}

write.table(isric_zs        ,file = "C:/Murilo/isric_zs.csv"        ,sep = ",")
write.table(dbs_00_15       ,file = "C:/Murilo/dbs_00_15.csv"       ,sep = ",")
write.table(dbs_isric_00_15 ,file = "C:/Murilo/dbs_isric_00_15.csv" ,sep = ",")

#--- Charts
par(mfrow=c(2,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))


h = hist(isric_zs$zs_mean[isric_zs$tif==utif[3]])
h$density = h$counts/sum(h$counts)

barplot(h$density*100, beside = T)
plot(h$density*100 ~ h$mids, type = "b")

#--- Measured Clay content 
hist(dbs$Clay_g.kg/10,
     freq = F,
     col  = "red",
     xlab = "Clay Content [%] - Measured",
     ylab = "Frequency",
     xlim = c(0,100),
     ylim = c(0,0.1),
     cex.lab = 1.5,
     cex.axis= 1.5,
     main =NULL)

#lines(hline_isric$density~hline_isric$mids,col="black")
#hline_isric = hist(isric_zs$zs_mean[isric_zs$tif==utif[3]]*1.6)

#--- ISRIC Clay content
hist(isric_zs$zs_mean[isric_zs$tif==utif[3]],
     freq = F,
     col  = "red",
     xlab = "Clay Content [%] - ISRIC(250m)",
     ylab = "Frequency",
     xlim = c(0,100),
     ylim = c(0,0.1),
     cex.lab = 1.5,
     cex.axis= 1.5,
     main =NULL)

lines(l$density~l$mids)

l = hist(isric_zs$zs_mean[isric_zs$tif==utif[3]])


sum(l$density)

 
plot(isric_zs$zs_medi[isric_zs$tif==utif[3]]~isric_zs$zs_mean[isric_zs$tif==utif[3]])

boxplot(isric_zs$zs_mean[isric_zs$tif==utif[3]])

hist(isric_zs$zs_mean[isric_zs$tif==utif[3]])
hist(dbs$Clay_g.kg)

utif



#-----------------#
#--- Functions ---#
#-----------------#

#--- Gather dbf files and write as .csv
wcsv = function(y){
  dbf_ls = dbf_df_zos$filename[dbf_df_zos$tif==y]
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
  colnames(f) = c("Id",
                  "ZoneID",
                  "FieldID",
                  "FieldOpID",
                  "ZoneMean",
                  "ZoneMin",
                  "ZoneMax",
                  "PolyMean",
                  "ZoneArea",
                  "PolyArea",
                  "zs_min",
                  "zs_max",
                  "zs_sum",
                  "zs_coun",
                  "zs_mean",
                  "zs_std",
                  "zs_uniq",
                  "zs_rang",
                  "zs_var",
                  "zs_medi",
                  "zs_mode",
                  "filename")
  f
}


ttc = function(df) {
  #Text to colum (filename column)
  ttc_fn = read.table(text = as.character(df$filename),sep="_",colClasses = "character",fill = T)
  colnames(ttc_fn) = c("tif","m","sl","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
  df = data.frame(df,ttc_fn)
}



filt = function(x){
  x = x[!(x$pe_id3=="Horta964917"),]   # Remove Horta from Santa Ernestina
  x = x[!(x$pe_id2=="SaoFrancisco"),]  # Remove Sao Francisco (out of .tif bounds)
  x = x[!(x$pe_id2=="Rodeio" & x$pe_id3=="Subfield2976547"),]  # Remove subfield from Rodeio due to field "ZONE"
  x = x[!(x$pe_id2==""),]  # T01 02 03 04...
}





