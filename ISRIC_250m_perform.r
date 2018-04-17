#--- ISRIC 250m
#--- Grid Resolution 250meter worldwide
#--- 0, 5, 15, 30, 60, 100, 200 cm depth

library(dplyr)
library(utils)
library(foreign) #Load library for DBF import

#--- Working space soil DB
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/FE_Costumer_Soil_Analysis/formated")

#--- Read DB
dbs = read.csv(file = "Soil_Analysis_DB_template.csv",header = T)

#--- Working space DBF files

#--- Centroids
setwd("C:/Murilo/GIS/BRA_Costumers/isric_cen")

#--- DBF file list centroids
dbf_list = dir(pattern = "*.dbf")
dbf_df   = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
#dbf_df$V1= NULL #Remove the "NA" col
colnames(dbf_df) = c("isricID","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
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

#--- Read and gather all dbf files and write them in CSVs named as utif only for ZS!
lapply(utif,wcsv)



#--- For centroids...
dbf_sev = lapply(dbf_df_cen$filename,rdbfv1)
cen_dbf = do.call("rbind", lapply(dbf_sev, as.data.frame))
write.csv(cen_dbf, file = "centroids.csv", quote = F)

#--- Read centroids when they are already saved into csv file
isric_cen = read.csv("centroids.csv")

idc = read.table(text = as.character(isric_cen$filename),sep="_",colClasses = "character",fill = T)
colnames(idc) = c("isricID","pe_id1","pe_id2","pe_id3","pe_id4","zband","clustermethod","dateofzoning")
isric_cen = data.frame(isric_cen, idc)

#--- Create new ID colum
isric_cen$shpname = paste(isric_cen$pe_id1,isric_cen$pe_id2,isric_cen$pe_id3,isric_cen$pe_id4, isric_cen$zband,isric_cen$clustermethod,isric_cen$dateofzoning, sep ="_")

#--- Include IDs in isric
isric_cen = merge(isric_cen,id_fn, by = "shpname")


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
dbs_isric_00_15_zs = merge(isric_zs, dbs_00_15, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-15 cm
dbs_isric_00_20_zs = merge(isric_zs, dbs_00_20, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-20 cm
dbs_isric_00_30_zs = merge(isric_zs, dbs_00_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-30 cm
dbs_isric_15_30_zs = merge(isric_zs, dbs_15_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #15-30 cm

#--- Merge isric_cen with dbs_m as function of (Farm, Field, Subfield, ZoneID)
dbs_isric_00_15_cen = merge(isric_cen, dbs_00_15, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-15 cm
dbs_isric_00_20_cen = merge(isric_cen, dbs_00_20, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-20 cm
dbs_isric_00_30_cen = merge(isric_cen, dbs_00_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #00-30 cm
dbs_isric_15_30_cen = merge(isric_cen, dbs_15_30, by = c("Farm_acron","Field","Subfield","ZoneID")) #15-30 cm

setwd("C:/Murilo/GIS/BRA_Costumers/isric_zs")
write.csv(dbs_isric_00_15_zs,file = "dbs_isric_00_15_zs.csv")
write.csv(dbs_isric_00_20_zs,file = "dbs_isric_00_20_zs.csv")
write.csv(dbs_isric_00_30_zs,file = "dbs_isric_00_30_zs.csv")
write.csv(dbs_isric_15_30_zs,file = "dbs_isric_15_30_zs.csv")

setwd("C:/Murilo/GIS/BRA_Costumers/isric_cen")
write.csv(dbs_isric_00_15_cen,file = "dbs_isric_00_15_cen.csv")
write.csv(dbs_isric_00_20_cen,file = "dbs_isric_00_20_cen.csv")
write.csv(dbs_isric_00_30_cen,file = "dbs_isric_00_30_cen.csv")
write.csv(dbs_isric_15_30_cen,file = "dbs_isric_15_30_cen.csv")



#--- Reading ZS CSV files
setwd("C:/Murilo/GIS/BRA_Costumers/isric_zs")
dbs_isric_00_15_zs = read.csv(file = "dbs_isric_00_15_zs.csv")
dbs_isric_00_20_zs = read.csv(file = "dbs_isric_00_20_zs.csv")
dbs_isric_00_30_zs = read.csv(file = "dbs_isric_00_30_zs.csv")
dbs_isric_15_30_zs = read.csv(file = "dbs_isric_15_30_zs.csv")

#--- Reading Centroids CSV files
setwd("C:/Murilo/GIS/BRA_Costumers/isric_cen")
dbs_isric_00_15_cen = read.csv(file = "dbs_isric_00_15_cen.csv")
dbs_isric_00_20_cen = read.csv(file = "dbs_isric_00_20_cen.csv")
dbs_isric_00_30_cen = read.csv(file = "dbs_isric_00_30_cen.csv")
dbs_isric_15_30_cen = read.csv(file = "dbs_isric_15_30_cen.csv")

#--- Chart Analysis

v = c("sl1","sl2","sl3","sl4")

#--- Sand
png("C:/Murilo/GIS/BRA_Costumers/sand_isric_zs.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Sand_g.kg"
tifname = "SNDPPT"
xla = "Measured - Sand fraction"
yla = "ISRIC - Sand fraction"
rsqrt_snd = sapply(v, crt)

dev.off() # end of chart exportation

#--- Clay
png("C:/Murilo/GIS/BRA_Costumers/clay_isric_zs.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Clay_g.kg"
tifname = "CLYPPT"
xla = "Measured - Clay fraction"
yla = "ISRIC - Clay fraction"
rsqrt_cly = sapply(v, crt)

dev.off() # end of chart exportation

#--- Silt
png("C:/Murilo/GIS/BRA_Costumers/silt_isric_zs.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Silt_g.kg"
tifname = "SLTPPT"
xla = "Measured - Silt fraction"
yla = "ISRIC - Silt fraction"
rsqrt_slt = sapply(v, crt)

dev.off() # end of chart exportation


#--- CEC
png("C:/Murilo/GIS/BRA_Costumers/cec_isric_zs.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Cation_Exchange_Capacity_C.E.C_mmolc.dm.3"
tifname = "CECSOL"
xla = "Measured - CEC mmolc/dm3"
yla = "ISRIC - CEC cmolc/kg"
rsqrt_cec = sapply(v, crt)

dev.off() # end of chart exportation


#--- OC
png("C:/Murilo/GIS/BRA_Costumers/oc_isric_zs.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Total_Organic_C_g.dm.3"
tifname = "ORCDRC"
xla = "Measured - Org C g/dm3"
yla = "ISRIC - Org C g/kg"
rsqrt_orc = sapply(v, crt)

dev.off() # end of chart exportation



#--- pH in Water cannot do because of lack of data only few measured data

#--- Write performance
rsqrd = rbind(rsqrt_slt,rsqrt_cly,rsqrt_snd,rsqrt_cec,rsqrt_orc)
write.csv(rsqrd, file = "C:/Murilo/GIS/BRA_Costumers/rsqrd_zs.csv")


#--- for centroids

dbs_isric_cen = rbind(dbs_isric_00_15_cen,dbs_isric_00_20_cen,dbs_isric_00_30_cen,dbs_isric_15_30_cen)


#--- Clay - Cen
png("C:/Murilo/GIS/BRA_Costumers/clay_isric_cen.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Clay_g.kg"
tifname = "CLYPPTM"
xla = "Measured - Clay frac"
yla = "ISRIC - Clay frac"
rsqrt_cly = sapply(v, crt)

dev.off() # end of chart exportation


#--- Silt - Cen
png("C:/Murilo/GIS/BRA_Costumers/silt_isric_cen.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Silt_g.kg"
tifname = "SLTPPTM"
xla = "Measured - Silt frac"
yla = "ISRIC - Silt frac"
rsqrt_slt = sapply(v, crt)

dev.off() # end of chart exportation

#--- Sand - Cen
png("C:/Murilo/GIS/BRA_Costumers/sand_isric_cen.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Sand_g.kg"
tifname = "SNDPPTM"
xla = "Measured - Sand frac"
yla = "ISRIC - Sand frac"
rsqrt_snd = sapply(v, crt)

dev.off() # end of chart exportation


#--- cec - Cen
png("C:/Murilo/GIS/BRA_Costumers/cec_isric_cen.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Cation_Exchange_Capacity_C.E.C_mmolc.dm.3"
tifname = "CECSOLM"
xla = "Measured - CEC mmolc/dm3"
yla = "ISRIC - CEC cmolc/kg"
rsqrt_cec = sapply(v, crt)

dev.off() # end of chart exportation

#--- ORC - Cen
png("C:/Murilo/GIS/BRA_Costumers/orc_isric_cen.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

cname = "Total_Organic_C_g.dm.3"
tifname = "ORCDRCM"
xla = "Measured - Org C g/dm3"
yla = "ISRIC - Org C g/kg"
rsqrt_orc = sapply(v, crt)

dev.off() # end of chart exportation

#--- Write performance
rsqrd = rbind(rsqrt_slt,rsqrt_cly,rsqrt_snd,rsqrt_cec,rsqrt_orc)
write.csv(rsqrd, file = "C:/Murilo/GIS/BRA_Costumers/rsqrd_cen.csv")


boxplot(dbs_isric_cen$Clay_g.kg/10,
        col  = "red",
        xlab = "Clay Content [%] - ISRIC(250m)",
        ylab = "Clay Content [%] - ISRIC(250m)",
        xlim = c(0,2),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.5,
        main =NULL)

boxplot(dbs_isric_cen$CLYPPTMsl12,
        col  = "red",
        xlab = "Clay Content [%] - ISRIC(250m)",
        ylab = "Frequency",
        xlim = c(0,2),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.5,
        main =NULL)


listisric[25:44]

lisric = listisric[25:44]

t = data.frame(CLYPPTMsl1 = dbs_isric_cen$CLYPPTMsl12,
               CLYPPTMsl2 = dbs_isric_cen$CLYPPTMsl22,
               CLYPPTMsl3 = dbs_isric_cen$CLYPPTMsl32,
               CLYPPTMsl4 = dbs_isric_cen$CLYPPTMsl42,
               
               SNDPPTMsl1 = dbs_isric_cen$SNDPPTMsl12,
               SNDPPTMsl2 = dbs_isric_cen$SNDPPTMsl22,
               SNDPPTMsl3 = dbs_isric_cen$SNDPPTMsl32,
               SNDPPTMsl4 = dbs_isric_cen$SNDPPTMsl42,
               
               SLTPPTMsl1 = dbs_isric_cen$SLTPPTMsl12,
               SLTPPTMsl2 = dbs_isric_cen$SLTPPTMsl22,
               SLTPPTMsl3 = dbs_isric_cen$SLTPPTMsl32,
               SLTPPTMsl4 = dbs_isric_cen$SLTPPTMsl42,
               
               CECSOLMsl1 = dbs_isric_cen$CECSOLMsl12,
               CECSOLMsl2 = dbs_isric_cen$CECSOLMsl22,
               CECSOLMsl3 = dbs_isric_cen$CECSOLMsl32,
               CECSOLMsl4 = dbs_isric_cen$CECSOLMsl42,
               
               ORCDRCMsl1 = dbs_isric_cen$ORCDRCMsl12,
               ORCDRCMsl2 = dbs_isric_cen$ORCDRCMsl22,
               ORCDRCMsl3 = dbs_isric_cen$ORCDRCMsl32,
               ORCDRCMsl4 = dbs_isric_cen$ORCDRCMsl42,
               
               MEACLAY    = dbs_isric_cen$Clay_g.kg/10,
               MEASILT    = dbs_isric_cen$Silt_g.kg/10,
               MEASAND    = dbs_isric_cen$Sand_g.kg/10,
               MEACECS    = dbs_isric_cen$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3,
               MEAORCD    = dbs_isric_cen$Total_Organic_C_g.dm.3
               )

dbs_is_st = stack(t)
lt = unique(dbs_is_st$ind)

write.csv(lt, file = "lt.csv")
lt_r = read.csv(file = "lt.csv")

dbs_is_bp = merge(dbs_is_st, lt_r, by = "ind")





dbsis_filt = dbs_is_bp[dbs_is_bp$Cl=="Clay",]
dbsis_filt$ind = factor(dbsis_filt$ind)

#--- Clay - Cen
png("C:/Murilo/GIS/BRA_Costumers/bp_clay.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

boxplot(dbsis_filt$values~dbsis_filt$ind,
        col  = "red",
        xlab = NULL,
        ylab = "Clay [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.0,
        cex.axis= 1.0,
        main =NULL)

dev.off() # end of chart exportation


dbsis_filt = dbs_is_bp[dbs_is_bp$Cl=="Sand",]
dbsis_filt$ind = factor(dbsis_filt$ind)

#--- Clay - Cen
png("C:/Murilo/GIS/BRA_Costumers/bp_sand.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

boxplot(dbsis_filt$values~dbsis_filt$ind,
        col  = "yellow",
        xlab = NULL,
        ylab = "Sand [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.0,
        cex.axis= 1.0,
        main =NULL)

dev.off() # end of chart exportation


dbsis_filt = dbs_is_bp[dbs_is_bp$Cl=="Silt",]
dbsis_filt$ind = factor(dbsis_filt$ind)

#--- Clay - Cen
png("C:/Murilo/GIS/BRA_Costumers/bp_silt.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

boxplot(dbsis_filt$values~dbsis_filt$ind,
        col  = "grey",
        xlab = NULL,
        ylab = "Silt [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,50),
        cex.lab = 1.0,
        cex.axis= 1.0,
        main =NULL)

dev.off() # end of chart exportation









#--- Plot Function
crt = function(sl){
  #y = dbs_isric_00_15_zs$zs_mean[dbs_isric_00_15_zs$tif==  tifname & 
  #                                 dbs_isric_00_15_zs$sl==sl]
  #x = dbs_isric_00_15_zs[dbs_isric_00_15_zs$tif==tifname & 
  #                         dbs_isric_00_15_zs$sl==sl, cname]/10
  
  y = dbs_isric_cen[,paste(tifname,sl,"2",sep="")]
  x = dbs_isric_cen[,cname]
  
  
  yl = paste(yla,"(",sl,")",sep="")
  xl = paste(xla,sep="")
  
curvplot(y,x,yl,xl)

}

curvplot = function(y,x,yl,xl){
  
  plot(y~x,
       xlab = xl,
       ylab = yl,
       cex.lab = 1.0,
       cex.axis= 1.0,
       main =NULL)
  
  fit1 = lm(y~x)
  fit2 = lm(y~poly(x,2,raw=TRUE))
  fit3 = lm(y~poly(x,3,raw=TRUE))
  fit4 = lm(y~poly(x,4,raw=TRUE))
  
  #xx = seq(min(x),max(x),length = (max-min)*1000)
  xx = seq(0,100,length = 1000)
  lines(xx, predict(fit1, data.frame(x=xx)),col="red")
  #lines(xx, predict(fit2, data.frame(x=xx)),col="green")
  #lines(xx, predict(fit3, data.frame(x=xx)),col="blue")
  #lines(xx, predict(fit4, data.frame(x=xx)),col="purple")
  
  legend("topleft", bty="n", legend=paste("R2: ", format(summary(fit1)$adj.r.squared, digits=4)))
  
  perf = data.frame(r2 = 0, rmse = 0, descr = "")
  perf$r2 = summary(fit1)$adj.r.squared
  perf$rmse = sqrt(sum((y-x)^2) / length(y))
  perf$descr= paste(yl, " vs ",xl, sep ="")
  perf
  
  #rsqrd[2] = summary(fit2)$adj.r.squared
  #rsqrd[3] = summary(fit3)$adj.r.squared
  #rsqrd[4] = summary(fit4)$adj.r.squared
  
}



#--- Charts
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))


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


#--- Gather dbf files and write as .csv
wcsvv1 = function(y){
  dbf_ls = dbf_df_zos$filename[dbf_df_zos$tif==y]
  dbf    = rdbfsevv1(dbf_ls)
  write.csv(dbf, file = paste(y,".csv",sep=""), quote = F)
}


#--- Function Read Several dbf
rdbfsev = function(z){
  dbf_files_df = lapply(z,rdbf)
  rdbfsev = do.call("rbind", lapply(dbf_files_df, as.data.frame))
  rdbfsev
}



rdbfsevv1 = function(z){
  dbf_files_df = lapply(z,rdbfv1)
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


#--- Read DBF function
rdbfv1 = function(x) {
  f = read.dbf(file = x, as.is = F)
  f$filename = x
  f$ACRON = NULL #Remove this additional field (added on Qgis only for layout)
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












