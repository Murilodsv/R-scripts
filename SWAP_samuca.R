#--- SWAP-Samuca Assessment and Calibration
#--- This script is intended to:
#--- 1) Read and build-up charts of SWAP-Samuca model for assessment and calibration
#--- 2) Be used as function to automate calibration procedure as described in my Murilo Vianna phd Thesis (2017) 

library(lubridate) #--- compute doy from date
library(plyr)

#--- Outputs Directory
setwd("C:/Murilo/SWAP_Sugarcanev1")

#--- Read Outputs
#--- Crop Default
plant_lines = readLines("Plant_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
plant_numlines = plant_lines[substr(plant_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
plant = read.table(text = plant_numlines)                   #Read numeric lines as data.frame
colnames(plant) = c("year","doy","das","dap","gdd","dw","reserv","rw","lw","tp","sw","sucw","fibw","tch","pol","adryw","lai","till","h","devgl","itn","swface","swfacp","rtpf","lfpf","skpf","tppf","ctype","status","stage")

#--- Detailed Internodes
detint_lines = readLines("DetIntePr_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detint_numlines = detint_lines[substr(detint_lines,1,1)=="2"]    #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detint = read.table(text = detint_numlines)                      #Read numeric lines as data.frame
colnames(detint) = c("year","doy","das","dap","diac","itn",paste(rep("itlen",35),1:35),paste(rep("av_itlen",35),1:35),paste(rep("itsuc",35),1:35),paste(rep("av_itsuc",35),1:35),paste(rep("ittdw",35),1:35),paste(rep("av_ittdw",35),1:35))

#--- Detailed Leaves
detleaf_lines = readLines("DetLeafPr_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detleaf_numlines = detleaf_lines[substr(detleaf_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detleaf = read.table(text = detleaf_numlines)                     #Read numeric lines as data.frame
colnames(detleaf) = c("year","doy","das","dap","diac","ngl","ndevgl",paste(rep("lfarea",11),1:11),paste(rep("av_lfarea",11),1:11),paste(rep("lfweight",11),1:11),paste(rep("av_lfweight",11),1:11))

#--- Detailed Stress Factors
detsfac_lines = readLines("DetPGFac_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detsfac_numlines = detsfac_lines[substr(detsfac_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detsfac = read.table(text = detsfac_numlines)                     #Read numeric lines as data.frame
colnames(detsfac) = c("year","doy","das","dap","diac","par","extcoef","lai","li","co2","rue","co2_fac","tstress","agefactor","swfacp","RGP_fac","pg","dRGP_pg","dw","RGP_pg","IPAR_acc","w","wa","w+wdead","wa+wdead","arue_dw","arue_w","arue_dwa","arue_wa","carbcheck")

#--- Detailed RootSystem
detroot_lines = readLines("DetRootSy_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
detroot_numlines = detroot_lines[substr(detroot_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
detroot = read.table(text = detroot_numlines)                     #Read numeric lines as data.frame
colnames(detroot) = c("year","doy","das","dap","diac","wr","rd","rootsene",paste(rep("rld",45),1:45),"tqropot","ptra")

#--- Model Parameters
par = read.table(file = "Param_Set.out",skip = 4)
colnames(par) = c("value","parname","type","class")

#--- Soil Water
swba = read.csv(file = "result.vap", skip = 11)

#--- Soil Water Reduction
wstr = read.csv(file = "result.str", skip = 6)

#--- Atmospheric
atm  = read.csv(file = "result.inc", skip = 6)

#--- Read Measured Data
fdr = read.csv(file = "SOIL_FDR_SP_DATA.csv")
et  = read.csv(file = "bowen.csv")
bio = read.csv(file = "biometrics.csv")

#--- Indexer: use year_doy as indexer for das from plant
indexc = data.frame(plant$das,plant$year,plant$doy)
colnames(indexc) = c("das","year","doy")

#--- Function to index das on other DBs
inx = function(df){
  df = merge(indexc,df,by = c("year","doy"))
  df = df[order(df$das),]#--- sort by das
}

#--- Include das in all db
#--- Note data must have "year" and "doy" collumns!!!!!!!
#--- Measured data
fdr = inx(fdr)    #FDR
et  = inx(et)     #ET
bio = inx(bio)    #Biometrics

#--- Simulated data
#--- Derive "year" and "doy" for atm, swba, wstr
atm$year = as.factor(format(as.Date(atm$Date, format="%d-%b-%Y"),"%Y"))
atm$doy  = as.factor(yday(as.Date(atm$Date, format="%d-%b-%Y")))

swba$year = as.factor(format(as.Date(swba$date, format="%d-%b-%Y"),"%Y"))
swba$doy  = as.factor(yday(as.Date(swba$date, format="%d-%b-%Y")))

wstr$year = as.factor(format(as.Date(wstr$Date, format="%d-%b-%Y"),"%Y"))
wstr$doy  = as.factor(yday(as.Date(wstr$Date, format="%d-%b-%Y")))

#--- Include das in all them
atm = inx(atm)    #Atmosphere
swba= inx(swba)   #Soil Water Balance
wstr= inx(wstr)   #Water stresses

#------------------------#
#-------- Charts --------#
#--- FDR vs Simulated ---#
#------------------------#

#--- seting the simulated depths as equal to FDR depths measurements (10, 20, 30, 60)
dsim = data.frame(fdr = colnames(fdr)[4:7], depth = c(-10,-19.5,-31.5,-58.5))

#--- Separating data for lines
l = merge(swba,dsim,by = "depth")
l = l[order(l$das),]#--- sort by das

par(mfrow=c(4,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

sapply(colnames(fdr)[4:7], fdrpl)
fdrpl = function(x){
  
  plot(fdr[,x]~fdr$das,
       col  = "black",
       xlab = "",
       ylab = "SWC (cm3 cm-3)",
       xlim = c(50,1500),
       ylim = c(0.15,0.45),
       cex.lab = 1.,
       cex.axis= 1.,
       main =NULL)
  
  lines(l$wcontent[l$fdr==x]~l$das[l$fdr==x], col = "grey")
  
}

