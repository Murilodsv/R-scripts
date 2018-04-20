#--- SWAP-Samuca Assessment and Calibration
#--- This script is intended to:
#--- 1) Read and build-up charts of SWAP-Samuca model for assessment and calibration
#--- 2) Be used as function to automate calibration procedure as described in my Murilo Vianna phd Thesis (2017) 

library(lubridate) #--- compute doy from date
library(plyr)
library(scales)

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
atm       = inx(atm)    #Atmosphere
swba      = inx(swba)   #Soil Water Balance
wstr      = inx(wstr)   #Water stresses

#------------------------#
#--- Machine Learning ---#
#------------------------#

#--- Compute statistical indexes of performance
#--- Performance function
mperf = function(sim,obs,vnam){
  
  # sim  - simulated values [R]
  # obs  - observed values [R]
  # vnam - Name of variable as string for chart axis  [S]
  
  #--- Statistical indexes
  fit   = lm(sim~obs)
  bias  = (1/length(obs)) * sum(sim-obs)
  mse   = (1/length(obs)) * sum((sim-obs)^2)
  rmse  = sqrt(mse)
  mae   = (1/length(obs)) * sum(abs(sim-obs))
  rrmse = rmse / mean(obs)
  rmae  = (1/length(obs[obs>0])) * sum(abs(sim[obs>0]-obs[obs>0])/abs(obs[obs>0]))
  ef    = 1 - (sum((sim-obs)^2) / sum((obs-mean(obs))^2))
  r     = sum((obs-mean(obs))*(sim-mean(sim)))/sqrt(sum((obs-mean(obs))^2)*sum((sim-mean(sim))^2))
  r2    = r^2
  d     = 1 - (sum((sim-obs)^2) / sum((abs(sim-mean(obs))+abs(obs-mean(obs)))^2))
  a     = summary(fit)$coefficients["(Intercept)","Estimate"]
  b     = summary(fit)$coefficients["obs","Estimate"]
  
  #--- Chart Sim ~ Obs
  varlab = vnam 
  
  mindt = min(obs,sim)
  maxdt = max(obs,sim)
  #--- Ploting limits 
  pllim = c(mindt-0.1*(maxdt-mindt),maxdt+0.1*(maxdt-mindt))
  xx = seq(min(obs),max(obs),length = (max(obs)-min(obs))*1000)
  z = summary(fit)
  
  plot(sim~obs,
       ylab = paste("Sim - ",varlab,sep = ""),
       xlab = paste("Obs - ",varlab,sep = ""),
       ylim = pllim,
       xlim = pllim)
  
  lines(xx, predict(fit, data.frame(obs=xx)),
        col = "black",
        lty = 1,
        lwd = 1.5)
  
  l11 = seq(pllim[1]-0.5*(maxdt-mindt), pllim[2] + 0.5 * (maxdt-mindt),length = 1000)
  
  lines(l11*1~l11,
        col = "red",
        lty = 2,
        lwd = 1.5)
  
  perf = data.frame(bias,
                    mse,
                    rmse,
                    mae,
                    rrmse,
                    rmae,
                    ef,
                    r,
                    r2,
                    d,
                    a,
                    b)
  
}

#--- Soil Water Content
#--- seting the simulated depths as equal to FDR depths measurements (10, 20, 30, 60)
dsim = data.frame(fdr = colnames(fdr)[4:7], depth = c(-10,-19.5,-28.5,-58.5))
l = merge(swba,dsim,by = "depth")
l = l[order(l$das),]              #--- sort by das

sim_swc10  = l[l$fdr==colnames(fdr)[4],c("das","wcontent")]
sim_swc20  = l[l$fdr==colnames(fdr)[5],c("das","wcontent")]
sim_swc30  = l[l$fdr==colnames(fdr)[6],c("das","wcontent")]
sim_swc60  = l[l$fdr==colnames(fdr)[7],c("das","wcontent")]
sim_swc    = data.frame(das = sim_swc10$das,sim_swc10 = sim_swc10$wcontent, sim_swc20 = sim_swc20$wcontent, sim_swc30 = sim_swc30$wcontent, sim_swc60 = sim_swc60$wcontent)

so_fdr = merge(fdr,sim_swc, by = "das")

s_swc10  = so_fdr$sim_swc10
s_swc20  = so_fdr$sim_swc20
s_swc30  = so_fdr$sim_swc30
s_swc60  = so_fdr$sim_swc60

o_swc10  = so_fdr$fdr10cm
o_swc20  = so_fdr$fdr20cm
o_swc30  = so_fdr$fdr30cm
o_swc60  = so_fdr$fdr60cm

s_swc = c(s_swc10,s_swc20,s_swc30,s_swc60)
o_swc = c(o_swc10,o_swc20,o_swc30,o_swc60)

png("p_swc_d.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_swc10  = mperf(s_swc10,o_swc10,"SWC at 10cm (cm3 cm-3)")
p_swc20  = mperf(s_swc20,o_swc20,"SWC at 20cm (cm3 cm-3)")
p_swc30  = mperf(s_swc30,o_swc30,"SWC at 30cm (cm3 cm-3)")
p_swc60  = mperf(s_swc60,o_swc60,"SWC at 60cm (cm3 cm-3)")

dev.off() # end of chart exportation

png("p_swc.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_swc    = mperf(s_swc,o_swc,"SWC (cm3 cm-3)")
dev.off() # end of chart exportation

#--- Atmosphere
et_obs = data.frame(das = et$das[et$type=="ET" & et$treat=="WithoutStraw"], et = et$et[et$type=="ET" & et$treat=="WithoutStraw"])
so_atm = merge(et_obs,atm,by = "das")
s_et = (so_atm$Tact + so_atm$Eact) * 10
o_et = so_atm$et

png("p_atm.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_atm  = mperf(s_et,o_et,"ET (mm d-1)")
dev.off() # end of chart exportation

#--- Biometrics
#Stalk Fresh Mass
colnames(bio)

o_sfm  = bio[!is.na(bio$SFM) & bio$type == "AVG",c("das","SFM")]
o_sdm  = bio[!is.na(bio$SDM) & bio$type == "AVG",c("das","SDM")]
o_lai  = bio[!is.na(bio$LAIGD) & bio$type == "AVG",c("das","LAIGD")]
o_til  = bio[!is.na(bio$T.AD) & bio$type == "AVG",c("das","T.AD")]
o_pol  = bio[!is.na(bio$SU.FMD) & bio$type == "AVG",c("das","SU.FMD")]
o_dgl  = bio[!is.na(bio$N.GL) & bio$type == "AVG",c("das","N.GL")]
o_sth  = bio[!is.na(bio$SHTD) & bio$type == "AVG",c("das","SHTD")]


so_sfm  = merge(o_sfm,plant[,c("das","tch")]  , by = "das")
so_sdm  = merge(o_sdm,plant[,c("das","sw")]   , by = "das")
so_lai  = merge(o_lai,plant[,c("das","lai")]  , by = "das")
so_til  = merge(o_til,plant[,c("das","till")] , by = "das")
so_pol  = merge(o_pol,plant[,c("das","pol")]  , by = "das")
so_dgl  = merge(o_dgl,plant[,c("das","devgl")], by = "das")
so_sth  = merge(o_sth,plant[,c("das","h")]    , by = "das")

png("p_sfm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sfm = mperf(so_sfm$tch,o_sfm$SFM, "SFM (t ha-1)")
dev.off() # end of chart exportation

png("p_sdm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sdm = mperf(so_sdm$sw,o_sdm$SDM, "SDM (t ha-1)")
dev.off() # end of chart exportation

png("p_lai.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_lai = mperf(so_lai$lai,o_lai$LAIGD, "LAI (m2 m-2)")
dev.off() # end of chart exportation

png("p_till.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_til = mperf(so_til$till,o_til$T.AD, "Tiller (till m-2)")
dev.off() # end of chart exportation

png("p_pol.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_pol = mperf(so_pol$pol,o_pol$SU.FMD, "POL (%)")
dev.off() # end of chart exportation

png("p_dgl.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_dgl = mperf(so_dgl$devgl,o_dgl$N.GL, "N° dev GL per stalk")
dev.off() # end of chart exportation

png("p_sth.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
p_sth = mperf(so_sth$h,o_sth$SHTD, "Height (m)")
dev.off() # end of chart exportation

#--- Pooling all results in p_all
p_all = rbind(p_swc10,
              p_swc20,
              p_swc30,
              p_swc60,
              p_swc,
              p_atm,
              p_sfm,
              p_sdm,
              p_lai,
              p_til,
              p_pol,
              p_dgl,
              p_sth)

p_all$model = c("p_swc10",
                "p_swc20",
                "p_swc30",
                "p_swc60",
                "p_swc",
                "p_atm",
                "p_sfm",
                "p_sdm",
                "p_lai",
                "p_til",
                "p_pol",
                "p_dgl",
                "p_sth")

#--- Write performance
write.csv(p_all, file = "Model_performance.csv")

#------------------------#
#-------- Charts --------#
#--- FDR vs Simulated ---#
#------------------------#

#--- seting the simulated depths as equal to FDR depths measurements (10, 20, 30, 60)
dsim = data.frame(fdr = colnames(fdr)[4:7], depth = c(-10,-19.5,-28.5,-58.5))

#--- Separating data for lines
l = merge(swba,dsim,by = "depth")
l = l[order(l$das),]#--- sort by das

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
png("so_swc.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(4,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
s = sapply(colnames(fdr)[4:7], fdrpl)
dev.off() # end of chart exportation


#--- Atmosphere
atm$etact = (atm$Tact + atm$Eact) * 10
atm$etpot = (atm$Tpot + atm$Epot) * 10

#--- temporal distribution
dt = 30 #time pack 

#--- classify das by dt
br = seq(min(atm$das),max(atm$das), by = dt)
lb = seq(min(atm$das) + 0.5*dt ,max(atm$das), by = dt)
if(length(br)==length(lb)){lb = lb[1:length(br)-1]}

atm$das_c = cut(atm$das,breaks = br, labels = lb, right = F)

png("so_atm.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- compute 1st and 3 quartiles
z = boxplot(atm$etact~atm$das_c)
bot = data.frame(das = lb, et = z$stats[2,])
top = data.frame(das = lb, et = z$stats[4,])

#--- plot obs et
plot(et$et[et$type == "ET" & et$treat == "WithoutStraw"]~et$das[et$type == "ET" & et$treat == "WithoutStraw"],
     ylab = expression("ET (mm " ~ d^{-1} ~ ")"),
     xlab = "DAS",
     ylim = c(0,10),
     xlim = c(min(atm$das), max(atm$das)) )

#--- plot sim et
lines(atm$etact~atm$das, col=alpha(rgb(1,0,0), 0.7))

#--- plot temporal distribution
lines(bot$et~bot$das,
      lty = 2,
      lwd = 1.5)
lines(top$et~top$das,
      lty = 2,
      lwd = 1.5)
points(et$et[et$type == "ET" & et$treat == "WithoutStraw"]~et$das[et$type == "ET" & et$treat == "WithoutStraw"])

#--- Add days of planting
pdays = plant$das[plant$dap==1]

#plant cane
lines(c(-1,11)~c(pdays[1],pdays[1]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)

#1st ratoon
lines(c(-1,11)~c(pdays[2],pdays[2]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)
#2nd ratoon
lines(c(-1,11)~c(pdays[3],pdays[3]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)
#3rd ratoon
lines(c(-1,11)~c(pdays[4],pdays[4]),
      col=alpha(rgb(0,0,0), 0.5),
      lty= 2)

dev.off()
#--- Biometrics

#--- Observed data (only average = "AVG") - Include boxplots in a nearfuture
o_sfm  = bio[!is.na(bio$SFM)    & bio$type == "AVG",c("das","SFM")]
o_sdm  = bio[!is.na(bio$SDM)    & bio$type == "AVG",c("das","SDM")]
o_lai  = bio[!is.na(bio$LAIGD)  & bio$type == "AVG",c("das","LAIGD")]
o_til  = bio[!is.na(bio$T.AD)   & bio$type == "AVG",c("das","T.AD")]
o_pol  = bio[!is.na(bio$SU.FMD) & bio$type == "AVG",c("das","SU.FMD")]
o_dgl  = bio[!is.na(bio$N.GL)   & bio$type == "AVG",c("das","N.GL")]
o_sth  = bio[!is.na(bio$SHTD)   & bio$type == "AVG",c("das","SHTD")]

o_bio  = c("SFM","SDM","LAIGD","T.AD","SU.FMD","N.GL" ,"SHTD")
s_bio  = c("tch","sw" ,"lai"  ,"till","pol"   ,"devgl","h")

pl_bio = function(obs,sim,yl,dxlab){
  
  pylim = c(min(sim$dat,obs$dat),max(sim$dat,obs$dat)*1.04)
  pxlim = c(min(sim$das,obs$das),max(sim$das,obs$das))
  
  if(dxlab){
  
  plot(obs$dat~obs$das,
       ylab = yl,
       xlab = "DAS",
       ylim = pylim,
       xlim = pxlim,
       yaxs="i")
  }else{
    
    plot(obs$dat~obs$das,
         ylab = yl,
         xlab = "",
         ylim = pylim,
         xlim = pxlim,
         yaxs="i",
         xaxt='n')
  }
  
  li = sapply(unique(sim$ctype), function(x) lines(sim$dat[sim$ctype==x]~sim$das[sim$ctype==x]))

  }

# C(bottom, left, top, right)

png("so_bio.png",units="in",width=12,height=12,pointsize=15,res=300)

par(mfrow=c(7,1), 
    mar = c(0., 0.5, 0., 0.5), 
    oma = c(3, 3, 0.5, 0),
    mgp = c(2, 1, 0),
    xpd = NA)

#--- ploting SFM
pl_bio(data.frame(das = o_sfm[,"das"],dat = o_sfm[,o_bio[1]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[1]] , ctype = plant[,"ctype"]),
       "SFM (t ha-1)",FALSE)

#--- ploting SDM
pl_bio(data.frame(das = o_sdm[,"das"],dat = o_sdm[,o_bio[2]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[2]] , ctype = plant[,"ctype"]),
       "SDM (t ha-1)",FALSE)

#--- ploting LAI
pl_bio(data.frame(das = o_lai[,"das"],dat = o_lai[,o_bio[3]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[3]] , ctype = plant[,"ctype"]),
       "LAI (m2 m-2)",FALSE)

#--- ploting Tillering
pl_bio(data.frame(das = o_til[,"das"],dat = o_til[,o_bio[4]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[4]] , ctype = plant[,"ctype"]),
       "Tiller (n° m-2)",FALSE)

#--- ploting POL
pl_bio(data.frame(das = o_pol[,"das"],dat = o_pol[,o_bio[5]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[5]] , ctype = plant[,"ctype"]),
       "POL (%)",FALSE)

#--- ploting dgl
pl_bio(data.frame(das = o_dgl[,"das"],dat = o_dgl[,o_bio[6]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[6]] , ctype = plant[,"ctype"]),
       "n° DGL",FALSE)

#--- ploting heigth
pl_bio(data.frame(das = o_sth[,"das"],dat = o_sth[,o_bio[7]]),
       data.frame(das = plant[,"das"],dat = plant[,s_bio[7]] , ctype = plant[,"ctype"]),
       "Height (m)",TRUE)
dev.off()


