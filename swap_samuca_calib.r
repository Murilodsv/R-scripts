#--- SWAP-Samuca Assessment and Calibration
#--- This script is intended to:
#--- 1) Read and build-up charts of SWAP-Samuca model for assessment and calibration
#--- 2) Be used as function to automate calibration procedure as described in my Murilo Vianna phd Thesis (2017) 

library(lubridate) #--- compute doy from date
library(plyr)
library(scales)


#--- Indexer: use year_doy as indexer for das from plant
indexc = data.frame(plant$das,plant$year,plant$doy)
colnames(indexc) = c("das","year","doy")

inx = function(df){
  #--- Function to index das on other DBs
  df = merge(indexc,df,by = c("year","doy"))
  df = df[order(df$das),]#--- sort by das
}

#--- Read Observed Data
fdr = read.csv(file = "SOIL_FDR_SP_DATA.csv")
et  = read.csv(file = "bowen.csv")
bio = read.csv(file = "biometrics.csv")

#--- Include das in all db
#--- Note data must have "year" and "doy" collumns!!!!!!!
#--- Measured data
fdr = inx(fdr)    #FDR
et  = inx(et)     #ET
bio = inx(bio)    #Biometrics

#--- Outputs Directory
setwd("C:/Murilo/SWAP_Sugarcanev1")

#--- Set up calibration procedure
#------ 1) Choose which parameter will be calibrated and include their names in calib$pname
#------ 2) Set the inital values foe each one at calib$pvalue
#------ 3) Set the minimmun and maximmun range for each parameter at calib$minva and calib$maxva

calib = data.frame(pname  = c("!dwat_dws","!dwat_dsuc","!albedo"),
                   pvalue = c(8          ,-3          ,0.23     ),
                   pminva = c(1          ,-10         ,0.05     ),
                   pmaxva = c(15         ,-0.01       ,0.50     ))

#--- Feature scaling
calib$svalue = (calib$pvalue - calib$pminva) / (calib$pmaxva - calib$pminva)

swap_samuca_calib = function(svalue){
  
  #----------------------------------------#
  #--- SWAP-SAMUCA Calibration function ---#
  #----------------------------------------#
  
  #--- Goals:
  #------- 1) Update parameters file; 
  #------- 2) Run the model; 
  #------- 3) Compute performance based on observed data
  
  #--- reference: Functional, structural and agrohydrological sugarcane crop modelling: towards a simulation platform for Brazilian farming systems (PhD Dissertation, 2017)
  #--- contact: Murilo Vianna (murilodsv@gmail.com)
  
  #--- Convert scaled to parameter values (feature scales method) 
  calib$pvalue = svalue * (calib$pmaxva - calib$pminva) + calib$pminva
  
  #--- Read model parameters
  par_s = read.table(file = "Param_Set.out",skip = 4)
  colnames(par_s) = c("value","parname","type","class")
  
  #--- Replace with new parameters values
  #--- Parameters names must match including "!": e.g. "!cfbs" 
  par_s$value[match(calib$pname, paste(par_s$parname))] = calib$pvalue

  #--- Parameters format
  vf    = "IIIIIII.DDDDDDDDDD"
  vfsize= nchar(vf)
  vp    = nchar(gsub("I.","",vf))
  v_int_size = vfsize - (vp + 1) 
  v_dec_size = vp

  desc_size = 19
  type_size = 3

  v_int   = floor(par_s$value)                #integer part
  v_dec   = par_s$value - floor(par_s$value)  #decimal fraction

  par_line= ""

  #--- Header
  par_line[1] = "Crop and Control Parameters for SWAP-Samuca Model"
  par_line[2] = ""
  par_line[3] = "Value              Name(Type)"
  par_line[4] = "------------------!-------------------"

  h_size = length(par_line)
  options(scipen=999) # disable scientific notation

  for(i in 1:length(v_int)){
  
    nspace = v_int_size - nchar(v_int[i])
  
    #--- Add spaces before integer
    for(n in 1:nspace){
        if(n==1){par_line[i + h_size] =""}
        par_line[i + h_size] =  paste(par_line[i + h_size]," ",sep="")
      }
  
    if(par_s$type[i] == "(I)"){
      #--- Add integer value
      par_line[i + h_size] =  paste(par_line[i + h_size],v_int[i],sep="")
      
      #--- Add spaces for integer parameters
      par_line[i + h_size] =  paste(par_line[i + h_size],"           ",sep="")
      
    }else{
      #--- Add real value
      par_line[i + h_size] =  paste(par_line[i + h_size],round(par_s$value[i],digits = 8),sep="")
      #--- Add dot and zero when values are Real and decimals == zero
      if(v_dec[i] == 0){
        par_line[i + h_size] =  paste(par_line[i + h_size],".0",sep="")
        
      }
      
      #--- Complete zeros untill precision size
      nzero = vfsize - nchar(par_line[i + h_size])
      
      for(n in 1:(nzero-1)){
        par_line[i + h_size] =  paste(par_line[i + h_size],"0",sep="")
      }
    
      #--- leave 1 space separation
      par_line[i + h_size] =  paste(par_line[i + h_size]," ",sep="")
    }
    
    #--- Add parameters description and coments
    par_line[i + h_size] =  paste(par_line[i + h_size],par_s$parname[i],sep="")
    
    nspace = desc_size - nchar(as.character(par_s$parname[i]))
    
    #--- Add spaces
    for(n in 1:nspace){
      par_line[i + h_size] =  paste(par_line[i + h_size]," ",sep="")
    }
    
    par_line[i + h_size] =  paste(par_line[i + h_size],par_s$type[i],sep="")
    par_line[i + h_size] =  paste(par_line[i + h_size]," ",sep="")
    par_line[i + h_size] =  paste(par_line[i + h_size],par_s$class[i],sep="")
    
  }

  #--- Re-write parameter file
  write.table(par_line, file = "Param_Set.out",quote = F, row.names = F, col.names = F)

  #--- Run SWAP_SAMUCA
  system("SWAP_Sugarcanev1")
  
  #--- Read Model Outputs
  #--- Crop Default
  plant_lines = readLines("Plant_SWAP-SAMUCA_PIRA.OUT")       #Read files lines
  plant_numlines = plant_lines[substr(plant_lines,1,1)=="2"]  #Separate only lines starting with "2" - Indicating its a numerical line (year = 2012,2013...)
  plant = read.table(text = plant_numlines)                   #Read numeric lines as data.frame
  colnames(plant) = c("year","doy","das","dap","gdd","dw","reserv","rw","lw","tp","sw","sucw","fibw","tch","pol","adryw","lai","till","h","devgl","itn","swface","swfacp","rtpf","lfpf","skpf","tppf","ctype","status","stage")
  
  #--- Soil Water
  swba = read.csv(file = "result.vap", skip = 11)
  
  #--- Soil Water Reduction
  wstr = read.csv(file = "result.str", skip = 6)
  
  #--- Atmospheric
  atm  = read.csv(file = "result.inc", skip = 6)
  
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
  
  
}# function End

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
