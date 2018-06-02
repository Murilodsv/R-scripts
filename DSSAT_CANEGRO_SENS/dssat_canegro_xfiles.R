#--- Create multiple X files from a list.csv
#--- Run DSSAT and read output data
#--- Generate a Sensitivity analysis
#--- Murilo Vianna
#--- May-2018

#--- Load Libraries 
library(scales)
library(plyr)
library(data.table)

setwd(wd)

#--- open lists
#scen = read.csv("scenarios_sim.csv")
#colnames(scen)
l_sc = scen_raw[,c("scenario","var","id")]
l_sc = unique(l_sc[l_sc$scenario!=0,])
l_sc = rbind(l_sc, data.frame(scenario = 0,
                              var      = basID,
                              id       = (scen_raw$id[scen_raw$scenario==0][1])))

#--- Read actual values modified and baseline
scalc = scen_raw

#--- reading master X FILE
mscx     = readLines("MAST0001.SCX")

#--- read DSSBatch.v47 Master
mbatch     = readLines("DSSBatch_Master.v47")

setwd(wd_dssat)

#--- DSSAT Default outputs
#--- Warning: If there is anyother non-default output in the wd these will be considered as default
DSSAT_out = data.frame(def_out = dir(pattern = "*.OUT"))
DSSAT_out$def_out = gsub(".OUT","", DSSAT_out$def_out)
DSSAT_out$def_ext = ".OUT"

#--- split simulations for analysis
split_sim = 10

spl_des = data.frame(s_sim = rep(split_sim, floor(length(l_sc$id)/split_sim)))
spl_des$s_sim[length(spl_des$s_sim)] = spl_des$s_sim[length(spl_des$s_sim)] + (length(l_sc$id)/split_sim - floor(length(l_sc$id)/split_sim)) * split_sim
spl_des$s_init = seq(1,length(spl_des$s_sim) * split_sim, by = split_sim)
spl_des$s_fina = spl_des$s_init + (spl_des$s_sim - 1)
#fsufix = "_SPPI"

#--- Shutdown the computer after simulations? (T/F)
#flshutdown = FALSE

for(sp in 1:length(spl_des$s_sim)){

#--- header for new DSSBatch.v47
new_bat = mbatch[1]
for(r in 2:10){
  new_bat = rbind(new_bat,mbatch[r])
}

for(sc in spl_des$s_init[sp]:spl_des$s_fina[sp]){

#--- Add description
new_scx = gsub(c("&DESCR"),paste(l_sc$scenario[sc], l_sc$var[sc],sep = " - "),mscx[1])

for(r in 2:12){
new_scx = rbind(new_scx,mscx[r])
}

nr = r + 1
replace = mscx[nr]
ser = "&TNAME                   "
rep = paste(l_sc$scenario[sc], l_sc$var[sc],sep = "-")
size_ser = 25
size_rep = nchar(rep)

if(size_ser == size_rep){
  rep = gsub(ser,rep,replace)
}else if(size_ser > size_rep){
  #--- Include spaces after
  for(j in 1:(size_ser - size_rep)){
    rep = paste(rep," ", sep = "")
  }
}else if(size_ser < size_rep){
  #--- Reduce character until match ser size
  d = 1
  while(size_ser < size_rep){
    rep = substr(paste(l_sc$scenario[sc], l_sc$var[sc],sep = "-"),1,nchar(paste(l_sc$scenario[sc], l_sc$var[sc],sep = "-")-d))
    d = d - 1
    size_rep = nchar(rep)
  }
}
replace = gsub(ser,rep,replace)
new_scx = rbind(new_scx,replace)

nr = nr + 1
for(r in nr:20){
  new_scx = rbind(new_scx,mscx[r])
}

nr = r + 1
replace = mscx[nr]

ser = "&ID_SOIL"
rep = l_sc$id[sc]
replace = gsub(ser,rep,replace)

ser = "&FLNAME"
rep = paste(l_sc$scenario[sc], scen$var[sc],sep = "-")
replace = gsub(ser,rep,replace)

new_scx = rbind(new_scx,replace)

nr = nr + 1
for(r in nr:56){
  new_scx = rbind(new_scx,mscx[r])
}

#--- save .SCX file
scx_name = paste("MVSS",sprintf("%04.0f", sc),".SCX",sep = "")
write.table(new_scx,file = scx_name,row.names = F,col.names = F,quote = F)

replace = mbatch[11]
replace = gsub("&SCX_NAME",scx_name,replace)
new_bat = rbind(new_bat,replace)

}

substr(dv,6,8)

dssbatch = paste("DSSBatch.v",substr(dv,6,8),sep="")
write.table(new_bat,file = dssbatch,row.names = F,col.names = F,quote = F)

#--- Call DSSAT047.exe and run X files list within DSSBatch.v47
system(paste("C:/",dv,"/DSCSM0",substr(dv,6,8),".EXE SCCAN0",substr(dv,6,8)," B ",dssbatch,sep=""))

tm = Sys.time()
print(paste("DSSAT Simulations completed at ",tm,sep = ""), quote = F)

#--- Copy to Batch folder and rename DSSAT outputs (add split sulfix)
file.copy(from = paste(DSSAT_out$def_out,DSSAT_out$def_ext,sep = ""), to = paste(wd,"/",DSSAT_out$def_out,"_",sp,"_",fsufix,DSSAT_out$def_ext,sep = ""))

}

#-------------------------------------------------------------------------

#--- Read and analyse output data
setwd(wd)

print(paste("Started to analyse outputs, please wait",sep = ""), quote = F)

for(pf in 1:length(spl_des$s_sim)){
  
  print(paste("Joining PlantGro.csv ",pf," of ",length(spl_des$s_sim),sep = ""), quote = F)
  
  fext = ".OUT"
  
  #--- Read simulated data
  plant_lines = readLines(paste("PlantGro_",pf,"_",fsufix,fext,sep=""))
  write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
              file = paste("PlantGro_",pf,"_",fsufix,"_numeric",fext,sep=""),
              row.names = F, col.names = F,quote = F)  
  plant = read.table(file = paste("PlantGro_",pf,"_",fsufix,"_numeric",fext,sep=""))                   #Read numeric lines as data.frame
  
  #--- Columns name accordingly to DSSAT output name
  colnames(plant) = pgro_names
  
  #--- Read treatments
  treat = trimws(substr(plant_lines[substr(plant_lines,2,6)=="TREAT"],19, 40))
  
  #--- Index outputs with treatments
  plant$treat = ""  
  j = 0
  for(i in 1:length(plant$dap)){
    
    if(plant$dap[i] == 0){j = j +1}
    
    plant$treat[i] = treat[j]
  
  }
  
  if(pf == 1){
    plantgro = plant
  }else{
    plantgro = rbind(plantgro,plant)
  }
  
}

print("Saving PlantGro.csv", quote = F)

#--- Write PlantGro.csv
write.csv(plantgro, file = paste("PlantGro_",fsufix,".csv",sep=""), row.names = F, quote = F)

#--- Output data to be analysed
incdata = wincdata
uincdata= wuincdata

#--- Indexer columns
inxdata = winxdata

lvar = unique(scalc$var)
lsce = unique(scalc$scenario[scalc$scenario!=0])

udf = data.frame(indata = incdata, unit = uincdata)

nvar = length(lvar)
nsce = length(lsce)

flinitscen_bp_all   = T
flinitscen_bp_dap   = T
flinitscen_perf_dap = T

start_time = Sys.time()

print(paste("Sensitivity started at ", start_time, sep =""))

for(v in lvar){
  
  flinitsce = T
  
  for(s in lsce){

    tlab = paste(s,v,sep = "-")
    
    scenario  = plantgro[plantgro$treat == tlab        , c(inxdata,incdata)]
    baseline  = plantgro[plantgro$treat == paste("0-",basID,sep="")    , c(inxdata,incdata)]

    #--- Rename columns
    colnames(baseline)[length(inxdata)+1:length(incdata)] = 
      paste(colnames(baseline)[length(inxdata)+1:length(incdata)],"BL",sep = "_")
    colnames(scenario)[length(inxdata)+1:length(incdata)] = 
      paste(colnames(scenario)[length(inxdata)+1:length(incdata)],"SC",sep = "_")

    #--- Baseline and Scenarios
    blsc          = merge(baseline, scenario, by = inxdata)
    blsc$treat    = tlab
    blsc$var_name = v
    blsc$valu_bas = mean(scalc$val[scalc$scenario == 0 & scalc$var == v])
    blsc$scen_exp = s
    blsc$scen_act = mean(scalc$val[scalc$scenario == s & scalc$var == v]) / blsc$valu_bas - 1

    if(flinitsce){
      blsc_var  = blsc
      flinitsce = F
    }else{
      blsc_var = rbind(blsc_var,blsc)
    }
  } #End of scenario loop
  
  write.csv(blsc_var, file = paste(fsufix,"_BLSC_",v,".csv",sep=""),row.names = F, quote = F)
  
  #--- Sensitivity Analysis
  for(d in incdata){
    
    #--- Pass values to s_analysis
    s_analysis = data.frame(sc   = blsc_var[,c(paste(d,"_SC",sep=""))],
                            bl   = blsc_var[,c(paste(d,"_BL",sep=""))],
                            v.sc = blsc_var[,"scen_act"],
                            v.bl = blsc_var[,"valu_bas"],
                            v.id = blsc_var[,"var_name"],
                            dap  = blsc_var[,"dap"])
  
    print(paste("Running sensitivity for ",v," ",d,sep=""))
      
    #--- inlcude 30 days packs (dt)
    dt = 30
    s_analysis$dap30 = .bincode(s_analysis$dap, breaks = seq(min(s_analysis$dap),max(s_analysis$dap)+dt,by = dt))*dt
    s_analysis$dap30[is.na(s_analysis$dap30)] = 0
    
    #--- Sensitivity (dVariable / dParameter, in 0-1)
    s_analysis$sens  = (s_analysis$sc / s_analysis$bl - 1) / (s_analysis$v.sc)
    
    #--- Variable variation, in 0-1
    s_analysis$rvar  = (s_analysis$sc / s_analysis$bl - 1)
    
    #--- convert NaN, NA or Inf to zero
    s_analysis$sens[is.infinite(s_analysis$sens) | is.na(s_analysis$sens)] = 0
    s_analysis$rvar[is.infinite(s_analysis$rvar) | is.na(s_analysis$rvar)] = 0
    
    
    #----------------------------------------------------------------
    #--- Boxplot of all sensitivity scenarios throughout all DAP
    #----------------------------------------------------------------
    
    #--- filter type
    btype = "Sens_all_DAP"
    
    #--- data used
    vanal = s_analysis$sens
    vfilt = s_analysis$v.sc
    vscen = s_analysis$v.id
    
    #--- y and x labels
    yl    = paste("d",d," d",unique(vscen),"-1 (%)", sep ="")
    xl    = paste("d",unique(vscen)," (%)",sep="")
    
    #--- plot and retrieve results
    imgname = paste(fsufix,"_bp_scen_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = plot_bp_sce(vanal,vfilt,vscen,yl,xl,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_sce df
    if(flinitscen_bp_all){
      v_bp_sce  = a_result
      flinitscen_bp_all = F
    }else{
      v_bp_sce = rbind(v_bp_sce,a_result)
    }

    #----------------------------------------------------------------
    #--- Boxplot of all sensitivity scenarios for lasts DAPs (30 dt)
    #----------------------------------------------------------------
    
    #--- filter type
    btype = "Sens_last_30DAPs"
    
    #--- data used
    vanal = s_analysis$sens[s_analysis$dap30== max(s_analysis$dap30[!is.na(s_analysis$dap30)])]
    vfilt = s_analysis$v.sc[s_analysis$dap30== max(s_analysis$dap30[!is.na(s_analysis$dap30)])]
    vscen = s_analysis$v.id
    
    #--- y and x labels
    yl    = paste("d",d," d",unique(vscen),"-1 (%)", sep ="")
    xl    = paste("d",unique(vscen)," (%)",sep="")
    
    #--- plot and retrieve results
    imgname = paste(fsufix,"_bp_scen_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = plot_bp_sce(vanal,vfilt,vscen,yl,xl,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_sce df
    v_bp_sce = rbind(v_bp_sce,a_result)
    
    #----------------------------------------------------------------
    #--- Boxplot of output variation as function of sensitivity scenarios throughout all DAP
    #----------------------------------------------------------------
    
    #--- filter type
    btype = "Var_all_DAP"
    
    #--- data used
    vanal = s_analysis$rvar*100
    vfilt = s_analysis$v.sc
    vscen = s_analysis$v.id
    
    #--- y and x labels
    yl    = paste("d",d," (%)", sep ="")
    xl    = paste("d",unique(vscen)," (%)",sep="")
    
    #--- plot and retrieve results
    imgname = paste(fsufix,"_bp_scen_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = plot_bp_sce(vanal,vfilt,vscen,yl,xl,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_sce df
    v_bp_sce = rbind(v_bp_sce,a_result)
    
    #----------------------------------------------------------------
    #--- Boxplot of output variation as function of sensitivity scenarios for lasts DAPs (30 dt)
    #----------------------------------------------------------------
    
    #--- filter type
    btype = "Var_last_30DAPs"
    
    #--- data used
    vanal = s_analysis$rvar[s_analysis$dap30== max(s_analysis$dap30[!is.na(s_analysis$dap30)])]*100
    vfilt = s_analysis$v.sc[s_analysis$dap30== max(s_analysis$dap30[!is.na(s_analysis$dap30)])]
    vscen = s_analysis$v.id
    
    #--- y and x labels
    yl    = paste("d",d," (%)", sep ="")
    xl    = paste("d",unique(vscen)," (%)",sep="")
    
    #--- plot and retrieve results
    imgname = paste(fsufix,"_bp_scen_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = plot_bp_sce(vanal,vfilt,vscen,yl,xl,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_sce df
    v_bp_sce = rbind(v_bp_sce,a_result)
    
    #----------------------------------------------------------------
    #--- Scenarios x Baseline Plot
    #----------------------------------------------------------------
    imgname = paste(fsufix,"_bp_perf_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
    par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
    plot(s_analysis$sc~s_analysis$bl,
         ylab = c(paste(d," - ",unique(s_analysis$v.id)," (All scenarios)",sep="")),
         xlab = c(paste(d," - ",unique(s_analysis$v.id)," (Baseline)",sep="")),
         cex.lab=1.5,
         cex.axis=1.2)
    
    #limits for line 
    line11 = c(min(s_analysis$sc,s_analysis$bl)-max(s_analysis$sc,s_analysis$bl),
      max(s_analysis$sc,s_analysis$bl)+max(s_analysis$sc,s_analysis$bl))
    lines(line11,line11, col = "red", lty = 5) # zero line
    dev.off() # end of chart exportation
    
    vnam = paste("all_scen_",v,"_",d,sep="")
    a_result = mperf(s_analysis$sc,s_analysis$bl,vnam,F)
    
    
    if(flinitscen_perf_dap){
      v_perf_sce  = a_result
      flinitscen_perf_dap = F
    }else{
      v_perf_sce = rbind(v_perf_sce,a_result)
    }
    
    #----------------------------------------------------------------
    #--- Sensitivity boxplots throughout DAPs
    #----------------------------------------------------------------
    
    #--- NEGATIVE SCENARIOS
    #--- boxplots results for sensitivity negative scenarios
    if(length(s_analysis$v.sc[s_analysis$v.sc<0]) > 0){
    
    btype = "Sens_lt0"
    
    vanal = s_analysis$sens[s_analysis$v.sc<0]
    vfilt = s_analysis$dap[s_analysis$v.sc<0]
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste(round(min(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% to ",
                round(max(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste("d",d," d",unique(s_analysis$v.id),"-1 (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen,leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_dap df
    if(flinitscen_bp_dap){
      v_bp_dap  = a_result
      flinitscen_bp_dap = F
    }else{
      v_bp_dap = rbind(v_bp_dap,a_result)
    }
    
    }
    #-----------------------------------------------------
    
    #--- POSITIVE SCENARIOS
    #--- boxplots results for sensitivity positive scenarios
    if(length(s_analysis$v.sc[s_analysis$v.sc>0]) > 0){
    
    btype = "Sens_gt0"
    
    vanal = s_analysis$sens[s_analysis$v.sc>0]
    vfilt = s_analysis$dap[s_analysis$v.sc>0]
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste("+",
                round(min(s_analysis$v.sc[s_analysis$v.sc>0])*100, digits = 0),
                "% to +",
                round(max(s_analysis$v.sc[s_analysis$v.sc>0])*100,digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste("d",d," d",unique(s_analysis$v.id),"-1 (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen, leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    #--- add to v_bp_dap df
    if(flinitscen_bp_dap){
      v_bp_dap  = a_result
      flinitscen_bp_dap = F
    }else{
      v_bp_dap = rbind(v_bp_dap,a_result)
    }
    
    }
    #------------------------------------------------------------
    
    #--- ALL SCENARIOS
    #--- boxplots results for all sensitivity scenarios
    btype = "Sens_all"
    
    vanal = s_analysis$sens[s_analysis$v.sc!=0]
    vfilt = s_analysis$dap[s_analysis$v.sc!=0]
    vscen = s_analysis$v.id[s_analysis$v.sc!=0]
    
    #--- legend and ylab
    if(min(s_analysis$v.sc[s_analysis$v.sc!=0]) < 0 ){
      brange = "-"
      }else{
      brange = "+"  
      }
    
    if(max(s_analysis$v.sc[s_analysis$v.sc!=0]) > 0 ){
      trange = "+"
    }else{
      trange = "-"  
    }
    
    
    leg = paste(brange,
                round(abs(min(s_analysis$v.sc[s_analysis$v.sc!=0]))*100, digits = 0),
                "% to ",
                trange,
                round(abs(max(s_analysis$v.sc[s_analysis$v.sc!=0]))*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste("d",d," d",unique(s_analysis$v.id),"-1 (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen, leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    #----------------------------------------------------------------
    #--- Variation boxplots throughout DAPs
    #----------------------------------------------------------------
    
    #--- Ranges from baseline
    ran = lsce[lsce > 0] + 0.01 # add 0.01 to ensure range will cover values rounded
    
    bp = boxplot(s_analysis$rvar * 100, plot = F)
    yli = c(abs(min(bp$out,bp$stats[1,],bp$stats[5,])),
    abs(max(bp$out,bp$stats[1,],bp$stats[5,])))
    
    yli = c(max(yli) * -1.0, max(yli))
    
    for(r in ran){
    
    btype = paste("Var_Ran_",r,sep="")
    
    vanal = s_analysis$rvar[s_analysis$v.sc>= -r & s_analysis$v.sc <= r] * 100
    vfilt = s_analysis$dap[s_analysis$v.sc>= -r & s_analysis$v.sc <= r]
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste(round(min(s_analysis$v.sc[s_analysis$v.sc>= -r & s_analysis$v.sc <= r])*100, digits = 0),
                "% to ",
                round(max(s_analysis$v.sc[s_analysis$v.sc>= -r & s_analysis$v.sc <= r])*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    #--- y lab
    yl  = paste("d",d," (%)", sep ="")
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen,leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    }
    
    #--- NEGATIVE SCENARIOS
    #--- boxplots results for variation in negative scenarios
    if(length(s_analysis$v.sc[s_analysis$v.sc<0]) > 0){
    
    btype = "Var_lt0"
    
    vanal = s_analysis$rvar[s_analysis$v.sc<0]*100
    vfilt = s_analysis$dap[s_analysis$v.sc <0]
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste(round(min(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% to ",
                round(max(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste("d",d," (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen,leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    
    }
    #--------------------------------------------------------------
    
    #--- POSITIVE SCENARIOS
    #--- boxplots results for variation in positive scenarios
    if(length(s_analysis$v.sc[s_analysis$v.sc>0]) > 0){
    
    btype = "Var_gt0"
    
    vanal = s_analysis$rvar[s_analysis$v.sc>0]*100
    vfilt = s_analysis$dap[s_analysis$v.sc>0]
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste("+",
                round(min(s_analysis$v.sc[s_analysis$v.sc>0])*100, digits = 0),
                "% to +",
                round(max(s_analysis$v.sc[s_analysis$v.sc>0])*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste("d",d," (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen,leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    
    }
    #--------------------------------------------------------
    
    #--- ALL SCENARIOS
    #--- boxplots results for all sensitivity scenarios
    btype = "Var_all"
    
    vanal = s_analysis$rvar[s_analysis$v.sc!=0]*100
    vfilt = s_analysis$dap[s_analysis$v.sc!=0]
    vscen = s_analysis$v.id[s_analysis$v.sc!=0]
    
    #--- legend and ylab
    if(min(s_analysis$v.sc[s_analysis$v.sc!=0]) < 0 ){
      brange = "-"
    }else{
      brange = "+"  
    }
    
    if(max(s_analysis$v.sc[s_analysis$v.sc!=0]) > 0 ){
      trange = "+"
    }else{
      trange = "-"  
    }
    
    
    leg = paste(brange,
                round(abs(min(s_analysis$v.sc[s_analysis$v.sc!=0]))*100, digits = 0),
                "% to ",
                trange,
                round(abs(max(s_analysis$v.sc[s_analysis$v.sc!=0]))*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")    
    yl  = paste("d",d," (%)", sep ="")
    
    bp = boxplot(vanal, plot = F)
    ylim_range = max(abs(bp$out),abs(bp$stats[1,]),abs(bp$stats[5,]))
    if(is.na(ylim_range) | is.infinite(ylim_range)){
      ylim_range = max(abs(bp$stats[1,]),abs(bp$stats[1,]))}
    
    yli = c(ylim_range * -1, ylim_range)
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = sens_bp_dap(vanal,vfilt,vscen, leg, yl,yli,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    #---------------------------
    
    if(length(s_analysis$v.sc[s_analysis$v.sc<0]) > 0){
      
    btype = "Out_lt0"
    
    vanal = s_analysis$sc[s_analysis$v.sc<0]
    vfilt = s_analysis$dap[s_analysis$v.sc<0]
    vbase = s_analysis$bl
    vfiltb= s_analysis$dap
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste(round(min(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% to ",
                round(max(s_analysis$v.sc[s_analysis$v.sc<0])*100, digits = 0),
                "% of ",unique(s_analysis$v.id),sep="")
    
    yl  = paste(d," (",udf$unit[udf$indata==d],")", sep ="")
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = values_bp_dap(vanal,vfilt,vbase,vfiltb,vscen,leg,yl,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    
    
    colnames(a_result)
    colnames(v_bp_dap)
    }
    #---------------------------
    if(length(s_analysis$v.sc[s_analysis$v.sc>0]) > 0){
    
    btype = "Out_gt0"
    
    vanal = s_analysis$sc[s_analysis$v.sc>0]
    vfilt = s_analysis$dap[s_analysis$v.sc>0]
    vbase = s_analysis$bl
    vfiltb= s_analysis$dap
    vscen = s_analysis$v.id
    
    #--- legend and ylab
    leg = paste("+",
                round(min(s_analysis$v.sc[s_analysis$v.sc>0])*100, digits = 0),
                "% to +",
                round(max(s_analysis$v.sc[s_analysis$v.sc>0])*100, digits = 0),
                "% of ",
                unique(s_analysis$v.id),sep="")
    
    yl  = paste(d," (",udf$unit[udf$indata==d],")", sep ="")
    
    #--- plot function
    imgname = paste(fsufix,"_bp_scen_dap_",btype,d,"_",v,".png",sep="")
    png(imgname,units="in",width=12,height=12,pointsize=15,res=300)
      par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
      a_result = values_bp_dap(vanal,vfilt,vbase,vfiltb,vscen,leg,yl,btype,d)
    dev.off() # end of chart exportation
    
    v_bp_dap = rbind(v_bp_dap,a_result)
    
    }
    
  }#--- end of incdata loop
  
  
}#--- end of lvar loop

write.csv(v_bp_sce,  file = paste(fsufix,"_v_bp_sce.csv"  ,sep=""),row.names = F,quote = F)
write.csv(v_perf_sce,file = paste(fsufix,"_v_perf_sce.csv",sep=""),row.names = F,quote = F)
write.csv(v_bp_dap,  file = paste(fsufix,"_v_bp_dap.csv"  ,sep=""),row.names = F,quote = F)

end_time = Sys.time()
print(paste("Started at", start_time, sep = ""))
print(paste("Ended at ", end_time, sep =""))
print(paste("Elapsed time was ", round(end_time - start_time, digits = 2), " (minutes)", sep =""))

#--- shutdown the machine after simulations (60 seconds count down)
if(flshutdown){system("shutdown -s -t 60")}

