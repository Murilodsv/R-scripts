dssat_sccan_calib = function(svalue){
  
  #------------------------------------------#
  #--- DSSAT-CANEGRO Calibration function ---#
  #------------------------------------------#
  
  #--- Goals:
  #------- 1) Update parameters file; 
  #------- 2) Run the model; 
  #------- 3) Compute performance based on observed data
  
  #--- reference: Functional, structural and agrohydrological sugarcane crop modelling: towards a simulation platform for Brazilian farming systems (PhD Dissertation, 2018)
  #--- contact: Murilo Vianna (murilodsv@gmail.com)
  
  #--- Track time
  start_time  = Sys.time()
  
  p             = 4 # parameter precision (#decimals)

  #--- Convert scaled to parameter values (feature scales method)
  calib$pvalue = calib$Calib_values
  calib$pvalue[calib$Calibrate] = svalue * (calib$Calib_range_max[calib$Calibrate] - calib$Calib_range_min[calib$Calibrate]) + calib$Calib_range_min[calib$Calibrate]
  
  #--- cultivar ID to follow-up optimization
  message(paste("Calibrating cultivar: ",calib_id,sep=""))
  message("")
  
  #--- write new parameters on screen
  message("Using the following parameters:")
  for(i in unique(calib$Par_orig[calib$Calibrate])){ 
    
    plab = i
    sizep = max(nchar(as.character(calib$Par_orig)))
    while(nchar(plab) < sizep){
      plab = paste(plab," ",sep = "")
    }
    
    message(paste(plab," = ",sprintf(paste("%.",p,"f",sep=""),calib$pvalue[calib$Par_orig==i]),sep=""))
  }
  
  #--- check for physiological meaning
  penalize = check_par_nonlogic(calib)
  
  #--- Check if estimated parameters are within user-boundaries 
  if(uselimits){
    
    #--- check constraints
    for(i in 1:length(svalue)) {
      if(svalue[i] > 1 | svalue[i] < 0){
        
        #--- msg
        message(paste("Warning: Parameter ",calib$Par_orig[calib$Calibrate][i], " is out of min and max range (Objective increased)",sep = ""))
        
        #--- Increase objective function 
        penalize = T
      }
    }
  }
  
  
  #--- Read model parameters MASTER files
  par_cul_file  = readLines(paste(wd,"/SCCAN047_M.CUL",sep = ""))
  par_eco_file  = readLines(paste(wd,"/SCCAN047_M.ECO",sep = ""))
  rep_line      = 4 # line of file where the replacement will be done
  
  #--- re-build cultivar file (.CUL)
  l_ftype = c(".CUL",".ECO")
  
  for(ftype in l_ftype){
    
    #---Read model parameters MASTER file
    p_file = readLines(paste(wd,"/SCCAN047_M",ftype,sep = ""))
    
    #--- list of parameters
    l_rp = unique(calib$Rep_ID[calib$File == ftype])
    
    for(rp in l_rp) {
      
      replace = sprintf(paste("%.",p,"f",sep=""),calib$pvalue[calib$Rep_ID==rp & calib$File==ftype])
      
      #--- check whether new parameter match size
      m_size = calib$Par_size[calib$Rep_ID==rp & calib$File==ftype] - nchar(replace)
      
      if(m_size > 0){
        #--- add spaces to match size
        while(m_size > 0){
          replace = paste(" ",replace,sep="")
          m_size = calib$Par_size[calib$Rep_ID==rp & calib$File==ftype] - nchar(replace)
        }
        
      }else if(m_size < 0){
        #--- remove precision to match size
        message(paste("Warning: Precision of parameter ",calib$Par_orig[calib$Rep_ID==rp & calib$File==ftype]," reduced to match file.CUL fmt",sep=""))
        
        red_p = p
        while(m_size < 0){
          red_p = red_p - 1
          
          if(red_p < 0){stop(paste("Parameter ",calib$Par_orig[calib$Rep_ID==rp & calib$File==ftype], " is too high for file.CUL fmt (try to reduce maximun range)",sep=""))}
          
          replace = sprintf(gsub(p,red_p,paste("%.",p,"f",sep="")),calib$pvalue[calib$Rep_ID==rp & calib$File==ftype])
          m_size = calib$Par_size[calib$Rep_ID==rp & calib$File==ftype] - nchar(replace)
        }
      }
      
      
      p_file[rep_line] = gsub(rp,
                              replace
                              ,p_file[rep_line])
      
    }
    
    #--- write parameter file 
    write(p_file,file =paste("C:/DSSAT",ds_v,"/Genotype/",parnm,ftype,sep = ""))
  }
  
  #--- set wd to run
  setwd(paste("C:/DSSAT",ds_v,"/",crop,"/",sep = ""))
  
  #--- write paramters used on the screen
  message("")
  message("Running DSSAT-Canegro...")
  
  #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
  system(paste("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste("DSSBatch.v",ds_v,sep=""),sep=""),show.output.on.console = pdssat)
  
  
  #--- Read simulated data
  plant_lines = readLines("PlantGro.OUT")
  
  #--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
  write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
              file = paste("PlantGro_",calib_id,".OUT",sep=""),
              row.names = F, col.names = F,quote = F)
  plant = read.table(file = paste("PlantGro_",calib_id,".OUT",sep=""))                   #Read numeric lines as data.frame
  
  #--- Columns name accordingly to DSSAT output name
  colnames(plant) = pgro_names
  
  #--- observed data
  obs = obs_df$obs
  
  #-- simulated data
  sim = plant[plant$dap %in% obs_df$dap,sscan_out]
  
  
  
  if(plotperf){
    
    png(paste(wd,"/perf_",calib_id,".png",sep=""),
        units="in", 
        width=24, 
        height=12, 
        pointsize=24, 
        res=300)
    
    par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
    objective = mperf(sim,obs,sscan_out,T,outidx,F)
    
    #--- write performance
    model_perf = mperf(sim,obs,sscan_out,F,outidx,T)
    
    perf_df = data.frame(cv = calib_id,model_perf)
    write.csv(perf_df, file = paste(wd,"/perf_",calib_id,".csv",sep=""),row.names = F)
    
    plot(plant[,sscan_out]~plant[,"dap"],
         type = "l",
         xlab = "DAP",
         ylab = sscan_out)
    points(obs_df$obs~obs_df$dap)
    
    legend("topleft",
           inset   = 0.02,
           legend  = c("Simulated","Observed"),
           col     = c("black","black"),
           lt      = c(1,0),
           pch     = c(NA,1),
           bg      = "grey",
           cex     = 1.0,
           box.lty = 1)
    
    dev.off()
    
  }else{
    #--- mperf compute several indexes, RMSE is outputed
    objective = mperf(sim,obs,sscan_out,plotperf,outidx,F)
  }
  
  #--- print this run objective
  message("")
  message(paste(outidx," is: ",objective,sep=""))
  
  if(plotdev){
    
    it_before = read.csv(file = paste(wd,"/optim_dev.csv",sep=""))
    
    obj_df = data.frame(n = (max(it_before$n)+1),obj = objective,inbounds = T)
    
    for(i in 1:length(svalue)) {
      df = data.frame(v = svalue[i] * (calib$Calib_range_max[calib$Calibrate][i] - calib$Calib_range_min[calib$Calibrate][i]) + calib$Calib_range_min[calib$Calibrate][i])
      colnames(df) = calib$Par_orig[calib$Calibrate][i]
      obj_df = cbind(obj_df,df)
      
      #--- Use constrained limits from par_set max and min?
      if(uselimits){
        
        if(penalize){
          #--- Flag values that are out of user-boundaries
          obj_df$inbounds = F
        }
      }
    }
    
    if(length(it_before$n) == 1){
      it_before = obj_df
      it_after = rbind(it_before,obj_df)
    }else{
      
      it_after = rbind(it_before,obj_df)
      if(length(dev.list())>0){dev.off()}
    }
    
    #--- plot on screen
    plot(it_after$obj~it_after$n, type = "l",ylab = outidx,xlab = "Number of iterations", ylim = c(0,max(it_after$obj)))
    lines(c(min(it_after$obj),min(it_after$obj))~c(-1000,max(it_after$n)*1000), lty = 3,col = "red")
    
    write.csv(it_after,file = paste(wd,"/optim_dev.csv",sep=""),row.names = F)
    
  }
  
  #--- Penalize objetive
  if(uselimits){
    if(penalize){
      if(plotdev){
        objective = max(it_before$obj)
      }else{
        penalty = 1000
        objective = objective * penalty
      }
    }
  }
  
  end_time    = Sys.time()
  message(paste("Elapsed time: ",round(end_time - start_time,digits = 2),"sec"))
  message("--------------------------------------------")
  
  
  objective
  
}


mperf     = function(sim,obs,vnam,dchart,outidx,out_all){
  
  #--- Compute statistical indexes of performance
  #--- Performance function
  # sim     - simulated values [R]
  # obs     - observed values [R]
  # vnam    - Name of variable as string for chart axis  [S]
  # dchart  - Display Chart? [T or F]
  
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
  if(length(unique(sim)) > 1){
    a     = summary(fit)$coefficients["(Intercept)","Estimate"]
    b     = summary(fit)$coefficients["obs","Estimate"]
  }
  
  if(dchart){
    #--- Chart Sim ~ Obs
    varlab = vnam 
    
    mindt = min(obs,sim)
    maxdt = max(obs,sim)
    #--- Ploting limits 
    pllim = c(mindt-0.1*(maxdt-mindt),maxdt+0.1*(maxdt-mindt))
    xx = seq(min(obs),max(obs),length = (max(obs)-min(obs))*1000)
    #z = summary(fit)
    
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
  }
  
  
  perf = data.frame(bias,
                    mse,
                    rmse,
                    mae,
                    rrmse,
                    rmae,
                    ef,
                    r,
                    r2,
                    d)
  if(out_all){
    out = perf
  }else{
    out = perf[,outidx] 
  }
  
  return(out)
}

check_par_nonlogic = function(calib){
  
  #--- check parameter physilogical logic
  out = F
  
  if(calib$pvalue[calib$Par_orig=="CHUPIBASE"] < calib$pvalue[calib$Par_orig=="TTPLNTEM"]){
    message("")
    message("Warning: CHUPIBASE lower than TTPLNTEM (Objective increased)")
    out = T
  }
  
  if(calib$pvalue[calib$Par_orig=="CHUPIBASE"] < calib$pvalue[calib$Par_orig=="TTRATNEM"]){
    message("")
    message("Warning: CHUPIBASE lower than TTRATNEM (Objective increased)")
    out = T
  }
  
  if(calib$pvalue[calib$Par_orig=="MAX_POP"]   < calib$pvalue[calib$Par_orig=="POPTT16"]){
    message("")
    message("Warning: MAX_POP lower than POPTT16 (Objective increased)")
    out = T
  }
  
  if(calib$pvalue[calib$Par_orig=="PI2"]   < calib$pvalue[calib$Par_orig=="PI1"]){
    message("")
    message("Warning: PI2 lower than PI1 (Objective increased)")
    out = T
  }
  
  if(calib$pvalue[calib$Par_orig=="PI2"]   < calib$pvalue[calib$Par_orig=="PI1"]){
    message("")
    message("Warning: PI2 lower than PI1 (Objective increased)")
    out = T
  }
  
  return(out)
  
}
