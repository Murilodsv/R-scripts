#--- Functions Created for sens_main.R
#--- Murilo Vianna
#--- May-2018

#--- Function to write as ascii
repfunc = function(rep_ns_df,replaced){
  
  #-----------------------------------------------------------------------------
  #
  #   rep_ns_df is an data frame with 4 collumns:
  #     ser: search string to be replaced within the given "replaced" variable
  #     val: value to be replaced 
  #     digits: number of digits to match ascii table requirements
  #     type: type of the variable: "C" = character, "R" = Real
  
  #   replaced is the target string to perform the replacements
  #-----------------------------------------------------------------------------
  
  
  for(i in 1:(length(rep_ns_df$val)-1)){
    
    ser = paste(rep_ns_df$ser[i])
    
    if(rep_ns_df$type[i] == "R"){
      
      #--- Check size of values
      size_ser = nchar(paste(rep_ns_df$ser[i]))
      size_val = nchar(sprintf(gsub("99",rep_ns_df$digits[i],"%.99f"), round(as.numeric(rep_ns_df$val[i]), digits = rep_ns_df$digits[i])))
      
      #--- format replacement
      if(size_ser == size_val){
        #--- Straight replacement
        rep = sprintf(gsub("99",rep_ns_df$digits[i],"%.99f"), round(as.numeric(rep_ns_df$val[i]), digits = rep_ns_df$digits[i]))
        
      }else if(size_ser > size_val){
        #--- Include spaces
        rep = ""
        for(j in 1:(size_ser - size_val)){
          rep = paste(" ",rep, sep = "")
        }
        
        rep = paste(rep,sprintf(gsub("99",rep_ns_df$digits[i],"%.99f"), round(as.numeric(rep_ns_df$val[i]), digits = rep_ns_df$digits[i])),sep = "")
        
      }else if(size_ser < size_val){
        #--- Reduce digits until match size
        d = 1
        while(size_ser < size_val){
          rep = sprintf(gsub("99",rep_ns_df$digits[i]-d,"%.99f"), round(as.numeric(rep_ns_df$val[i]), digits = rep_ns_df$digits[i]-d))
          size_val = nchar(rep)
          d = d - 1
        }
      }
      
    }else{
      #--- straight replace
      
      #--- Check size of values
      size_ser = nchar(paste(rep_ns_df$ser[i]))
      size_val = nchar(rep_ns_df$val[i])
      
      if(size_ser > size_val){
        #--- Include spaces
        rep = ""
        
        for(j in 1:(size_ser - size_val)){
          rep = paste(" ",rep, sep = "")
        }
        
        rep = paste(rep,rep_ns_df$val[i],sep="")
        
      }else{
        rep = paste(rep_ns_df$val[i])
      }
    }
    
    replaced = gsub(ser,rep,replaced)
    
  }
  replaced
}

#--- Boxplot of all sensitivity scenarios function
plot_bp_sce = function(vanal,vfilt,vscen,yl,xl,btype,d){
  
  #--- Include mean, max and min for each scenario
  mean_sens = aggregate(vanal, by = list((vfilt)), mean)
  max_sens  = aggregate(vanal, by = list((vfilt)), max )
  min_sens  = aggregate(vanal, by = list((vfilt)), min )
  
  s_bp = boxplot(vanal~vfilt,
                 outline = F,
                 xaxt='n',
                 yaxt='n',
                 plot = F)
  
  s_bpdf = data.frame(sce    = round(as.numeric(s_bp$names),digits = 2),
                      b1     = s_bp$stats[1,],
                      b2     = s_bp$stats[2,],
                      b3     = s_bp$stats[3,],
                      b4     = s_bp$stats[4,],
                      b5     = s_bp$stats[5,],
                      n      = s_bp$n,
                      v.id   = unique(vscen),
                      out.id = d,
                      bptype = btype)
  
  #--- Include mean, max and min sensitivity
  colnames(mean_sens) = c("sce","mean")
  colnames(max_sens)  = c("sce","max")
  colnames(min_sens)  = c("sce","min")
  
  mean_sens$sce = round(mean_sens$sce, digits = 2)
  max_sens$sce  = round(max_sens$sce , digits = 2)
  min_sens$sce  = round(min_sens$sce , digits = 2)
  
  s_bpdf = merge(s_bpdf,mean_sens,by="sce")
  s_bpdf = merge(s_bpdf,max_sens,by="sce")
  s_bpdf = merge(s_bpdf,min_sens,by="sce")
  
  yli = max(abs(s_bpdf$b1),abs(s_bpdf$b5))
  
  yli = c(yli * -1.05, yli * 1.05)
  yli[is.na(yli)] = 0
  
  boxplot(vanal~vfilt,
          outline = F,
          ylim = yli,
          xaxt='n',
          yaxt='n')
  
  lines(c(-1000,1000),c(0,0), col = "red", lty = 3) # zero line
  
  p = unique(vfilt)
  p = length(p[p<0]) + 0.5
  
  lines(c(p,p),c(-1000,1000), col = "red", lty = 3) # zero line
  
  boxplot(vanal~vfilt,
          outline = F,
          col   = "lightblue",
          names = sort(round(unique(vfilt), digits = 2)*100),
          ylim = yli,
          add   = T,
          ylab  = yl,
          xlab  = xl,
          cex.lab=1.5,
          cex.axis=1.2)
  
  s_bpdf
}

#--- Boxplots ~ dap
sens_bp_dap = function(vanal,vfilt,vscen,leg,yl,yli,btype,d){
  
  #--- Include mean, max and min for each scenario
  mean_sens = aggregate(vanal, by = list((vfilt)), mean)
  max_sens  = aggregate(vanal, by = list((vfilt)), max )
  min_sens  = aggregate(vanal, by = list((vfilt)), min )
  
  s_bp = boxplot(vanal~vfilt,plot = F)
  
  #--- extract to df
  s_bpdf = data.frame(dap = round(as.numeric(s_bp$names), digits = 0),
                      b1     = s_bp$stats[1,],
                      b2     = s_bp$stats[2,],
                      b3     = s_bp$stats[3,],
                      b4     = s_bp$stats[4,],
                      b5     = s_bp$stats[5,],
                      n      = s_bp$n,
                      v.id   = unique(vscen),
                      out.id = d,
                      bptype = btype)
  
  #--- Boxplot charts by DAP
  plot(s_bpdf$b3~s_bpdf$dap,
       type = "l",
       ylim = yli,
       col = "red",
       lwd = 1.5,
       ylab = yl,
       xlab = "DAP",
       cex.lab=1.5,
       cex.axis=1.2)
  
  
  lines(c(-1000,1000),c(0,0), col = "black", lty = 5) # zero line
  lines(s_bpdf$b1~s_bpdf$dap)                         # bottom of BP
  lines(s_bpdf$b2~s_bpdf$dap, col = "blue",lty = 1)   # bottom box of BP
  lines(s_bpdf$b4~s_bpdf$dap, col = "blue",lty = 1)   # top box of BP
  lines(s_bpdf$b5~s_bpdf$dap)                         # top of BP
  lines(s_bpdf$b3~s_bpdf$dap, col = "red",  lwd = 1.5)# Median of BP
  points(s_bp$group,s_bp$out, col = "grey", pch = ".")# Outliers
  
  legend("topleft",inset = 0.01, legend =  leg, bg = "lightblue",cex = 1.5, box.lty = 1)
  
  
  #--- Include mean, max and min sensitivity
  colnames(mean_sens) = c("dap","mean")
  colnames(max_sens)  = c("dap","max")
  colnames(min_sens)  = c("dap","min")
  
  mean_sens$dap = round(mean_sens$dap, digits = 0)
  max_sens$dap  = round(max_sens$dap , digits = 0)
  min_sens$dap  = round(min_sens$dap , digits = 0)
  
  s_bpdf = merge(s_bpdf,mean_sens,by="dap")
  s_bpdf = merge(s_bpdf,max_sens ,by="dap")
  s_bpdf = merge(s_bpdf,min_sens ,by="dap")
  s_bpdf
}

#--- Mperf for Scenarios x Baseline
mperf = function(sim,obs,vnam,dchart){
  
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
  a     = summary(fit)$coefficients["(Intercept)","Estimate"]
  b     = summary(fit)$coefficients["obs","Estimate"]
  
  if(dchart){
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
         xlim = pllim,
         cex.lab=1.5,
         cex.axis=1.2)
    
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
  
  
  perf = data.frame(id = vnam,
                    bias,
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
  
  perf
}

#--- Boxplots of simulated outputs ~ dap
values_bp_dap = function(vanal,vfilt,vbase,vfiltb,vscen,leg,yl,btype,d){
  
  #--- Include mean, max and min for each scenario
  mean_sens = aggregate(vanal, by = list((vfilt)), mean)
  max_sens  = aggregate(vanal, by = list((vfilt)), max )
  min_sens  = aggregate(vanal, by = list((vfilt)), min )
  
  s_bp = boxplot(vanal~vfilt,plot = F)
  
  s_bp_bl = boxplot(vbase~vfiltb,plot = F)
  
  #--- extract to df
  s_bpdf = data.frame(dap = as.numeric(s_bp$names),
                      b1 = s_bp$stats[1,],
                      b2 = s_bp$stats[2,],
                      b3 = s_bp$stats[3,],
                      b4 = s_bp$stats[4,],
                      b5 = s_bp$stats[5,])
  
  #--- extract to df
  s_bpdf_bl = data.frame(dap = as.numeric(s_bp_bl$names),
                         b1 = s_bp_bl$stats[1,],
                         b2 = s_bp_bl$stats[2,],
                         b3 = s_bp_bl$stats[3,],
                         b4 = s_bp_bl$stats[4,],
                         b5 = s_bp_bl$stats[5,])
  
  ylim_range = c(min(s_bp$out,s_bp$stats[1,],
                     s_bp_bl$out,s_bp_bl$stats[1,]),
                 max(s_bp$out,s_bp$stats[5,],
                     s_bp_bl$out,s_bp_bl$stats[1,]))
  
  ylim_range[is.na(ylim_range) | is.infinite(ylim_range)] = 0
  
  #--- Boxplot charts by DAP
  plot(s_bpdf$b3~s_bpdf$dap,
       type = "l",
       ylim = ylim_range,
       col = "red",
       lwd = 1.5,
       ylab = yl,
       xlab = "DAP",
       cex.lab=1.5,
       cex.axis=1.2)
  
  
  lines(s_bpdf$b1~s_bpdf$dap)                         # bottom of BP
  lines(s_bpdf$b2~s_bpdf$dap, col = "blue",lty = 1)   # bottom box of BP
  lines(s_bpdf$b4~s_bpdf$dap, col = "blue",lty = 1)   # top box of BP
  lines(s_bpdf$b5~s_bpdf$dap)                         # top of BP
  lines(s_bpdf$b3~s_bpdf$dap, col = "red",  lwd = 1.5)# Median of BP
  points(s_bp$group,s_bp$out, col = "grey", pch = ".")# Outliers
  
  lines(s_bpdf_bl$b1~s_bpdf_bl$dap, col = "black", lty = 3)
  lines(s_bpdf_bl$b2~s_bpdf_bl$dap, col = "blue",  lty = 3)
  lines(s_bpdf_bl$b3~s_bpdf_bl$dap, col = "red",   lty = 3)
  lines(s_bpdf_bl$b4~s_bpdf_bl$dap, col = "blue",  lty = 3)
  lines(s_bpdf_bl$b5~s_bpdf_bl$dap, col = "black", lty = 3)
  points(s_bp_bl$group,s_bp_bl$out, col = "orange", pch = ".")
  
  legend("topleft",inset = 0.01, legend =  leg, bg = "lightblue",cex = 1.5, box.lty = 1)
  
  #--- export data to df
  s_bpdf = data.frame(dap    = round(as.numeric(s_bp$names), digits = 0),
                      b1     = s_bp$stats[1,],
                      b2     = s_bp$stats[2,],
                      b3     = s_bp$stats[3,],
                      b4     = s_bp$stats[4,],
                      b5     = s_bp$stats[5,],
                      n      = s_bp$n,
                      v.id   = unique(vscen),
                      out.id = d,
                      bptype = btype)
  
  #--- Include mean, max and min sensitivity
  colnames(mean_sens) = c("dap","mean")
  colnames(max_sens)  = c("dap","max")
  colnames(min_sens)  = c("dap","min")
  
  mean_sens$dap = round(mean_sens$dap, digits = 0)
  max_sens$dap  = round(max_sens$dap , digits = 0)
  min_sens$dap  = round(min_sens$dap , digits = 0)
  
  s_bpdf = merge(s_bpdf,mean_sens,by="dap")
  s_bpdf = merge(s_bpdf,max_sens ,by="dap")
  s_bpdf = merge(s_bpdf,min_sens ,by="dap")
  s_bpdf
}