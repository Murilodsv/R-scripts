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
  rmae  = (1/length(obs)) * sum(abs(sim-obs)/abs(obs))
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

