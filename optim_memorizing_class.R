#--- Adapted from G. Kraus

# define a reference class with a field runs, 
# and create an instance
runRecorder <- setRefClass("runRecorder",fields=c("runs"))
optruns <- runRecorder$new(runs=NULL)

#function takes an optional parameter runsrecorder which has to be of class 
#runRecorder
sim_rmse <- function(par_list, psi, obs_theta, runsrecorder=NULL) {
  
  theta_res = par_list[1]
  theta_sat = par_list[2]
  n         = par_list[3]
  alpha     = par_list[4]
  
  if(theta_res <0 | (theta_res > theta_sat * 0.95)){
    return(1e)
  }
  
  #psi = c(15000,330,10) # in m
  
  #--- m = f(n)
  m = 1-1/n
  
  #--- apply the MVG equation
  theta = theta_res + (theta_sat - theta_res) / (((1+(alpha * abs(psi))^n))^m)
  
  #--- rmse
  rmse = sqrt(mean((theta - obs_theta)^2))
  
  #--- storing runing info in reference class
  if(!is.null(runsrecorder) && "runRecorder" %in% class(runsrecorder)) 
  {
    runsrecorder$runs <- rbind(runsrecorder$runs, 
                               data.frame(theta_res = par_list[1],
                                          theta_sat = par_list[2],
                                          n         = par_list[3],
                                          alpha     = par_list[4],
                                          RMSE=rmse))
  }  
  return(rmse)
}

#---
obs_theta = c(0.339, 0.318, 0.316, 0.317, 0.294, 0.276, 0.260, 0.241)
psi       = c(10   ,    20,    60,   100,   330,  1000,  3000, 15000)

p_ini = c(0.05, 0.35, 1.10, 0.20)


optruns$runs <- NULL
optim(p_ini, sim_rmse, 
      control=list(maxit=50000,reltol=0.001), psi=psi, obs_theta=obs_theta, runsrecorder=optruns)


plot(optruns$runs$RMSE)



