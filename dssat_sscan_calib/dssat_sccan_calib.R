#---------------------------------#
#--- DSSAT CANEGRO Calibration ---#
#--- Murilo Vianna (Jun-2018)  ---#
#---------------------------------#

#--- Description: R-Script for dssat/canegro calibration (.CUL and .ECO) 
#--- (this is not GLUE or GenCalc) 

#--- Script Setup
wd        = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/cultivars/dssat_calib"
ds_v      = 47
crop      = "Sugarcane"
xfile     = "SCGO0001.SCX" # IMPORTANT MAKE SURE THE CULTIVAR SELECT IN THE X FILE IS THE SAME AS IN THE .CUL AND .ECO MASTER FILE!
parnm     = "SCCAN047"
obs_raw   = read.csv(paste(wd,"/gogo_field_data.csv",sep=""))
pgro_head = read.csv(paste(wd,"/PlantGro_Head.csv",sep=""))
pdssat    = F
savepng   = F # save to png file? (can cause to slow down time performance)

#--- load functions
source(paste(wd,"/f_dssat_sccan_calib.R",sep=""))

#--- PlantGro header
pgro_names  = pgro_head$R_head

#--- prepare batch call
bfile = readLines(paste(wd,"/DSSBatch_Master.v47",sep=""))
bfile[4] = gsub("<calib_xfile>",xfile,bfile[4])

#--- write batch in Crop folder
write(bfile,file = paste("C:/DSSAT",ds_v,"/",crop,"/","DSSBatch.v",ds_v,sep = ""))

#--- Read parameters set up
par_set   = read.csv(paste(wd,"/dssat_canegro_calib_par.csv",sep=""))
calib = par_set

l_cv = unique(obs_raw$Cultivar)

for(cv in l_cv){

  #--- logical settings
  plotperf = F # plot sim x obs?
  plotdev  = T # follow-up optimization?
  uselimits= T # use min and max boudaries to drive optimization? (in set_par max and min) 
  
  #--- current cultivar
  calib_id  = cv
  
  #--- Number of iterations
  nopt = 15

  message(paste("Start of optimization for ",cv,sep=""))
  
  #--- Feature scaling (0-1)
  calib$svalue = (calib$Init_values - calib$Calib_range_min) /  (calib$Calib_range_max - calib$Calib_range_min)
  
  #--- Parameters to be calibrated
  svalue = calib$svalue[calib$Calibrate]
  
  #--- number of parameters
  npar = length(svalue)
  
  #--- wipe optimization file
  obj_df = data.frame(n = 0, obj = 0,inbounds = T)
  
  for(i in 1:length(svalue)) {
    df = data.frame(v = calib$Init_values[calib$Calibrate][i])
    colnames(df) = calib$Par_orig[calib$Calibrate][i]
    obj_df = cbind(obj_df,df)
  }
  
  write.csv(obj_df,file = paste(wd,"/optim_dev.csv",sep=""),row.names = F)
  
  #--- read observed data for cultivar (cv)
  crop_height = data.frame(cv  = cv,
                           dap = obs_raw$DAP[obs_raw$Cultivar==calib_id & obs_raw$Type=="Stalk_height_m"],
                           obs = obs_raw$Value[obs_raw$Cultivar==calib_id & obs_raw$Type=="Stalk_height_m"])
  
  stalk_fresh = data.frame(cv  = cv,
                           dap = obs_raw$DAP[obs_raw$Cultivar==calib_id & obs_raw$Type=="Stalk_Fresh_Mass_t_ha-1"],
                           obs = obs_raw$Value[obs_raw$Cultivar==calib_id & obs_raw$Type=="Stalk_Fresh_Mass_t_ha-1"])
  
  stalk_till  = data.frame(cv  = cv,
                           dap = obs_raw$DAP[obs_raw$Cultivar==calib_id & obs_raw$Type=="Tillering_n_m-1"],
                           obs = obs_raw$Value[obs_raw$Cultivar==calib_id & obs_raw$Type=="Tillering_n_m-1"])
  
  stalk_pol   = data.frame(cv  = cv,
                           dap = obs_raw$DAP[obs_raw$Cultivar==calib_id & obs_raw$Type=="POL_%"],
                           obs = obs_raw$Value[obs_raw$Cultivar==calib_id & obs_raw$Type=="POL_%"])
  
  #--- update observed data for calibration (cv)
  obs_df      = crop_height
  
  #--- model output used for calibraton (same as pgro_names())
  sscan_out   = "shtd"
  
  #--- Statistical index used as objective function by the optimization (for different indexes see mperf())
  outidx      = "rmse" # Using Root Mean Squared Error (RMSE)
  
for(i in 1:nopt){
  
  #--- Optimize (using default - Nelder_Mead)
  optim(svalue,dssat_sccan_calib)
  
  #--- restart iniial conditions (try to fall on global minimum)
  new_val = rnorm(1000,0.5,0.25)
  new_val = new_val[new_val>=0 & new_val<=1]
  
  #--- pick randomized points within the distribution
  new_val = new_val[abs(rnorm(length(svalue),500,100))]
  
  #--- check if the number of paramters match with requirements
  if(length(new_val) < npar){
    ntry = 1
    while(length(new_val) == npar){
      new_val = new_val[abs(rnorm(length(svalue),500,100))]
      ntry = ntry + 1
      if(ntry > 1000){stop("Failed to find new inital conditions, please review the initial parameters setup.")}
    }
  }
  
  svalue = new_val
}
  
  message(paste("End of optimization for ",cv,sep=""))
  
  #--- write optimization parameters
  opt_par = read.csv(paste(wd,"/optim_dev.csv",sep=""))
  write.csv(opt_par,file = paste(wd,"/optim_dev_",cv,".csv",sep=""),row.names = F)
  
  #--- Best set of parameters
  svalue_df =  opt_par[opt_par$obj==min(opt_par$obj[opt_par$inbounds]),as.character(calib$Par_orig[calib$Calibrate])]
  
  if(length(svalue_df[,as.character(calib$Par_orig[calib$Calibrate][1])]) > 1){
    #---use pick up the median minimun values
    svalue_df = sapply(as.character(calib$Par_orig[calib$Calibrate]),function(x) median(svalue_df[,x]))
  }
  
  svalue = svalue_df
  
  #--- scale paramters
  svalue = (svalue - calib$Calib_range_min[calib$Calibrate]) / (calib$Calib_range_max[calib$Calibrate] - calib$Calib_range_min[calib$Calibrate])
  
  #--- check best parameters performance
  plotperf = T # plot sim x obs?
  plotdev  = F # follow-up optimization?
  dssat_sccan_calib(svalue)

}

