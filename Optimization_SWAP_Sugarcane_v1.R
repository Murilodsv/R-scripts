# This code was created to input a FORTRAN model (.exe)
# as a function inside R enviroment. In addition, this function
# calculate the RMSE of the model against observed data. 
# Thus, the RMSE can be minimized using a vector of parameters
# as input, such as the function, for the "optim(par, f)" function.
# Murilo Vianna, 13-04-2016 (murilodsv@gmail.com)

#######################
######Instructions#####
#######################

# To execute this script, please make your model's parameters
# readable and writable for R and supply the observed data(obs).

#set the working directory 
setwd("C:/Users/Murilo/Dropbox/DOUTORADO/WUR/Performance/SWAP_SAMUCA_V2_CALIBRATION")



# Create a vector with the inital parameters set -> x
# Read the observed data to be compared with simulated values -> obs
# Put all parameters in the function and exchange the values of the 
# target parameters to the vector of initial values - > x[1:n]
# The function will read the parameters and re-write the parameter's file

# To read measured csv data table
PIRA = read.table("PIRA_MEASURED_DATA.CSV", sep = ",", header = TRUE)
CROR = read.table("CROR_MEASURED_DATA.CSV", sep = ",", header = TRUE)
x = read.table("Param_Set - Copia.out")
x = x$V1
d = 0.25
x = rep(1,2)
x[1] = 2.00
x[2] = 1.00
#x[3] = 0.32
#x[4] = 18.0
#x[5] = 350.0
#x[6] = 11.0
#x[7] = 20.0
#x[8] = 169.0
#x[9] = 450.0

TIME_PIRA = as.numeric(file.mtime("Plant_Coupling-SAMUCA_PIRA.out"))
TIME_CROR = as.numeric(file.mtime("Plant_SWAP-SAMUCA_CROR.OUT"))

fun = function(x) { 
  
  #Reading parameters
  
#  for (n in 1:9){
    
#    if(x[n]<=0){
#      x[n] = 0.1
      
#    }
    
#  }
  
  
  maxgl     =  5.5000
  tb        =  9.00
  rue       =  2.2076
  sla       =  46.00
  extcoef   =  0.7000
  sgpf      =  0.8800
  dpercoeff =  0.32
  sucmax    =  0.8500
  srl       =  25.00
  chustk    =  450.0
  chupeak   =  1200.0
  chudec    =  1500.0
  chumat    =  2500.0
  popmat    =  11.5
  poppeak   =  19.0
  phyloc    =  171.0
  mla       =  492.0
  sw1       = x[1]
  sw2       = x[2]
  
  print(x)
  
  #Vector of new parameter
  inp = c(maxgl,tb,rue,sla,extcoef,sgpf,dpercoeff,sucmax,srl,chustk,chupeak,chudec,chumat,popmat,poppeak,phyloc,mla,sw1,sw2)
  
  #Re-Writing parameter's file
  write.table(format(inp,digits=5),"Param_Set.OUT",dec=".",row.names = F, col.names = F,quote=F)
  
  Sys.sleep(d)
  #Running the model (.bat and .exe)
  
  system("SWAP_Sugarcane_Calibration_PIRA")
  Sys.sleep(d)
  system("SWAP_Sugarcanev1")
  Sys.sleep(d)
  
  system("SWAP_Sugarcane_Calibration_CROR")
  Sys.sleep(d)
  system("SWAP_Sugarcanev1")
  Sys.sleep(d)
  
  #Reading model's output
  S_PIRA = read.table("Plant_Coupling-SAMUCA_PIRA.OUT",skip = 7)
  S_CROR = read.table("Plant_SWAP-SAMUCA_CROR.OUT",skip = 7)
  
  #Matching output with observed data
  S_SDM_PIRA = S_PIRA$V7[match(PIRA$DAP[PIRA$CLASS =="SDM"],S_PIRA$V4)]
  S_SFM_PIRA = S_PIRA$V9[match(PIRA$DAP[PIRA$CLASS =="SFM"],S_PIRA$V4)]
  S_LAI_PIRA = S_PIRA$V12[match(PIRA$DAP[PIRA$CLASS =="LAI"],S_PIRA$V4)]
  S_TIL_PIRA = S_PIRA$V13[match(PIRA$DAP[PIRA$CLASS =="TILL"],S_PIRA$V4)]
  S_HGT_PIRA = S_PIRA$V14[match(PIRA$DAP[PIRA$CLASS =="HEIGHT"],S_PIRA$V4)]
  
  S_SDM_CROR = S_CROR$V7[match(CROR$DAP[CROR$CLASS =="SDM"],S_CROR$V4)]
  S_SFM_CROR = S_CROR$V9[match(CROR$DAP[CROR$CLASS =="SFM"],S_CROR$V4)]
  S_LAI_CROR = S_CROR$V12[match(CROR$DAP[CROR$CLASS =="LAI"],S_CROR$V4)]
  S_TIL_CROR = S_CROR$V13[match(CROR$DAP[CROR$CLASS =="TILL"],S_CROR$V4)]
  S_HGT_CROR = S_CROR$V14[match(CROR$DAP[CROR$CLASS =="HEIGHT"],S_CROR$V4)]
  S_POL_CROR = S_CROR$V11[match(CROR$DAP[CROR$CLASS =="POL"],S_CROR$V4)]
  
  M_SDM = c(PIRA$VALUE[PIRA$CLASS=="SDM"],CROR$VALUE[CROR$CLASS=="SDM"])
  M_SFM = c(PIRA$VALUE[PIRA$CLASS=="SFM"],CROR$VALUE[CROR$CLASS=="SFM"])
  M_LAI = c(PIRA$VALUE[PIRA$CLASS=="LAI"],CROR$VALUE[CROR$CLASS=="LAI"])
  M_TIL = c(PIRA$VALUE[PIRA$CLASS=="TILL"],CROR$VALUE[CROR$CLASS=="TILL"])
  M_HGT = c(PIRA$VALUE[PIRA$CLASS=="HEIGHT"],CROR$VALUE[CROR$CLASS=="HEIGHT"])
  
  S_SDM = c(S_SDM_PIRA, S_SDM_CROR)
  S_SFM = c(S_SFM_PIRA, S_SFM_CROR)
  S_LAI = c(S_LAI_PIRA, S_LAI_CROR)
  S_TIL = c(S_TIL_PIRA, S_TIL_CROR)
  S_HGT = c(S_HGT_PIRA, S_HGT_CROR)
  
  #Calculating RMSE
  
  RRMSE_SDM = (sqrt(sum((S_SDM - M_SDM)^2)/length(M_SDM)))/mean(M_SDM)
  RRMSE_SFM = (sqrt(sum((S_SFM - M_SFM)^2)/length(M_SFM)))/mean(M_SFM)
  RRMSE_LAI = (sqrt(sum((S_LAI - M_LAI)^2)/length(M_LAI)))/mean(M_LAI)
  RRMSE_TIL = (sqrt(sum((S_TIL - M_TIL)^2)/length(M_TIL)))/mean(M_TIL)
  RRMSE_HGT = (sqrt(sum((S_HGT - M_HGT)^2)/length(M_HGT)))/mean(M_HGT)
  
  RRMSE = mean(c(RRMSE_SDM,RRMSE_SFM,RRMSE_LAI,RRMSE_TIL,RRMSE_HGT))*100
  
  RRMSE
  
}

# After testing wheter your model and all files are working as a function 
# run the "optim()" function of R, using as input the vector of initial
# parameters (x) and the function (fun) that calculates the RMSE of your model

# optim(x,fun)
# optim(x,fun, method = "L-BFGS-B", lower = c(30,300), upper = c(100, 800))
#optim(x,fun, method = "L-BFGS-B", lower = c(8,20,8), upper = c(15,100,40))

#optim(x,fun, method = "BFGS")

#optim(x,fun, method = "L-BFGS-B", lower = rep(0,7), upper = rep(1000,7))
# This function will try to minimize the RMSE of your model testing diferent
# set of parameters (x). You can choose diferent methods for the optimization,
# such as set lower and upper limit for each parameter in the process.


