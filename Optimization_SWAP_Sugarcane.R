# This code was created to input a FORTRAN model (.exe)
# as a function inside R enviroment. In addition, this function
# calculate the RMSE of the model against observed data. 
# Thus, the RMSE can be minimized using a vector of parameters
# as input, such as the function, for the "optim(par, f)" function.
# Murilo Vianna, 13-04-2016 (murilodsv@gmail.com)

                #######################
                ######Intructions######
                #######################
                
# To execute this script, please make your model's parameters
# readable and writable for R and supply the observed data(obs).

#set the working directory 
setwd("C:/Murilo/DOUTORADO/WUR/SWAP_Sugarcanev2/SWAP_Sugarcanev2/SWAP_Sugarcanev2")

# Create a vector with the inital parameters set -> x
# Read the observed data to be compared with simulated values -> obs
# Put all parameters in the function and exchange the values of the 
# target parameters to the vector of initial values - > x[1:n]
# The function will read the parameters and re-write the parameter's file

fun = function(x) { 
  
  #Reading parameters
  
  maxgl     =  5.5000
  tb        =  11.053
  rue       =  2.2076
  sla       =  x[1]
  extcoef   =  0.7000
  sgpf      =  0.8800
  dpercoeff =  0.3230
  sucmax    =  0.9500
  srl       =  18.000
  chustk    =  350.00
  chupeak   =  1200.0
  chudec    =  1500.0
  chumat    =  2500.0
  popmat    =  11.000
  poppeak   =  20.000
  phyloc    =  169.00
  mla       =  x[2]
  
  #Vector of new parameter
  inp = c(maxgl,tb,rue,sla,extcoef,sgpf,dpercoeff,sucmax,srl,chustk,chupeak,chudec,chumat,popmat,poppeak,phyloc,mla)
  
  #Re-Writing parameter's file
  write.table(format(inp,digits=5),"Param_Set.OUT",dec=".",row.names = F, col.names = F,quote=F)
  
  #Running the model (.exe)
  system("SWAP_Sugarcanev2")
  
  #Reading model's output
  sim = read.table("Plant_Coupling-SAMUCA.OUT",skip = 7)
  
  #Matching output with observed data
  sim = sim$V12[match(obs$dap,sim$V4)]
  
  #Calculating RMSE
  rmse = sqrt(sum((sim - obs$lai)^2)/nrow(obs))
  print(rmse)
  
}

# After testing wheter your model and all files are working as a function 
# run the "optim()" function of R, using as input the vector of initial
# parameters (x) and the function (fun) that calculates the RMSE of your model

# optim(x,fun)

# This function will try to minimize the RMSE of your model testing diferent
# set of parameters (x). You can choose diferent methods for the optimization,
# such as set lower and upper limit for each parameter in the process.


