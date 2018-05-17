#--- Create multiple soil profiles based on a soil template (MV_Template.SOL) and scenarios (scenarios.csv)
#--- sensitivity analysis

library(scales)
library(plyr)
library(data.table)

#--- Working dir
wd       = "C:/DSSAT47/Sugarcane" #simulated data WD
setwd(wd)

#--- read sensitivy scenarios
scen = read.csv("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/Modeling/DSSAT_CANEGRO/Sensitivity/Soil/scenarios.csv")

#--- reading soil profile
sol     = readLines("C:/DSSAT47/Soil/MV_Template.sol")

#--- Reference soil profile
sol_ID  = sol[3]
sol_pd  = read.table(text = sol[7])
colnames(sol_pd) = c("SCOM","SALB","SLU1","SLDR","SLRO","SLNF","SLPF","SMHB","SMPX","SMKE")
sol_pd$ID = substr(sol_ID,2,11)

sol_ld  = read.table(text = sol[9:12])
colnames(sol_ld) = c("SLB","SLMH","SLLL","SDUL","SSAT","SRGF","SSKS","SBDM","SLOC","SLCL","SLSI","SLCF","SLNI","SLHW","SLHB","SCEC","SADC")
sol_ld$ID = substr(sol_ID,2,11)

#transpose
sol_pd_t = data.frame(varname = colnames(sol_pd),value_bl = transpose(sol_pd)$V1)
sol_ld_t = data.frame(varname = colnames(sol_ld),transpose(sol_ld))
colnames(sol_ld_t)[2:5] = c("SL1","SL2","SL3","SL4")

#--- Master soil prof
msol_ID  = sol[14]
msol_pd  = read.table(text = sol[18])
colnames(msol_pd) = c("SCOM","SALB","SLU1","SLDR","SLRO","SLNF","SLPF","SMHB","SMPX","SMKE")
msol_pd$ID = substr(msol_ID,2,3)

msol_ld  = read.table(text = sol[20:23])
colnames(msol_ld) = c("SLB","SLMH","SLLL","SDUL","SSAT","SRGF","SSKS","SBDM","SLOC","SLCL","SLSI","SLCF","SLNI","SLHW","SLHB","SCEC","SADC")
msol_ld$ID = substr(msol_ID,2,3)

#transpose
msol_pd_t = data.frame(varname = colnames(msol_pd),value_mp = transpose(msol_pd)$V1)
msol_ld_t = data.frame(varname = colnames(msol_ld),transpose(msol_ld))
colnames(msol_ld_t)[2:5] = c("SL1","SL2","SL3","SL4")

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

len     = length(scen$var)  # number of scenarios
nlayers = 4                 # Number of soil layers
pmsol   = 18                # line of master profile whereas profile data is
lmsol   = 20                # line of master profile whereas starts soil layers

new_tot_sol = "*Soils: Murilo Vianna (MV) sensitivity test based on Piracicaba, SP. Brazil"

for(sc in 1:len){

s = scen$scenario[sc]  
  
new_sol = gsub(c("ID"),scen$id[sc],msol_ID)
new_sol = gsub(c("SCEN"),scen$scenario[sc],new_sol)
new_sol = rbind(" ",new_sol)
new_sol = rbind(new_sol,sol[4])
new_sol = rbind(new_sol,sol[5])
new_sol = rbind(new_sol,sol[6])

nsol_pd = sol_pd
if(scen$pos[sc] == "pd"){nsol_pd[,as.character(scen$var[sc])] = scen$val[sc]}
nsol_pd_t = data.frame(varname = colnames(nsol_pd),value_np = transpose(nsol_pd)$V1)

rep_nsol_pd = data.frame(ser = as.character(msol_pd_t$value_mp), val = as.character(nsol_pd_t$value_np),stringsAsFactors = F)
rep_nsol_pd$digits = c(0,2,1,2,1,2,2,0,0,0,0)
rep_nsol_pd$type   = c("C","R","R","R","R","R","R","C","C","C","ID")

replaced = repfunc(rep_nsol_pd,sol[pmsol])

new_sol = rbind(new_sol,replaced)
new_sol = rbind(new_sol,sol[8])


for(sl in 1:nlayers){
  
nsol_ld = sol_ld[sl,]
if(substr(scen$pos[sc],1,2) == "ld"){
  nsol_ld[,as.character(scen$var[scen$scenario == s & scen$var == scen$var[sc] & scen$pos == paste("ld",sl,sep="") 
    ])] = scen$val[scen$scenario == s & scen$var == scen$var[sc] & scen$pos == paste("ld",sl,sep="")]
  }
nsol_ld_t = data.frame(varname = colnames(nsol_ld),value_np = transpose(nsol_ld)$V1)

rep_nsol_ld = data.frame(ser = as.character(msol_ld_t[,paste("SL",sl,sep="")]), val = as.character(nsol_ld_t$value_np),stringsAsFactors = F)
rep_nsol_ld$digits = c(0,0,3,3,3,3,2,2,2,1,1,1,3,1,0,1,0,0)
rep_nsol_ld$type   = c("R","C","R","R","R","R","R","R","R","R","R","R","R","R","R","R","R","C")

replaced = repfunc(rep_nsol_ld,sol[lmsol+sl-1])
new_sol = rbind(new_sol,replaced)

}

new_tot_sol = rbind(new_tot_sol,new_sol)

}


write.table(new_tot_sol,file = "C:/DSSAT47/Soil/MV.SOL",row.names = F,col.names = F,quote = F)
