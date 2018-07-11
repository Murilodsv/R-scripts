#----------------------------------------------#
#----------- Crop Parameters Function ---------#
#-----------      DSSAT/CANEGRO       ---------#
#----------------------------------------------#


#--- Function dedicated to create DSSAT/CANEGRO crop parameters files (.CUL and .ECO)

#--- Working directory with parameters values and master parameter file
wd = "C:/Users/Dropbox/MuriloVianna/Modeling/DSSAT_CANEGRO/Setup/crop_par"

#--- set the WD
setwd(wd)

#--- load parameters DB
culpar_dbfnm = "dssat_canegro_culpar.csv"
ecopar_dbfnm = "dssat_canegro_ecopar.csv"
culpar_db = read.csv(file = paste(wd,"/",culpar_dbfnm,sep=""))
ecopar_db = read.csv(file = paste(wd,"/",ecopar_dbfnm,sep=""))

#--- load functions to create .CUL and .ECO
source("dspar_sccan_f.R")

#--- write file.cul and file.eco
write(dscul_sccan047(culpar_db), file = "file.cul")
write(dseco_sccan047(ecopar_db), file = "file.eco")
