
#------------------------------------------------------------------------------#
#--- Create multiple soil profiles based on a soil template (MV_Template.SOL)  #
#--- Run DSSAT and analyse results for a sensitivity analysis                  #
#--- Created by Murilo Vianna - May, 2018.                                     #
#------------------------------------------------------------------------------#

#--- How to use:
#--- Make sure all library packages are install
#--- Create a folder called "Batch" in the corresponding DSSAT crop folder (e.g. C:\DSSAT47\Sugarcane\Batch)
#--- Provide the csv files with soil scenarios
#--- follow the bellow coments for setup

#--- User setup
dv    = "DSSAT47"         # DSSAT version                  
crop  = "Sugarcane"       # Crop to be simulated
simID = "SPPI"            # Simulation ID (all filename will have this ID)
sname = "MV.SOL"          # Soil filenema (.SOL) with all generated scenarios
basID = "BASELINE"        # Baseline ID (Same as in file scenarios_sim.csv)

#--- Parameters setup
rsol_ID   = 3             # Row number of reference profile where soil ID is (e.g. *MVPI102502)
rsol_pd   = 7             # Row number of reference profile where soil pd data is 
rsol_ld   = c(9,14)       # Start and end row number whereas layers data are within in the reference profile
rmsol_ID  = 17            # Row number of master profile where soil ID is (e.g. *ID)
rmsol_pd  = 21            # Row number of master profile where soil pd data is
rmsol_ld  = c(23,28)      # Start and end row number whereas layers data are within in the master profile

#--- Header for .sol file
hsolfile  = "*Soils: Murilo Vianna (MV) sensitivity test based on profile *WI_FRBR064  WISE, SP. Brazil"

#--- Run simulations and sensitivity after soil creation (T/F)
runall     = TRUE

#--- Shutdown the computer after simulations? (T/F)
flshutdown = FALSE

#--- Provide the name of all outputs in PlantGro.out
#--- Columns name accordingly to DSSAT output name 
#--- NOTE: change "#" for "."

# Output names for CANEGRO:
pgro_names     = c("year",
                    "doy",
                    "das",
                    "dap",
                    "t.sd",
                    "gstd",
                    "laigd",
                    "lgdmd",
                    "smfmd",
                    "smdmd",
                    "sucmd",
                    "rdmd",
                    "badmd",
                    "t.ad",
                    "l.sd",
                    "suid",
                    "lddmd",
                    "laitd",
                    "wspd",
                    "wsgd",
                    "wstd",
                    "shtd",
                    "rdpd",
                    "rl1d",
                    "rl2d",
                    "rl3d",
                    "rl4d",
                    "rl5d",
                    "rl6d",
                    "rl7d",
                    "rl8d",
                    "rl9d",
                    "rl10d",
                    "eosa",
                    "rtld",
                    "li.d",
                    "slad",
                    "pgrd",
                    "brdmd",
                    "ldgfd",
                    "ttebc",
                    "ttspc",
                    "ttlec",
                    "su.dmd",
                    "su.fmd",
                    "cwsi",
                    "respcf")

#--- Select the output data to be analysed in the sensitivity (wincdata) and their units(wuincdata)
wincdata = c("smfmd" ,"su.fmd","shtd","badmd" , "smdmd" , "sucmd" , "rdmd"  , "wspd", "laitd" ,"t.ad")
wuincdata= c("t ha-1","%"     ,"(m)" ,"t ha-1", "t ha-1", "t ha-1", "t ha-1", "0-1" , "m2 m-2","# m-2")

#--- Indexer columns to match data
winxdata = c("year","doy","dap")

#--- End of user setup
#-----------------------------------------------------------------------------

#--- Run!
wd_dssat  = paste("C:/",dv,"/",crop,sep="")
wd        = paste("C:/",dv,"/",crop,"/Batch",sep="")

setwd(wd)
source("dssat_canegro_soil_sensitivity.R")






