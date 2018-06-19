---------------------------------
# DSSAT/CANEGRO Calibration
Murilo Vianna (Jun-2018)
----------------------------------


# Main Goal:
Calibrate .CUL and .ECO crop parameters of DSSAT/CANEGRO (PBM)

# Methods:
It uses the general purpose optimization function "optim()" embedded in R environment. User can set up the optimization method, objective function, observed data, and parameters to be calibrated. Charts and tables are provided as outputs with calibrated parameters.

# Description:
1) Set the dssat_canegro_calib_par.csv file with initial parameters and boundaries
2) Place the following files in the same directory: 
- f_dssat_sccan_calib.R;
- dssat_sccan_calib.R;
- PlantGro_Head.csv;
- dssat_canegro_calib_par.csv;
- DSSBatch_Master.v47;
- SCCAN047_M.CUL;
- SCCAN047_M.ECO;

3) Open dssat_sccan_calib.R and follow the comented instructions
