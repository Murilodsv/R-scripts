YYDOY = function(dat){
  #---------------------------------------------------------#
  #--- Function to convert regular dates in DSSAT format ---#
  #---------------------------------------------------------#
  #--- Murilo Vianna, Jun-2018
  
  #--- Converts from YYYY-MM-DD to YYDOY format
  
  year = format(as.Date(dat), "%Y")
  doy  = as.Date(dat) - as.Date(paste(year,"-01-01",sep="")) + 1
  
  #--- Re-build DSSAT DATE format (YYDOY)
  return (paste(substr(year,3,4),sprintf("%003.0f",doy),sep = ""))
}
