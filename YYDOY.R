YYDOY = function(dat,todssat,millennium){
  #---------------------------------------------------------#
  #--- Function to convert regular dates in DSSAT format ---#
  #---------------------------------------------------------#
  #--- Murilo Vianna, Jul-2018
  #--- Usage: 
  #---    dat:        The data to be converted (single or vector) in the R Date format YYYY-MM-DD or YYDOY (DSSAT)
  #---    todssat:    A logical. If true the convertion YYYY-MM-DD -> YYDOY will be the output, if false YYDOY -> YYYY-MM-DD
  #---    millennium: If todssat = f: specify the millenium of data (1000 or 2000), this is a limitation of DSSAT format
  #---------------------------------------------------------#
  
  if(missing(toddsat)){toddsat=T}
  
  if(todssat){
    #--- Test data
    if(typeof(dat) != "double"){stop("Input data is not in Date format (YYYY-MM-DD). Please check dat format.")}
    
    #--- Converts from YYYY-MM-DD to YYDOY format
    year = format(as.Date(dat), "%Y")
    doy  = as.Date(dat) - as.Date(paste0(year,"-01-01")) + 1  
    
    #--- Re-build DSSAT DATE format (YYDOY)
    return (paste0(substr(year,3,4),sprintf("%003.0f",doy)))    
  }else{
    
    #--- check if millenium is set otherwise default=2000 
    if(missing(millennium)){millennium=2000}
    
    #--- Test data
    if(typeof(dat) != "character"){stop("Input data is not in DSSAT Date format (YYDOY). Please check dat format.")}
        
    #--- passing data
    doy = as.numeric(substr(dat,3,5))
    year= as.numeric(substr(dat,1,2))
    
    
    #--- Re-build DATE format (YYYY-MM-DD)
    return(as.Date(paste0(millennium+year,"-01-01")) + (doy - 1)) 
    
  }
}
