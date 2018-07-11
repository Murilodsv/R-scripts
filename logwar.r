
logwar = function(msg,warfile,warnew){

  #--- function to log warnings in the warfile
  
  write("", file = warfile, append = !warnew)
  write(paste("Warning at:", Sys.time()), file = warfile, append = !warnew)
  write(msg, file = warfile, append = !warnew)
}
