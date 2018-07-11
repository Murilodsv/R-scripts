#--- Loading Functions

dscul_sccan047 = function(culpar_db){
  
  #----------------------------------------------#
  #----------- Crop Parameters Function ---------#
  #-----------  DSSAT/CANEGRO CUL file  ---------#
  #----------------------------------------------#
  
  #--- Description:
  #--- Provide the .CUL parameters in a data frame (culpar_db) with the following column names:
  #--- "Par_orig"         Original parameter name (.CUL file)
  #--- "Rep_ID"           The replacement target 
  #--- "Par_size"         The parameter character length (including "." and spaces for FORTRAN fmt) 
  #--- "Precision"        The parameter decimal precision
  #--- "Value"            Parameter value
  #--- "ECO_ID"           The corresponding ecotype ID (DSSAT format)
  #--- "VAR_ID"           The cultivar ID (DSSAT format)
  #--- "VAR_name"         Cultivar description (no longer than 22 characters)
  #--- IMPORTANT:
  #--- Place the Master file.cul in the same working directory (The replacement will happen within this files)
  
  #--- Contact: murilodsv@gmail.com
  #--- Murilo Vianna, Jul-2018
  #----------------------------------------------#
  
  #--- setting working directory
  wd = getwd()
  
  #--- read cultivar master file
  culpar_m = readLines(paste(wd,"/SCCAN047_M.CUL",sep=""))
  
  h_idx = 1
  while(substr(culpar_m[h_idx],1,4) != "@VAR"){
    h_idx = h_idx + 1
  }
  
  #--- data to be replaced
  culpar_mrep = culpar_m[h_idx+1]
  
  #--- header
  culpar_mhea = culpar_m[1:h_idx]
  
  l_cv = unique(culpar_db$VAR_ID)
  
  #--- character length for .cul and .eco files fmt
  chlen_des_cul = 22 # 22 is for the DSSAT v4.7
  
  #--- Header of file.CUL
  culpar_fmt = culpar_mhea
  
  for(cv in l_cv) {
    #--- cv parameters
    cv_culpar = culpar_db[culpar_db$VAR_ID == cv, ]
    cv_culpar_fmt = culpar_mrep
    
    l_par = unique(cv_culpar$Rep_ID)
    
    for (p in l_par) {
      replace = sprintf(paste("%.", cv_culpar$Precision[cv_culpar$Rep_ID == p], "f", sep =
                                ""),
                        cv_culpar$Value[cv_culpar$Rep_ID == p])
      
      #--- check whether new parameter match size
      m_size = cv_culpar$Par_size[cv_culpar$Rep_ID == p] - nchar(replace)
      
      if (m_size > 0) {
        #--- add spaces to match size
        while (m_size > 0) {
          replace = paste(" ", replace, sep = "")
          m_size = cv_culpar$Par_size[cv_culpar$Rep_ID == p] - nchar(replace)
        }
        
      } else if (m_size < 0) {
        #--- remove precision to match size
        message(
          paste(
            "Warning: Precision of parameter ",
            cv_culpar$Par_orig[cv_culpar$Rep_ID == p],
            " reduced to match file.CUL fmt",
            sep = ""
          )
        )
        
        red_p = cv_culpar$Precision[cv_culpar$Rep_ID == p]
        while (m_size < 0) {
          red_p = red_p - 1
          
          if (red_p < 0) {
            stop(
              paste(
                "Parameter ",
                cv_culpar$Par_orig[cv_culpar$Rep_ID == p],
                " is too high for file.CUL fmt (Check Parameter units)",
                sep = ""
              )
            )
          }
          
          replace = sprintf(paste("%.", red_p, "f", sep = ""), cv_culpar$Value[cv_culpar$Rep_ID ==
                                                                                 p])
          m_size = calib$Par_size[calib$Rep_ID == p] - nchar(replace)
        }
      }
      
      cv_culpar_fmt = gsub(p, replace, cv_culpar_fmt)
      
    }
    
    #--- add cultivar ID
    cv_culpar_fmt = gsub("<cvID>" , cv_culpar$VAR_ID[1], cv_culpar_fmt)
    cv_culpar_fmt = gsub("<cvECO>", cv_culpar$ECO_ID[1], cv_culpar_fmt)
    
    cv_des = as.character(cv_culpar$VAR_name[1])
    
    #--- Match cha len size
    size_des = chlen_des_cul
    if (nchar(cv_des) < size_des) {
      #--- add spaces to match .CUL fmt
      for (i in 1:(size_des - nchar(cv_des))) {
        cv_des = paste0(cv_des, " ")
      }
    } else if (nchar(cv_des) > size_des) {
      #--- Reduce description to match .CUL fmt
      cv_des = substr(cv_des, 1, size_des)
      message(
        paste0(
          "Description of cultivar ",
          cv_culpar$VAR_ID[1],
          " reduced to ",
          cv_des,
          " to match .CUL fmt"
        )
      )
    }
    
    cv_culpar_fmt = gsub("<cvDES>", cv_des, cv_culpar_fmt)
    
    culpar_fmt = c(culpar_fmt, cv_culpar_fmt)
    
  }
  
  return(culpar_fmt)
  
}

dseco_sccan047 = function(ecopar_db){
  
  #----------------------------------------------#
  #----------- Crop Parameters Function ---------#
  #-----------  DSSAT/CANEGRO ECO file  ---------#
  #----------------------------------------------#
  
  #--- Description:
  #--- Provide the .ECO parameters in a data frame (ecopar_db) with the following column names:
  #--- "Par_orig"         Original parameter name (.ECO file)
  #--- "Rep_ID"           The replacement target 
  #--- "Par_size"         The parameter character length (including "." and spaces for FORTRAN fmt) 
  #--- "Precision"        The parameter decimal precision
  #--- "Value"            Parameter value
  #--- "ECO_ID"           The corresponding ecotype ID (DSSAT format)
  #--- "ECO_name"         Ecotype description (no longer than 18 characters)
  
  #--- IMPORTANT:
  #--- Place the Master file.eco in the same working directory (The replacement will happen within this files)
  
  #--- Contact: murilodsv@gmail.com
  #--- Murilo Vianna, Jul-2018
  #----------------------------------------------#
  
  
  #--- setting working directory
  wd = getwd()
  
  #--- read cultivar master file
  ecopar_m = readLines(paste(wd, "/SCCAN047_M.ECO", sep = ""))
  
  h_idx = 1
  while (substr(ecopar_m[h_idx], 1, 4) != "@ECO") {
    h_idx = h_idx + 1
  }
  
  #--- data to be replaced
  ecopar_mrep = ecopar_m[h_idx + 1]
  
  #--- header
  ecopar_mhea = ecopar_m[1:h_idx]
  
  l_ec = unique(ecopar_db$ECO_ID)
  
  chlen_des_eco = 18 # 18 is for the DSSAT v4.7
  
  #--- Header of file.ECO
  ecopar_fmt = ecopar_mhea
  
  for (ec in l_ec) {
    #--- ECO parameters
    ec_ecopar = ecopar_db[ecopar_db$ECO_ID == ec, ]
    ec_ecopar_fmt = ecopar_mrep
    
    l_par = unique(ec_ecopar$Rep_ID)
    
    for (p in l_par) {
      replace = sprintf(paste("%.", ec_ecopar$Precision[ec_ecopar$Rep_ID == p], "f", sep =
                                ""),
                        ec_ecopar$Value[ec_ecopar$Rep_ID == p])
      
      #--- check whether new parameter match size
      m_size = ec_ecopar$Par_size[ec_ecopar$Rep_ID == p] - nchar(replace)
      
      if (m_size > 0) {
        #--- add spaces to match size
        while (m_size > 0) {
          replace = paste(" ", replace, sep = "")
          m_size = ec_ecopar$Par_size[ec_ecopar$Rep_ID == p] - nchar(replace)
        }
        
      } else if (m_size < 0) {
        #--- remove precision to match size
        message(
          paste(
            "Warning: Precision of parameter ",
            ec_ecopar$Par_orig[ec_ecopar$Rep_ID == p],
            " reduced to match file.ECO fmt",
            sep = ""
          )
        )
        
        red_p = ec_ecopar$Precision[ec_ecopar$Rep_ID == p]
        while (m_size < 0) {
          red_p = red_p - 1
          
          if (red_p < 0) {
            stop(
              paste(
                "Parameter ",
                ec_ecopar$Par_orig[ec_ecopar$Rep_ID == p],
                " is too high for file.eco fmt (Check Parameter units)",
                sep = ""
              )
            )
          }
          
          replace = sprintf(paste("%.", red_p, "f", sep = ""), ec_ecopar$Value[ec_ecopar$Rep_ID ==
                                                                                 p])
          m_size = calib$Par_size[calib$Rep_ID == p] - nchar(replace)
        }
      }
      
      ec_ecopar_fmt = gsub(p, replace, ec_ecopar_fmt)
      
    }
    
    #--- add ECOID ID
    ec_ecopar_fmt = gsub("<ecoID>", ec_ecopar$ECO_ID[1], ec_ecopar_fmt)
    
    ec_des = as.character(ec_ecopar$ECO_name[1])
    
    #--- Match cha len size
    size_des = chlen_des_eco
    if (nchar(ec_des) < size_des) {
      #--- add spaces to match .ECO fmt
      for (i in 1:(size_des - nchar(ec_des))) {
        ec_des = paste0(ec_des, " ")
      }
    } else if (nchar(ec_des) > size_des) {
      #--- Reduce description to match .ECO fmt
      ec_des = substr(ec_des, 1, size_des)
      message(
        paste0(
          "Description of ecotype ",
          ec_ecopar$ECO_name[1],
          " reduced to ",
          ec_des,
          " to match .ECO fmt"
        )
      )
    }
    
    ec_ecopar_fmt = gsub("<ecoDES>", ec_des, ec_ecopar_fmt)
    
    ecopar_fmt = c(ecopar_fmt, ec_ecopar_fmt)
    
  }
  
  return(ecopar_fmt)
  
}