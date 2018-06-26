#-------------------------------------------#
#------ Simulate future sugarcane G&D ------#
#-------------------------------------------#

#---Three methodologies:
#------ 1) Alter the future wth data with all past wth data available and simulate DSSAT/CANEGRO
#------ 2) Simulate all years without modifing wth and pick the best match between the current year simulations and past years
#------ 3) Uses best fit wth data between rainfall normal and all other wth

#--- Murilo Vianna (Jun-2018)

wd            ="C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/Modeling/Future_Sim"

xfile         = "FUWE0001.SCX"
xfile_orig    = "FUWE0002.SCX"
xfile_unmod   = "FUWE0003.SCX"
xfile_bm      = "FUWE0004.SCX"
ds_v          = 47
crop          = "Sugarcane"

today_dap     = 150
wth_orig      = "SPPI"
wth_nm        = "PIFW"
wth_bm        = "PIBM" #--- best match (solomon)
sc_out        = "su.fmd"#"smfmd"
nmonths       = 12 #--- size on normal year
planting_doy  = 225 # from xfile
planting_year = 2008# from xfile

#--- load functions
source(paste(wd,"/Future_sim_f.R",sep=""))

#--- list original wth files
l_wth = dir("C:/DSSAT47/Weather")

lwth_df = data.frame(wthfile = l_wth[substr(l_wth,1,4)=="SPPI"],
                     year    = as.numeric(paste("20",substr(l_wth[substr(l_wth,1,4)=="SPPI"],5,6), sep="")))

lwth_df$year[lwth_df$year> format(Sys.Date(),"%Y")] = lwth_df$year[lwth_df$year> format(Sys.Date(),"%Y")] - 100

lwth_df$wthfile_new = gsub(wth_orig,wth_nm,lwth_df$wthfile)
lwth_df$wthfile_bm  = gsub(wth_orig,wth_bm,lwth_df$wthfile)

#--- sort by year
lwth_df = lwth_df[order(lwth_df$year),]

#--- wth file head
wth_head = readLines(paste("C:/DSSAT47/Weather/",lwth_df$wthfile[1],sep=""),n = 5)

wth_head = gsub(wth_orig,wth_nm, wth_head)

#--- read original wth files
for(fwth in lwth_df$wthfile){
  
  wth = read.table(paste("C:/DSSAT47/Weather/",fwth,sep=""),skip = 5)
  wth = wth[,1:5]
  colnames(wth) = c("DATE","SRAD","TMAX","TMIN","RAIN")
  
  wth$year    = lwth_df$year[lwth_df$wthfile==fwth]
  
  if(fwth == lwth_df$wthfile[1]){
    
    wth_df = data.frame(wth)
    
  }else{
    wth_df = rbind(wth_df,wth)
    
  }
  
}

#--- compute doy from date
wth_df$doy = wth_df$DATE

if(length(wth_df$doy[wth_df$DATE > 1000 & wth_df$DATE < 10000])>0){
  wth_df$doy[wth_df$DATE > 1000 & wth_df$DATE < 10000] = as.numeric(substr(wth_df$DATE[wth_df$DATE > 1000 & wth_df$DATE < 10000],2,4))
}

if(length(wth_df$doy[wth_df$DATE >= 10000])> 0){
wth_df$doy[wth_df$DATE >= 10000] = as.numeric(substr(wth_df$DATE[wth_df$DATE >= 10000],3,5))
}

#--- Re-build DSSAT DATE format (YYDOY)
wth_df$DATE_FMT = paste(substr(wth_df$year,3,4),sprintf("%003.0f",wth_df$doy),sep = "")

#--- Separate this year simulations wth data
wth_cy = wth_df[wth_df$year>planting_year | (wth_df$year==planting_year & wth_df$doy>= planting_doy),][1:today_dap,]

#--- replace in all time-series
for(yr in unique(wth_df$year)){
  
  wth_df_yr = wth_df[wth_df$year==yr,]
  
  m_doy = data.frame(doy = wth_df_yr$doy)
  m_doy = merge(m_doy,wth_cy,by = "doy")
  
  wth_df_yr[wth_df_yr$doy %in% m_doy$doy,c("DATE","SRAD","TMAX","TMIN","RAIN")] = m_doy[,c("DATE","SRAD","TMAX","TMIN","RAIN")]
  
  out_wth = data.frame(date = wth_df_yr$DATE_FMT,
                       srad = sprintf("%5.1f",wth_df_yr$SRAD),
                       tmax = sprintf("%5.1f",wth_df_yr$TMAX),
                       tmin = sprintf("%5.1f",wth_df_yr$TMIN),
                       rain = sprintf("%5.1f",wth_df_yr$RAIN))
  
  
  
  write(wth_head, file = paste("C:/DSSAT47/Weather/",lwth_df$wthfile_new[lwth_df$year==yr],sep = ""), append = F, sep = "")
  
  write.table(out_wth, file =  paste("C:/DSSAT47/Weather/",lwth_df$wthfile_new[lwth_df$year==yr],sep = ""),
              append = T,
              row.names = F,
              col.names = F,
              quote = F)
  
  if(yr == unique(wth_df$year)[1]){
    deb_out = wth_df_yr
  }else{
    deb_out = rbind(deb_out,wth_df_yr)
  }
  
}

#--------------------------------------------------------------
#--- Run for all modified wth

#--- prepare batch call
bfile = readLines(paste(wd,"/DSSBatch_Master.v47",sep=""))
bfile[4] = gsub("<calib_xfile>",xfile,bfile[4])

#--- write in Crop folder
write(bfile,file = paste("C:/DSSAT",ds_v,"/",crop,"/","DSSBatch.v",ds_v,sep = ""))

#--- set wd to run
setwd(paste("C:/DSSAT",ds_v,"/",crop,"/",sep = ""))

#--- Call DSSAT047.exe and run X files list within DSSBatch.v47
system(paste("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste("DSSBatch.v",ds_v,sep=""),sep=""))

#--- Read simulated data
plant_lines = readLines("PlantGro.OUT")

setwd(wd)

#--- PlantGro Head
pgro_head = read.csv("PlantGro_Head.csv")

#--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
            file = "PlantGro_numeric.OUT",
            row.names = F, col.names = F,quote = F)
plant = read.table(file = "PlantGro_numeric.OUT")                   #Read numeric lines as data.frame

#--- Columns name accordingly to DSSAT output name
colnames(plant) = pgro_head$R_head

#--- Read Runs (last year series)
run = trimws(substr(plant_lines[substr(plant_lines,2,4)=="RUN"],6, 12))

#--- Index outputs with treatments
plant$run = ""  
j = 0
for(i in 1:length(plant$dap)){
  
  if(plant$dap[i] == 0){j = j +1}
  
  plant$run[i] = run[j]
  
}

#--- separate current year and past years simulations
plantgro_cy  = plant[plant$run=="30",]
plantgro_s   = plant[plant$run!="30",]

#--- plot until today data~dap
par(mfrow=c(1,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

plot(plantgro_cy[plantgro_cy$dap<=today_dap,sc_out]~plantgro_cy$dap[plantgro_cy$dap<=today_dap],
     xlim = c(0,max(plant$dap)),
     ylim = c(0,max(plant[,sc_out])),
     lty  = 1,
     col  = "red",
     lwd  = "2",
     type = "l",
     ylab = sc_out,
     xlab = "DAP")

#--- plot predictions (past series data~dap)

for(run in unique(plantgro_s$run)){
  
  lines(plantgro_s[plantgro_s$run==run & plantgro_s$dap>today_dap,sc_out]~
          plantgro_s$dap[plantgro_s$run==run & plantgro_s$dap>today_dap],
        col = "grey")
  
}

bp = boxplot(plantgro_s[plantgro_s$dap>today_dap,sc_out]~
               plantgro_s$dap[plantgro_s$dap>today_dap],
             plot = F)

fut_pred = data.frame(bp1 = bp$stats[1,],
                      bp2 = bp$stats[2,],
                      bp3 = bp$stats[3,],
                      bp4 = bp$stats[4,],
                      bp5 = bp$stats[5,],
                      dap = as.numeric(bp$names))

#--- plot bp limits
lines(fut_pred$bp3~fut_pred$dap, lty = 3)
lines(fut_pred$bp1~fut_pred$dap, lty = 1)
lines(fut_pred$bp5~fut_pred$dap, lty = 1)

#--------------------------------------------------------------
#--- Run for "observed" with wth omited (dap>today_dap)


#--- prepare batch call
bfile = readLines(paste(wd,"/DSSBatch_Master.v47",sep=""))
bfile[4] = gsub("<calib_xfile>",xfile_orig,bfile[4])

#--- write in Crop folder
write(bfile,file = paste("C:/DSSAT",ds_v,"/",crop,"/","DSSBatch.v",ds_v,sep = ""))

#--- set wd to run
setwd(paste("C:/DSSAT",ds_v,"/",crop,"/",sep = ""))

#--- Call DSSAT047.exe and run X files list within DSSBatch.v47
system(paste("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste("DSSBatch.v",ds_v,sep=""),sep=""))

#--- Read simulated data
plant_lines = readLines("PlantGro.OUT")

setwd(wd)

#--- PlantGro Head
pgro_head = read.csv("PlantGro_Head.csv")

#--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
            file = "PlantGro_numeric_orig.OUT",
            row.names = F, col.names = F,quote = F)
plant_orig = read.table(file = "PlantGro_numeric_orig.OUT")                   #Read numeric lines as data.frame

#--- Columns name accordingly to DSSAT output name
colnames(plant_orig) = pgro_head$R_head

#--- plot future data (observed data)
lines(plant_orig[plant_orig$dap>today_dap,sc_out]~plant_orig$dap[plant_orig$dap>today_dap],col = "red",lty = 3)

#--------------------------------------------------------------
#--- Run for all year without modifying wth

#--- prepare batch call
bfile = readLines(paste(wd,"/DSSBatch_Master.v47",sep=""))
bfile[4] = gsub("<calib_xfile>",xfile_unmod,bfile[4])

#--- write in Crop folder
write(bfile,file = paste("C:/DSSAT",ds_v,"/",crop,"/","DSSBatch.v",ds_v,sep = ""))

#--- set wd to run
setwd(paste("C:/DSSAT",ds_v,"/",crop,"/",sep = ""))

#--- Call DSSAT047.exe and run X files list within DSSBatch.v47
system(paste("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste("DSSBatch.v",ds_v,sep=""),sep=""))

#--- Read simulated data
plant_lines = readLines("PlantGro.OUT")

setwd(wd)

#--- PlantGro Head
pgro_head = read.csv("PlantGro_Head.csv")

#--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
            file = "PlantGro_numeric_unmod.OUT",
            row.names = F, col.names = F,quote = F)
plant_unmod = read.table(file = "PlantGro_numeric_unmod.OUT")                   #Read numeric lines as data.frame

#--- Columns name accordingly to DSSAT output name
colnames(plant_unmod) = pgro_head$R_head


#--- Read Runs (last year series)
run = trimws(substr(plant_lines[substr(plant_lines,2,4)=="RUN"],6, 12))

#--- Index outputs with treatments
plant_unmod$run = ""  
j = 0
for(i in 1:length(plant_unmod$dap)){
  
  if(plant_unmod$dap[i] == 0){j = j +1}
  
  plant_unmod$run[i] = run[j]
  
}
plant_unmod

obs = plant_orig[plant_orig$dap<=today_dap,sc_out]
dchart = F

if(dchart){
par(mfrow=c(5,6), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
}

for(run in unique(plant_unmod$run)){

  sim = plant_unmod[plant_unmod$dap<=today_dap & plant_unmod$run==run,sc_out]
  
  vnam = paste(run)
  
  if(run == unique(plant_unmod$run)[1]){
    
    perf = mperf(sim,obs,vnam,dchart,"rmse")
  }else{
    perf = rbind(perf,mperf(sim,obs,vnam,dchart,"rmse"))
  }
  
}

#--- sort by outidx
perf = perf[order(perf$rmse),]

#--- plot best match of past simulations
lines(plant_unmod[plant_unmod$run==perf$vnam[2] & plant_unmod$dap > today_dap,sc_out]~
      plant_unmod[plant_unmod$run==perf$vnam[2] & plant_unmod$dap > today_dap,"dap"],
      col = "blue")

#--------------------------------------------------------------
#--- Run for the best match between normal rainfall year and all years

#--- use the best fit of normal rain and previous years (proposed by solomon)
wth_df$date_greg = as.Date(paste(wth_df$year,"-01-01",sep="")) + (wth_df$doy - 1)
wth_df$month = format(as.Date(wth_df$date_greg), "%m")

#--- monthly means
month_wth = data.frame(month = unique(wth_df[,c("month","year")])[1],
                       year  = unique(wth_df[,c("month","year")])[2],
                       tmax  = aggregate(TMAX ~ month + year, data = wth_df, mean)$TMAX,
                       tmin  = aggregate(TMIN ~ month + year, data = wth_df, mean)$TMIN,
                       srad  = aggregate(SRAD ~ month + year, data = wth_df, mean)$SRAD,
                       rain  = aggregate(RAIN ~ month + year, data = wth_df, sum)$RAIN)

#--- normal
normal_wth = data.frame(month = seq(1,12),
                        tmax  = aggregate(TMAX ~ month, data = wth_df, mean)$TMAX,
                        tmin  = aggregate(TMIN ~ month, data = wth_df, mean)$TMIN,
                        srad  = aggregate(SRAD ~ month, data = wth_df, mean)$SRAD,
                        rain  = aggregate(rain ~ month, data = month_wth, mean)$rain)

starting_month = format(as.Date(paste(planting_year,"-01-01",sep="")) + (planting_doy - 1),"%m")

#--- tag number of "runs"
r = 0
rsim = 0
v_run = rep(0,length(month_wth$month[month_wth$year<planting_year]))
v_mon = rep(0,length(month_wth$month[month_wth$year<planting_year]))

for(m in 1:length(month_wth$month)){
  
  if(m >= as.numeric(starting_month)){
    if(m == as.numeric(starting_month)){rsim = 1}
    r = r + 1
    if(r > nmonths){
        r = 1
        rsim = rsim + 1
      }
  }
  v_run[m] = rsim
  v_mon[m] = r
}

#--- index in monthly basis
v_nor = rep(0, length(normal_wth$month))
v_nor[normal_wth$month[normal_wth$month==as.numeric(starting_month)]] = 1
v_nor[normal_wth$month[normal_wth$month>as.numeric(starting_month)]]  = normal_wth$month[normal_wth$month>as.numeric(starting_month)] - as.numeric(starting_month)+1
v_nor[normal_wth$month[normal_wth$month<as.numeric(starting_month)]]  = normal_wth$month[normal_wth$month<as.numeric(starting_month)] - as.numeric(starting_month)+1 + nmonths

#--- use rain in the comparison
wth_dcomp = "rain"

#--- set normal as "observed"
obs = data.frame(mrun = v_nor,
                 rain = normal_wth[,wth_dcomp])

#--- add indexes
month_wth$v_run = v_run
month_wth$mrun  = v_mon

#--- compute rmse of (normal x each year)
for(run in unique(v_run[v_run>0])){
  
  perf_data_df = obs
  perf_data_df = merge(perf_data_df,month_wth[month_wth$v_run==run,], by = "mrun")
  if(run ==unique(v_run[v_run>0])[1]){
    perf_wth = mperf(perf_data_df$rain.y, perf_data_df$rain.x,run,F,"rmse")
  }else{
    perf_wth = rbind(perf_wth,mperf(perf_data_df$rain.y, perf_data_df$rain.x,run,F,"rmse"))
  }
  
}

#--- sort by outidx
perf_wth = perf_wth[order(perf_wth$rmse),]

if(dchart){

plot(month_wth$rain[month_wth$v_run==perf_wth$vnam[1]]~
       month_wth$mrun[month_wth$v_run==perf_wth$vnam[1]],
     type = "b")
points(obs$rain~obs$mrun, col = "red",pch = 8)

}

best_wth = month_wth[month_wth$v_run==perf_wth$vnam[1],]

#--- wth file head
wth_head = readLines(paste("C:/DSSAT47/Weather/",lwth_df$wthfile[1],sep=""),n = 5)

wth_head = gsub(wth_orig,wth_bm, wth_head)

#--- replace in all time-series
for(yr in unique(best_wth$year)){
  
  wth_df_yr = wth_df[wth_df$year==yr,]
  
  out_wth = data.frame(date = wth_df_yr$DATE_FMT,
                       srad = sprintf("%5.1f",wth_df_yr$SRAD),
                       tmax = sprintf("%5.1f",wth_df_yr$TMAX),
                       tmin = sprintf("%5.1f",wth_df_yr$TMIN),
                       rain = sprintf("%5.1f",wth_df_yr$RAIN))
  
  write(wth_head, file = paste("C:/DSSAT47/Weather/",lwth_df$wthfile_bm[lwth_df$year==yr],sep = ""), append = F, sep = "")
  
  write.table(out_wth, file =  paste("C:/DSSAT47/Weather/",lwth_df$wthfile_bm[lwth_df$year==yr],sep = ""),
              append = T,
              row.names = F,
              col.names = F,
              quote = F)
  
}


xfile_bm_full  = readLines(paste(wd,"/FUWE0001_master.SCX",sep=""))



xfile_bm_full = gsub("<pyr>",substr(best_wth$year[1],3,4) ,xfile_bm_full)
xfile_bm_full = gsub("<hyr>",substr(best_wth$year[length(best_wth$year)],3,4)  ,xfile_bm_full)
xfile_bm_full = gsub("<station>",wth_bm,xfile_bm_full)

write(xfile_bm_full, paste("C:/DSSAT",ds_v,"/",crop,"/",xfile_bm,sep = ""))

#--- prepare batch call
bfile = readLines(paste(wd,"/DSSBatch_Master.v47",sep=""))
bfile[4] = gsub("<calib_xfile>",xfile_bm,bfile[4])

#--- write in Crop folder
write(bfile,file = paste("C:/DSSAT",ds_v,"/",crop,"/","DSSBatch.v",ds_v,sep = ""))

#--- set wd to run
setwd(paste("C:/DSSAT",ds_v,"/",crop,"/",sep = ""))

#--- Call DSSAT047.exe and run X files list within DSSBatch.v47
system(paste("C:/DSSAT",ds_v,"/DSCSM0",ds_v,".EXE SCCAN0",ds_v," B ",paste("DSSBatch.v",ds_v,sep=""),sep=""))

#--- Read simulated data
plant_lines = readLines("PlantGro.OUT")

setwd(wd)

#--- PlantGro Head
pgro_head = read.csv("PlantGro_Head.csv")

#--- Note: writing file is required to speed up! (for some reason is faster than reading directly from plant_lines variable)
write.table(plant_lines[substr(plant_lines,2,3)=="19" | substr(plant_lines,2,3)=="20"],
            file = "PlantGro_numeric_bm.OUT",
            row.names = F, col.names = F,quote = F)
plant_bm = read.table(file = "PlantGro_numeric_bm.OUT")                   #Read numeric lines as data.frame

#--- Columns name accordingly to DSSAT output name
colnames(plant_bm) = pgro_head$R_head

lines(plant_bm[plant_bm$dap>today_dap,sc_out]~
        plant_bm[plant_bm$dap>today_dap,"dap"],col = "green")





