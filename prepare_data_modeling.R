#----------------------------------------------------#
#---- SVS data preparation for model calibration ----#
#----------------------------------------------------#

#------------- Murilo Vianna (Jun 2018) -------------#
#----------------------------------------------------#


site = "Primavera_do_Leste_MT"
crop = "Soybean"

pdate = as.Date("2017-11-23") #seeding date

wd_sd = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/SVS/Primavera_do_leste/Sensor Data/Edge-of-Field/Sensors Data/WX400243"
setwd(wd_sd)

#--- list of sensor data files
l_sd = dir(wd_sd, pattern = "SD",recursive = T)

hd = read.table(paste(wd_sd,"/",l_sd[1],sep=""), dec = ",", skip = 2, nrows = 1)
hd = sapply(seq(1:length(hd)), function(x) hd[1,x])
hd = c("date", "time", as.character(hd))

for(sd in l_sd){
  
  if(sd == l_sd[1]){
    sdata = read.table(paste(wd_sd,"/",sd,sep=""),
                       dec = ",",
                       skip = 3,
                       fill = T,
                       col.names = hd)
    
  }else{
    #--- append data to a single df
    sdata = rbind(sdata,read.table(paste(wd_sd,"/",sd,sep=""),
                       dec = ",",
                       skip = 3,
                       fill = T,
                       col.names = hd))
    
  }
}

sdata$date = as.Date(sdata$date)

#--- remove all data before planting date
sdata = sdata[sdata$date>=pdate,]

#--- list data
l_data = colnames(sdata)

#--- only data
l_data = l_data[l_data!="date" & l_data!="time"]

for(v in l_data){
  
  exd = sdata[,c("time","date",v)]
  colnames(exd) = c("time","date","v")
  ag_exd = aggregate(v ~ time + date, data = exd, mean)
  
  if(v == l_data[1]){
    ag_sdata = merge(dt,ag_exd, by = c("date","time"))
  }else{
    ag_sdata = merge(ag_sdata,ag_exd, by = c("date","time"), all.x = T) 
  }
  
  colnames(ag_sdata)[length(ag_sdata)] = v
    
}

time = strptime(paste(ag_sdata$date,ag_sdata$time), format = "%Y-%m-%d %H:%M")

png("sdata.png",units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(7,2), mar = c(0.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
for(v in l_data){
  plot(time,
       ag_sdata[,v],
       type = "l",
       xaxt='n',
       ylab = v)
}
dev.off()

write.csv(ag_sdata, file = "sdata.csv", row.names = F)

