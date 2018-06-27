#----------------------------------------------------#
#---- SVS data preparation for model calibration ----#
#----------------------------------------------------#

#------------- Murilo Vianna (Jun 2018) -------------#
#----------------------------------------------------#


site = "Primavera_do_Leste_MT"
crop = "Soybean"

wd = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/SVS"

pdate = as.Date("2017-11-23") #seeding date

setwd(wd)

wd_sdin   = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/SVS/Primavera_do_leste/Sensor Data/In-Field-Stations/In-Field-Station-Zone#/Sensors Reading Data/WX400244"
wd_sdedge = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/SVS/Primavera_do_leste/Sensor Data/Edge-of-Field/Sensors Data/WX400243"

wd_sd = wd_sdedge

#--- list of sensor data files
l_sd = dir(wd_sd, pattern = "SD",recursive = T)

hd = read.table(paste(wd_sd,"/",l_sd[1],sep=""), dec = ",", skip = 2, nrows = 1)
hd = sapply(seq(1:length(hd)), function(x) hd[1,x])
hd = c("date", "time", as.character(hd))

s_nm = as.character(read.table(paste(wd_sd,"/",l_sd[1],sep=""), dec = ",", nrows = 1)[1,1])

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

#--- unique dates and times
dt = unique(sdata[,c("date","time")])

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

nchart1 = length(l_data)
nchart2 = 1
if(nchart1 > 7){
  nchart1 = nchart1 / 2
  nchart2 = 2
  
  if(nchart1 %% nchart2 >0){nchart1 = nchart1 + 1}
}

png(paste(s_nm,"_hourly_data.png",sep=""),units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(nchart1,nchart2), mar = c(0.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
for(v in l_data){
  plot(time,
       ag_sdata[,v],
       type = "l",
       xaxt='n',
       ylab = v)
}
dev.off()

write.csv(ag_sdata, file = paste(s_nm,"_hourly_data.csv",sep=""), row.names = F)

#--- daily data

ag_sdata_daily_avg = aggregate.data.frame(ag_sdata[,c("date",l_data)], by = list(ag_sdata$date), mean)
ag_sdata_daily_max = aggregate.data.frame(ag_sdata[,c("date",l_data)], by = list(ag_sdata$date), max)
ag_sdata_daily_min = aggregate.data.frame(ag_sdata[,c("date",l_data)], by = list(ag_sdata$date), min)
daily_rnf          = aggregate(RNF ~ date, data = ag_sdata, sum)
colnames(daily_rnf)= c("date","TRNF")

ag_sdata_daily = ag_sdata_daily_avg
ag_sdata_daily$TMAX = ag_sdata_daily_max$TMP
ag_sdata_daily$TMIN = ag_sdata_daily_min$TMP
ag_sdata_daily = merge(ag_sdata_daily,daily_rnf,by = ("date"),all.x = T)

l_data = c(l_data,"TMAX","TMIN","TRNF")

ag_sdata_daily = ag_sdata_daily[,c("date",l_data)]

nchart1 = length(l_data)
nchart2 = 1
if(nchart1 > 7){
  nchart1 = nchart1 / 2
  nchart2 = 2
  
  if(nchart1 %% nchart2 >0){nchart1 = nchart1 + 1}
}

png(paste(s_nm,"_daily_data.png",sep=""),units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(nchart1,nchart2), mar = c(0.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))
for(v in l_data){
  if(v == l_data[length(l_data)]){
    plot(ag_sdata_daily[,v]~
           ag_sdata_daily[,"date"],
         type = "l",
         #xaxt='n',
         ylab = v)
  }else{
  plot(ag_sdata_daily[,v]~
         ag_sdata_daily[,"date"],
       type = "l",
       xaxt='n',
       ylab = v)
  }
}
dev.off()

write.csv(ag_sdata_daily, file = paste(s_nm,"_daily_data.csv",sep=""), row.names = F)


