#year DOY TMAX TMIN RAIN SRAD RH WIND ETO
par(mfrow=c(2,2)) 

year = 2003

if(year %% 4 == 0){
  dnum = 366  
}else{
  dnum = 365  
}

iday = dnum - length(WTH$V1[WTH$V1==year])
output = data.frame(rep(year,dnum-iday),(iday+1):dnum,WTH$V4[WTH$V1==year],WTH$V5[WTH$V1==year])
colnames(output) = c("year","doy","tmax","tmin")

output

plot(output$doy,output$tmax)
hist(output$tmax)

plot(output$doy,output$tmin)
hist(output$tmin)
