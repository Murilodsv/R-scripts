Tair = function(tmax,tmin,jd,at.hour,lat,a=1.86,b=2.20,c=-0.17)
  
{
  #Conditional test to ensure the same length size of input vectors in the function
  
  if(length(tmax)!=length(tmin)| length(tmax)!=length(jd)| length(tmax)!=length(at.hour)| length(tmax)!=length(lat))
    
    stop("Sorry, all the input vectors must have the same length size for this function")
  
  out = cbind(tmax,tmin,jd,at.hour,lat,a,b,c)
  out
  
    
  #Astronomic calculation of earth's declination
  dec = 23.45*sin(((360/365)*(jd-80)*pi/180))
  
  #Photoperiod calculation as function of dec and lat
  N = acos((-tan((lat)*pi/180))*(tan((dec)*pi/180)))*(180/pi)*(2/15)
  
  #Sunrise and Sunset time as function of N
  hns = ((N/2)-12)*-1
  hps = (N/2)+12
  
  #Correction coefficient for tmin ocorrence
  bb = 12-N/2+c
  be = 12+N/2
  bbd.iday = at.hour-bb
  bbe = at.hour-be
  
  #Conditional test to distinguish day and night time  
  if.night = at.hour>hps
  if.day = at.hour<=hps
  bbd.inight1 = rep(0,length(if.night))
  bbd.inight1[if.night] = at.hour[if.night]-hps[if.night]
  bbd.inight2 = rep(0,length(if.day))
  bbd.inight2[if.day] = (24+at.hour[if.day])-hps[if.day]
  
  bbd.inight = bbd.inight2 + bbd.inight1
  bbd.inight2 = (24+be)+at.hour
  ddy = N-c
  tsn = (tmax-tmin)*sin(((pi*ddy)/(N+2*a)))+tmin
  
  #Air temperature for night time
  ti.night = tmin+(tsn-tmin)*exp(-b*bbd.inight/(24-N))
  
  #Air temperature for day time
  ti.day = (tmax-tmin)*sin(((pi*bbd.iday)/(N+2*a)))+tmin
  
  #Conditional test to calculate air temperature at day and night time
  if.night = (at.hour<bb | at.hour>hps)
  if.day = (at.hour>=bb & at.hour<=hps)
  
  ti.air.day = rep(0,length(if.day))
  ti.air.day[if.day] = ti.day[if.day]
  ti.air.night = rep(0,length(if.night))
  ti.air.night[if.night] = ti.night[if.night]
  
  #Union of day and night time air temperatures
  ti.air.all = ti.air.night+ti.air.day
  
  #Union of outputs as data frame format
  ti.air = data.frame(jd,at.hour,lat,ti.air.all)
  ti.air
}





