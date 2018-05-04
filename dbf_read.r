library(dplyr)
library(utils)
library(foreign) #Load library for DBF import

#--- Working space soil DB
setwd("C:/Murilo/GIS/Zoning/Brazil_Project/Rename/MUN")

#--- DBF files
dbf_list         = dir(pattern = "*.dbf")
dbf_df           = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
colnames(dbf_df) = c("geo_level","folder_ID","grower","farm","field","date","zoning_op")
dbf_df$zoning_op = gsub(".dbf","",dbf_df$zoning_op)
dbf_df$filename  = dbf_list
dbf_zon          = dbf_df

#--- Colnames to retrieve
#--- Note: shapefiles without these colnames will be ignored
c_dbf = c("GEOCODIGO",
          "NOME",
          "UF",
          "ID_UF",
          "REGIAO",
          "MESOREGIAO",
          "MICROREGIA",
          "ZoneID",
          "ZoneMean",
          "ZoneMin",
          "ZoneMax",
          "PolyMean",
          "ZoneArea",
          "PolyArea")

dbf_bind = dbf_zon
for(i in 1:length(dbf_zon$filename)){

  dbf = read.dbf(file = dbf_zon$filename[i], as.is = F)
  
  tryCatch({
  print(paste("FileNumber: ",i, "       Done: ",
              round(i/length(dbf_zon$filename) *100,digits = 2), "%",sep = ""))
  dbf = dbf[,c_dbf]
  dbf$filename = dbf_zon$filename[i]
  
  if(i == 1){
    rm(dbf_bind)
    dbf_bind = dbf
  }else{
    dbf_bind = rbind(dbf_bind,dbf)
  }
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dbf_list         = dbf_bind$filename
dbf_df           = read.table(text = dbf_list,sep="_",colClasses = "character",fill = T)
colnames(dbf_df) = c("geo_level","folder_ID","grower","farm","field","date","zoning_op")
dbf_df$zoning_op = gsub(".dbf","",dbf_df$zoning_op)
dbf_zon_all      = dbf_df

#--- Index DBFs atributes with DBFs names
dbf_all = data.frame(dbf_bind,dbf_zon_all)
dbf_all$year  = as.numeric(substr(dbf_all$date, 1, 4))
dbf_all$month = as.numeric(substr(dbf_all$date, 5, 6))
dbf_all$day   = as.numeric(substr(dbf_all$date, 7, 8))

#--- Aggregate areas by zone mean area
poly_area = aggregate(PolyArea ~ grower + farm + field + ZoneID, data = dbf_all, mean)

#--- Merge municipalities
poly_mun = merge(poly_area, dbf_all, by=c("grower", "farm", "field", "ZoneID"))

by_MICROREGIA = aggregate(PolyArea.x ~ MICROREGIA, data = poly_mun, sum)
by_MICROREGIA1 = aggregate(PolyArea.y ~ MICROREGIA, data = poly_mun, sum)

plot(by_MICROREGIA$PolyArea.x ~ by_MICROREGIA1$PolyArea.y)
lines(seq(1:1000000000)~)e

z = dbf_all %>% count(MICROREGIA)
z$n = z$n / sum(z$n)

par(mfrow=c(1,1), mar = c(9, 4.0, 0.5, 0.5), oma = c(0, 0, 0, 0))
barplot(z$n * 100, 
        names.arg= z$MICROREGIA,
        ylab = "(%)",
        cex.names=0.75,
        cex.axis = 1.0,
        las = 2,
        ylim = c(0,100))


state.x77



aggregate(x = dbf_all$ZoneID, by = list(Grower = dbf_all$grower,
                                        Farm   = dbf_all$farm,
                                        Zone   = dbf_all$ZoneID<2), length)









help(count)

unique(dbf_all$NOME)
unique(dbf_all$MICROREGIA)


count(dbf_all$ZoneID ~dbf_all$NOME)
sapply(unique(dbf_all$MICROREGIA),length)

count(dbf_all$ZoneID)
help(count)
