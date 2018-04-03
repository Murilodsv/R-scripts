#--- Working space
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/FE_Costumer_Soil_Analysis/formated")

#--- Read DB
dbs = read.csv(file = "Soil_Analysis_DB_template.csv",header = T)

#--- Unique Farms Labels and soil depths
farms = unique(dbs$Farm)
depths= unique(dbs$depth_cm)

#---------------------------------------------------------------------------------

#------------------------#
#--- Overall Analysis ---#
#------------------------#

png("overall_texture.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

#--- Chart settings
par(mfrow=c(3,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- Clay content
hist(dbs$Clay_g.kg/10, 
     col  = "red",
     xlab = "Clay Content [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     cex.lab = 1.5,
     cex.axis= 1.5,
     main =NULL)

#--- Silt content
hist(dbs$Silt_g.kg/10, 
     col  = "grey",
     xlab = "Silt Content [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     cex.lab = 1.5,
     cex.axis= 1.5,
     main =NULL)

#--- Sand content
hist(dbs$Sand_g.kg/10, 
     col  = "yellow",
     xlab = "Sand Content [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     cex.lab = 1.5,
     cex.axis= 1.5,
     main =NULL)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("overall_om.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

hist(dbs$Organic_Matter_g.dm.3, 
     col  = "brown",
     xlab = "Organic Matter [g dm-3] [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     main =NULL)

hist(dbs$Total_Organic_C_g.dm.3, 
     col  = "brown",
     xlab = "Total Organic Matter [g dm-3] [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     main =NULL)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("overall_ph.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

hist(dbs$pH_Water, 
     col  = "white",
     xlab = "pH in Water",
     ylab = "Frequency",
     xlim = c(2,9),
     main =NULL)

hist(dbs$pH_CaCl2, 
     col  = "white",
     xlab = "pH CaCl2",
     ylab = "Frequency",
     xlim = c(2,9),
     main =NULL)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("overall_cec_al.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(2,1), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

hist(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3, 
     col  = "grey",
     xlab = "CEC [mmolc dm-3]",
     ylab = "Frequency",
     xlim = c(0,200),
     
     main =NULL)

hist(dbs$Al_Saturation, 
     col  = "grey",
     xlab = "Al saturation [%]",
     ylab = "Frequency",
     xlim = c(0,100),
     main =NULL)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

#-----------------------#
#--- By costumer site --#
#-----------------------#

png("texture_by_site.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,3), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- Clay content
boxplot(dbs$Clay_g.kg/10~dbs$Farm_acron, col = "red",
        xlab = NULL,
        ylab = "Clay content [%]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2,
        las = 2)

#--- Silt content
boxplot(dbs$Silt_g.kg/10~dbs$Farm_acron, col = "grey",
        xlab = NULL,
        ylab = "Silt content [%]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2,
        las = 2)

#--- Sand content
boxplot(dbs$Sand_g.kg/10~dbs$Farm_acron, col = "yellow",
        xlab = NULL,
        ylab = "Sand content [%]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2,
        las = 2)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("om_by_site.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- Organic Matter
boxplot(dbs$Organic_Matter_g.dm.3~dbs$Farm_acron, col = "brown",
        xlab = NULL,
        ylab = "Organic Matter [g dm-3]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,100),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

#--- Total Organic Matter
boxplot(dbs$Total_Organic_C_g.dm.3~dbs$Farm_acron, col = "brown",
        xlab = NULL,
        ylab = "Total Organic Matter [g dm-3]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,100),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("ph_by_site.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- ph CaCl2
boxplot(dbs$pH_CaCl2~dbs$Farm_acron, col = "white",
        xlab = NULL,
        ylab = "pH CaCl2",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(2,9),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

#--- ph in Water
boxplot(dbs$pH_Water~dbs$Farm_acron, col = "white",
        xlab = NULL,
        ylab = "pH in Water",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(2,9),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("cec_al_by_site.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- cec
boxplot(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3~dbs$Farm_acron, col = "grey",
        xlab = NULL,
        ylab = "CEC [mmolc dm-3]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(0,200),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

#--- Al saturation
boxplot(dbs$pH_Water~dbs$Farm_acron, col = "grey",
        xlab = NULL,
        ylab = "Al saturation [%]",
        xlim = c(0.5,length(farms)+0.5),
        ylim = c(4,8),
        cex.lab = 1.0,
        cex.axis= 1.0,
        las = 2)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

#-----------------------#
#---     By zones    ---#
#-----------------------#

png("textures_by_zone_5z.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,3), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- Clay content ~ Zones (only 5 zones)
boxplot(dbs$Clay_g.kg[dbs$nzones==5]/10~dbs$Zone.ID[dbs$nzones==5], col = "red",
        xlab = NULL,
        ylab = "Clay Content [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2)
        

lines(tapply(dbs$Clay_g.kg[!is.na(dbs$Clay_g.kg) & dbs$nzones ==5]/10,
             dbs$Zone.ID[!is.na(dbs$Clay_g.kg)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)

#--- Silt content ~ Zones (only 5 zones)
boxplot(dbs$Silt_g.kg[dbs$nzones==5]/10~dbs$Zone.ID[dbs$nzones==5], col = "grey",
        xlab = "Zones",
        ylab = "Silt Content [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2)

lines(tapply(dbs$Silt_g.kg[!is.na(dbs$Silt_g.kg) & dbs$nzones ==5]/10,
             dbs$Zone.ID[!is.na(dbs$Silt_g.kg)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)

#--- Sand content ~ Zones (only 5 zones)
boxplot(dbs$Sand_g.kg[dbs$nzones==5]/10~dbs$Zone.ID[dbs$nzones==5], col = "yellow",
        xlab = NULL,
        ylab = "Sand Content [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.5,
        cex.axis= 1.2)

lines(tapply(dbs$Sand_g.kg[!is.na(dbs$Sand_g.kg) & dbs$nzones ==5]/10,
             dbs$Zone.ID[!is.na(dbs$Sand_g.kg)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("om_by_zone_5z.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- Organic Matter ~ Zones (only 5 zones)
boxplot(dbs$Organic_Matter_g.dm.3[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "brown",
        xlab = "Zones",
        ylab = "Organic Matter [g dm-3]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.2,
        cex.axis= 1.2)

lines(tapply(dbs$Organic_Matter_g.dm.3[!is.na(dbs$Organic_Matter_g.dm.3) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$Organic_Matter_g.dm.3)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)


#--- Total Organic Matter ~ Zones (only 5 zones)
boxplot(dbs$Total_Organic_C_g.dm.3[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "brown",
        xlab = "Zones",
        ylab = "Total Organic Matter [g dm-3]",
        xlim = c(0.5,5.5),
        ylim = c(0,100),
        cex.lab = 1.2,
        cex.axis= 1.2)

lines(tapply(dbs$Total_Organic_C_g.dm.3[!is.na(dbs$Total_Organic_C_g.dm.3) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$Total_Organic_C_g.dm.3)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)

dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("ph_by_zone_5z.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- ph CaCl2 ~ Zones (only 5 zones)
boxplot(dbs$pH_CaCl2[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "white",
        xlab = "Zones",
        ylab = "pH CaCl2",
        xlim = c(0.5,5.5),
        ylim = c(2,9))

lines(tapply(dbs$pH_CaCl2[!is.na(dbs$pH_CaCl2) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$pH_CaCl2)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)


#--- ph Water ~ Zones (only 5 zones)
boxplot(dbs$pH_Water[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "white",
        xlab = "Zones",
        ylab = "pH Water",
        xlim = c(0.5,5.5),
        ylim = c(2,9))

lines(tapply(dbs$pH_Water[!is.na(dbs$pH_Water) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$pH_Water)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)


dev.off() # end of chart exportation

#---------------------------------------------------------------------------------

png("cec_al_by_zone_5z.png",
    units="in", 
    width=12, 
    height=12, 
    pointsize=15, 
    res=300)

par(mfrow=c(1,2), mar = c(4.5, 4.5, 0.5, 0.5), oma = c(0, 0, 0, 0))

#--- cec ~ Zones (only 5 zones)
boxplot(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "grey",
        xlab = "Zones",
        ylab = "CEC [mmolc dm-3]",
        xlim = c(0.5,5.5),
        ylim = c(0,200))

lines(tapply(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3[!is.na(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$Cation_Exchange_Capacity_C.E.C_mmolc.dm.3)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)

#---Al saturation ~ Zones (only 5 zones)
boxplot(dbs$Al_Saturation[dbs$nzones==5]~dbs$Zone.ID[dbs$nzones==5], col = "grey",
        xlab = "Zones",
        ylab = "Al saturation [%]",
        xlim = c(0.5,5.5),
        ylim = c(0,50))

lines(tapply(dbs$Al_Saturation[!is.na(dbs$Al_Saturation) & dbs$nzones ==5],
             dbs$Zone.ID[!is.na(dbs$Al_Saturation)& dbs$nzones == 5],
             mean),
      col = "black",lty = 3,lwd = 3)


dev.off() # end of chart exportation

