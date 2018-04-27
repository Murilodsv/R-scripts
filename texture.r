#------------------------------------------------#
#--- Plot texture results on triangles charts ---#
#------------------------------------------------#

#--- Require soiltexture package
#--- source: https://cran.r-project.org/web/packages/soiltexture/index.html

#--- Soil Texture

library(soiltexture)

#--- setwd
setwd("C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/FE_Costumer_Soil_Analysis/formated")

wise   = read.csv("WISE_BR_DSSAT.csv")
wise_f = read.csv("WISE_BR_DSSAT_full.csv")
c_sdb  = read.csv("Soil_Analysis_DB_template.csv")

wise = wise[wise$SLCL>0 & wise$SLSI>0 & wise$SLOC,] #remove -99
wise_tex = data.frame(CLAY = wise$SLCL,
                      SILT = wise$SLSI, 
                      SAND = 100 - (wise$SLCL + wise$SLSI),
                      OC   = wise$SLOC)


text_exp(wise_tex,"wise_text_BRA30.png","WISE - Texture 30cm (BR)")


wise_f = wise_f[wise_f$SLCL>0 & wise_f$SLSI>0 & wise_f$SLOC,] #remove -99
wise_tex_f = data.frame(CLAY = wise_f$SLCL,
                        SILT = wise_f$SLSI, 
                        SAND = 100 - (wise_f$SLCL + wise_f$SLSI),
                        OC   = wise_f$SLOC)

text_exp(wise_tex_f,"wise_text_BRA.png","WISE - Texture (BR)")

c_sdb = c_sdb[!is.na(c_sdb$Clay_g.kg) & !is.na(c_sdb$Silt_g.kg) & !is.na(c_sdb$Sand_g.kg),] #remove -99

c_sdb_tex = data.frame(CLAY = c_sdb$Clay_g.kg/10,
                       SILT = c_sdb$Silt_g.kg/10, 
                       SAND = c_sdb$Sand_g.kg/10)

text_exp(c_sdb_tex,"costumers_text_BRA.png","Costumers - Texture (BR)")


text_exp = function(df,pngname,tit){
  
png(pngname,units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
#--- BR 2013 soil classification
TT.plot(class.sys = "SiBCS13.TT",
        tri.data  = df,
        #z.name = "OC",
        class.p.bg.col = F,
        main = tit,
        cex = 1,
        cex.lab = 1,
        cex.axis= 1,
        lwd = 1,
        lwd.axis = 1,
        arrows.show = T)

dev.off()

png(paste("pp_",pngname,sep=""),units="in",width=12,height=12,pointsize=15,res=300)
par(mfrow=c(1,1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))

#--- Probality contour
geo <- TT.geo.get()
#
kde.res <- TT.kde2d(
  geo = geo,
  tri.data = df
) #
#
TT.contour(
  x = kde.res,
  geo = geo,
  main = "Probability density estimate of the texture data",
  lwd = 2,
  col = "red"
) #
#
TT.plot(class.sys = "SiBCS13.TT",
        tri.data  = df,
        geo       = geo,
        class.p.bg.col = F,
        main      = tit,
        cex       = 1,
        cex.lab   = 1,
        cex.axis  = 1,
        lwd       = 1,
        lwd.axis  = 1,
        arrows.show = T,
        grid.show = F,
        add = T,
        col = "black")


dev.off()

}

#--- BRAZILIAN CLASSIFICATION
leg_BR13 = data.frame(abbr = c("MA","A","S","MeS","MeA","MeAr","ArMe","MAr"),
                      name = c("Muito Argilosa",
                               "Argilosa",
                               "Siltosa",
                               "Média Siltosa",
                               "Média Argilosa",
                               "Média Arenosa",
                               "Arenosa Média",
                               "Muito Arenosa"))


#--- USDA Soil Calissification
TT.plot(class.sys = "USDA.TT",
        class.p.bg.col = TRUE)
leg_USDA = data.frame(abbr = c(
"Cl"    , 		
"SiCl"  , 	
"SaCl"  , 	
"ClLo"  , 	
"SiClLo", 	
"SaClLo",	
"Lo" 		,
"SiLo" 	,
"SaLo" 	,
"Si" 		,
"LoSa" 	,
"Sa" 	 	),
name = c(
  "Clay",
  "Silty clay",
  "Sandy clay",
  "Clay loam",
  "Silty clay loam",
  "Sandy clay loam",
  "Loam",
  "Silty loam",
  "Sandy loam",
  "Silt",
  "Loamy sand",
  "Sand"))
  
colnames(wise)

