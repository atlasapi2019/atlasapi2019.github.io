
setwd("D:/Geografía/atlasapi2019.github.io")

library(dplyr)
library(highcharter)
library(ggplot2)


mapdata <- get_data_from_map(download_map_data("countries/mx/mx-all"))
mapdata$name[11] <- "Ciudad de México"
mapdata$name[9] <- "Estado de México"
desord <- mapdata$name[-1]
estadist <- read.csv("tabs/estadisticas.csv", head= T)
estadist[,1] <- as.character(estadist[,1])
estadist$Entidad[24] <- "San Luis Potosí"
estadist2 <- estadist[, c(1,2)]
mapdata_estados <- as.data.frame(mapdata$name[-1])
names(mapdata_estados) <- "entidad"
estadist2$pos <- 1:dim(estadist2)[1]
posit <- c(rep(0,length(desord)))

for (i in 1:dim(estadist2)[1]) {
  for(j in 1:length(desord)) {
    ifelse(desord[i] == estadist2$Entidad[j], posit[i] <- estadist2$pos[j], posit[i] <- posit[i]*1)
  }
}
napic <- c(NA, estadist2$APICULTORES[posit])

apic <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = napic)

apicult <- hcmap("countries/mx/mx-all", data = apic, value = "napic", 
                 joinBy = c("hc-a2", "code"), name = "Apicultores",
                 dataLabels = list(enabled = TRUE, format = '{point.name}'),
                 borderColor = "#0EAD82", borderWidth = 0.1) 
apicult




asoc <- read.csv("tabs/asoc_ent.csv", head=T)
hchart(asoc, "bar", hcaes(x = asoc[,1], y = asoc[,2], group = asoc[,3])) %>% hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = "Asociaciones Apícolas y Otras Organizaciones, por entidad federativa, 2017") %>% 
  hc_plotOptions(bar = list(size = 300)) %>% 
  hc_xAxis(title = "") %>% hc_yAxis(title = "")



acop <- read.csv("tabs/estadisticas.csv", head= T)
acop <- acop[acop$ACOPIADORES_DE_MIEL > 5,c(1,5)]
o <- order(acop[,2], decreasing= T)
acop <- acop[o,]

hchart(acop, "column", hcaes(x = acop[,1], y = acop[,2])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Entidades con mayor número de unidades económicas acopiadoras de miel, 2017") %>% 
  hc_yAxis(title = "") %>% hc_xAxis(title = "") %>% 
  hc_plotOptions(column = list(size = 300)) %>%
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "Acopiadores", sep = " ")) 




expor <- read.csv("tabs/estadisticas.csv", head= T)
expor <- expor[expor$EXPORTADORES > 0,c(1,6)]
o2 <- order(expor[,2], decreasing= T)
expor <- expor[o2,]

hchart(expor, "column", hcaes(x = expor[,1], y = expor[,2])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Entidades con unidades económicas exportadoras de miel, 2017") %>% 
  hc_yAxis(title = "") %>% hc_xAxis(title = "") %>% 
  hc_plotOptions(column = list(size = 100)) %>%
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "Exportadores", sep = " " ))



criad <- read.csv("tabs/estadisticas.csv")
criad <- criad[!(criad[,7]== 0 & criad[,8] == 0 & criad[,9] == 0), c(1,7,9,8)]
criad2 <- as.data.frame(rep(criad$Entidad,3))
criad2$datos <- c(criad[,2], criad[,3], criad[,4])
criad2$producto <- c(rep("Criadores de Abejas Reina",length(criad$Entidad)), rep("Criadores de Abejas Reina Progenitoras",length(criad$Entidad)), 
                      rep("Productores de Núcleos o Paquetes de Abejas",length(criad$Entidad)))

hchart(criad2, "bar", hcaes(x = criad2[,1], y = criad2[,2], group = criad2[,3])) %>% hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = "Criadores de abejas reina y Núcleos, por entidad federativa, 2017") %>% 
  hc_plotOptions(bar = list(size = 300)) %>% 
  hc_xAxis(title = "") %>% hc_yAxis(title = "", minTickInterval=1)




cert <- read.csv("tabs/estadisticas.csv")
cert <- cert[!(cert[,10] == 0 & cert[,11] == 0 & cert[,12] == 0), c(1,10,11,12,18)]
cert2 <- as.data.frame(rep(cert$Entidad,4))
cert2$datos <- c(cert[,2], cert[,3], cert[,4], cert[,5])
cert2$producto <- c(rep("Abejas Reina Producidas Certificadas",length(cert$Entidad)), rep("Abejas Reina Producidas No Certificadas",length(cert$Entidad)), 
                     rep("Núcleos Producidos",length(cert$Entidad)), rep("Colmenas para polinización",length(cert$Entidad)))

hchart(cert2, "column", hcaes(x = cert2[,1], y = cert2[,2], group = cert2[,3])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Abejas reina, núcleos y colmenas para polinización producidas por entidad federativa, 2017") %>% 
  hc_yAxis(title = "", max = 90000) %>% hc_xAxis(title = "") %>% 
  hc_plotOptions(column = list(size = 300)) %>% hc_colors(colors = c("orange", "green", "violet", "brown")) %>%
  hc_tooltip(enabled = T, pointFormat = "<b>{point.y:.0f} </b>")




cera <- read.csv("tabs/estadisticas.csv", head= T)
cera2 <- cera[cera$CERA_kg > 2000,c(1,14)]
o <- order(cera2[,2], decreasing= T)
cera3 <- cera2[o,]

hchart(cera3, "column", hcaes(x = cera3[,1], y = cera3[,2])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Producción de cera por entidad federativa, 2017") %>% 
  hc_yAxis(title = "") %>% hc_xAxis(title = "") %>% 
  hc_plotOptions(column = list(size = 300)) %>%
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "kg", sep = " ")) 




otr_prod <- read.csv("tabs/estadisticas.csv")
otr_prod <- otr_prod[!(otr_prod[,13] == 0 & otr_prod[,15] == 0 & otr_prod[,16] == 0 & otr_prod[,17] == 0), c(1,13,15:17)]
otr_prod2 <- as.data.frame(rep(otr_prod$Entidad,4))
otr_prod2$datos <- c(otr_prod[,5], otr_prod[,2], otr_prod[,3],otr_prod[,4])
otr_prod2$productos <- c(rep("Propóleos",length(otr_prod$Entidad)), rep("Miel Orgánica",length(otr_prod$Entidad)), 
                    rep("Jalea Real",length(otr_prod$Entidad)), rep("Polen",length(otr_prod$Entidad)))
otr_prod2$num <- rep(1:length(otr_prod$Entidad),4)
otr_prod2 <- otr_prod2[otr_prod2$datos > 0,]
otr_prod2 <- otr_prod2[order(otr_prod2[,4]),]

hchart(otr_prod2, "scatter", hcaes(x = otr_prod2[,1], y = otr_prod2[,2], group = otr_prod2[,3])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Producción de Miel orgánica y otros productos apícolas, por entidad federativa, 2017") %>% 
  hc_yAxis(title = "") %>% hc_xAxis(title = "") %>% hc_colors(colors = c("orange", "green", "violet", "brown")) %>% 
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "kg", sep = " " )) 



terr <- read.csv("tabs/superficie_apicola.csv")
terr2 <- as.data.frame(rep(terr$entidad,3))
terr2$datos <- c(terr[,2], terr[,4], terr[,6])
terr2$tipo <- c(rep("Terrenos de ganadería ejidal",length(terr$entidad)), 
                     rep("Terrenos de ganadería comunal",length(terr$entidad)), rep("Terrenos de ganadería privada",length(terr$entidad)))
terr2 <- terr2[terr2$datos > 0,]

hchart(terr2, "scatter", hcaes(x = terr2[,1], y = terr2[,2], group = terr2[,3])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Terrenos dedicados a la actividad apícola, por entidad federativa, 2017") %>% 
  hc_yAxis(title = "", type= 'logarithmic', min = 1) %>% hc_xAxis(title = "") %>% hc_colors(colors = c("orange", "green", "violet", "brown")) %>% 
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "terrenos", sep = " " )) 




hect <- read.csv("tabs/superficie_apicola.csv")
hect2 <- as.data.frame(rep(hect$entidad,3))
hect2$datos <- c(hect[,3], hect[,5], hect[,7])
hect2$tipo <- c(rep("Ganadería ejidal",length(hect$entidad)), 
                rep("Ganadería comunal",length(hect$entidad)), rep("Ganadería privada",length(hect$entidad)))
hect2 <- hect2[hect2$datos > 0,]

hchart(hect2, "scatter", hcaes(x = hect2[,1], y = hect2[,2], group = hect2[,3])) %>% hc_add_theme(hc_theme_gridlight()) %>% 
  hc_title(text = "Hectáreas dedicadas a la actividad apícola, por entidad federativa, 2017") %>% 
  hc_yAxis(title = "", type= 'logarithmic', min = 1) %>% hc_xAxis(title = "") %>% hc_colors(colors = c("orange", "green", "violet", "brown")) %>% 
  hc_tooltip(enabled = T, pointFormat = paste(" <b>{point.y:.0f} </b>", "hectáreas", sep = " " ))



library(dplyr)
library(highcharter)


mapdata2 <- get_data_from_map(download_map_data("countries/mx/mx-all"))
mapdata2$name[11] <- "Ciudad de México"
mapdata2$name[9] <- "Estado de México"
desord2 <- mapdata2$name[-1]
colm <- read.csv("tabs/abejas_mayo_2018.csv", head= T)
colm[,1] <- as.character(colm[,1])
colm$Entidad[24] <- "San Luis Potosí"
colm2 <- colm[, c(1,2)]
mapdata_estados2 <- as.data.frame(mapdata2$name[-1])
names(mapdata_estados2) <- "entidad"
colm2$pos <- 1:dim(colm2)[1]
posit2 <- c(rep(0,length(desord2)))

for (i in 1:dim(colm2)[1]) {
  for(j in 1:length(desord2)) {
    ifelse(desord2[i] == colm2$Entidad[j], posit2[i] <- colm2$pos[j], posit2[i] <- posit2[i]*1)
  }
}
ncolm <- c(NA, colm2$Numero_de_colmenas[posit2])

colm3 <- mapdata2 %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = ncolm)

colmen <- hcmap("countries/mx/mx-all", data = colm3, value = "ncolm", 
                joinBy = c("hc-a2", "code"), name = "Colmenas",
                dataLabels = list(enabled = TRUE, format = '{point.name}'),
                borderColor = "#0EAD82", borderWidth = 0.1) 
colmen








### themes:
# economistterr <- read.csv("tabs/estadisticas.csv")
# elementary
# ffx
# google
# gridlight




