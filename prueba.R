
setwd("D:/Geografía/atlasapi2019.github.io")

library(dplyr)
library(ggplot2)


estadist <- read.csv("tabs/estadisticas.csv", head= T)
estadist[,1] <- as.character(estadist[,1])
estadist$Entidad[24] <- "San Luis Potosí"
estadist2 <- estadist[, c(1,2)]

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







library(sp)
library(rgdal)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(mapview)  
library(raster)
library(bitops)
library(rjson)
library(xts)


proApic <- readOGR("geodata/INEGI_DENUE_21012019_APICOLA/INEGI_DENUE_21012019.shp",
                   "INEGI_DENUE_21012019", verbose=FALSE)
salen1 <- c(3913258,2218442,3096733,3640449,4083614,4184935,4449017,4486570)
proApicola <- subset(proApic, !(is.element(proApic@data$id, salen1)))
x0 <- rep("Actividades apícolas", dim(proApicola@data)[1])
proApicola@data <- cbind(proApicola@data, x0)


proMiel <- readOGR("geodata/INEGI_DENUE_21012019_MIEL/INEGI_DENUE_21012019.shp",
                   "INEGI_DENUE_21012019", verbose=FALSE)
entran <- c(1662213,2565829,1426730,1751907,4282468,3518984) 
salen2 <- c(1812561,3163906,3164094,2603960,1764794,555821,2477068,2465731,452114,4552918,1573930,3946536,
            3426513,450223,485578,1436249,6743142,4108619)
cod_scian <- c(311221,311999,321920,325412,325610,431194,431199,461190,461130,461140,461150,464113,493130)
ueMiel <- subset(proMiel, ((is.element(proMiel@data$codigo_act, cod_scian)
                            | is.element(proMiel@data$id, entran)) & !(is.element(proMiel@data$id, salen2))))
ueMiel@data$x0 <- rep("Venta de Miel", dim(ueMiel@data)[1])


otrApic <- readOGR("geodata/INEGI_DENUE_01082019_OTRAPIC/INEGI_DENUE_01082019.shp",
                   "INEGI_DENUE_01082019", verbose=FALSE)
otrApic@data$x0 <- rep("Actividades apícolas", dim(otrApic@data)[1])

ueMielApic <- raster::bind(ueMiel, proApicola)
ueMielApic <- raster::bind(ueMielApic, otrApic)

#writeOGR(ueMielApic, "geodata/puntos_miel.shp", "puntos_miel", "ESRI Shapefile", encoding = "UTF-8")
#write.csv(ueMielApic@data,"geodata/puntos_miel.csv")

quakes.df <- split(ueMielApic, ueMielApic$x0)

l <- leaflet() %>% addTiles() %>%
  addFullscreenControl()

names(quakes.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=quakes.df[[df]],
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 
                 popup = paste0( "<strong>id: </strong>", quakes.df[[df]]$id, "</br>",
                                 "<strong>NOMBRE_ESTABLECIMIENTO: </strong>", quakes.df[[df]]$nom_estab, "</br>",
                                 "<strong>TIPO: </strong>", quakes.df[[df]]$nombre_act, "</br>",
                                 "<strong>MUNICIPIO: </strong>", quakes.df[[df]]$municipio, "</br>",
                                 "<strong>ENTIDAD: </strong>", quakes.df[[df]]$entidad
                 )
      )
  })


l %>% 
  
  addMiniMap(toggleDisplay = TRUE, position = "bottomleft") %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")%>%
  
  addEasyButton(easyButton(
    icon="fa-crosshairs", 
    title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addLayersControl(
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% addMouseCoordinates()  %>%
  addHomeButton(extent(proMiel), layer.name = "i",  position = "topleft") 












library(highcharter)
library(dygraphs)

bx <- read.csv("tabs/ent_ton_miel_03_18.csv", encoding = "UTF")
bx2 <- bx[1:32,-c(1:2)]
estados <- as.character(bx$Estado)[1:32]
miel_est <- ts(data = t(bx2), start = 2003, end = 2018, names = estados)


g4 <- hchart(miel_est[,1], animation = T, name = "AGS",visible = F) %>% hc_add_theme(hc_theme_flat()) %>%  
  hc_add_series(name = "BC", data = miel_est[,2],visible = F)%>%
  hc_add_series(name = "BCS", data = miel_est[,3],visible = F)%>%
  hc_add_series(name = "CAMP", data = miel_est[,4])%>%
  hc_add_series(name = "COH", data = miel_est[,5],visible = F)%>%
  hc_add_series(name = "COL", data = miel_est[,6],visible = F)%>%
  hc_add_series(name = "CHPS", data = miel_est[,7])%>%
  hc_add_series(name = "CHI", data = miel_est[,8],visible = F)%>%
  hc_add_series(name = "CDMX", data = miel_est[,9],visible = F)%>%
  hc_add_series(name = "DGO", data = miel_est[,10],visible = F)%>%
  hc_add_series(name = "GTO", data = miel_est[,11],visible = F)%>%
  hc_add_series(name = "GRO", data = miel_est[,12],visible = F)%>%
  hc_add_series(name = "HGO", data = miel_est[,13],visible = F)%>%
  hc_add_series(name = "JAL", data = miel_est[,14])%>%
  hc_add_series(name = "MEX", data = miel_est[,15],visible = F)%>%
  hc_add_series(name = "MICH", data = miel_est[,16],visible = F)%>%
  hc_add_series(name = "MOR", data = miel_est[,17],visible = F)%>%
  hc_add_series(name = "NAY", data = miel_est[,18],visible = F)%>%
  hc_add_series(name = "NL", data = miel_est[,19],visible = F)%>%
  hc_add_series(name = "OAX", data = miel_est[,20],visible = F)%>%
  hc_add_series(name = "PUE", data = miel_est[,21],visible = F)%>%
  hc_add_series(name = "QUER", data = miel_est[,22],visible = F)%>%
  hc_add_series(name = "QRO", data = miel_est[,23],visible = F)%>%
  hc_add_series(name = "SLP", data = miel_est[,24],visible = F)%>%
  hc_add_series(name = "SIN", data = miel_est[,25],visible = F)%>%
  hc_add_series(name = "SON", data = miel_est[,26],visible = F)%>%
  hc_add_series(name = "TAB", data = miel_est[,27],visible = F)%>%
  hc_add_series(name = "TAM", data = miel_est[,28],visible = F)%>%
  hc_add_series(name = "TLX", data = miel_est[,29],visible = F)%>%
  hc_add_series(name = "VER", data = miel_est[,30])%>%
  hc_add_series(name = "YUC", data = miel_est[,31])%>%
  hc_add_series(name = "ZAC", data = miel_est[,32],visible = F)%>%
  hc_title(text = "Producción de miel  por entidad federativa, 2003-2018 (Toneladas)") %>% 
  hc_subtitle(text = "Atlas de las Abejas y Derivados Apícolas") 

g4




library(dplyr)
library(highcharter)
library(ggplot2)

asoc <- read.csv("tabs/asoc_ent.csv", head=T)

hchart(asoc, "bar", hcaes(x = asoc[,1], y = asoc[,2], group = asoc[,3])) %>% hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = "Asociaciones Apícolas y Otras Organizaciones, por entidad federativa, 2017") %>% 
  hc_plotOptions(bar = list(size = 300)) %>% 
  hc_xAxis(title = "") %>% hc_yAxis(title = "")






library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(mapview)  



prod_miel <- readOGR("geodata", "ent_a", verbose=FALSE, encoding = "UTF")
prodmiel2018 <- read.csv("tabs/prodmiel2018.csv", head= T)
prod_miel$total2018 <- prodmiel2018[,3]
map_prod_miel <- leaflet(data=prod_miel)
clust <- kmeans(prod_miel$total2018,5)
breaks <- sort(c(0, max((prod_miel$total2018)[clust$cluster == 1]),
                 max((prod_miel$total2018)[clust$cluster == 2]),
                 max((prod_miel$total2018)[clust$cluster == 3]),
                 max((prod_miel$total2018)[clust$cluster == 4]),
                 max((prod_miel$total2018)[clust$cluster == 5])))
breaks2 <- c((round(breaks[-6],1)),(ceiling(breaks[6])))
binpal <- colorBin("Oranges", prod_miel$total2018, breaks2)

map_prod_miel %>%
  setView(lng = -102.43, lat = 22.37, zoom = 5.4) %>%
  addTiles() %>% addFullscreenControl() %>%
  addPolygons(
    stroke = FALSE,
    fillOpacity = 1.0,
    smoothFactor = 0.1,
    color = ~binpal(total2018),
    popup = paste0("<strong>CVE_ENT: </strong>", prod_miel$CVE_ENT, "</br>",
                   "<strong>ENTIDAD: </strong>", prod_miel$NOMGEO, "</br>",
                   "<strong>PRODUCCIÓN: </strong>", prod_miel$total2018)
  ) #%>%
#  addLegend(
#   "bottomleft", pal = binpal, values = ~total2018,
#    title = "Producción de Miel <br/> (Toneladas)",
#    opacity = 0.7
#  )





exportaciones <- read.csv("tabs/exportaciones.csv", head= T)
hist(as.numeric(paste(exportaciones$dolares)))

exp1 <- subset(exportaciones, exportaciones$pais_de_origen_destino=="Japón")
exp2 <- as.vector(tapply(exp1$dolares,exp1$anio,sum))
write.csv(exp2,"exp2.csv")





#```{r graficaexport, echo = FALSE, message = FALSE, fig.height= 8, fig.width= 10, warning = FALSE, cache=F}

library(plotly)

dest <- read.csv("tabs/destinos.csv", encoding = "UTF")
dest2 <- dest[1:6,-1]
dest3 <- as.data.frame(t(dest2))
paises <- c("ALEM","USA","UK","ARAB_SAUD","SUIZA","BELG")
names(dest3) <- paises
dest3$years <- c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
dest3$years <- factor(dest3$years, levels = dest3[["years"]])

pexp <- plot_ly(dest3, x = ~years, y = ~ALEM, name = 'Alemania', type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
  add_trace(y = ~USA, name = 'Estados Unidos', line = list(color = 'darkblue', width = 4, dash = NULL)) %>%
  add_trace(y = ~UK, name = 'Reino Unido', line = list(color = 'green', width = 4, dash = NULL)) %>%
  add_trace(y = ~ARAB_SAUD, name = 'Arabia Saudita', line = list(color = 'cyan', width = 4, dash = NULL)) %>%
  add_trace(y = ~SUIZA, name = 'Suiza', line = list(color = 'darkorchid', width = 4, dash = NULL)) %>%
  add_trace(y = ~BELG, name = 'Bélgica', line = list(color = 'dodgerblue', width = 4, dash = NULL))  %>%
  layout(title = "Gráfica 14. Valor de las exportaciones a los principales países de destino, 2002-2018",
         xaxis = list(title = "Año"),
         yaxis = list (title = "Miles de dólares"))
pexp 




library(DiagrammeR)

DiagrammeR::mermaid("
                    graph TB
                    A(  Cepa madre con <br/> 10 cuadros +  REINA  ) --> B[ Cepa madre con 5 cuadros + REINA <br/> a 20 metros de sitio original  ]
                    A-->C(  Colonia Nueva con 5 cuadros S/REINA <br/> al sitio de la madre   )
                    C-->A
                    style A fill:orange,color:#ebd4cb,stroke:#ebd4cb,stroke-width:2px;
                    style B fill:gold,color:#ebd4cb,stroke:#ebd4cb,stroke-width:2px;
                    style C fill:gold,color:#ebd4cb,stroke:#ebd4cb,stroke-width:1px;
                    ")


DiagrammeR::mermaid("
                    graph LR
                    A(  Colmena madre  ) --> B( Colmena con 8 cuadros </br> S/R al sitio de la madre. </br> Después de 8 días, éstas se dividen )
                    A --> C(  Colmena con 2 cuadros + </br> R a 10 metros del sitio original )
                    B --> D(  4 Colmenas con 2 cuadros </br> + Realeras )
                    
                    style A fill:orange,color:#ebd4cb,stroke:#ebd4cb,stroke-width:2px;
                    style B fill:gold,color:#ebd4cb,stroke:#ebd4cb,stroke-width:2px;
                    style C fill:cyan,color:#ebd4cb,stroke:#ebd4cb,stroke-width:1px;
                    style D fill:gold,color:#ebd4cb,stroke:#ebd4cb,stroke-width:1px;
                    ")


