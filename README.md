# TRABAJO-FINAL
# PUNO
############# Cargar las librerias ###########################

library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgee)
library(mapview)
library(mapedit)
library(ggplot2)
library(lubridate)

############ INICIALIZAR EARTH ENGIME ##############################

ee_Initialize("carlosflores37")
setwd("C:/Users/provincia_puno")

############ CARGAR SHP DE LAS PROVINCIAS #########################

puno_prov<- st_read("shp/Provincias.shp")


#################### CARGAR LOS DEM ###############################

dem1 <- raster("DM_PROV_PUNO/ASTGTM_S16W070_dem.tif")
dem2 <- raster("DM_PROV_PUNO/ASTGTM_S16W071_dem.tif")
dem3 <- raster("DM_PROV_PUNO/ASTGTM_S17W070_dem.tif")
dem4 <- raster("DM_PROV_PUNO/ASTGTM_S17W071_dem.tif")

############## Unir 4 raster #########################

join_dem <- merge(dem1, dem2,dem3,dem4)

########### FILTRAR LA PROVINCIA PUNO ##################
puno <- puno_prov%>% 
  filter(PROCOD98 == 2101)
mapview(list(join_dem, puno))
View(puno_prov)

############ PASAR DE MASK RASTER AS SPATIAL #################
sp_puno <- as(puno, "Spatial")
puno_mask<- mask(join_dem, sp_puno)        
plot(puno_mask, 
     main = "Variacion de altura de la provincia Puno") 
mean(puno_mask)

########### DELIMITAR EL AREA DEL TRABAJO(corte) #####################
cut_puno <- crop(join_dem, sp_puno)
plot(cut_puno)
puno_height <- mask(cut_puno, sp_puno)
plot(puno_height)

############# ALTITUD PROMEDIO ####################
mean_height <- raster::extract(puno_height, sp_puno,fun = mean)

############ AREA RELATIVA A PUNO #################

A_puno <- mapview(puno_height) %>% 
  editMap()
A_sf <- A_puno$all
plot(st_geometry(A_sf))
mapview(list(puno_height, A_sf))

############ combirtiendo a earth engime ############
A_ee <- sf_as_ee(A_sf)


########### accediendo al dataset del earth engime ##########

############ TEMPERATURA MINIMA ###################
ee_temp_min <- ee$ImageCollection('NOAA/CFSV2/FOR6H')$
  filterDate("2019-01-01", "2020-01-01")$
  first()
temp_min_stack <- ee_as_raster(imag  = ee_temp_min,
                         region = A_ee$geometry())
temp_min_area <- temp_min_stack[[8]]
plot(temp_min_area)
mapview(list(temp_min_area, puno_height))

########### ALTITUD GEOPOTENCIAL(gpm)######################
geopotencial_height <- temp_min_stack[[3]]
plot(geopotencial_height)

######### CREANDO PUNTOS CON MAPEDIT ##############################

points <- mapview(puno_height) %>% 
  editMap()
points_sf <- points$all
plot(puno_height,
     main = "Alturas de Puno")
plot(points_sf, add = T)

######### AGREGANDO COLUMNA DE TEMPERATURA MINIMA Y GEOPOTENCIAL ##############
points_sf$temp_min <- raster::extract(temp_min_area, points_sf)
points_sf$geopotencial <- raster::extract(puno_height, points_sf)

######### RELACIONANDO ELEMENTOS CLIMÁTICOS ###################### 
view <- points_sf%>%
  as_tibble() %>% 
  dplyr::select(temp_min,geopotencial)
plot(view)

############ PLOT TEMPERATURA MINIMA VS ALTITUD GEOPOTENCIAL ##############
plot(view$temp_min, view$geopotencial,
     main = "T_min vs altitud geopotencial",
     ylab = "Altitud geopotencial",
     xlab = " Temperatura minima")
