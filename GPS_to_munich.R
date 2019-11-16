remotes::install_github("atmoschem/vein")
remotes::install_github("atmoschem/eixport")

library(sf)
library(eixport)
library(vein)
library(units)
library(cptcity)
setwd("~/Documents/pedals/ae1/") # changfe to the path where you cloned the repository
dir.create("GPS/munich")
dir.create("GPS/munich/GPS_TO_OSM/")
dir.create("GPS/munich/GPS_TO_OSM/rds_emis_to_munich")
dir.create("GPS/munich/GPS_TO_OSM/csv_emis_to_munich")
# Reads streets GPS emissions 
# creates emissions grid
# pass the emissions t the OSM network
# Transform units in g/s
# if it run by the first time, uncomment all
# doubts at sergio.ibarra@uspbr
# Need internet

# Reading street emissions
ev1 <- readRDS("GPS/streets/EVD_E100_street.rds")
ev2 <- readRDS("GPS/streets/EVD_E25_street.rds")
ev3 <- readRDS("GPS/streets/EVHS_E100_street.rds")
ev4 <- readRDS("GPS/streets/EVHS_E25_street.rds")
ev5 <- readRDS("GPS/streets/EVRL_E100_street.rds")
ev6 <- readRDS("GPS/streets/EVRL_E25_street.rds")
ev7 <- readRDS("GPS/streets/FS_E100_street.rds")
ev8 <- readRDS("GPS/streets/FS_E25_street.rds")

# agregating evaporative
ev <- ev1
horas <- paste0("h", 1:168)
for(i in 1:168){
  ev[[horas[i]]] <- ev1[[horas[i]]] + ev2[[horas[i]]] + ev3[[horas[i]]] +
    ev4[[horas[i]]] + ev5[[horas[i]]] + ev6[[horas[i]]] +
    ev7[[horas[i]]] + ev8[[horas[i]]]
}

hc1 <- readRDS("GPS/streets/NMHC_B5_street.rds")
hc2 <- readRDS("GPS/streets/NMHC_E100_street.rds")
hc3 <- readRDS("GPS/streets/NMHC_E25_street.rds")
# as dim hcs  are different, they need to be gridded
hc <- hc1
# agregating all HC
for(i in 1:168){  hc[[horas[i]]] <- hc1[[horas[i]]] + hc2[[horas[i]]] + hc3[[horas[i]]] + 
  ev[[horas[i]]]
}

# reading
co <- readRDS("GPS/streets/CO_street.rds")
no <- readRDS("GPS/streets/NO_street.rds")
no2 <- readRDS("GPS/streets/NO2_street.rds")

# # cropping
area <- read_sf("../routes.shp")
co <- st_crop(co, st_bbox(area))
no <- st_crop(no, st_bbox(area))
no2 <- st_crop(no2, st_bbox(area))
hc <- st_crop(hc, st_bbox(area))


# splitting by vertex
co <- st_explode(co)
no <- st_explode(no)
no2 <- st_explode(no2)
hc <- st_explode(hc)

# creating buffer
areab <- st_buffer(area, 100)
areab <- st_union(areab)

# intersecting
co <- st_intersection(co, areab)
no <- st_intersection(no, areab)
no2 <- st_intersection(no2, areab)
hc <- st_intersection(hc, areab)

# stcast
co <- st_cast(co, "LINESTRING")
no <- st_cast(no, "LINESTRING")
no2 <- st_cast(no2, "LINESTRING")
hc <- st_cast(hc, "LINESTRING")

# CO
names(co) <- gsub(pattern = "h", replacement = "V", x = names(co))
horas <- paste0("V", 1:168)
for(i in 1:168){  co[[horas[i]]] <- set_units(as.numeric(co[[horas[i]]])/3600, "g/s")}
saveRDS(co, "GPS/munich/GPS_TO_OSM/rds_emis_to_munich/CO_hourly_grams_seconds.rds")
lf <- to_munich(co[horas])
write.csv(lf$Emissions, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Emissions_CO_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Streets_CO_hourly_grams_seconds.csv", row.names = FALSE)


# NO2
names(no2) <- gsub(pattern = "h", replacement = "V", x = names(no2))
horas <- paste0("V", 1:168)
for(i in 1:168){  no2[[horas[i]]] <- set_units(as.numeric(no2[[horas[i]]])/3600, "g/s")}
saveRDS(no2, "GPS/munich/GPS_TO_OSM/rds_emis_to_munich/NO2_hourly_grams_seconds.rds")
lf <- to_munich(no2[horas])
write.csv(lf$Emissions, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Emissions_NO2_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Streets_NO2_hourly_grams_seconds.csv", row.names = FALSE)

# NO
names(no) <- gsub(pattern = "h", replacement = "V", x = names(no))
horas <- paste0("V", 1:168)
for(i in 1:168){  no[[horas[i]]] <- set_units(as.numeric(no[[horas[i]]])/3600, "g/s")}
saveRDS(no, "GPS/munich/GPS_TO_OSM/rds_emis_to_munich/NO_hourly_grams_seconds.rds")
lf <- to_munich(no[horas])
write.csv(lf$Emissions, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Emissions_NO_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Streets_NO_hourly_grams_seconds.csv", row.names = FALSE)


# HC
names(hc) <- gsub(pattern = "h", replacement = "V", x = names(hc))
horas <- paste0("V", 1:168)
for(i in 1:168){  hc[[horas[i]]] <- set_units(as.numeric(hc[[horas[i]]])/3600, "g/s")}
saveRDS(hc, "GPS/munich/GPS_TO_OSM/rds_emis_to_munich/HC_hourly_grams_seconds.rds")
lf <- to_munich(hc[horas])
write.csv(lf$Emissions, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Emissions_HC_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "GPS/munich/GPS_TO_OSM/csv_emis_to_munich/Streets_HC_hourly_grams_seconds.csv", row.names = FALSE)


par(bg = 'white')
plot(hc["V1"], pal = cptcity::cpt(colorRampPalette = T), 
     breaks = "quantile", 
     main = "HC emissions at 00:00 [g/s]",
     bg = "black",
     axes = T)
