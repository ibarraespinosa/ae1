library(sf)
library(eixport)
library(vein)
library(osmdata)
library(units)
library(cptcity)
setwd("~/Documents/pedals/ae1/") # changfe to the path where you cloned the repository
dir.create("TDM/munich")
dir.create("TDM/munich/TDM_TO_OSM/")
dir.create("TDM/munich/TDM_TO_OSM/rds_emis_to_munich")
dir.create("TDM/munich/TDM_TO_OSM/csv_emis_to_munich")
# Reads streets TDM emissions 
# creates emissions grid
# pass the emissions t the OSM network
# Transform units in g/s
# if it run by the first time, uncomment all
# doubts at sergio.ibarra@uspbr
# Need internet

# Reading street emissions
ev1 <- readRDS("TDM/streets/EVD_E100_street.rds")
ev2 <- readRDS("TDM/streets/EVD_E25_street.rds")
ev3 <- readRDS("TDM/streets/EVHS_E100_street.rds")
ev4 <- readRDS("TDM/streets/EVHS_E25_street.rds")
ev5 <- readRDS("TDM/streets/EVRL_E100_street.rds")
ev6 <- readRDS("TDM/streets/EVRL_E25_street.rds")
ev7 <- readRDS("TDM/streets/FS_E100_street.rds")
ev8 <- readRDS("TDM/streets/FS_E25_street.rds")

# agregating evaporative
ev <- ev1
horas <- paste0("h", 1:168)
for(i in 1:168){
  ev[[horas[i]]] <- ev1[[horas[i]]] + ev2[[horas[i]]] + ev3[[horas[i]]] +
    ev4[[horas[i]]] + ev5[[horas[i]]] + ev6[[horas[i]]] +
    ev7[[horas[i]]] + ev8[[horas[i]]]
}

hc1 <- readRDS("TDM/streets/NMHC_B5_street.rds")
hc2 <- readRDS("TDM/streets/NMHC_E100_street.rds")
hc3 <- readRDS("TDM/streets/NMHC_E25_street.rds")
hc <- hc2
dim(hc1)
dim(hc2)
dim(hc3)

# agregating hc
for(i in 1:168){  hc[[horas[i]]] <- hc2[[horas[i]]] + hc3[[horas[i]]]}

# readiing
co <- readRDS("TDM/streets/CO_street.rds")
no <- readRDS("TDM/streets/NO_street.rds")
no2 <- readRDS("TDM/streets/NO2_street.rds")

# # cropping
area <- read_sf("../routes.shp")
co <- st_crop(co, st_bbox(area))
no <- st_crop(no, st_bbox(area))
no2 <- st_crop(no2, st_bbox(area))
hc1 <- st_crop(hc1, st_bbox(area))
hc <- st_crop(hc, st_bbox(area))

# #grid 50 m
g <- make_grid(co, 50)

# emis_grid
gco <- emis_grid(co, g)
gno <- emis_grid(no, g)
gno2 <- emis_grid(no2, g)
ghc <- emis_grid(hc, g)
ghc1 <- emis_grid(hc1, g)
gev <- emis_grid(ev, g)

# agregating total hc
ghctotal <- ghc
for(i in 1:168){  ghctotal[[horas[i]]] <- ghc[[horas[i]]] + ghc1[[horas[i]]] + gev[[horas[i]]]}


# downloading OSM
osm <- osmdata_sf(
  add_osm_feature(
    opq(bbox = st_bbox(st_transform(area, 4326))),
    key = 'highway'))$osm_lines[, c("highway")]
osm <- st_crop(osm, st_bbox(st_transform(area, 4326)))
st <- c("motorway", "motorway_link", "trunk", "trunk_link",
        "primary", "primary_link", "secondary", "secondary_link",
        "tertiary", "tertiary_link")

# selecting streets
osm <- osm[osm$highway %in% st, ]
saveRDS(osm, "TDM/munich/TDM_TO_OSM/osm.rds")
osm <- readRDS("TDM/munich/TDM_TO_OSM/osm.rds")

# Exploding lines
osm <- eixport::sfx_explode(osm) # it seems that it only works with lat lon

plot(osm, axes = T)

# creating buffer
areab <- st_buffer(area, 100)
areab <- st_union(areab)
osm$id <- 1:nrow(osm)

# intersecting
osmb <- st_intersection(st_transform(osm, 31983), areab)

# selecting streets inside area
osmbb <- st_transform(osm[osm$id %in% unique(osmb$id), ], 31983)
plot(area$geometry, reset = F, col = "blue", lwd = 3, axes = T)
plot(areab, add = T)
plot(osmbb$geometry, add = T, col = "red")
legend(x = 327000, y = 7395550, 
       legend = c("Rotas Bicicleta","Buffer Rotas 100 m", "Ruas com emissoes"), 
       fill = c("blue", "black", "red"),text.col = "black")

# CO
names(gco) <- gsub(pattern = "h", replacement = "V", x = names(gco))
a <- grid_emis(spobj = osmbb, g = gco)
horas <- paste0("V", 1:168)
for(i in 1:168){  a[[horas[i]]] <- set_units(as.numeric(a[[horas[i]]])/3600, "g/s")}
saveRDS(a, "TDM/munich/TDM_TO_OSM/rds_emis_to_munich/CO_hourly_grams_seconds.rds")
lf <- to_munich(a[horas])
write.csv(lf$Emissions, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Emissions_CO_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Streets_CO_hourly_grams_seconds.csv", row.names = FALSE)


# NO2
names(gno2) <- gsub(pattern = "h", replacement = "V", x = names(gno2))
a <- grid_emis(spobj = osmbb, g = gno2)
horas <- paste0("V", 1:168)
for(i in 1:168){  a[[horas[i]]] <- set_units(as.numeric(a[[horas[i]]])/3600, "g/s")}
saveRDS(a, "TDM/munich/TDM_TO_OSM/rds_emis_to_munich/NO2_hourly_grams_seconds.rds")
lf <- to_munich(a[horas])
write.csv(lf$Emissions, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Emissions_NO2_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Streets_NO2_hourly_grams_seconds.csv", row.names = FALSE)

# NO
names(gno) <- gsub(pattern = "h", replacement = "V", x = names(gno))
a <- grid_emis(spobj = osmbb, g = gno)
horas <- paste0("V", 1:168)
for(i in 1:168){  a[[horas[i]]] <- set_units(as.numeric(a[[horas[i]]])/3600, "g/s")}
saveRDS(a, "TDM/munich/TDM_TO_OSM/rds_emis_to_munich/NO_hourly_grams_seconds.rds")
lf <- to_munich(a[horas])
write.csv(lf$Emissions, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Emissions_NO_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Streets_NO_hourly_grams_seconds.csv", row.names = FALSE)


# HC
names(ghc) <- gsub(pattern = "h", replacement = "V", x = names(ghc))
a <- grid_emis(spobj = osmbb, g = ghc)
horas <- paste0("V", 1:168)
for(i in 1:168){  a[[horas[i]]] <- set_units(as.numeric(a[[horas[i]]])/3600, "g/s")}
saveRDS(a, "TDM/munich/TDM_TO_OSM/rds_emis_to_munich/HC_hourly_grams_seconds.rds")
lf <- to_munich(a[horas])
write.csv(lf$Emissions, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Emissions_HC_hourly_grams_seconds.csv", row.names = FALSE)
write.csv(lf$Street, "TDM/munich/TDM_TO_OSM/csv_emis_to_munich/Streets_HC_hourly_grams_seconds.csv", row.names = FALSE)


par(bg = 'white')
plot(a["V1"], pal = cptcity::cpt(colorRampPalette = T), 
     breaks = "quantile", 
     main = "HC emissions at 00:00 [g/s]",
     bg = "black",
     axes = T)
