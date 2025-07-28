library(sf)
mapa_theft <- theft_n_trespass %>% inner_join(dane_test, by="ID")%>%filter(Year==2018, Arrest=="True", 
                                                                           Primary.Type.x == "THEFT")
mapa_trespass <- theft_n_trespass %>% inner_join(dane_test, by="ID")%>%filter(Year==2018 & Arrest=="True" 
                                                                              & Primary.Type.x == "CRIMINAL TRESPASS")

mapa_theft <- mapa_theft[!is.na(mapa_theft$Longitude),]
mapa_trespass <- mapa_trespass[!is.na(mapa_trespass$Longitude),]

mapa_trespass <-st_as_sf(mapa_trespass, coords = c("Longitude", "Latitude"), crs = 4326)
mapa_theft <-st_as_sf(mapa_theft, coords = c("Longitude", "Latitude"), crs = 4326)


library(rJava)
library(OpenStreetMap)

upperLeft = c(42.037, -87.975)

lowerRight = c(41.637, -87.473)

base_map  = openmap(upperLeft, lowerRight, type="osm")


plot(base_map)

mapa_theft.osm <- st_transform(mapa_theft, osm())

plot(st_geometry(mapa_theft.osm), pch=16, col="#80000010", add=T)

plot(base_map)

mapa_trespass.osm <- st_transform(mapa_trespass, osm())

plot(st_geometry(mapa_trespass.osm), pch=16, col="#80000010", add=T)

districts_csv <- read.csv("PoliceDistrictDec2012_20250508.csv", stringsAsFactors = FALSE)
districts_sf <- st_as_sf(districts_csv, wkt = "the_geom", crs = 4326)
districts_osm <- st_transform(districts_sf, osm())
districts_centroids <- st_centroid(districts_osm)


plot(base_map)
plot(st_geometry(districts_osm), border = "black", add = TRUE)
plot(st_geometry(mapa_theft.osm), pch=16, col="#80000010", add=T)
text(
  x = st_coordinates(districts_centroids)[, 1],
  y = st_coordinates(districts_centroids)[, 2],
  labels = districts_centroids$DIST_NUM,
  col = "blue",
  cex = 0.8,
  font = 2,
  title("Aresztowania w związku z kradzieżą")
)

plot(base_map)
plot(st_geometry(districts_osm), border = "black", add = TRUE)
plot(st_geometry(mapa_trespass.osm), pch=16, col="#80000010", add=T)
text(
  x = st_coordinates(districts_centroids)[, 1],
  y = st_coordinates(districts_centroids)[, 2],
  labels = districts_centroids$DIST_NUM,
  col = "blue",
  cex = 0.8,
  font = 2,
  title("Aresztowania w związku z włamaniami")
)


