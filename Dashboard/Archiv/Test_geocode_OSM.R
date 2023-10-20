library(tmaptools)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(leaflet)


Schweiz <- geocode_OSM("Switzerland", details = TRUE)
bb(Schweiz$bbox)
Schweiz$bbox
bbox <- bb("Schweiz")

Berlin <- geocode_OSM("Berlin", details = TRUE)
Berlin$coords

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_make_valid(world)
geocode_OSM("Berlin")$coords

ch <- geocode_OSM("Schweiz")$coords

longbridge <- geocode_OSM("Langenbruck", details = TRUE)

geocode_OSM("Europa")$bbox

geocode_OSM("Berlin", details = TRUE)$display_name

geocode_OSM("Langgöns", details = TRUE)
bs <- geocode_OSM("Basel")$coords
zh <- geocode_OSM("Zürich")$coords

# distanzfunktion
approx_distances(c(bs), c(zh))
approx_distances(st_zm(bb("Basel")), st_zm(bb("Zürich")))
approx_distances(geocode_OSM("Basel")$bbox, geocode_OSM("Zürich")$bbox)
ch[1]
bb("Basel")
c(ch[1], ch[2])

geocode_OSM("Schnapsnase")

Schnapsnase <- function(x) {
  if (x == "Schnapsnase") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

Schnapsnase("Hai")
Schnapsnase("Schnapsnase")
x <- geocode_OSM("Schweiz")$coords

location <- geocode_OSM("Deutschland", as.data.frame = TRUE)
location_point <- st_point(c(location$lon, location$lat))
st_crs(location_point) <- st_crs(4326)
world <- st_transform(world, st_crs(location_point))
st_crs(location_point) <- st_crs(12)
# lon <- location$lon
# lat <- location$lat
country_input <- world[which(st_contains(world, st_point(c(location$lon, location$lat)))),]
county_code <- countrycode(country_input$iso_a2, "iso2c", "country.name")



# Installieren und laden Sie zuerst die notwendigen Pakete
install.packages(c("tmaptools", "countrycode", "rnaturalearth", "sf", "lwgeom"))
library(tmaptools)
library(countrycode)
library(rnaturalearth)
library(sf)
library(lwgeom)

# Nutzen Sie die Funktion geocode_OSM() um die Koordinaten zu erhalten
location <- geocode_OSM("Deutschland", as.data.frame = TRUE)
print(location) # gibt Längen- und Breitengrad zurück

# Erzeugen Sie einen Punkt mit den Koordinaten und setzen Sie das Koordinatensystem (CRS)
location_point <- st_sfc(st_point(c(location$lon, location$lat)), crs = 4326)

# Laden Sie die Welt-Länderdaten
world <- ne_countries(scale = "medium", returnclass = "sf")

# Stellen Sie sicher, dass die Welt-Daten im gleichen CRS sind
world <- st_transform(world, st_crs(location_point))

# Machen Sie die Geometrie gültig
world <- st_make_valid(world)

# Identifizieren Sie das Land, indem Sie die Koordinaten verwenden
country <- world[which(st_contains(world, location_point)),]

# Anschließend können Sie den Ländercode mithilfe des `countrycode` Pakets erhalten
country_code <- countrycode(country$iso_a2, "iso2c", "country.name")
print(country_code)

poland_shiny %>% 
  filter(month>= 9 | month <= 3) %>% 
  group_by(year, month) %>% 
  summarise(max_month = max(sd)) %>% 
  ungroup() %>% 
  arrange(desc(max_month))

poland_shiny %>% 
  filter(month>= 9 | month <= 3) %>% 
  group_by(year, month) %>% 
  summarise(min_month = min(sd)) %>% 
  ungroup() %>% 
  arrange(desc(min_month))


poland_shiny %>% 
  filter(month>= 9 | month <= 3) %>% 
  slice_max(sd)

Brugg <- geocode_OSM("Brugg", details = TRUE)
