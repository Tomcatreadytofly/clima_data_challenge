# list of packages
pkgs <- c("shiny", "RPostgres", "DBI", "tidyverse", "lubridate", 
          "shinydashboard", "glue", "leaflet", "RColorBrewer", 
          "celestial", "DataExplorer", "tmaptools", "countrycode", 
          "rnaturalearth", "rnaturalearthdata", "sf", "scales", "zoo", "DT", "osmdata")

# loop over the list of packages
for (pkg in pkgs) {
  # check if the package is installed
  if (!require(pkg, character.only = TRUE)) {
    # install the package if it's not installed
    install.packages(pkg)
    # load the library
    library(pkg, character.only = TRUE)
  } else {
    # load the library if the package is already installed
    library(pkg, character.only = TRUE)
  }
}

# Erstellen der Datenbankverbindung
db <- "schneefall_database"
db_host <- "v000727.edu.ds.fhnw.ch"
db_port <- "50011"
db_user <- 'schneefall'
db_pass <- "Klima2023"

conn <- dbConnect(
  Postgres(),
  dbname = db,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)



# ------- Koordinaten ------------
# bbox für Europa (erlaupte Orte sind in diesem Range)
bbox_island <- geocode_OSM("island")$bbox
bbox_e <- geocode_OSM("europe")$bbox

bbox_europe <- st_bbox(c(bbox_island[1], bbox_e[2], bbox_e[3], bbox_e[4]), crs = 4326)


bbox_a <- geocode_OSM("Aarau")$bbox # Referenzwert für einen Ort innerhalb Europa


# Kontroll-Boolean zum Vergleich
isin_europe1 <- bbox_a < bbox_europe
isin_europe2 <- bbox_a > bbox_europe

# ---------------- Daten in eine interne Tabelle laden ------------


# -------------- Alle Stationen mit Kennzahlen --------------
# Kennzahlen
sta_key_v <- dbGetQuery(conn, "SELECT * FROM stations_kv")
# save(sta_key_v, file = "sta_key_v.RData")
# load("data/sta_key_v.RData")

# Koordinaten
stations <- dbGetQuery(conn, "select * from stations")
# save(stations, file = "stations.RData")
# load("data/stations.RData")

# ------------------ Startdatensatz für Brugg ----------------------------

# Schweiz mit Zürich als nächste Station zu Brugg
ch_shiny <- dbGetQuery(conn, glue("SELECT * FROM sta_15191"))
# save(ch_shiny, file = "ch_shiny.RData")
# load("data/ch_shiny.RData")

# ----------------- Kennzahlen für Europa -----------------------
ekv <- dbGetQuery(conn, "SELECT * FROM europe_kv")
# load("data/europe_key_values.RData")
# europe_key_values <- europe


# ------------------- Startwerte -----------------------------
# Startwert fuer textInput country
val_loc <- "Brugg"

# Startwerte für den Slider years_rng1
min_y_1 <- ch_shiny %>% select(year) %>% min()
max_y_1 <- ch_shiny %>% select(year) %>% max()
min_y_2 <- min_y_1
max_y_2 <- max_y_1
val_min_y_2 <- ifelse(min_y_2 <= 1950, 1950, min_y_2)
min_y_single <- min_y_1
max_y_single <- max_y_1
choices <- unique(ch_shiny$sd)

# Konstante für Textinput
e_country <- "ungültige Eingabe"
not_europe <- "nicht in Europa"
titel_kennzahlen <- "Europa mit Schneehöhe ab 20 cm"
ref_per <- "Referenzperiode 1950-1980"
vergl_per <- "Vergleichsperiode ab 2012"
title_dummy_tab1 <- "Dummy Tab 1"
title_dummy_tab2 <- "Dummy Tab 2"






