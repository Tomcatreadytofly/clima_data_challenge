function(input, output, session) {

  # -------------- Rueckgabe Informationen ueber Ort oder Land ----------------
  # NEU
  bbox <- bb(input$country)
  location_coords <- geocode_OSM(input$country)$coords

  
  # -------------- Stationen in Naehe zu gewaehlter Stadt oder Land  ----------------
  # NEU
  closest_stations <- stations %>%
    rowwise() %>%
    mutate(distanz = approx_distances(location_coords, (c(lon, lat)))) %>%
    ungroup() %>%
    slice_min(distanz, n =  input$n_stations)

  # --------------- Datensatz aus Datenbank mit den angezeigten Stationen der Karte (Wintermonate) --------------
  # closest_stations_snow_data as cssd
  # NEU
  cssd <- data.frame(dbGetQuery(conn, glue("SELECT * FROM cdk WHERE staid {closest_stations$staid} AND month > 8 OR month < 4")))
 #  cssd <- ch_shiny %>%  filter(month>= 9 | month <= 3) %>% filter(staid %in% closest_stations$staid)

    # -------------- Test Output von textInput in body -------------
  # NEU
output$test <- renderUI({
  # renderText({
    req(input$country) # req() is used to make sure that the input is available
    e_country <- "unguelitge Eingabe"

   test <- tryCatch({
    geocode_OSM(input$country)
      }, error = function(e) {
     return(NULL)
   })
   if (is.null(test)) {
      updateTextInput(session, "country", value = e_country)
        return(NULL)
      } else if (input$country == "Lummerland"){
        return(NULL)
      }
    })

  # ---------------- Ausgabe Daten aus Textinput --------------------------



#   # ----------- fuer Dashboardtab filtern nach Land und Jahr -----------
#   df_country_filtered <- reactive({
#     inputcountry <- tolower(input$country)
    # if statement to select all countries
    # if (input$country == text_001) {
    #   df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT * FROM cdk WHERE year between {input$dash_year_range[1]} and {input$dash_year_range[2]}")))
    # } else {
    #   df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT * FROM {inputcountry} WHERE year between {input$dash_year_range[1]} and {input$dash_year_range[2]}")))
    # }
  # })
# # -------------------- fuer Dashboard filtern nach Land und Stationen fuer die Koordinaten ----------------------
#   df_sta_filtered <- reactive({
#     inputcountry <- tolower(input$country)
#     #if statement to select all countries
#     if (input$country == text_001) {
#       # df_sta_filtered <- testdata %>% select(staname, staid, lon, lat) %>% distinct()
#       df_sta_filtered <- data.frame(dbGetQuery(conn, glue("SELECT DISTINCT (staname), (staid), (lon), (lat) FROM cdk")))
#     } else {
#       # df_sta_filtered <- testdata %>% filter(country == input$dash_country) %>%  select(staname, staid, lon, lat) %>% distinct()
#       df_sta_filtered <- data.frame(dbGetQuery(conn, glue("SELECT DISTINCT (staname), (staid), (lon), (lat) FROM {inputcountry}")))
#     }
#   })
  # # ------------------- Dashboard Anzahl Stationen ----------------------
  # # output$numStation <- renderUI({
  # #   output_class <- "output-class"
  # #   HTML(paste("<div class='", output_class, "'>", "Anzahl Stationen:", nrow(closest_stations()), "</div>")) # , sep = ""
  # # })
  # # NEU
  #  output$numStation <- renderInfoBox({
  #    output_class <- "output-class"
  #   val <- nrow(closest_stations)
  #   infoBox(
  #     title = "", #Ausgewaehlte Optionen
  #     value = HTML(paste("<div class='", output_class, "'>", "Anzahl Stationen:", val, "</div>")),
  #     icon = icon("tower-cell"),
  #     color = "blue",
  #     width = 12
  #   )
  # })
# ------------------- auf Karte filtern nach sichtbaren Stationen ----------------------
  # NEU
  stainbounds <- reactive({
    if (is.null(input$map_bounds))
      return(closest_stations[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    data.frame(subset(closest_stations, lat >= latRng[1] & lat <= latRng[2] & lon >= lngRng[1] & lon <= lngRng[2]))
  })
# -------------------- auf Karte ausgabe der sichtbaren Kartenraender ------------------------
  # NEU
  output$boundaries <- renderTable({
    input$map_bounds
  })
# ------------------- Ausgabe der sichtbaren Stationen als Tabelle ----------------------
  # NEU
  output$in_bounds <- renderTable({
    stainbounds()
  })
# ------------------ Karte mit sichtbaren Stationen ----------------------
# NEU
output$map_visible_stations <- renderLeaflet({
  lon_mean <- mean(closest_stations$lon)
  lat_mean <- mean(closest_stations$lat)
  leaflet() %>%
    addTiles() %>%
    setView(lng = lon_mean, lat = lat_mean, zoom = 6) %>%
    addMarkers(data = closest_stations, lng = ~lon, lat = ~lat, popup = ~staname, clusterOptions = markerClusterOptions())
})

  # # ------------------ Karte rund um gesuchte Stadt ----------------------
  # output$map_selected_country <- renderLeaflet({
  #   lon_mean <- mean(df_sta_filtered()$lon)
  #   lat_mean <- mean(df_sta_filtered()$lat)
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = lon_mean, lat = lat_mean, zoom = 6) %>%
  #     addMarkers(data = closest_station(), lng = ~lon, lat = ~lat, popup = ~stahght, clusterOptions = markerClusterOptions())
  # })
  # 
  
  
  
  # ----------- Dashboard Info Box fuer ausgewaehlte Optionen -----------
  # NEU
  output$selected_country <- renderInfoBox({
     output_class <- "output-class" # HTML klasse
    location <- input$country # Info aus dem Eingabefeld "country"
    years <- input$years # Info aus dem Sliderinput "years"
    stahght <- input$sta_hight # Info aus dem Sliderinput "sta_hight"

    infoBox(
      title = "", #Ausgewaehlte Optionen
      value = if (location == "Lummerland"){
        HTML(paste("Hier Lummerland Hauptbahnhof"))
      } else {
        HTML(paste("<div class='", output_class, "'>","Ausgewaehltes Land:", location, "<br>",
                   "Hoehe der Stationen m.ue.M.:", stahght[1], "-", stahght[2], "<br>",
                    "Ausgewaehlte Jahre:", years[1], "-", years[2], "</div>"))
      },
      icon = icon("globe"),
      color = "blue",
      width = 12
    )
  })

  # ------------------ Value Box max_month (höchster Wert im Monat) ------------
  # # NEU
  # output$max_month <- renderValueBox({
  #   # val <- cssd # mit dem höchsten Wert analog Month with Largest... für den Monat einer Station
  # info <- cssd %>% slice_max(sd)
  # val <- info  %>% select(sd)
  # staname <- info %>% select(staname)
  # month <- info %>% select(month) %>% mutate(month = month(month))
  # year <- info %>% select(year)
  # date <- info %>% select(date)
  # valueBox(title = paste(month, year),
  #          value = paste(val, "cm"),
  #          subtitle = paste("Höchste gemessene Schneemenge der Station", staname, 
  #                           "im Jahr", year, "und Monat", month),
  #          icon = icon("snowflake"),
  #          color = "blue",
  #          width = 4
  #         )
  #   # weitere Stationen in der Tabelle aufführen mit ihren jeweiligen Höchstwerten
  # })
  # 
  # ------------------ Value Box min_month (tiefster Wert im Monat) ------------
  
  # hier analog dem max_month
  
#   # ----------- Value Box fuer durchschnittlicher Schneefall -----------
#   output$meanBox <- renderValueBox({
#     val <- round(mean(df_country_filtered()$sd), 2)
#     valueBox(
#       value = paste(val, "cm"),
#       subtitle = "Durchschnittliche Schneehoehe",
#       icon = icon("snowflake"),
#       color = "blue",
#       width = 4
#     )
#   })
# # ----------- Value Box fuer maximaler Schneefall -----------
#   output$maxSdBox <- renderValueBox({
#     val <- round(max(df_country_filtered()$sd), 2)
#     valueBox(
#       value = paste(val, "cm"),
#       subtitle = "Maximale Schneehoehe",
#       icon = icon("snowflake"),
#       color = "blue",
#       width = 4
#     )
#   })
# # ----------- Value Box fuer minimaler Schneefall -----------
#   output$minSdBox <- renderValueBox({
#     val <- round(min(df_country_filtered()$sd), 2)
#     valueBox(
#       value = paste(val, "cm"),
#       subtitle = "Minimale Schneehoehe",
#       icon = icon("snowflake"),
#       color = "blue",
#       width = 4
#     )
#   })
#   # # ----------- Value Box fuer Anzahl Stationen -----------
#   # output$numStationBox <- renderValueBox({
#   #   val <- nrow(df_sta_filtered())
#   #   valueBox(
#   #     value = val,
#   #     subtitle = "Anzahl Stationen",
#   #     icon = icon("globe"),
#   #     color = "yellow",
#   #     width = 4
#   #   )
#   # })
#
#
  }