function(input, output, session) {

  # TODO: Auswahl der sichtbaren Stationen mit dem Marker (sollte nur ein Marker pro Station sein)
  # TODO: Anpassen der Jahre, Datum etc. in der Tabelle
  # TODO: Valuebox für Max month und extremsten Wert
  # TODO: Anpassen des Textes für die Ausgewaehlten Optionen mit if-statement analaog games
  # TODO: Jahrauswahl gemaess sliderInput anpassen auf das Dataframe
  # TODO: Reiter Entwicklung Schneefall entfernen und nur mein nächster Schneeurlaub belassen

  # -------------- Rueckgabe Informationen ueber Ort oder Land ----------------
  # NEU
  location <- reactive({
    input$country
  })

  bbox <- reactive({
    bb(location())
  })

  location_coords <- reactive({
    geocode_OSM(location())$coords
  })

  country <- reactive({
    unique(closest_stations()$country)
    })

  # -------------- Stationen in Naehe zu gewaehlter Stadt oder Land  ----------------
  # NEU
    closest_stations <- reactive({
    stations %>%
      rowwise() %>%
      mutate(distanz = approx_distances(location_coords(), (c(lon, lat)))) %>%
      ungroup() %>%
      slice_min(distanz, n =  input$n_stations)
  })

  # --------------- Datensatz aus Datenbank mit den angezeigten Stationen der Karte (Wintermonate) --------------
  # closest_stations_snow_data as cssd
  # NEU
  # cssd <- reactive({
  #   data.frame(dbGetQuery(conn, glue("SELECT * FROM cdk WHERE staid {closest_stations$staid} AND month > 8 OR month < 4")))
  #   })
 
  cssd <- reactive({
    # country <- unique(closest_stations()$country)
    country <- country()
    staids <- paste(unique(closest_stations()$staid), collapse = ",")
    ch_shiny <- dbGetQuery(conn, glue("SELECT * FROM {country} where month in (9,10,11,12,1,2,3) and staid in ({staids})"))
    data.frame(ch_shiny)
    })
 # ------------------- Dashboard Anzahl Stationen ----------------------
  # output$numStation <- renderUI({
  #   output_class <- "output-class"
  #   HTML(paste("<div class='", output_class, "'>", "Anzahl Stationen:", nrow(df_sta_filtered()), "</div>")) # , sep = ""
  # })
   output$numStation <- renderInfoBox({
     output_class <- "output-class"
    val <- input$n_stations
    infoBox(
      title = "", #Ausgewählte Optionen
      value = HTML(paste("<div class='", output_class, "'>", "Anzahl Stationen:", val, "</div>")),
      icon = icon("tower-cell"),
      color = "blue",
      width = 12
    )
  })

   # ------------------ Value Box max_month (hoechster Wert im Monat) ------------
  # NEU
  # max_month <- reactive({
  #   cssd() %>% slice_max(sd) %>% select(sd) 
  # })
  # 
  # staname <- reactive({
  #   cssd() %>% slice_max(sd) %>% select(staname)
  # })
  # 
  # max_month <- reactive({
  #   cssd() %>% slice_max(sd) %>% select(month)
  # })
  # 
  # max_year <- reactive({
  #   cssd() %>% slice_max(sd) %>% select(year)
  # })
  # 
  # 
  # output$max_month <- renderValueBox({
  # info <- cssd() %>% slice_max(sd)
  # val <- max_month()
  # staname <- head(staname(),1)
  # month <- month(max_month())
  # year <- max_year()
  # valueBox(
  #          value = paste(val, "cm"),
  #          subtitle = paste("Hoechste gemessene Schneemenge der Station", staname,
  #                           "im Jahr", year, "und Monat", month),
  #          icon = icon("snowflake"),
  #          color = "blue",
  #          width = 4
  #         )
  #   # weitere Stationen in der Tabelle aufführen mit ihren jeweiligen Höchstwerten
  # })

  # ------------------ Value Box max_month ALTERNATIVE (hoechster Wert im Monat) ------------
  # NEU
   max_snow <- reactive({
     cssd() %>% slice_max(sd)
   })
   

  output$tabletest <- renderTable({
    cssd() %>% head(input$n_stations)
  })
  
  output$max_month2 <- renderValueBox({
    value <- paste(max_snow()$sd, "cm")
    # subtitle <- paste("Hoechste gemessene Schneemenge der Station", max_snow()$staname,
    #                    "im Jahr", max_snow()$year, "und Monat", max_snow()$month)
    # valueBox(
    #   value = value,
    #   subtitle = subtitle,
    #   icon = icon("snowflake"),
    #   color = "blue",
    #   width = 4
    # )

    valueBox(
      value = "50 cm",
      subtitle = "Hoechste je gemessene Schneemenge der Station am 1. August 1291",
      icon = icon("snowflake"),
      color = "blue",
      width = 4
    )
    # weitere Stationen in der Tabelle aufführen mit ihren jeweiligen Höchstwerten
  })
  
  

# ------------------- auf Karte filtern nach sichtbaren Stationen ----------------------
  # NEU
  stainbounds <- reactive({
    if (is.null(input$map_bounds))
      return(closest_stations()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    data.frame(subset(closest_stations(), lat >= latRng[1] & lat <= latRng[2] & lon >= lngRng[1] & lon <= lngRng[2]))
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
  lon_mean <- mean(closest_stations()$lon)
  lat_mean <- mean(closest_stations()$lat)
  leaflet() %>%
    addTiles() %>%
    setView(lng = lon_mean, lat = lat_mean, zoom = 6) %>%
    addMarkers(data = cssd(), lng = ~lon, lat = ~lat, popup = ~stahght, clusterOptions = markerClusterOptions())
})

  # ----------- Dashboard Info Box fuer ausgewaehlte Optionen -----------
  # NEU
  output$selected_country <- renderInfoBox({
    output_class <- "output-class" # HTML klasse
    location <- input$country # Info aus dem Eingabefeld "country"
    years <- input$years # Info aus dem Sliderinput "years"
    # stahght <- input$sta_hight # Info aus dem Sliderinput "sta_hight"
    stahght <- cssd() %>% select(hght)

    infoBox(
      title = "", #Ausgewaehlte Optionen
      value = if (location == "Lummerland"){
        HTML(paste("Hier Lummerland Hauptbahnhof"))
      } else {
        HTML(paste("<div class='", output_class, "'>","Ausgewaehltes Land:", location, "<br>",
                   # "Hoehe der Stationen m.ue.M.:", stahght[1], "-", stahght[2], "<br>",
                   "Hoehe der Stationen m.ue.M.:", min(stahght), "-", max(stahght), "<br>",
                    "Ausgewaehlte Jahre:", years[1], "-", years[2], "</div>"))
      },
      icon = icon("globe"),
      color = "blue",
      width = 12
    )
  })

    # --------------- Plot für Schneefall Entwicklung ----------------
  #---------------------Plot Antonia: Balkenplot Abweichung zum Durchschnitt pro Jahr  -------------------------------


  output$columns_difference <- renderPlot({
    overall_avg_snow <- mean(cssd()$sd)

    cssd() %>%
      group_by(winter_season, stahght) %>%
      summarize(avg_snow = mean(sd), difference = avg_snow - overall_avg_snow ) %>%
      arrange(desc(difference)) %>%
      ggplot(aes(winter_season, difference, fill = ifelse(difference < 0, "below average", "above average"))) +
      geom_col()+
      labs(x = "year",
           y = "Difference to average",
           title = "Comparison of mean snow to average across all years",
           fill = "")+
      facet_wrap(~stahght)
  })

  #---------------------Plot Antonia: Scatterplot Vergleich letztes Jahr vs alle Jahre -------------------------------
  output$scatterplot_years <- renderPlot({

    df_days <- data.frame(md = format(seq(as.Date("2021-09-01"), as.Date("2022-04-30"), by = "day"), "%m-%d"),
                          day_nr = 1:length(format(seq(as.Date("2021-09-01"), as.Date("2022-04-30"), by = "day"), "%m-%d")))

    data_plot <- cssd() %>%
      mutate(season = ifelse(month %in% c(10, 11, 12, 1, 2, 3), "winter", "summer"),) %>%
      filter(winter_season > min(year), season == "winter") %>%
      mutate(md = format(date, "%m-%d")) %>%
      inner_join(df_days, by = "md") %>%
      filter(winter_season > input$years[1]&winter_season < input$years[2],
             sd > 0)

    ggplot(data_plot) +
      geom_point(aes(x = day_nr, y = sd, color = winter_season != max(winter_season)), alpha = 0.3) +
      geom_point(data = subset(data_plot, winter_season == chosen_year), aes(x = day_nr, y = sd), color = "red", alpha = 0.7) +
      scale_color_manual(values = c("red", "darkgrey"), labels = c(chosen_year, paste(min(data_plot$winter_season), "-", max(data_plot$winter_season)))) +
      labs(x = "Day",
           y = "Snow Depth",
           color = "",
           title = "Schneehöhen in den Wintermonaten",
           caption = "rot zeigt das ausgewählte Jahr") +
      theme_minimal()+
      facet_wrap(~stahght)

  })


  }