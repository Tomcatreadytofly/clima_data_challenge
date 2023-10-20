function(input, output, session) {

  # -------------- Rückgabe Informationen über Ort oder Land ----------------
  # Location gemäss Texteingabe
  location <- reactiveVal(val_loc) # Startwert
  
  # -------------- Test Output von textInput in body -------------
  observeEvent(input$search, {
      
      req(input$country) # req() is used to make sure that the input is available
      bbox_input <- geocode_OSM(input$country)$bbox
      isin_input1 <- bbox_input < bbox_europe
      isin_input2 <- bbox_input >= bbox_europe
      result1 <- all(isin_input1 == isin_europe1)
      result2 <- all(isin_input2 == isin_europe2)

     
    # Test the input
      test <- tryCatch({
        geocode_OSM(input$country)
        }, error = function(e) {
        return(NULL)
        })
      
      # if statements
      if (is.null(test)) {
        updateTextInput(session, "country", value = e_country)
        return(NULL)
      } else if (input$country == "Lummerland"){
        return(NULL)
      } else if (result1 == FALSE || result2 == FALSE) {
        updateTextInput(session, "country", value = not_europe)
        return(NULL)
      }
      location(input$country)
      })
  # -------------- Ende Test Output von textInput in body -------------
  
  
  output$dependent_slider <- renderUI({
    sliderInput("year", "Sie interessieren sich nur für ein bestimmtes Jahr? Wählen Sie es hier aus:", min = input$years_rng1[1], max = input$years_rng1[2], value = input$years_rng1[2], step = 1, sep = "")
  })
  
  
  # ------------------ reaktive Variablen ----------------------
  title_country_plots <- reactive({
    glue("Entwicklung des Schnees über den Zeitraum 
         von {input$years_rng2[1]} bis {input$years_rng1[2]}")
  })
  

  output$tab_title_country <- reactive({
    closest_stations_n() %>% select(country) %>% head(1)
  })
  
  # -------------- Stationen in Nähe zu gewählter Stadt oder Land  ----------------
  
  # Koordinaten der Location für die Abstandsmessung
  location_coords <- reactive({
    geocode_OSM(location())$coords
  })
  
  # Nächste Station kleinste Distanz zur Location
    closest_stations <- reactive({
    stations %>%
      rowwise() %>%
      mutate(distanz = approx_distances(location_coords(), (c(lon, lat)), target = "km")) %>%
      ungroup() %>%
      # slice_min(distanz, n =  input$n_stations)
        slice_min(distanz, n = 4)
  })

    
    closest_stations_n <- reactive({
      closest_stations() %>% slice_min(distanz, n =  as.numeric(input$n_stations))
    })
  # -----------------Schneehöhe reaktiv machen ----------------------

 # ---------------- Datensatz aus DB mit allen Infos zu closest stations (Wintermonate) ---------------
  # Schneedaten aus DB für die ausgewählten Stationen aus Tabelle für Land
  cssd <- reactive({
    
    stalist <- unique(closest_stations_n()$staid)
    stations_corrected <- paste0("sta_", stalist)
    query <- paste("SELECT * FROM ", paste(stations_corrected, collapse = " UNION ALL SELECT * FROM "), sep="")

    ch_shiny <- dbGetQuery(conn, query)
   
   data.frame(ch_shiny)
   
    })

  # -------------------------- EUROPA ----------------------------------------------------------------------
  # ----------- Karte mit Europa Übersicht -----------
  output$map_europe <- renderLeaflet({
    popup <- paste0(
      "<i>", sta_key_v$stahght, "</i>",
      "<br/>", "Veränderung durchschn. Schneemenge: ", sta_key_v$snow_diff_pct, 
      "<br/>", "Veränderung Anz. Tage mit mind. 11.2 cm Schnee ", sta_key_v$days_diff_pct)
    
    # setlng <- unname((bbox_europe[1] + bbox_europe[3])/2)
    # setlat <- unname((bbox_europe[2] + bbox_europe[4])/2)
    setlng <- 10
    setlat <- 58
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = setlng, lat = setlat, zoom = 3) %>%
      # addMarkers(data = cssd(), lng = ~lon, lat = ~lat, popup = ~stahght, clusterOptions = markerClusterOptions())
      addMarkers(data = sta_key_v, lng = ~lon, lat = ~lat, popup = popup, clusterOptions = markerClusterOptions())
  })

  # ------------------ Tabelle alle Stationen in Europa --------------------------------
  output$table_europa <- DT::renderDT({
    # data <- sta_key_v %>%
    #   select(country, region, stahght, dur_diff_pct)
    
     data <- sta_key_v %>%  select(country, stahght, snow_p1, snow_p2, snow_diff_pct,days10_p1, days10_p2, days_diff_pct, dur_diff ) %>% 
       mutate(dur_diff = round(dur_diff),
              country = str_to_title(country))

    # Spaltenüberschriften umbenennen
    colnames(data) <- c("Land", "Station", "Schnee P1", "Schnee P2", "Schnee Differenz", "Anz. Tage P1", "Anz. Tage P2", "Tage Differenz", "Dauer in Tagen")

    # Spaltenüberschriften an Tabelle übergeben
      data


  })

# -------------------------- Value Box für Kennzahlen -----------------------------
  # reaktive valueBox zum Abschnitt Serie, je nach Auswahl des Select Inputs
  output$box1 <- renderValueBox({

    valueBox(
      value = HTML(paste(ekv$days_diff_pct)),
      subtitle = HTML(paste("weniger Schneetage gibt es in ganz Europa im Vergleich zur Referenzperiode:", br(), br(), ekv$days10_p1,"Tage vs.", ekv$days10_p2,  "Tage")),
      icon = icon("snowman", class = "white-icon"),
      color = "blue",
      width = 12
    )
  })
  
  output$box2 <- renderValueBox({
   
    valueBox(
     value = HTML(paste(ekv$snow_diff_pct)),
      subtitle = HTML(paste("weniger Schnee liegt durchschnittlich in ganz Europa:", br(), br(), br(), ekv$snow_p1, "cm vs.", ekv$snow_p2, "cm")),
      color = "blue",
      icon = icon("snowflake",class = "white-icon"),
      width = 12

    )
  })
  
  
  output$box3 <- renderValueBox({
    valueBox(
       value = HTML(paste(ekv$first_diff, "Tage")),
      subtitle = HTML(paste("später kommt der erste Schneetag in Europa:", br(), br(), br(), ekv$first_p1_date, "vs.", ekv$first_diff_date)),
      color = "blue",
      icon = icon("calendar-xmark", class = "white-icon"),
      width = 12
    )
  })
  
  
  output$box4 <- renderValueBox({
    valueBox(
      value = HTML(paste(ekv$dur_diff, "Tage")),
      subtitle = HTML(paste("so viel kürzer ist die Dauer zwischen dem ersten und dem letzten Schneetag:", br(), br(),br(), ekv$dur_p1, "Tage vs.", ekv$dur_p2, "Tage")),
      color = "blue",
      icon = icon("snowplow", class = "white-icon"),
      width = 12
    )
  })

  # ------------------------- LAND ------------------------------------------------------
  # -------------------------- Karte für Land --------------------------------
  output$map_country <- renderLeaflet({
    lng <- closest_stations_n()$lon %>% mean()
    lat <- closest_stations_n()$lat %>% mean()

    data <- closest_stations_n()
    input_data <- geocode_OSM(location(), details = TRUE)

    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = lng, lat = lat, zoom = 7) %>%
      addMarkers(
        data = data,
        # lng = closest_stations_n()$lon,
        lng = ~lon,
        # lat = closest_stations_n()$lat
        lat = ~lat,
        # popup = cssd()$stahght
        popup = ~stahght
      ) %>%
      addCircleMarkers(
        data = input_data,
        lng = input_data$coords[1],
        lat = input_data$coords[2],
        popup = input_data$display,
        color = "red"
      ) %>%
      addMarkers(
        data = input_data,
        lng = input_data$coords[1],
        lat = input_data$coords[2],
        popup = input_data$display
      )
  })

  # -------------------------- Tabelle für Land mit den angezeigten Stationen -----------------------------------
  output$table_country <- DT::renderDT({
    # data <- cssd()
    # data <- data %>% select(stahght, country, cn, staid) %>% distinct()
    data <- closest_stations_n() %>% 
      select(stahght, country, first_year, last_year, distanz) %>% 
      mutate(country = str_to_title(country)) %>% 
      mutate(distanz = round(distanz * 100, 1))
    # distkm <- glue("Distanz zu {input$country()} in km")

    # Spaltenüberschriften umbenennen
    colnames(data) <- c("Stationsname und Höhe", "Land", "Messdaten seit", "Messdaten bis", "Distanz in km")

    # "Distanz in km"
    # Spaltenüberschriften an Tabelle übergeben
      data
  })

  # ------------------------- einzelne Stationen ------------------------------------------------------------
  
  # ------------------ PLOTS ---------------------------------------
  # ---------------- Scatterplot----------------
output$snowdepth <- renderPlot({
  cssd_refperiod <- cssd()%>%
    filter(winter_season > input$years_rng2[1]&
             winter_season > input$years_rng2[2],
           season == "winter") %>%
    group_by(stahght, day_nr) %>%
    summarize(snow = mean(sd)) %>%
    mutate(rolling_avg = zoo::rollmean(snow, 7, na.pad = TRUE))

  ggplot(cssd()) +
    geom_point(aes(x = day_nr, y = sd), alpha = 0.5, shape = 8, color = "#78aed3") +
    geom_line(data = cssd_refperiod, aes(x = day_nr, y = rolling_avg), color = "#E9333F", linewidth = 1) +
    geom_line(data = subset(cssd(), winter_season == input$year), aes(x = day_nr, y = sd), color = "#1338BE", linewidth=1) +
    labs(x = "Monat",
         y = "Schneehöhe in cm",
         title = paste("Schneehöhen im Winterhalbjahr", input$year),
         subtitle = paste("rot: durchschnittlicher Verlauf Referenzperiode", input$years_rng2[1], "-", input$years_rng2[2],
         "\nblau: durchschnittlicher Verlauf der gewählten Periode", input$years_rng1[1], "-", input$years_rng1[2]),
         caption = "European Climate Assessment & Dataset projects (ECAD)") +
    scale_x_continuous(breaks = c(15,45,75,105,135,165,195,225),
                       labels = c("15" = "Sept", "45" = "Okt", "75" = "Nov", "105" = "Dez", "135" = "Jan", "165" = "Feb", "195" = "Mar", "225" = "Apr"))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 22),     # Größe des Titels
      plot.subtitle = element_text(size = 16),  # Größe des Untertitels
      axis.title.x = element_text(size = 16),   # Größe der x-Achsentitel
      axis.title.y = element_text(size = 16),   # Größe der y-Achsentitel
      legend.title = element_text(size = 14),   # Größe der Legendentitel
      legend.text = element_text(size = 12),    # Größe der Legendentext
      plot.caption = element_text(size = 12),    # Größe des Caption
      strip.text = element_text(size = 14),    # Größe des facet_wrap Titel
      axis.text.x = element_text(size = 12),   # Größe der x-Achsenticks
      axis.text.y = element_text(size = 12)    # Größe der y-Achsenticks
      )+
    facet_wrap(~stahght)
})

  #---------------- Wahrscheinlichkeit----------------
output$probability <- renderPlot({
  ref_per <- input$years_rng2
  ver_per <- input$years_rng1

  df_prob <- cssd() %>%
    filter((winter_season >= ref_per[1] & winter_season <= ref_per[2]) |
             (winter_season >= ver_per[1]  & winter_season <= ver_per[2]),
           season == "winter") %>%
    mutate(snow_slider = ifelse(sd > input$snow_height, 1, 0),
           period = ifelse(winter_season >= ref_per[1] & winter_season <= ref_per[2],
                           paste(ref_per[1], ref_per[2], sep = "-"),
                           paste(ver_per[1], ver_per[2], sep = "-" ))) %>%
    group_by(stahght, period, day_nr) %>%
    summarize(p_snow_slider= round(mean(snow_slider, na.rm = TRUE), 2)) %>%
    mutate(rolling_avg = zoo::rollmean(p_snow_slider, k=7, fill=NA, align='right'))

  ggplot(df_prob) +
    #geom_line(aes(day_nr, p_snow_slider,color = period)) +
    geom_line(aes(day_nr, rolling_avg,color = period), size = 1)+
    scale_x_continuous(breaks = c(breaks = c(1,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240)),
                       labels = c("1"= "", "15" = "Sept", "30" = "", "45" = "Okt", "60" = "","75" = "Nov", "90" = "",
                                  "105" = "Dez", "120" = "","135" = "Jan", "150" = "","165" = "Feb","180" = "",
                                  "195" = "Mar","210" = "",  "225" = "Apr", "240" = ""))+
    scale_color_manual(values = c("#E9333F", "#1338BE"),
                       labels = c(paste(ref_per[1], ref_per[2], sep = "-"),
                                  paste(ver_per[1], ver_per[2], sep = "-" )),
                       name = "Zeitraum" )+
    labs(title = "Schneewahrscheinlichkeit",
         subtitle = glue("Wahrscheinlichkeit in %, dass min. {input$snow_height} cm Schnee liegt"),
         x = "Monat",
         y = "Wahrscheinlichkeit in %",
         #labels = "Zeitraum",
         caption = "die Linien wurden mit einem rolling average gesmootht \nDatenquelle: European Climate Assessment & Dataset projects (ECAD)")+
    scale_y_continuous(labels = percent)+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 22),     # Größe des Titels
      plot.subtitle = element_text(size = 20),  # Größe des Untertitels
      axis.title.x = element_text(size = 16),   # Größe der x-Achsentitel
      axis.title.y = element_text(size = 16),   # Größe der y-Achsentitel
      legend.title = element_text(size = 14),   # Größe der Legendentitel
      legend.text = element_text(size = 12),    # Größe der Legendentext
      plot.caption = element_text(size = 12),    # Größe des Caption
      strip.text = element_text(size = 14),    # Größe des facet_wrap Titel
      axis.text.x = element_text(size = 12),   # Größe der x-Achsenticks
      axis.text.y = element_text(size = 12)    # Größe der y-Achsenticks
    )+
    facet_wrap(~stahght)
}) #end plot

  #-----------vergleichsplot-----------

  output$comparison <- renderPlot({
    
    ref_per <- input$years_rng2
    ver_per <- input$years_rng1
    
    monthly_avg_combined <- rbind(
      cssd() %>%
        filter(year >= ref_per[1] & year <= ref_per[2],
               season =="winter") %>%
        mutate(month = factor(month(date), levels = c(10,11,12,1,2,3,4),
                              labels = c("Okt", "Nov", "Dez", "Jan", "Feb", "Mar", "Apr")),
               snowday = ifelse(sd >= input$snow_height,1,0)) %>%
        group_by(stahght, month) %>%
        summarise(sd = sum(snowday)/(ref_per[2]-ref_per[1])) %>%
        mutate(dataset = "Monthly Average 1"),
      
      cssd() %>%
        filter(year >= ver_per[1] & year <= ver_per[2],
               season =="winter") %>%
        mutate(month = factor(month(date), levels = c(10,11,12,1,2,3,4),
                              labels = c("Okt", "Nov", "Dez", "Jan", "Feb", "Mar", "Apr")),
               snowday = ifelse(sd >= input$snow_height,1,0)) %>%
        group_by(stahght, month) %>%
        summarise(sd = sum(snowday)/(ver_per[2]-ver_per[1])) %>%
        mutate(dataset = "Monthly Average 2")
    )

    ggplot(monthly_avg_combined, aes(x = month, y = sd, fill = dataset)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7, size = 0.5) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.5)+
      labs(title = "Vergleich durchschnittliche Schneetage pro Monat",
           subtitle = paste("Relevante Schneehöhe: ", input$snow_height),
           caption = "European Climate Assessment & Dataset projects (ECAD)",
           x = "",
           y = "Durchschn. Anzahl Tage") +
      scale_fill_manual(values = c("Monthly Average 2" = "#1338BE", "Monthly Average 1" = "#E9333F"),
                        labels = c(paste(ref_per[1], ref_per[2], sep = "-"),
                                   paste(ver_per[1], ver_per[2], sep = "-" )),
                        name = "Zeitperiode")+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 22),     # Größe des Titels
        plot.subtitle = element_text(size = 20),  # Größe des Untertitels
        axis.title.x = element_text(size = 16),   # Größe der x-Achsentitel
        axis.title.y = element_text(size = 16),   # Größe der y-Achsentitel
        legend.title = element_text(size = 14),   # Größe der Legendentitel
        legend.text = element_text(size = 12),    # Größe der Legendentext
        plot.caption = element_text(size = 12),    # Größe des Caption
        strip.text = element_text(size = 14),    # Größe des facet_wrap Titel
        axis.text.x = element_text(size = 12),   # Größe der x-Achsenticks
        axis.text.y = element_text(size = 12)    # Größe der y-Achsenticks
      )+
      facet_wrap(~stahght)
    
  }) #end Plot Comparison

  #---------Ramin Schneesicherheit--------------

output$schneesicherheit <- renderPlot({

  cssd() %>%
    filter(sd > input$snow_height,
           year >= input$years_rng2[1] & year <= input$years_rng1[2]) %>%
    group_by(stahght, winter_season) %>%
    count() %>%
    ggplot(aes(winter_season, n))+
    geom_col(fill =  "#78aed3")+
    geom_smooth(method = "lm", se = FALSE, color = "#E9333F")+
    theme_minimal()+
    labs(title = glue("Anzahl Tage mit über {input$snow_height} cm Schnee"),
         x = "Jahr",
         y = "Anzahl Tage",
         caption = "European Climate Assessment & Dataset projects (ECAD)"
    )+
    theme(
      plot.title = element_text(size = 22),     # Größe des Titels
      plot.subtitle = element_text(size = 20),  # Größe des Untertitels
      axis.title.x = element_text(size = 16),   # Größe der x-Achsentitel
      axis.title.y = element_text(size = 16),   # Größe der y-Achsentitel
      legend.title = element_text(size = 14),   # Größe der Legendentitel
      legend.text = element_text(size = 12),    # Größe der Legendentext
      plot.caption = element_text(size = 12),    # Größe des Caption
      strip.text = element_text(size = 14),    # Größe des facet_wrap Titel
      axis.text.x = element_text(size = 12),   # Größe der x-Achsenticks
      axis.text.y = element_text(size = 12)    # Größe der y-Achsenticks
    )+
    facet_wrap(~stahght)
}) # end plot Schneesicherheit


  # ---------------- ANTONIA HIER PLOT plot_land_2 EINFÜLLEN -------------------
  

  output$plot_land_2 <- renderPlot({
    
    cssd() %>% 
      group_by(stahght, winter_season) %>%
      filter(season == "winter") %>% 
      summarize(snow = mean(sd)) %>% 
      ungroup() %>% 
      mutate(rolling_avg = zoo::rollmean(snow, 3,na.pad = TRUE)) %>% 
      ggplot(aes(winter_season, rolling_avg))+
      geom_smooth(method = "lm", color = "#E9333F", se = FALSE, linewidth = 1)+
      geom_line(color = "#78aed3", linewidth = 1)+
      theme_minimal()+
      labs(title = "Durchschn. Schneemenge pro Jahr",
           x = "Jahr",
           y = "Schneehöhe in cm",
           caption = "European Climate Assessment & Dataset projects (ECAD)"
      )+
      theme(
        plot.title = element_text(size = 22),     # Größe des Titels
        plot.subtitle = element_text(size = 20),  # Größe des Untertitels
        axis.title.x = element_text(size = 16),   # Größe der x-Achsentitel
        axis.title.y = element_text(size = 16),   # Größe der y-Achsentitel
        legend.title = element_text(size = 14),   # Größe der Legendentitel
        legend.text = element_text(size = 12),    # Größe der Legendentext
        plot.caption = element_text(size = 12),    # Größe des Caption
        strip.text = element_text(size = 14),    # Größe des facet_wrap Titel
        axis.text.x = element_text(size = 12),   # Größe der x-Achsenticks
        axis.text.y = element_text(size = 12)    # Größe der y-Achsenticks
      )+
      facet_wrap(~stahght)

  })
  # end plot plot_land_2

  # })

  

  
  
# ------------------ TESTS --------------------------------------------------
# TEST UI ZUR KONTROLLE DES TEXTINPUTS
  output$selection <- renderUI({
    paste(closest_stations_n()$staname)
    # paste(location())
    # paste(location_coords()[1])
    # paste(country())
      })


  # TEST KARTE
   output$map <- renderLeaflet({  # Render the leaflet map
     lng <- closest_stations_n()$lon %>% mean()
     lat <- closest_stations_n()$lat %>% mean()

    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = lng, lat = lat, zoom = 8) %>%
      addMarkers(
        data = closest_stations_n(),
        lng = ~lon,
        lat = ~lat
      )
  })

  } # end server