
dashboardPage(
  # dashboardHeader(title = "Schneefall Dashboard"),
   dashboardHeader(
      # tags$div(
      # class = "logo",
      # style = "text-align: center;",
      # tags$img(src = "snow_dashboard.jpeg", height = 50))
    ),
  #   # title = tags$img(src = "snow_dashboard.jpeg", height = 50, style = "display: block; margin: 0 auto;"),
  #    tags$head(
  #     tags$style(
  #       HTML(
  #         "
  #         .skin-blue .main-sidebar {
  #           width: 300px;
  #         }
  #         .main-header {
  #           margin-left: 300px; /* Setzen Sie den linken Rand auf die Breite der Sidebar */
  #         }
  #         .sidebar {
  #           width: 300px;
  #         }
  #         .content-wrapper {
  #           margin-left: 300px;
  #         }
  #         "
  #       )
  #     )
  #   )
  # ),
  dashboardSidebar(
  # tags$div(
  #     class = "logo",
  #     style = "text-align: center;",
  #     tags$img(src = "snow_dashboard.jpeg", height = 50)
  #   ),
    title = tags$img(src = "snow_dashboard.jpeg", height = 69, style = "display: block; margin: auto; margin-bottom: 20px"),
     tags$head(
      tags$style(
        HTML(
          "
          .skin-blue .main-sidebar {
            width: 300px;
          }
          .main-header {
            margin-left: 300px; /* Setzen des linken Rands auf die Breite der Sidebar */
          }
          .sidebar {
            width: 300px;
          }
          .content-wrapper {
            margin-left: 300px;
          }

          "
        )
      )

  ),
    box(
                 title = "Auswahl",
                 # status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 width = 12,
                 align = "center",
                 # Eingabe fuer Ort oder Land (Freitext)
                 textInput("country", "Ort oder Land", value = val_loc),
                 # Slider fuer Hoehe ue.M. der Station
                 sliderInput("sta_hight", "Hoehe ue.M.", min = 50, max = 3000, value = c(500, 1000), step = 500, sep = "`"),
                 # sliderInput("sta_hight", "Hoehe ue.M.", min = min_sta, max = max_sta, value = val_sta, step = 500, sep = "`"),
                 # Slider fuer die Jahre
                 sliderInput("years","Jahre", min = 1990, max = 2025, value = c(2000, 2020), step = 1, sep = ""),
                 # sliderInput("years","Jahre", min = min_y, max = max_y, value = val_min_y, val_max_y, step = 1, sep = ""),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), # class = "menu-item"
      menuItem("Entwicklung Schneefall", tabName = "snowdev", icon = icon("chart-bar")),
      menuItem("Mein naechster Schneeurlaub", tabName = "map", icon = icon("map"))
    ) # end sidebarMenu
  ) # end box
  ), # end dashboardSidebar
  # dashboardBody()
  dashboardBody(
    uiOutput("test")
    # tags$head(
    # tags$style(HTML("
    #      .fixed-value-box .small-box .small-box-footer,
    #     # .fixed-value-box .small-box .icon-large {
    #     #   display: ;
    #     # }
    #     .fixed-value-box .small-box {
    #       height: 100px;
    #       width: 300px;
    #       # height: 200px;
    #       # width: 600px;
    #     }
    #     .fixed-value-box .small-box .inner h3 {
    #       # font-size: 20px;
    #       font-size: 36px;
    #     }
    #     .fixed-value-box .small-box .inner p {
    #       # font-size: 14px;
    #       font-size: 20px;
    #     }
    #     .fixed-box .large-box {
    #       height: 600px;
    #       width: 800px;
    #     }
    #     .slider-label {
    #     font-size: 20px;
    #     }
    #     .output-class {
    #     font-size: 20px;
    #     font-weight: normal;
    #     }
    #         ") # end HTML
    #  ) # end style
    # ), # end head
    # tabItems(
    # #   # ----------------------------- Dashboard ----------------------------------------
    #   tabItem(tabName = "dashboard",
    #           fluidRow(
    #             column(width = 12,
    #                    # ----------- Info Box fuer ausgewaehlte Optionen -----------
    #                    box(
    #                      title = "Ausgewaehlte Optionen",
    #                      status = "primary",
    #                      solidHeader = TRUE,
    #                      collapsible = TRUE,
    #                      width = 4,
    #                      # -------------- Ausgewaehlter Ort --------------
    #                      infoBoxOutput("selected_country", width = 12),
    #                       # -------------- Ausgewaehlter Zeitraum --------------
    #                       # infoBoxOutput("selected_years", width = 12),
    #                       # -------------- Ausgewaehlte Hoehe --------------
    #                       # infoBoxOutput("selected_height", width = 12),
    #                      # ---------------- Anzahl Stationen ----------------
    #                      # infoBoxOutput("numStation", width = 12),
    #                    ) # end box
              #          div(class = "fixed-value-box",
              #              valueBoxOutput("max_month", width = 4),
              #          # valueBoxOutput("min_month", width = 4)
              #          ) # end div
              #        ) # end column
              # ) # end fluidRow
              # fluidRow(
              #   column(width = 12,
              #          div(class = "fixed-value-box",
              #          # ----------- Value Box fuer durchschnittlicher Schneefall -----------
              #          valueBoxOutput("meanBox"),
              #          # ----------- Value Box fuer maximaler Schneefall -----------
              #          valueBoxOutput("maxSdBox"),
              #          # ----------- Value Box fuer minimaler Schneefall -----------
              #          valueBoxOutput("minSdBox")
              #          # # ----------- Value Box fuer Anzahl Stationen -----------
              #          # valueBoxOutput("numStationBox"))
              #          ))),
              # # ----------------------------- Karte mit dem Land mit visiblen Stationen -----------------
              # fluidRow(
              #   column(width = 12,
              #          tags$h3("Verteilung der Stationen über das ausgewählte Land"),
              #          # ----------- Tabelle für Map Boundaries -----------
              #          tableOutput("boundaries"),
              #          # ----------- Karte mit den ausgewählten Ländern ------------
              #          leafletOutput("map_visible_stations"),
              #          tags$h3("Namen der Messstationen"),
              #          # ----------- Tabelle für sichtbare Stationen -----------
              #          tableOutput("in_bounds")
              #   ) # end column
              # ), # end fluiRow
              # 
              # # ----------------------------- Karte mit dem Ausgewählten Land -----------------
              # fluidRow(
              #   column(width = 12,
              #          tags$h3("Stationen im Umkreis der gesuchten Ortschaft: "),
              #          # ----------- Tabelle für Map Boundaries -----------
              #          tableOutput("boundaries"),
              #          # ----------- Karte mit den ausgewählten Ländern ------------
              #          leafletOutput("map_selected_country"),
              #          tags$h3("Namen der Messstationen"),
              #          # ----------- Tabelle für sichtbare Stationen -----------
              #          tableOutput("in_bounds")
      #           ) # end column
      #         # ) # end fluiRow
      #         # 
      #         
      # ) # end tabItem dashboard
    #     # ----------------------------- Schneefall Entwicklung ----------------------------------------
    #   tabItem(tabName = "snowdev",
    #           # # ----------------------------- Karte mit dem Ausgewaehlten Land -----------------
    #           #  fluidRow(
    #           #   tags$h4("Map Boundaries"),
    #           #   # ----------- Tabelle fuer Map Boundaries -----------
    #           #   tableOutput("boundaries"),
    #           #   # ----------- Karte mit den ausgewaehlten Laendern ------------
    #           #   leafletOutput("map_selected_country"),
    #           #   tags$h4("Stations visible"),
    #           #   # ----------- Tabelle fuer sichtbare Stationen -----------
    #           #   tableOutput("in_bounds")
    #           # ),
    #           fluidRow(
    #             column(width = 8,
    #                    box(
    #                      title = "Einstellungen",
    #                      status = "primary",
    #                      solidHeader = TRUE,
    #                      collapsible = TRUE,
    #                      # selectInput("snowdev_country", "Land", choices = country_choices, selected = "Switzerland"),
    #                      # sliderInput("snowdev_year_range1",   "Jahresintervall 1", min = min(years), max = max(years), value = c(min(years), min(years)+20), step = 1, sep = ""),
    #                      sliderInput("snowdev_year_range2",   "Vergleichsperiode", min = min(years), max = max(years), value = c(max(years)-20, max(years)), step = 1, sep = "")
    #                    ))
    #           ),
    #           fluidRow(
    #             column(width = 12,
    #                    infoBoxOutput("selected_country2"),
    #                    valueBoxOutput("sdBox2"),
    #                    valueBoxOutput("maxSdBox2"),
    #                    valueBoxOutput("minSdBox2"),
    #                    valueBoxOutput("numStationBox2")
    #             )),
    #           fluidRow(
    #             tags$h4("Map Boundaries"),
    #             tableOutput("boundaries2"),
    #             leafletOutput("map_selected_country2"),
    #             tags$h4("Stations visible"),
    #             tableOutput("in_bounds2")
    #           )
    #   ), # end tabItem snowdev
    #     # ----------------------------- Map ----------------------------------------
    #   tabItem(tabName = "map",
    #           fluidRow(
    #             column(width = 8,
    #                    box(
    #                      title = "Waehle aus",
    #                      status = "primary",
    #                      solidHeader = TRUE,
    #                      collapsible = TRUE,
    #                      selectInput("country3", "Land", choices = country_choices, selected = "Switzerland"),
    #                      sliderInput("year_range3",   "Jahresintervall", min = min(years), max = max(years), value = c(max(years)-20, max(years)), step = 1, sep = "")
    #                    ))
    #           ),
    #           fluidRow(
    #             column(width = 12,
    #                    infoBoxOutput("selected_country3"),
    #                    valueBoxOutput("sdBox3"),
    #                    valueBoxOutput("maxSdBox3"),
    #                    valueBoxOutput("minSdBox3"),
    #                    valueBoxOutput("numStationBox3")
    #             )),
    #           fluidRow(
    #             tags$h4("Map Boundaries"),
    #             tableOutput("boundaries3"),
    #             leafletOutput("map_selected_country3"),
    #             tags$h4("Stations visible"),
    #             tableOutput("in_bounds3")
    #           )
      # ) # end tabItem map
    # ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


