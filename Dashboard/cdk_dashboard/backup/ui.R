
dashboardPage(
   dashboardHeader( ),

  dashboardSidebar(
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
         ) # end HTML
        ) # end tags$style
      ), # end tags$head
          box(  title = "Auswahl",
                 # status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 width = 12,
                 align = "center",
                 # Eingabe für Ort oder Land (Freitext) und Anzahl Stationen
                 textInput("country", "Suche nach Ort oder Land", value = val_loc),
                 numericInput("n_stations", "Anzahl Stationen im Umkreis", value = 1, min = 1, max = 10),
                 # Slider für Höhe ü.M. der Station
                 # sliderInput("sta_hight", "Höhe ü.M.", min = 50, max = 3000, value = c(500, 1000), step = 500, sep = "`"),
                 # sliderInput("sta_hight", "Höhe ü.M.", min = min_sta, max = max_sta, value = val_sta, step = 500, sep = "`"),
                 # Slider für die Jahre
                 sliderInput("years","Jahre", min = 1990, max = 2025, value = c(2000, 2020), step = 1, sep = ""),
                 # sliderInput("years","Jahre", min = min_y, max = max_y, value = val_min_y, val_max_y, step = 1, sep = ""),
               # sliderInput("years","Jahre", min = min_y, max = max_y, value = val_min_y, val_max_y, step = 1, sep = ""),
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), # class = "menu-item"
                  menuItem("Entwicklung Schneefall", tabName = "snowdev", icon = icon("chart-bar")),
                  menuItem("Mein naechster Schneeurlaub", tabName = "map", icon = icon("map"))
                ) # end sidebarMenu
          ) # end box
  ), # end dashboardSidebar
  dashboardBody(
       # ----------------------------- Dashboard ----------------------------------------
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 12,
                       # ----------- Info Box fuer ausgewaehlte Optionen -----------
                       box(
                         title = "Ausgewaehlte Optionen",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 4,
                         # -------------- Ausgewaehlter Ort --------------
                         infoBoxOutput("selected_country", width = 12),
                          # -------------- Ausgewaehlter Zeitraum --------------
                          # infoBoxOutput("selected_years", width = 12),
                          # -------------- Ausgewaehlte Hoehe --------------
                          # infoBoxOutput("selected_height", width = 12),
                         # ---------------- Anzahl Stationen ----------------
                         infoBoxOutput("numStation", width = 12)
                       ), # end box
                       div(class = "fixed-value-box",
                           valueBoxOutput("max_month2", width = 4)
                       # valueBoxOutput("min_month", width = 4)
                       ) # end div
                     ) # end column
              ), # end fluidRow
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
              # ----------------------------- Karte mit dem Land mit visiblen Stationen -----------------
              fluidRow(
                column(width = 12,
                       tags$h3("Verteilung der Stationen über das ausgewählte Land"),
                       # ----------- Tabelle für Map Boundaries -----------
                       tableOutput("boundaries"),
                       # ----------- Karte mit den ausgewählten Ländern ------------
                       leafletOutput("map_visible_stations"),
                       tags$h3("Namen der Messstationen"),
                       # ----------- Tabelle für sichtbare Stationen -----------
                       tableOutput("tabletest")
                ) # end column
              ) # end fluiRow
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
              #   ) # end column
              # ) # end fluiRow
      #
      ), # end tabItem dashboard
        # ----------------------------- Schneefall Entwicklung ----------------------------------------
      tabItem(tabName = "snowdev",
              # --------------- Plot für Schneefall Entwicklung ----------------

          column(12,
                h2(""),
                div(style = "margin-bottom: 20px;"),
                p(""),

                p(""),
                #---------------------Plot Antonia: Balkenplot Abweichung zum Durchschnitt pro Jahr  -------------------------------
                plotOutput("columns_difference"),
                p("")
                ), # end column
              column(12,
                #---------------------Plot Antonia: Scatterplot Vergleich letztes Jahr vs alle Jahre -------------------------------
                h3(""),
                p(""),
                plotOutput("scatterplot_years"),
                div(style = "margin-bottom: 40px;")
              # ), # end column
              # column(12,
              #   # ------------------------ Plot 3 ------------------------
              #   h3(""),
              #   p(""),
              #   plotOutput("plot3"),
              #   div(style = "margin-bottom: 30px;")
              ) # end column
        ) # end tabItem snowdev
  ) # end dashboardBody
) # end dashboardPage


