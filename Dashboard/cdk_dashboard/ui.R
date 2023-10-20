
dashboardPage(
   dashboardHeader(),

  dashboardSidebar(
    title = tags$img(src = "snow_dashboard.jpeg", height = 59, style = "display: block; margin: auto; margin-bottom: 20px"),
     tags$head(
      tags$style(
        HTML(
          "
          .skin-blue .main-sidebar {
            width: 200px;
          }
     #      # .main-header {
     #      #   margin-left: 300px; /* Setzen des linken Rands auf die Breite der Sidebar */
     #      # }
          .sidebar {
            width: 200px;
          }
     #      # .content-wrapper {
     #      #   margin-left: 300px;
     #      # }
          h5 {
          font-color: #ffffff;
          margin-left: 15px;
          font-weight: bold;
          }

          "
         ) # end HTML
        ) # end tags$style
      ), # end tags$head
           # Eingabe für Ort oder Land (Freitext) und Anzahl Stationen
               sidebarMenu(
                fluidRow(
                  column(12, textInput("country", "Geben Sie einen Ort oder Land in Europa ein.", value = val_loc))
                ), # end fluidRow
                actionButton("search", "Suche"),
                h5(""),
                 # numericInput("n_stations", "Wie viele Stationen wollen Sie sehen?", value = 1, min = 1, max = 4),
                selectInput("n_stations", "Wie viele Stationen wollen Sie sehen?", choices = 1:4, selected = 1),
                sliderInput("snow_height", "Wählen Sie die Schneehöhe aus, die Sie interessiert.", min = 0, max = 50, value = 5, step = 1, sep = ""),
                h5("Sie wollen vergleichen?"),
                h5("Kein Problem!"),
                sliderInput("years_rng1","Wählen Sie hier die Jahre aus, die Sie vergleichen wollen:", min = min_y_1, max = max_y_1, value = c((max_y_1 - 10), max_y_1), step = 1, sep = ""),
                sliderInput("years_rng2","Und hier wählen Sie die Referenz aus:", min = min_y_2, max = max_y_2, value = c(val_min_y_2, (val_min_y_2 + 30)), step = 1, sep = ""),
                uiOutput("dependent_slider")
                # uiOutput("selection")
           ) # end sidebarMenu
  ), # end dashboardSidebar
  dashboardBody(
    tags$head(
   tags$style(
     HTML("
 @font-face {
         font-family: 'Helvetica';
         src: url('resources/Helvetica.ttf') format('truetype');
      }

         h1, h2, h3, h4, p {
          font-family: 'Helvetica', sans-serif;
        }

       p {
         font-size: 18px;
        color: #000000;
        }

      h1 {
         font-size: 22px;
        color: #000000;
        # margin-left: 5px;
        font-weight: bold;
      }

      h2 {
        font-size: 22px;
      color: #000000;
      font-weight: bold;
      margin-left: 15px;
      
      }

    h3 {
      font-size: 22px;
      color: #000000;
      font-weight: bold;
      vertical-align: middle;
    }

    h4 {
     font-size: 18px;
     color: #000000;
    }
    
    h6 {
    font-size: 16px;
    color: #000000;
    margin-left: 15px;
    }

    .my-value-box {
    margin: 10px; /* abstand um die Box */
    vertical-align; top;
    font-family: 'Helvetica', sans-serif;
    font-color: #ffffff;
    }
    .white-icon {
        color: #ffffff;
        font-size: 70%;
        padding-right: 10px;
      }

    .fixed-value-box .small-box .small-box-footer,
    .fixed-value-box .small-box .icon-large {
      font-family: 'Helvetica', sans-serif;
      # display: none;
      vertical-align: middle;
      horizontal-align: middle;
    }

    .fixed-value-box .small-box {
      font-family: 'Helvetica', sans-serif;
      height: 200px;
      width: 350px;
    }

    .fixed-value-box .small-box .inner h3 {
      font-family: 'Helvetica', sans-serif;
      font-size: 30px;
      color: #FFFFFF;

    }

    .fixed-value-box .small-box .inner p {
      font-family: 'Helvetica', sans-serif;
      font-size: 18px;
      color: #FFFFFF;
      height: 100px;
      width: 330px;
    }

    .fixed-value-box .small-box .inner h3 {
      font-size: 28px;
    }

    .fixed-value-box .small-box .inner p {
      font-size: 18px;
    }
    .right-float {
        float: right;
      }

    .slider-label {
    font-size: 20px;
    }
           ") # end HTML
     ) # end style
    ) # end head
        ,
               # ----------------------------- Dashboard Body -----------------
             
                h1("Die Folgen des Klimawandels auf den Schnee"),
                # br(),
                h4("Das ist das Dashboard der Klimadaten-Challenge der Fachhochschule 
                   Nordwestschweiz. Hier können Sie sehen und erforschen, wie sich die Schneehöhe in Europa über 
                   die Jahre verändert hat."),
              
                 fluidRow(
                  tabBox(
                    title = "",
                    id = "tabbox_all_boxes",
                    height = "1500px",
                    width = 12,
                    # ---------------------------- Europa ---------------------------
                    tabPanel("Europa",

                tabBox(height = "600px",
                    tabPanel("Karte",
                       tags$h3("Messstationen in Europa"),
                       # ----------- Karte mit Europa Übersicht -----------
                       leafletOutput("map_europe", height = "500px")
                    ) # end tabPanel   
                       ,
                    tabPanel("Tabelle",
                       # ----------- Karte mit den ausgewählten Ländern ------------
                       # leafletOutput("map_visible_stations"),
                       tags$h3("Tabellarische Übersicht der Messstationen"),
                       h6(HTML(paste("P1 = Periode von 1950 - 1980, P2 = Periode ab 2010", br(),
                                     "Schnee bedeutet durchschnittliche Schneehöhe pro Tag im Winter", br(),
                                     "Tage bedeuten Anzahl Tage über 11,2cm Schnee"))),
                       # ----------- Tabelle für sichtbare Stationen -----------
                       # tableOutput("tabletest")
                       DT::DTOutput("table_europa")
                    ) # end tabPanel
                ) # end tabBox
               ,
                             # --------------------- Box mit Kennzahlen ---------------------
                             tabBox(height = "600px",
                              tabPanel("Kennzahlen",
                           fluidRow(
                             tags$h2("Kennzahlen über ganz Europa"),
                             tags$h6(glue("Verglichen werden die Mittelwerte der Referenzperiode (1950 - 1980) mit den Mittelwerten ab 2010. 
                                     Als Anzahl Schneetage wurden die Tage gezählt, welche eine Mindestschneehöhe von {ekv$snow_p1}cm 
                                     (Durchschnitt der Referenzperiode) aufweisen.")),
                             div(class = "fixed-value-box",
                             valueBoxOutput("box1"),
                             div(class = "right-float",
                             # h3("h3 Titel"),
                             valueBoxOutput("box2")
                             ) # end div
                           ) # end div
                    ), # end fluidRow
                           fluidRow(
                              div(class = "fixed-value-box",
                             valueBoxOutput("box3"),
                                 div(class = "right-float",
                             valueBoxOutput("box4")
                            ) # end div
                                  ) # end div
                ) # end fluidRow
                  ) # end tabPanel
                    ) #end tabBox
                 # ------------------- Ende Box mit Kennzahlen zu Europa ---------------

                    ) # end tabPanel Europa
                    # ----------------------------- Ende Europa -----------------
                    ,
                    # ----------------------------- Land -----------------
                  tabPanel("Land",
                           # --------------------------- Box mit Karte  -----------------
                tabBox(height = "900px",
                       fluidRow(
                         column(12,
                                h3("Nächste Stationen zu Ihrem ausgewählten Ort")
                                ) # end fluidRow
                                ), # end column
                       # ----------- Karte mit ausgewähltem Ort und nächsten Stationen ------------
                    tabPanel("",
                      # uiOutput("tab_title_country"),
                             # tags$h4("Nächste Stationen zu Ihrem ausgewählten Ort"),
                             leafletOutput("map_country", height = "500px"),
                             br(),
                      # ----------- Tabelle für sichtbare Stationen -----------
                             tags$h4("Liste der Stationen"),
                             br(),
                             DT::DTOutput("table_country")
                    ) # end tabPanel
                    ) # end tabBox
                           ,
                           tabBox(height = "900px",
                           fluidRow(
                             column(12,
                  # ------------------- Plots für Land ------------------------------
                                    h3("Entwicklung der Schneemenge und Schneetage")
                             # h4("Anzahl Tage mit mehr als 0cm Schnee")
                             ) # end column
                           ) # end fluidRow
                           ,
                           tabPanel("")
                           ,
                           fluidRow(
                             br(),
                                    plotOutput("schneesicherheit", height = "300px")
                                    ) # end fluidRow
                           ,
                           fluidRow(
                                    plotOutput("plot_land_2")
                           ) # end fluidRow
                           ) # end tabBox
                    ) # end tabPanel gewähltes Land
                    # ----------------------------- Ende gewähltes Land -----------------
                    ,
                    # ----------------------------- einzelne Station ----------------
                    tabPanel("Stationen",
                      fluidRow(
                        column(12,
                        h3("Verlauf der Wintersaison")
                                ) # end column
                      ) # end fluidRow
                            ,
                      fluidRow(
                        column(12,
                               h4("Wie hat sich der Verlauf der Saison für die gewählten Stationen entwickelt?"))
                      ),
                           fluidRow(
                             column(12,
                          plotOutput("snowdepth")
                             ) # end column
                           ) # end fluidRow
                        ,

                        fluidRow(
                          column(12,
                          br(),
                          plotOutput("probability")
                          ) # end column
                        ) # end fluidRow
                          ,
                        fluidRow(
                          column(12,
                          br(),
                          plotOutput("comparison")
                          )
                      )# end fluidRow
                             # ----------------------------- Ende einzelne Station ----------------
                    ) # end tabPanel einzelne Station
                    # ----------------------------- Ende "tabbox_all_boxes" TabBox ----------------
                  ) # end tabBox
                ) # end tabBox all boxes
  ) # end dashboardBody
) # end dashboardPage


