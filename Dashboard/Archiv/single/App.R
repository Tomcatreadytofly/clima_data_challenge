library(shinydashboard)
library(leaflet)
library(shiny)
library(markdown)
library(RPostgres)
library(DBI)
library(tidyverse)
library(lubridate)
library(glue)
library(RColorBrewer)
library(celestial)
library(DataExplorer)



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

country <- dbListTables(conn)
patterns <- c("snow", "cdk")
country <- country[!grepl(paste(patterns, collapse = "|"), country)]
country <- sort(country)
columns <- dbListFields(conn, "cdk")
columns <- sort(columns)
years <- dbGetQuery(conn, glue("SELECT DISTINCT (year) FROM cdk ORDER BY (year)"))
years <- as.integer(years$year)

# ch_stations_all <- dbGetQuery(conn, glue("SELECT DISTINCT (staname), (staid), (lon), (lat) FROM switzerland ORDER BY (staname)"))
# ch_stations_all <- ch_stations_all %>% arrange(staname)
# country_stations <- dbGetQuery(conn, glue("SELECT DISTINCT (country), (staname), (staid), (lon), (lat) FROM cdk ORDER BY (country)"))
# country_stations <- country_stations %>% arrange(country, staname)
# country_coords <- country_stations %>%  group_by(country) %>% summarise(lon = mean(lon), lat = mean(lat)) %>% distinct() %>% ungroup()
# country_coords <- country_coords %>% arrange(country)
# ch_coords <- ch_stations_all %>%  summarise(lon = mean(lon), lat = mean(lat)) %>% distinct()

# Selection for testing dashboard with 10000 rows
# test_data_10000 <- dbGetQuery(conn, glue("SELECT * FROM switzerland WHERE (year) > 2015 LIMIT 10000"))
# test_data_10000 <- test_data_10000 %>% mutate_at(c("year", "month", "winter_season"), as.integer)
# glimpse(years)

# Choices for dropdown menu
country_choices <- str_to_title(country)

# add "all countries" to country_choices
country_choices <- c("All Countries", country_choices)


ui <- dashboardPage(
dashboardHeader(title = "Schneefall Dashboard"),
# dashboardSidebar(),
dashboardSidebar(
  # selectInput("country", "Country", choices = country_choices, selected = "Switzerland"),
  sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#                             # menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
)),
dashboardBody(
    tabItems(
    tabItem(tabName = "dashboard",
              box(
                title = "Wähle aus",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput("country", "Land", choices = country_choices, selected = "Switzerland"),
                sliderInput("year_range",   "Jahresintervall", min = min(years), max = max(years), value = c(max(years)-20, max(years)), step = 1, sep = "")
            ),
            fluidRow(
              column(width = 12,
           infoBoxOutput("selected_country"),
              valueBoxOutput("sdBox"),
              valueBoxOutput("maxSdBox"),
              valueBoxOutput("minSdBox"),
              valueBoxOutput("numStationBox")
            )),
            fluidRow(
              tags$h4("Map Boundaries"),
              tableOutput("boundaries"),
              leafletOutput("map_selected_country"),
              tags$h4("Stations visible"),
              tableOutput("in_bounds")
            )
    ),
    tabItem(tabName = "plot",
      fluidRow(
        selectInput("country_plot", "Land", choices = country_choices, selected = "Switzerland"),
        sliderInput("year_range_plot1",   "Jahresintervall 1", min = min(years), max = max(years), value = c(max(years)-20, max(years)), step = 1, sep = ""),
        sliderInput("year_range_plot2",   "Jahresintervall 2", min = min(years), max = max(years), value = c(max(years)-40, max(years)-20), step = 1, sep = ""),
        plotOutput("plot1"),
        plotOutput("plot2"),
        plotOutput("plot3")

      )

    )
)))



server <- function(input, output, session) {

  df_country_filtered <- reactive({
    inputcountry <- tolower(input$country)
    # if statement to select all countries
    if (input$country == "All Countries") {
      df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (country), (date), (month), (year), (sd),(winter_season) FROM cdk WHERE year >= input$year_range[1] & year <= input$year_range[2]")))
    } else {
    df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (date), (month), (year), (sd),(winter_season) FROM {inputcountry} WHERE year >= input$year_range[1] & year <= input$year_range[2]")))
      }
  })
  
  df_sta_filtered <- reactive({
    inputcountry <- tolower(input$country)
    #if statement to select all countries
    if (input$country == "All Countries") {
      df_sta_filtered <- data.frame(dbGetQuery(conn, glue("SELECT DISTINCT (country) (staname), (staid), (lon), (lat) FROM cdk")))
    } else {
    df_sta_filtered <- data.frame(dbGetQuery(conn, glue("SELECT DISTINCT (staname), (staid), (lon), (lat) FROM {inputcountry}")))
  }
  })

  stainbounds <- reactive({
    if (is.null(input$map_bounds))
      return(df_sta_filtered()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    data.frame(subset(df_sta_filtered(), lat >= latRng[1] & lat <= latRng[2] & lon >= lngRng[1] & lon <= lngRng[2]))
  })

  output$boundaries <- renderTable({
    input$map_bounds
  })

  output$in_bounds <- renderTable({
    stainbounds()
  })

 output$map_selected_country <- renderLeaflet({
   lon_mean <- mean(df_sta_filtered()$lon)
   lat_mean <- mean(df_sta_filtered()$lat)
   leaflet() %>%
        addTiles() %>%
        setView(lng = lon_mean, lat = lat_mean, zoom = 6) %>%
        addMarkers(data = df_sta_filtered(), lng = ~lon, lat = ~lat, popup = ~staname, clusterOptions = markerClusterOptions())
 })

  output$selected_country <- renderInfoBox({
    # val <- input$country
    infoBox(
      title = "Ausgewählte Optionen",
      # value = val,
      value = HTML(paste("Ausgewähltes Land:", input$country, "<br>",
                    "Ausgewählte Jahre:", input$year_range[1], "-", input$year_range[2])),
      icon = icon("globe"),
      color = "blue",
      width = 12
    )
  })

  output$sdBox <- renderValueBox({
    val <- round(mean(df_country_filtered()$sd), 2)
    valueBox(
      value = val,
      subtitle = "Durchschnittliche Schneehöhe",
      icon = icon("snowflake"),
      color = "blue",
      width = 4
    )
  })
  
  output$maxSdBox <- renderValueBox({
    val <- round(max(df_country_filtered()$sd), 2)
    valueBox(
      value = val,
      subtitle = "Maximale Schneehöhe",
      icon = icon("snowflake"),
      color = "blue",
      width = 4
    )
  })

  output$minSdBox <- renderValueBox({
    val <- round(min(df_country_filtered()$sd), 2)
    valueBox(
      value = val,
      subtitle = "Minimale Schneehöhe",
      icon = icon("snowflake"),
      color = "blue",
      width = 4
    )
  })

    output$numStationBox <- renderValueBox({
        val <- nrow(df_sta_filtered())
        valueBox(
        value = val,
        subtitle = "Anzahl Stationen",
        icon = icon("globe"),
        color = "yellow",
        width = 4
        )
    })

  df_country_plot1 <- reactive({
    inputcountry <- tolower(input$country_plot)
    # if statement to select all countries
    if (input$country_plot == "All Countries") {
      df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (country), (date), (month), (year), (sd),(winter_season) FROM cdk WHERE year >= input$year_range_plot1[1] & year <= input$year_range_plot1[2]")))
    } else {
    df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (date), (month), (year), (sd),(winter_season) FROM {inputcountry} WHERE year >= input$year_range_plot1[1] & year <= input$year_range_plot1[2]")))
      }
  })

  df_country_plot2 <- reactive({
    inputcountry <- tolower(input$country_plot)
    # if statement to select all countries
    if (input$country_plot == "All Countries") {
      df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (country), (date), (month), (year), (sd),(winter_season) FROM cdk WHERE year >= input$year_range_plot2[1] & year <= input$year_range_plot2[2]")))
    } else {
    df_country_filtered <- data.frame(dbGetQuery(conn, glue("SELECT (date), (month), (year), (sd),(winter_season) FROM {inputcountry} WHERE year >= input$year_range_plot2[1] & year <= input$year_range_plot2[2]")))
      }
  })
  # scatterplot mit zusätzlicher Linie für den Durchschnitt und Bereich für die Standardabweichung
    output$plot1 <- renderPlot({
      ggplot(df_country_plot1(), aes(x = date, y = sd)) +
        geom_point() +
        geom_smooth() +
        geom_hline(yintercept = mean(df_country_plot1()$sd), color = "red", linetype = "dashed") +
        geom_ribbon(aes(ymin = mean(df_country_plot1()$sd) - sd(df_country_plot1()$sd), ymax = mean(df_country_plot1()$sd) + sd(df_country_plot1()$sd)), alpha = 0.2) +
        labs(x = "Jahr", y = "Schneehöhe in cm", title = "Schneehöhe über die Jahre") +
        theme(plot.title = element_text(hjust = 0.5))
    })


  # barplot mit zusätzlicher Linie für den Durchschnitt
    output$plot2 <- renderPlot({
        ggplot(df_country_plot2(), aes(x = date, y = sd)) +
        geom_bar(stat = "identity", fill = "blue") +
        geom_smooth() +
        geom_hline(yintercept = mean(df_country_plot2()$sd), color = "red", linetype = "dashed") +
        labs(x = "Jahr", y = "Schneehöhe in cm", title = "Schneehöhe über die Jahre") +
        theme(plot.title = element_text(hjust = 0.5))
    })

  # histogram mit Start an der Line des Durchschnittes damit alle Werte über dem Durchschnitt nach oben gehen und alle Werte unter dem Durchschnitt nach unten.
    output$plot3 <- renderPlot({
        ggplot(df_country_plot2(), aes(x = sd)) +
        geom_histogram(binwidth = 1, fill = "blue") +
        geom_vline(xintercept = mean(df_country_plot2()$sd), color = "red", linetype = "dashed") +
        labs(x = "Schneehöhe in cm", y = "Anzahl", title = "Schneehöhe über die Jahre") +
        theme(plot.title = element_text(hjust = 0.5))
    })
}

shinyApp(ui, server)
