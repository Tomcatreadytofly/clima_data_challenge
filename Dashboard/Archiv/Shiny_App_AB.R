library(shiny)
library(RPostgres)
library(DBI)
library(tidyverse)
library(lubridate)
library(shiny)
library(glue)
library(leaflet)
#library(RColorBrewer)


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

ui <- fluidPage(
  tags$h1("Average Snow per year and month"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Please select period:", min = 1900, max = 2023, value = c(1930, 1940), step = 1),
      sliderInput(inputId = "year2", label = "Please select second period:", min = 1900, max = 2023, value = c(1990, 2000), step = 1),
      tags$h4("Map Boundaries"),
      tableOutput("boundaries"),
      leafletOutput(outputId = "map"),
      tags$h4("Stations visible"),
      tableOutput("in_bounds")

    ),
    mainPanel(
  plotOutput("plot1"),
  plotOutput("plot2"))
)
)


server <- function(input, output) {

  stations <- dbGetQuery(conn, "select distinct(staid), lat, lon, staname from snow")
  stinbounds <- reactive({
    if (is.null(input$map_bounds))
      return(stations[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    data.frame(subset(stations,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2]))
    })

  # rendering 2 tables (Stations in Bound, map boundaries)
  output$boundaries <- renderTable({input$map_bounds})
  output$in_bounds <- renderTable(stinbounds())

  # Render map
  output$map <- renderLeaflet({
      leaflet(data = stations) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng = 23, lat = 60, zoom = 6) %>% 
      addMarkers(~lon, ~lat, label = ~staname ,
                 clusterOptions = markerClusterOptions()) 
      

  })
  
  df1 <- reactive({
    staid_as_string <- paste(as.character(stinbounds()$staid), collapse = ",")
    df <- dbGetQuery(conn, glue("SELECT * FROM cdk WHERE year between {input$year[1]} and {input$year[2]} and staid in ({staid_as_string})"))
    data.frame(df)
  })
  
  
  df2 <- reactive({
    # Connect to the DB
    staid_as_string <- paste(as.character(stinbounds()$staid), collapse = ",")
    df <- dbGetQuery(conn, glue("SELECT * FROM cdk WHERE year between {input$year2[1]} and {input$year2[2]} and staid in ({staid_as_string})"))
    data.frame(df)
  })
  
  
 output$plot1 <- renderPlot({
   df_st <- stinbounds()
   avg_snow <- df1() %>% 
     filter(staid %in% df_st$staid, month %in% c(1,2,3,4,10,11,12)) %>% 
     mutate(month = factor(month, levels = c(10,11,12,1,2,3,4))) %>% 
     group_by(staid, year, month) %>% 
     summarize(avg_snow = mean(sd))
    ggplot(avg_snow, aes(month, avg_snow, fill = staid))+ 
      geom_boxplot()+
      scale_color_brewer(palette = "PuBu")
 })
 
 
 output$plot2 <- renderPlot({
    df_st <- stinbounds()
    df1_total <-  df1() %>% 
      filter(staid %in% df_st$staid) %>% 
      group_by(staid, year) %>% 
      summarize(total_snow = sum(sd)) %>% 
      mutate(year = row_number())
   df1_total["period"] <- 1
   
   df2_total <-  df2() %>% 
      filter(staid %in% df_st$staid) %>% 
      group_by(staid, year) %>% 
      summarize(total_snow = sum(sd)) %>% 
      mutate(year = row_number())
    df2_total["period"] <- 2 
     
     combined <- rbind(df1_total, df2_total)
     combined$period <- as.factor(combined$period)
    ggplot(combined, aes(year, total_snow, fill = period))+
      geom_area()
  })
 
 
}

shinyApp(ui = ui, server = server)
