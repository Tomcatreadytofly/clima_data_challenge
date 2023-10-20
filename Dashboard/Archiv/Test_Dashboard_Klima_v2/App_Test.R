#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# # Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )

# library(shiny)
library(shinydashboard)

library(ggplot2)
library(rsconnect)
library(tidyverse)

load("df_snow.RData")

# create the UI using the header, sidebar and body
header <- dashboardHeader(
  title = "Schneefall"
  # dropdownMenu(
  #   type = "messages",
  #   messageItem(
  #     from = "Klimadaten-Team",
  #     message = "Hier sind die Messstationen zu sehen"
  #   )
  # )
)

# create an empty sidebar
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
    menuItem("Map", tabName = "Map", icon = icon("map")),
    menuItem("Plot", tabName = "Plot", icon = icon("bar-chart-o"))
  )
)

# create an empty body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Dashboard",
            fluidRow(
              valueBox(
                "Durchschnitt",
                value = trunc(mean(df_snow$height)),
                subtitle = "Durchschnittliche Schneehöhe",
                icon = icon("snowflake-o"),
                color = "blue", 
                width = 4
              ),
              valueBox(
                "Höchststand",
                value = df_snow$year[which.max(df_snow$height)],
                subtitle = "Jahr mit der grössten Schneehöhe",
                icon = icon("snowflake-o"),
                color = "teal",
                width = 4
              ),
              valueBox(
                "Tiefststand",
                value = df_snow$year[which.min(df_snow$height)],
                subtitle = "Jahr mit der kleinsten Schneehöhe",
                icon = icon("snowflake-o"),
                color = "fuchsia",
                width = 4
              ),
              valueBox(
                "Anzahl Stationen",
                value = length(unique(df_snow$station)),
                subtitle = "Anzahl Stationen",
                icon = icon("map-marker"),
                color = "green",
                width = 4
              ),
              valueBox(
                "Anzahl Länder",
                value = length(unique(df_snow$country)),
                subtitle = "Anzahl Länder",
                icon = icon("globe"),
                color = "yellow",
                width = 4
              ),
              valueBox(
                "Anzahl Jahre",
                value = length(unique(df_snow$year)),
                subtitle = "Anzahl Jahre",
                icon = icon("calendar"),
                color = "red",
                width = 4
              )
            )
    ),
    
    tabItem(tabName = "Map", 
            sliderInput(
              inputId = "periode1",
              min = 1900,
              label = "Vergleichsperiode 1",
              max = max(df_snow$year),
              value = c(1900, max(df_snow$year)),
              sep = "",
              
            ),
            sliderInput(
              inputId = "periode2",
              label = "Vergleichsperiode 2",
              min = 1900,
              max = max(df_snow$year),
              value = c(1900, max(df_snow$year)),
              sep = ""
            ),
            selectInput(
              inputId = "country",
              label = "Land",
              choices = df_snow$country,
              selected = "switzerland",
              multiple = TRUE
            ),
            leafletOutput("map")
    ),
    
    tabItem(tabName = "Plot", 
            sliderInput(
              inputId = "animation",
              label = "Jähliche Entwicklung",
              min = 1900,
              max = max(df_snow$year),
              value = 1900,
              sep = "",
              step = 1,
              animate = animationOptions(
                interval = 200,
                loop = TRUE
              )
            ),
            plotOutput("plot_country")
    )
  )
)

# create the UI using the header, sidebar and body
ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body, 
                    skin = "black"
)

server <- function(input, output) {
  
  output$plot_country <- renderPlot({
    
    df_snow_filtered <- df_snow %>% 
      filter(year == input$animation)
    
    ggplot(
      data = df_snow_filtered,
      aes(country, mean(height), fill = country)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Land", y = "Schneehöhe (m)", title = "Schneehöhe nach Land")
  })
  
  output$map <- renderLeaflet({
    
    df_snow_filtered <- df_snow %>% 
      filter(country == input$country)
    
    
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = df_snow_filtered$longitude_deg,
        lat = df_snow_filtered$latitude_deg,
        radius = log(df_snow_filtered$winter_months),
        label = df_snow_filtered$station,
        weight = 2,
        opacity = 0.5
      )
  })
}

shinyApp(ui = ui, server = server)

# rsconnect::accountInfo()
# rsconnect::deployApp("C:/Users/lbz/OneDrive - FHNW/Studium/RProjects/cdk-klima-challenge/website/Test_Dashboard_Klima_v2")
