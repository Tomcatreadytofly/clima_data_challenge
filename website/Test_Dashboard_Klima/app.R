#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Schneefall"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sliderInput(
        inputId = "year",
        label = "Jahr",
        min = min(df_snow$year),
        max = max(df_snow$year),
        value = c(min(df_snow$year), max(df_snow$year)),
        sep = ""
      ),
      selectInput(
        inputId = "country",
        label = "Land",
        choices = df_snow$country,
        selected = "switzerland"
      ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("map"),
          plotOutput("plot_country")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot_country <- renderPlot({
    
    df_snow_filtered <- subset(
      df_snow, 
      country == input$country,
      # year == input$year
    ) 
    
    ggplot(
      data = df_snow_filtered,
      aes(station)) +
      geom_bar()
    
    #   qmplot(longitude_deg, latitude_deg, data = country, geom = "point", color = height) +
    # facet_wrap(~ country)
  })
  
  output$map <- renderLeaflet({
    
    df_snow_filtered <- subset(
      df_snow,
      country == input$country,
      # year == input$year
    )
    
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

# Run the application 
shinyApp(ui = ui, server = server)
