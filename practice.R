library(shiny)
library(sf)

load("~/Desktop/DATA450Spring24/yearly_datasets.Rdata")
x<-read_sf("~/Downloads/IND_adm/IND_adm1.shp")
ui <- fluidPage(
  titlePanel("Rainfall In India"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = names(yearly_datasets))
    ),
    mainPanel(
      plotOutput("plot"),
      imageOutput("gif")
    )
  )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    selected_year <- input$year
      ggplot() +
      geom_sf(data = x, col = "black",fill = "white") +
      geom_sf(data = yearly_datasets[[selected_year]], aes(color = ANNUAL), size = 10) +
      scale_color_viridis_c() +
      coord_sf() +
      theme_minimal() +
      labs(title = paste("Rainfall in India - Year", selected_year))
  })
  
  output$gif <- renderImage({
    gif_path <- file.path("~/Desktop/DATA450Spring24/Rainfall_Data", "Kerala.gif")
    list(src = gif_path,
         contentType = "image/gif",
         width = "500px")  
  },
  deleteFile = FALSE) 
}

# Run the Shiny app
shinyApp(ui = ui, server = server)