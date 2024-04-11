library(shiny)
library(sf)
library(ggplot2)

load("yearly_datasets.Rdata")
x <- read_sf("IND_adm/IND_adm1.shp")
choices<-c("Kerala")
ui <- fluidPage(
  titlePanel("Rainfall In India"),
  sidebarLayout(
    sidebarPanel(
      actionButton("monthly_btn", "Monthly Data"),
      actionButton("annual_btn", "Annual Data"),
      conditionalPanel(
        condition = "(input.monthly_btn > 0 && input.annual_btn >= 0)  ",
        selectInput("state", "Select State:", choices = choices)
      ),
      conditionalPanel(
        condition = "input.annual_btn > 0 || input.monthly_btn >= 0",
        selectInput("year","Select Year:", choices = names(yearly_datasets))
      ),
      actionButton("run_btn", "Run")
    ),
    mainPanel(
      plotOutput("plot"),
      imageOutput("gif")
    )
  )
)

server <- function(input, output, session) {
  data_type <- reactiveVal(NULL)
  
  observeEvent(input$monthly_btn, {
    data_type("Monthly")
  })
  
  observeEvent(input$annual_btn, {
    data_type("Annual")
  })
  
  observeEvent(input$run_btn, {
    if (is.null(data_type())) {
      showModal(modalDialog(
        title = "Error",
        "Please select either Monthly or Annual data.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(data_type(), {
    if (data_type() == "Monthly") {
      updateSelectInput(session, "state", "Select State:", choices = c("Kerala"))
    }
  })
  
  output$plot <- renderPlot({
    if (!is.null(data_type()) && input$run_btn > 0 && data_type() == "Annual") {
      if (!is.null(input$year)) {
        selected_year <- input$year
        ggplot() +
          geom_sf(data = x, col = "black", fill = "white") +
          geom_sf(data = yearly_datasets[[selected_year]], aes(color = ANNUAL), size = 10) +
          scale_color_viridis_c() +
          coord_sf() +
          theme_minimal() +
          labs(title = paste("Rainfall in India - Year", selected_year))
      } else {
        print("Input null")
        
      }
    }
  })
  
  output$gif <- renderImage({
    if (!is.null(data_type()) && input$run_btn > 0 && data_type() == "Monthly") {
      gif_path <- file.path("~/Desktop/DATA450Spring24/MainProjectSYE", paste0(input$state, ".gif"))
      list(src = gif_path,
           contentType = "image/gif",
           width = "500px")
    }
  },
  deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)