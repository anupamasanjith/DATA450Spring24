library(shiny)
library(shinydashboard)
library(sf)
library(ggplot2)

load("yearly_datasets.RData")
x <- read_sf("IND_adm/IND_adm1.shp")
choices <- c("Andaman & Nicobar Islands", "Arunachal Pradesh", "Assam & Meghalaya", "Bihar", "Chhattisgarh",
             "Coastal Andhra Pradesh", "Coastal Karnataka", "East Madhya Pradesh", "East Rajasthan", 
             "East Uttar Pradesh", "Gangetic West Bengal", "Gujarat Region", "Haryana Delhi & Chandigarh", 
             "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", "Kerala", "Konkan & Goa", "Lakshadweep", 
             "Madhya Maharashtra", "Matathwada", "Naga Mani Mizo Tripura", "North Interior Karnataka", 
             "Orissa", "Punjab", "Rayalseema", "Saurashtra & Kutch", "South Interior Karnataka", 
             "Sub Himalayan West Bengal & Sikkim", "Tamil Nadu", "Telangana", "Uttarakhand", 
             "Vidarbha", "West Madhya Pradesh", "West Rajasthan", "West Uttar Pradesh")

## UI 
ui <- dashboardPage(
  dashboardHeader(title = "Rainfall In India"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Monthly Data", tabName = "monthly"),
      menuItem("Annual Data", tabName = "annual")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "monthly",
              fluidRow(
                selectInput("Region_monthly", "Select Region:", choices = choices),
                actionButton("run_btn_monthly", "Run")
              ),
              imageOutput("gif_monthly")
      ),
      tabItem(tabName = "annual",
              fluidRow(
                selectInput("year_annual", "Select Year:", choices = names(yearly_datasets)),
                actionButton("run_btn_annual", "Run")
              ),
              plotOutput("plot_annual", height = 400) 
      )
    )
  )
)

## server
server <- function(input, output, session) {
  
  observeEvent(input$run_btn_monthly, {
    output$gif_monthly <- renderImage({
      req(input$Region_monthly)
      gif_path <- file.path("gifs", paste0(input$Region_monthly, ".gif"))
      list(src = gif_path,
           contentType = "image/gif",
           width = "500px")
    })
  })
  
  observeEvent(input$run_btn_annual, {
    output$plot_annual <- renderPlot({
      req(input$year_annual)
      selected_year <- input$year_annual
      ggplot() +
        geom_sf(data = x, col = "black", fill = "white") +
        geom_sf(data = yearly_datasets[[selected_year]], aes(color = ANNUAL), size = 10) +
        scale_color_viridis_c() +
        coord_sf() +
        theme_minimal() +
        labs(title = paste("Rainfall in India - Year", selected_year))
    })
  })
}

## Run
shinyApp(ui = ui, server = server)
