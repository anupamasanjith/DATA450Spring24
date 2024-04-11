library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
data<- read.csv("~/Desktop/DATA450Spring24/App-1/Indiatourismagestatistics.csv")
names<-c("Year","FTAs","% 0-14","% 15-24","% 25-34","% 35-44","% 45-54","% 55-64","% 65+","% Not Reported")
names(data)<-names
data2<-mutate(data,
       `0-14` = `FTAs` * (`% 0-14` / 100)/1000,
       `15-24` = `FTAs` * (`% 15-24` / 100)/1000,
       `25-34` = `FTAs` * (`% 25-34` / 100)/1000,
       `35-44` = `FTAs` * (`% 35-44` / 100)/1000,
       `45-54` = `FTAs` * (`% 45-54` / 100)/1000,
       `55-64` = `FTAs` * (`% 55-64` / 100)/1000,
       `65+` = `FTAs` * (`% 65+` / 100)/1000,
       `NotReported` = `FTAs` * (`% Not Reported` / 100)/1000
) %>% 
  select(Year, `0-14`, `15-24`, `25-34`, `35-44`, `45-54`, `55-64`, `65+`, `NotReported`) %>%
  pivot_longer(cols = -Year, names_to = "Age_Group", values_to = 'FTA')


# UI
ui <- fluidPage(
  titlePanel("Distribution of Foreign Tourism Arrivals by Age Group Over Years"),
  
  mainPanel(
    plotOutput("agePlot"),
    plotOutput("Plot")
  )
)


# server
server <- function(input, output) {
  output$agePlot <- renderPlot({
    ggplot(data2, aes(x = Year, y = FTA, fill = Age_Group)) +
      geom_bar(stat = "identity") +
      labs(
           x = "Year",
           y = "FTA's (Thousands)",
           fill = "Age Group") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = "Paired") +
      coord_flip()
  })
  output$Plot<- renderPlot({
    plot(`FTAs`~Year, data = data, col = "blue", pch = 16)
    lines(data$FTAs, data$Year, type = "l", col = "red") 
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)