#ui.R

library(shiny)
igrbank <- readRDS(file = "data/igr-tidy.rds")

shinyUI(fluidPage(
  
  titlePanel(
    img(src = "nesrea_logo.png", height = 100, width = 120)
    ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Plot of IGR from various NESREA Offices"),
      
      selectInput("state",
                 label = "Location", choices = levels(igrbank$off), selected = "HQ"),
      
      width = 3
      ),

    mainPanel(
      plotOutput("chart1")
      )
  
  )
))