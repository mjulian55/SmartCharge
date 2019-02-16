# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(tidyverse)

# Input parameters:

# Market Segment
# User selects which market segment as a radio button from 4 choices

# Month
# ser selects a date (Month and Year) that their interventions will be based on from a drop-down menu

# Discount amount
# User enters a value in a slider that has a minimum and maximum value based on the electricity prices

# Rebate Amount
# User enters a value in a slider that has a minimum and maximum value based on the electricity prices

# Intervention Hours
# User sets a range of hours that the price intervention will occur (rebate or discount) by setting two sliders on hours of the day

# Throttling Amount
# User chooses a percentage on a slider from 0-100% of the charging reduction that will happen due to throttling

# Throttling Hours
# User sets a range of hours that throttling will occur by setting two sliders on hours of the day



ui <- fluidPage(
  # App Title
  titlePanel("Smart Charge - EV Charging Demand Profile Simulation App"),
    navbarPage("      ",
               tabPanel("Demand Graphs"),
               
               tabPanel("GHG Implications"),
               
               tabPanel("Air Quality Impacts")
               
               )
  
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      
    )
  )

)




# Define server logic required to create a demand profile

# Outputs:
# Baseline EV Charging Demand Profile
# New EV Charging Demand Profile
# Greenhouse Gas (CO2) Implications
# Air Quality Implications (NOX)

server <- function(input, output) {
   
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

