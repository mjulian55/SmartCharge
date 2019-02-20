# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(tidyverse)

# Input parameters:


# Throttling Amount
# User chooses a percentage on a slider from 0-100% of the charging reduction that will happen due to throttling

# Throttling Hours
# User sets a range of hours that throttling will occur by setting two sliders on hours of the day



ui <- fluidPage(
  
  
  # App Title
  
  # Sidebar for inputs
  sidebarLayout(
   sidebarPanel(strong("PARAMETERS"), 
                
                # Number of Chargers widget
                numericInput("num", label = h4("Number of Chargers"), value = 900),

                # Market Segment dropdown widget
                selectInput("radio", label = h4("Market Segment"), 
                  choices = list("Workplace" = 1, 
                  "Destination Center" = 2, 
                  "Fleet" = 3, 
                  "Multi-Unit Dwelling" = 4), 
                  selected = 1),
                
                # Date Selection of Invervention (Month and Year)
                (dateInput("date", label = h4("Month and Year"), 
                format = "mm/yyyy", value = "2014-01-01")),
                
                # Discount price selection widget
                # NOTE: Ultimately this slider bar should be nested under some other things.
                # For instance, a radio button of "Apply Discount - Y/N?"
                # If yes, the slider bar comes up to choose the amount, but another slider should appear for the time period.
                sliderInput("slider1", label = h4("Discount Price (in cents)"), 
                  min = 0, max = 40, value = 5),
                
                # Discount period widget
                sliderInput("slider2", label = h4("Discount Period"), min = 0, 
                  max = 24, value = c(11, 15)),
                
                # Rebate price selection widget
                sliderInput("slider1", label = h4("Rebate Price (in cents)"), 
                  min = 0, max = 40, value = 10),
                
                # Discount period widget
                sliderInput("slider2", label = h4("Rebate Period"), min = 0, 
                  max = 24, value = c(18, 21)),
                
                # Throttling percent widget
                numericInput("num", label = h4("Throttling"), min = 0, max = 1, value = 0.0),
                
                # Action Button (Simulate Demand)
                actionButton("action", label = "Charge")
                
                ),
   
   mainPanel(titlePanel("Smart Charge - EV Charging Demand Profile Simulation App"),
             navbarPage("SMART CHARGE",
                        
                        
                        tabPanel("About",
                                 
                                 "This Shiny Application is used for the Bren School Group Project Smart Charge"),
                        
                        tabPanel("Demand Graphs"),
                        
                        tabPanel("GHG Implications"),
                        
                        tabPanel("Air Quality Impacts")
                        
             )
     
   )

))


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

