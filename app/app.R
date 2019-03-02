# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(tidyverse)
library(markdown)


ui <- fluidPage(

   
   mainPanel(navbarPage("SMART CHARGE",
                        
                        tabPanel("About", 
                        includeMarkdown("About_App.Rmd")),
                      
                        
                        tabPanel("Demand Graphs",
                        
                          sidebarLayout(
                            sidebarPanel(
                                 
                                 # Number of chargers widget
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
                                 # If yes, slider appears to choose amount, another slider appears for time period.
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
                            mainPanel(
                              plotOutput("Demand_Graph")
                            )
                            )),
                           
                        
                        
                        
                        
                        tabPanel("GHG Implications",
                                 
                                 # Number of chargers widget
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
                                 # If yes, slider appears to choose amount, another slider appears for time period.
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
                        
                        tabPanel("Air Quality Impacts",
                                 
                                 # Number of chargers widget
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
                                 # If yes, slider appears to choose amount, another slider appears for time period.
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
      )
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
  source("../hourly_demand_function.R")

  output$Demand_Graph <- renderPlot({
    # generate graph and change the things that are reactive from the sliders
    ggplot()
 
    })

  
  
  
}

# This website will make sure any changes in your Input UI tab will be shared amongst all tabs: https://stackoverflow.com/questions/47226158/share-some-ui-between-tabs-with-different-output-on-shiny

# Run the application 
shinyApp(ui = ui, server = server)

