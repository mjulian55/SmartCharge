# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(markdown)


ui <- fluidPage(

theme = shinytheme("sandstone"),
  
 navbarPage("SMART CHARGE - An Electric Vehicle Charging Demand Simulation App"),
             tabsetPanel(
                        
                        tabPanel("Overview", 
                        includeMarkdown("Overview_App.Rmd")),
                      
                        
                        tabPanel("Demand Graphs",
                        
                          sidebarLayout(
                            sidebarPanel(
                                 
                                 # Number of chargers widget
                                 numericInput("intervention_chargers", label = h4("Number of Chargers"), value = 900),
                                 # Market Segment dropdown widget
                                 selectInput("seg", label = h4("Market Segment"), 
                                             c("Workplace" = "Workplace",
                                             "Destination Center" = "Destination Center",
                                             "Fleet" = "Fleet",
                                             "Multi Unit Dwelling" = "Multi Unit Dwelling")),
                                 
                                 # Date Selection of Invervention (Month and Year)
                                 selectInput("date", 
                                             label = h4("Month and Year"), 
                                             c("June 2017" = "2017/06/01",
                                               "July 2017" = "2017/07/01",
                                               "August 2017" = "2017/08/01",
                                               "September 2017" = "2017/09/01",
                                               "October 2017" = "2017/10/01",
                                               "November 2017" = "2017/11/01",
                                               "December 2017" = "2017/12/01",
                                               "January 2018" = "2018/01/01",
                                               "February 2018" = "2018/02/01",
                                               "March 2018" = "2018/03/01",
                                               "April 2018" = "2018/04/01",
                                               "May 2018" = "2018/05/01",
                                               "June 2018" = "2018/06/01",
                                               "July 2018" = "2018/07/01",
                                               "August 2018" = "2018/08/01",
                                               "September 2018" = "2018/09/01",
                                               "October 2018" = "2018/10/01",
                                               "November 2018" = "2018/11/01"),
                                             selected = "2018/11/01",),
                                 
                                 sliderInput("discount", label = h4("Discount Price (in cents)"), 
                                             min = 0, max = 20, value = 5),
                                 
                                 # Discount period widget
                                 sliderInput("discount_period", label = h4("Discount Period"), 
                                             min = 0, max = 24, value = c(11, 15)),
                                 
                                 # Rebate price selection widget
                                 sliderInput("rebate", label = h4("Rebate Price (in cents)"), 
                                             min = 0, max = 40, value = 10),
                                 
                                 # Discount period widget
                                 sliderInput("rebate_period", label = h4("Rebate Period"),
                                             min = 0, max = 24, value = c(18, 21)),
                                 
                                 # Throttling percent widget
                                 numericInput("num", label = h4("Throttling"), min = 0, max = 1, value = 0.0),
                                 # Action Button (Simulate Demand)
                                 actionButton("action", label = "Charge")
                                 
                                 ),
                            mainPanel(
                              plotOutput("Demand_Graph")
    )
  )
),

                        tabPanel("Instructions",
                          includeMarkdown("Instructions_App.Rmd"))
                                 
  )
)


# Define server logic required to create a demand profile
# Outputs:
# Baseline EV Charging Demand Profile
# New EV Charging Demand Profile
# Greenhouse Gas (CO2) Implications
# Air Quality Implications (NOX)

server <- function(input, output) {
  source("hourly_demand_function.R")

  output$Demand_Graph <- renderPlot({
    
    month <- month(as.Date(input$date, "%Y/%m/%d"))
    year <- year(as.Date(input$date, "%Y/%m/%d"))
    shiny_seg <- as.character(input$seg)
    intervention_hrs <- c(input$discount_period[1],input$discount_period[2])
    
    
    # generate graph and change the things that are reactive from the sliders
    app_model_run <- hourly_demand(price_change = -input$discount/100,
                                   intervention_hours = intervention_hrs,
                                   int_equals_baseline = FALSE,
                                   intervention_chargers = input$intervention_chargers,
                                   month = month, year = year, 
                                   seg = shiny_seg)
    
    
    
    ggplot(app_model_run$EV_Demand) +
      geom_line(aes(x = Hr, y = Xf)) + 
      theme_classic()
 
    })

  
  
  
}

# This website will make sure any changes in your Input UI tab will be shared amongst all tabs: https://stackoverflow.com/questions/47226158/share-some-ui-between-tabs-with-different-output-on-shiny

# Run the application 
shinyApp(ui = ui, server = server)

