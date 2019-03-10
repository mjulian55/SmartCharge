# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(markdown)
library(shinycssloaders)


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
                                             selected = "2018/11/01"),
                                 
                                 checkboxGroupInput("price_intervention", h4("Choose a Price Intevention"),
                                                    choices = c("Discount", "Rebate"),
                                                    selected = NULL),
                                 conditionalPanel(condition = "input.price_intervention.includes('Discount')",
                                                  sliderInput("discount", label = h4("Discount Price (in cents)"),
                                                              min = 0, max = 20, value = 5),
                                                  
                                                  sliderInput("discount_period", label = h4("Discount Period"), 
                                                              min = 0, max = 24, value = c(12, 15))),
                                 
                                 conditionalPanel(condition = "input.price_intervention.includes('Rebate')",
                                                  sliderInput("rebate", label = h4("Rebate Price (in cents)"), 
                                                              min = 0, max = 20, value = 0),
                                                  
                                                  sliderInput("rebate_period", label = h4("Rebate Period"),
                                                              min = 0, max = 24, value = c(17, 21))),
                                                  
                                 sliderInput("throttling", label = h4("Throttling"), min = 0, max = 1, value = 0.0),
                                 #Throttling hours
                                 sliderInput("throttle_period", label = h4("Throttling Period"),
                                             min = 0, max = 24, value = c(6, 11)),
                                 
                                 # Action Button (Simulate Demand)
                                 actionButton("action", label = "Charge")
                                 
                                 ),
                            mainPanel(
                              withSpinner(plotOutput("Demand_Graph"), type = 6)
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
  source("simulation_function.R")
  
  output$Demand_Graph <- renderPlot({
    
    month <- month(as.Date(input$date, "%Y/%m/%d"))
    year <- year(as.Date(input$date, "%Y/%m/%d"))
    shiny_seg <- as.character(input$seg)
    
    
    
    if(is.null(input$price_intervention)){
      price_change_conditional <- 0
      
      intervention_hrs_conditional <- seq(input$discount_period[1],input$discount_period[2],1)
      
      price_change_2_conditional <- 0
      
      intervention_hrs_conditional_2 <- seq(input$rebate_period[1],input$rebate_period[2],1)
    } else if(input$price_intervention == c("Discount","Rebate")) {
      price_change_conditional <- -input$discount/100
      
      intervention_hrs_conditional <- seq(input$discount_period[1],input$discount_period[2],1)
      
      price_change_2_conditional <- input$rebate/100
      
      intervention_hrs_conditional_2 <- seq(input$rebate_period[1],input$rebate_period[2],1)
    } else if(input$price_intervention == "Discount") {
      price_change_conditional <- -input$discount/100
      
      intervention_hrs_conditional <- seq(input$discount_period[1],input$discount_period[2],1)
      
      price_change_2_conditional <- 0
      
      intervention_hrs_conditional_2 <- seq(input$rebate_period[1],input$rebate_period[2],1)
      
    } else if (input$price_intervention == "Rebate") {
      price_change_conditional <- input$rebate/100
      
      intervention_hrs_conditional <- seq(input$rebate_period[1],input$rebate_period[2],1)
      
      price_change_2_conditional <- 0
      
      intervention_hrs_conditional_2 <- seq(input$discount_period[1],input$discount_period[2],1)
    } else {
      price_change_conditional <- -input$discount/100
      
      intervention_hrs_conditional <- seq(input$discount_period[1],input$discount_period[2],1)
      
      price_change_2_conditional <- input$rebate/100
      
      intervention_hrs_conditional_2 <- seq(input$rebate_period[1],input$rebate_period[2],1)
    }
    
   
  
    throttling_period <- seq(input$throttle_period[1],input$throttle_period[2],1)
    
    
    # generate graph and change the things that are reactive from the sliders
    app_model_run <- simulation(simulations = 10,
                                sim_price_change = price_change_conditional,
                                sim_intervention_hours = intervention_hrs_conditional,
                                sim_price_change_2 = price_change_2_conditional,
                                sim_intervention_hours_2 = intervention_hrs_conditional_2,
                                sim_int_equals_baseline = FALSE,
                                sim_intervention_chargers = input$intervention_chargers,
                                sim_month = month, 
                                sim_year = year, 
                                sim_seg = shiny_seg,
                                sim_throttle_amount = -input$throttling,
                                sim_throttle_hours = throttling_period)
    
    
    
    ggplot(app_model_run$sim_result_summary) +
      geom_line(aes(x = Hr, y = Xf_mean), color = "seagreen") + 
      geom_line(aes(x = Hr, y = X0_mean), color = "lightsalmon3") +
      theme_classic()
 
    })

  
  
  
}

# This website will make sure any changes in your Input UI tab will be shared amongst all tabs: https://stackoverflow.com/questions/47226158/share-some-ui-between-tabs-with-different-output-on-shiny

# Run the application 
shinyApp(ui = ui, server = server)

