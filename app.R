# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(tidyverse)
library(markdown)


ui <- fluidPage(

   
   mainPanel(navbarPage("SMART CHARGE",
                        
                        tabPanel("Overview", 
                        includeMarkdown("About_App.Rmd")),
                      
                        
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
                                             c("November 2018" = "2018/11/01",
                                               "October 2017" = "2017/10/01")),
                                 
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
  )
),

                        tabPanel("Instructions")
                                 
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
  source("hourly_demand_function.R")

  output$Demand_Graph <- renderPlot({
    
    month <- month(as.Date(input$date, "%Y/%m/%d"))
    year <- year(as.Date(input$date, "%Y/%m/%d"))
    shiny_seg <- as.character(input$seg)
    
    
    # generate graph and change the things that are reactive from the sliders
    app_model_run <- hourly_demand(int_equals_baseline = FALSE, intervention_chargers = input$intervention_chargers, month = month, year = year, seg = shiny_seg)
    
    
    
    ggplot(app_model_run$EV_Demand) +
      geom_line(aes(x = Hr, y = Xf)) + 
      theme_classic()
 
    })

  
  
  
}

# This website will make sure any changes in your Input UI tab will be shared amongst all tabs: https://stackoverflow.com/questions/47226158/share-some-ui-between-tabs-with-different-output-on-shiny

# Run the application 
shinyApp(ui = ui, server = server)

