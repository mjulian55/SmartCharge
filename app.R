# Smart Charge Group Project - EV Charging Demand Profile Simulation App
# App Name???

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(markdown)
library(shinycssloaders)
library(gganimate)


# USER INTERFACE (UI)

ui <- fluidPage(

theme = shinytheme("united"),
  
 navbarPage("SMART CHARGE - An Electric Vehicle Charging Demand Simulation App"),
             tabsetPanel(
                        
                        tabPanel("Overview", 
                        includeMarkdown("Overview_App.Rmd")),
                      
                        
                        tabPanel("Simulation Graphs",
                        
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput("sim", label = h4("Number of Simulations. \n Over 10 simulations results in long loading times"), 
                                          c("1" = "1",
                                            "10" = "10",
                                            "100" = "100",
                                            "1000" = "1000"),
                                          selected = "10"),
                                 
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
                                                    selected = "Discount"),
                                 conditionalPanel(condition = "input.price_intervention.includes('Discount')",
                                                  sliderInput("discount", label = h4("Discount Price (in cents/kWh)"),
                                                              min = 0, max = 20, value = 5),
                                                  
                                                  sliderInput("discount_period", label = h4("Discount Period (Hour Ending)"), 
                                                              min = 0, max = 24, value = c(12, 15))),
                                 
                                 conditionalPanel(condition = "input.price_intervention.includes('Rebate')",
                                                  sliderInput("rebate", label = h4("Rebate Price (in cents/kWh)"), 
                                                              min = 0, max = 20, value = 0),
                                                  
                                                  sliderInput("rebate_period", label = h4("Rebate Period (Hour Ending)"),
                                                              min = 0, max = 24, value = c(17, 21))),
                                                  
                                 sliderInput("throttling", label = h4("Throttling Amount (%)"), min = 0, max = 1, value = 0.0),
                                 #Throttling hours
                                 sliderInput("throttle_period", label = h4("Throttling Period"),
                                             min = 0, max = 24, value = c(6, 11))
                                 
                                 ),
                            mainPanel(
                              withSpinner(plotOutput("Demand_Graph"), type = 6),
                              
                              br(),
                              br(),
                              br(),
                              br(),
                             
                              
                              withSpinner(tableOutput("Emissions_Table"), type = 6),
                              
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              withSpinner(plotOutput("Monte_Carlo"), type = 6)
                              
    )
  )
),

                        tabPanel("Instructions",
                          includeMarkdown("Instructions_App.Rmd"))
                                 
  )
)


# SERVER

server <- function(input, output) {
  source("simulation_function.R")
  source("emissions_function.R")
  
  
  output$Demand_Graph <- renderPlot({
    
    
    #Color information
    
    discount_color <- "cadetblue4"
    rebate_color <- "thistle2"
    throttle_color <- "darksalmon"
    
    
    month <- month(as.Date(input$date, "%Y/%m/%d"))
    year <- year(as.Date(input$date, "%Y/%m/%d"))
    shiny_seg <- as.character(input$seg)
    
    
    
    if(is.null(input$price_intervention)){
      price_change_conditional <- 0
      
      intervention_hrs_conditional <- seq(input$discount_period[1],input$discount_period[2],1)
      
      price_change_2_conditional <- 0
      
      intervention_hrs_conditional_2 <- seq(input$rebate_period[1],input$rebate_period[2],1)
    } else if(length(input$price_intervention) > 1) {
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
    app_model_run <- simulation(simulations = as.numeric(input$sim),
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
    
    # graph of all the sim results grouped by their run number, color by the method
    # ggplot
    # gganimate
    
    output$Monte_Carlo <- renderPlot({
    
      app_model_run$sim_result_EV_Demand$method_draw <- as.factor(app_model_run$sim_result_EV_Demand$method_draw)
        
      ggplot() +
        geom_line(data = app_model_run$sim_result_EV_Demand,
                  aes(x = Hr, y = Xf, group = run_number, color = method_draw), 
                      size = 1, 
                      alpha = 0.25) +
        
        ggtitle("Monte Carlo Simulation Displaying EV Charging Demand \nWith the Simulation Run a Selected Amount of Times") +
        theme(plot.title = element_text(size = 40, face = "bold")) +
        xlab("Hour of the Day") +
        ylab("Electricity Demand (kilowatts)") +

        geom_rect(aes(xmin= input$discount_period[1],
                      xmax=input$discount_period[2],
                      ymin=-Inf,
                      ymax=Inf,
                      fill = "Discount"),
                  alpha=ifelse("Discount" %in% input$price_intervention & input$discount >0, 0.02,0)) + #this is the discount rectangle

        geom_rect(aes(xmin=input$rebate_period[1],
                      xmax=input$rebate_period[2],
                      ymin=-Inf,
                      ymax=Inf,
                      fill="Rebate"),
                  alpha=ifelse("Rebate" %in% input$price_intervention & input$rebate >0, 0.02,0)) + #This is the rebate rectangle
        

        geom_rect(aes(xmin= input$throttle_period[1],
                      xmax=input$throttle_period[2],
                      ymin=-Inf,
                      ymax=Inf,
                      fill = "Throttle"),
                  alpha=ifelse(input$throttling >0, 0.02,0)) + #this is the discount rectangle
        
        scale_x_continuous(limits = c(1,24),breaks = c(1:24), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
       scale_fill_brewer("Interventions",palette = "Set2" , guide = guide_legend(override.aes = list(alpha = 0.5))) +
       scale_color_brewer("Simulation Method", palette = "Dark2", direction = -1) +
        theme(legend.position = "bottom") +
        theme_classic()
      
    })
    
    
    
#    discount_text <- ifelse("Discount" %in% input$price_intervention &input$discount >0, c("Hours",input$discount_period[1],"-", input$discount_period[2]),"") %>% 
#      paste(collapse = " ")
    
      
#    rebate_text <- ifelse("Rebate" %in% input$price_intervention &input$rebate >0, c("Hours",input$rebate_period[1],"-", input$rebate_period[2]),"") %>% 
#      paste(collapse = " ")
    
    discount_text <-c(input$discount_period[1],"-", input$discount_period[2]) %>% 
      paste(collapse = " ")
    
    rebate_text <- c(input$rebate_period[1],"-", input$rebate_period[2]) %>% 
      paste(collapse = " ")
    
    
    intervention_hours <- paste("Hours",
      ifelse(input$discount > 0 & "Discount" %in% input$price_intervention,
             discount_text,
             ""),
      ifelse(length(input$price_intervention) >1 &input$discount > 0 & input$rebate > 0,
             "and",
             ""),
      ifelse(input$rebate > 0 & "Rebate" %in% input$price_intervention,
             rebate_text,
             ""),
      collapse = " ")
    
    time_periods <- c(intervention_hours,"4 PM - 9 PM", "All Hours")
    periods <- c("Price Intervention","Peak Demand", "Total")
    
    output$Emissions_Table <- renderTable({
      app_emissions_run <- emissions_fcn(app_model_run)
      app_emissions_table <- app_emissions_run$Emissions_Table %>%
        filter(Time != "other") %>% 
        select("Time Periods" = Xinitial,"Hours" = Time,"Change in Demand (kWh)" = Xchange, "Change in Cost ($)" = CustCostChange, "Change in CO2 Emissions (kg)" = CO2Change, "Change in NOX Emissions (kg)" = NOXChange, "Social Costs of NOX Emission Change ($)" = NOXChangeCost, "Social Costs of CO2 Emission Change ($)" = CO2ChangeCost)
      
      
      app_emissions_table$'Hours' <- time_periods
      app_emissions_table$'Time Periods' <- periods
      
      app_emissions_table
      
      # making interventions hours reactive will be really challenging
      
    })
    
    ggplot(app_model_run$sim_result_summary) +
      geom_line(aes(x = Hr, y = Xf_mean,color = "Intervention Demand"), 
                size = 2.5, 
                alpha = 0.75) + #This is the Xf line
      
      
      geom_line(aes(x = Hr,
                    y = X0_mean,
                    color = "Baseline Demand"), 
                size = 2.5, 
                alpha = 0.75) + #This is the X0 line
      ggtitle("Title") +
      xlab("Hour of the Day") +
      ylab("Electricity Demand (kilowatts)") +
      
      
      geom_rect(aes(xmin= input$discount_period[1],
                    xmax=input$discount_period[2],
                    ymin=-Inf,
                    ymax=Inf,
                    fill = "Discount"),
                alpha=ifelse("Discount" %in% input$price_intervention & input$discount >0, 0.02,0)) + #this is the discount rectangle
      
      
      
      
      geom_rect(aes(xmin=input$rebate_period[1],
                    xmax=input$rebate_period[2],
                    ymin=-Inf,
                    ymax=Inf,
                    fill="Rebate"),
                alpha=ifelse("Rebate" %in% input$price_intervention & input$rebate >0, 0.02,0)) + #This is the rebate rectangle
      
      
      

      geom_rect(aes(xmin= input$throttle_period[1],
                    xmax=input$throttle_period[2],
                    ymin=-Inf,
                    ymax=Inf,
                    fill = "Throttle"),
                alpha=ifelse(input$throttling >0, 0.02,0)) + #this is the discount rectangle
      
      
      scale_x_continuous(limits = c(1,24),breaks = c(1:24), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_brewer("Interventions",palette = "Set2" , guide = guide_legend(override.aes = list(alpha = 0.5))) +
      scale_color_brewer("Demand", palette = "Dark2", direction = -1) +
      #scale_fill_manual('Interventions', values = c("Disount" = 'cadetblue3',"Rebate" = 'thistle3', "Throttle" = 'darksalmon'),  guide = guide_legend(override.aes = list(alpha = 0.5))) +
      theme(legend.position = "bottom") +
      theme_classic()
    
    
 
    })
}



shinyApp(ui = ui, server = server)

