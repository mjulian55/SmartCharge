
library(tidyverse)
library(dplyr)


#Load default periods (intervention hour is set above); decide if moving these defaults above; and default year
pk <- c(17:21) #target window to shift out off (this is only used in the output calculations below, not for the function)
yr <- 2018 # only real options are 2030 or 2018. if you pick anything else it defualts to 2018. 

#Insert costs ($/kg)
NOXcost <- 21.93 #$/kg
Curtailmentcost <- 0.15 #$/MWH
CO2cost <-0.05 #$/kg

#emissions_x <- emission_output(run_x$EV_Demand)
# emissions_x<- emission_output(hourly_demand()) 
# Emissionfcn <- function (EV_Demand_run1, peak_hour = pk) {
# add a column to output of hourly demand that specifies intervention hours (binary 1: 0; intervention hours = 1; not = 0); read that in for intervention hour; read in peak hours as another input; then calculate everything else other hour of concern (peak), and other
#}

emissions_fcn <- function(EVDemand, peak_hours = pk, emissions_year = yr) {
  
  #Load emissiosn factors based on year
  # Elasticity 
  if(emissions_year == 2030) {
    Hourly_EF <- read_csv("Hrly_EF_2030.csv")
  } else{
    Hourly_EF <- read_csv("Hrly_EF_2018.csv")
  }
  
  #Create Date Frame for Emissions Outputs
  Emissions <- data.frame(Hr = c(1:24)) %>% 
    mutate(CO2Baseline=Hourly_EF$CO2Baseline) %>% 
    mutate(CO2Marginal=Hourly_EF$CO2Marginal_gas) %>% 
    mutate(NOXBaseline=Hourly_EF$NOXBaseline) %>% 
    mutate(NOXMarginal=Hourly_EF$NOXMarginal_gas) %>% #want to change this so it selects the baseline and marginal based on year input and the gas/no gas scenario automatically in the function
    mutate(PercSolar=Hourly_EF$SolarPerc) %>% 
    mutate(I01 = EVDemand$sim_result_summary$I01_mean) %>% 
    mutate(Xi = EVDemand$sim_result_summary$Xi_mean) %>% # column for initial demand at each hour not scaled
    mutate(X0 = EVDemand$sim_result_summary$X0_mean) %>% # column for initial demand at each hour scaled by chargers
    mutate(Xf = EVDemand$sim_result_summary$Xf_mean) %>% 
    mutate(curt = EVDemand$sim_result_summary$Curt_mean) %>% 
    mutate(Xdelta = EVDemand$sim_result_summary$Xint_effect_mean) %>% 
    # column for new demand at each hour. we'll need to update "Xt" to reflect the new column post comms/all interventions
    mutate(Pbase = EVDemand$sim_result_summary$P0_mean*EVDemand$sim_result_summary$X0_mean) %>% #total price paid in that hour ($/kwh * kwh)
    mutate(Pnew = EVDemand$sim_result_summary$P1_mean*EVDemand$sim_result_summary$Xf_mean) #total price paid in that hour ($/kwh * kwh)
  
  Emissions <- Emissions %>% 
    mutate(Xcurtpot = Xdelta*PercSolar) 
  
  Emissions <- Emissions %>% 
    mutate(Xcurt = ifelse(Xcurtpot<curt,ifelse(curt == 0,0,Xcurtpot),curt)) %>% #sum and run percentages on Xcurt 
    mutate(Xextra = ifelse(Xcurtpot>curt, ifelse(Xcurt-Xcurtpot< 0,0, Xcurt-Xcurtpot),0))
  
  #Calculate emissions for baseline demand (scaled and not)
  Emissions <- Emissions %>%
    #mutate (CO2Xi = Xi*CO2Baseline) %>% 
    #mutate (NOXXi = Xi*NOXBaseline) %>% 
    mutate (CO2X0 = X0*CO2Baseline) %>% 
    mutate (NOXX0 = X0*NOXBaseline)
  
  #calculate change in emissions
  Emissions <- Emissions %>%
    mutate (CO2Xdelta = Xdelta*CO2Marginal) %>% 
    mutate (NOXXdelta= Xdelta*NOXMarginal)
  
  #calculate emissions for final
  Emissions <- Emissions %>%
    mutate (CO2Xf = CO2X0+CO2Xdelta) %>% # baseline emissions - change for emissions associated with final demand
    mutate (NOXXf = NOXX0+NOXXdelta)
  
  #Set intervention hours, peak hours, other hours based on intervention hours selected for hourly demand fcn and peak hours input above. 
  #Find interventions hours
  intervention_emission_hours <- which(Emissions[,7] == 1) #pulls intervention hours from initial hourly demand fcn
  #Find other hours based on intervention hours set in hourly function and peak hour input
  other_emission_hours <- c(1:24) #create 1:24 for all other hours
  other_emission_hours <-other_emission_hours[!other_emission_hours %in% intervention_emission_hours] #remove the intervention hours
  other_emission_hours <-other_emission_hours[!other_emission_hours %in% peak_hours] #remove the pk hours
  
  ## Final summations
  # sum baseload by period
  X0_i_h <- sum(Emissions$X0[intervention_emission_hours]) 
  X0_pk <- sum(Emissions$X0[peak_hours]) 
  X0_o_h <- sum(Emissions$X0[other_emission_hours]) 
  X0_sum <- sum(Emissions$X0)
  
  # sum final load by period
  Xf_i_h <- sum(Emissions$Xf[intervention_emission_hours]) 
  Xf_pk <- sum(Emissions$Xf[peak_hours]) 
  Xf_o_h <- sum(Emissions$Xf[other_emission_hours]) 
  Xf_sum <- sum(Emissions$Xf)
  
  # sum intervention change by period
  Xdelta_i_h <- sum(Emissions$Xdelta[intervention_emission_hours]) 
  Xdelta_pk <- sum(Emissions$Xdelta[peak_hours]) 
  Xdelta_o_h <- sum(Emissions$Xdelta[other_emission_hours]) 
  Xdelta_sum <- sum(Emissions$Xdelta)
  
  # sum baseline cost by period
  P0_i_h <- sum(Emissions$Pbase[intervention_emission_hours]) 
  P0_pk <- sum(Emissions$Pbase[peak_hours]) 
  P0_o_h <- sum(Emissions$Pbase[other_emission_hours]) 
  P0_sum <- sum(Emissions$Pbase)
  
  # sum final cost by period
  Pf_i_h <- sum(Emissions$Pnew[intervention_emission_hours]) 
  Pf_pk <- sum(Emissions$Pnew[peak_hours]) 
  Pf_o_h <- sum(Emissions$Pnew[other_emission_hours]) 
  Pf_sum <- sum(Emissions$Pnew)
  
  # sum change in cost by period
  Pdelta_i_h <- Pf_i_h - P0_i_h 
  Pdelta_pk <- Pf_pk - P0_pk
  Pdelta_o_h <- Pf_o_h - P0_o_h
  Pdelta_sum <- Pf_sum - P0_sum
  
  #Sum base emissions by period (intervention window, peak window, other)
  CO2X0_i_h <- sum(Emissions$CO2X0[intervention_emission_hours]) 
  CO2X0_pk <- sum(Emissions$CO2X0[peak_hours]) 
  CO2X0_o_h <- sum(Emissions$CO2X0[other_emission_hours]) 
  CO2X0_sum <- sum(Emissions$CO2X0)
  NOXX0_i_h <- sum(Emissions$NOXX0[intervention_emission_hours])
  NOXX0_pk <- sum(Emissions$NOXX0[peak_hours])
  NOXX0_o_h <- sum(Emissions$NOXX0[other_emission_hours]) 
  NOXX0_sum <- sum(Emissions$NOXX0)
  
  #Sum final emissions by period (intervention window, peak window, other)
  CO2Xf_i_h <- sum(Emissions$CO2Xf[intervention_emission_hours]) #11-3 window
  CO2Xf_pk <- sum(Emissions$CO2Xf[peak_hours]) #peak 4pm-9pm window
  CO2Xf_o_h <- sum(Emissions$CO2Xf[other_emission_hours]) #all other hours
  CO2Xf_sum <- sum(Emissions$CO2Xf)
  NOXXf_i_h <- sum(Emissions$NOXXf[intervention_emission_hours]) #11-3 window
  NOXXf_pk <- sum(Emissions$NOXXf[peak_hours]) #peak 4pm-9pm window
  NOXXf_o_h <- sum(Emissions$NOXXf[other_emission_hours]) #all other hours
  NOXXf_sum <- sum(Emissions$NOXXf)
  
  #Sum change in emissions by period (intervention window, peak window, other)
  CO2Xdelta_i_h <- sum(Emissions$CO2Xdelta[intervention_emission_hours]) #11-3 window
  CO2Xdelta_pk <- sum(Emissions$CO2Xdelta[peak_hours]) #peak 4pm-9pm window
  CO2Xdelta_o_h <- sum(Emissions$CO2Xdelta[other_emission_hours]) #all other hours
  CO2Xdelta_sum <- sum(Emissions$CO2Xdelta)
  NOXXdelta_i_h <- sum(Emissions$NOXXdelta[intervention_emission_hours]) #11-3 window
  NOXXdelta_pk <- sum(Emissions$NOXXdelta[peak_hours]) #peak 4pm-9pm window
  NOXXdelta_o_h <- sum(Emissions$NOXXdelta[other_emission_hours]) #all other hours
  NOXXdelta_sum <- sum(Emissions$NOXXdelta)
  
  #Calculate NOx impacts in DACs vs not (assuming peaker DACs account for 25% of load, even though they account for 70% of plants)
  NOxDACXdelta_i_h <- 0
  NOxDACXdelta_pk <- NOXXdelta_pk*.25 *.70
  NOxDACXdelta_o_h <- 0
  NOxDACXdelta_sum <- NOxDACXdelta_pk
  
  
  #Put into new df
  Emissions_Table <- data.frame(Time= c("intervention hours", "peak period", "other", "Total"), Xinitial = c(X0_i_h, X0_pk, X0_o_h, X0_sum), Xfinal = c(Xf_i_h, Xf_pk, Xf_o_h, Xf_sum), Xchange = c(Xdelta_i_h, Xdelta_pk, Xdelta_o_h, Xdelta_sum), CustCostInitial = c(P0_i_h, P0_pk, P0_o_h, P0_sum), CustCostFinal = c(Pf_i_h, Pf_pk, Pf_o_h, Pf_sum), CustCostChange = c(Pdelta_i_h, Pdelta_pk, Pdelta_o_h, Pdelta_sum), CO2Initial = c(CO2X0_i_h, CO2X0_pk, CO2X0_o_h, CO2X0_sum), CO2Final = c(CO2Xf_i_h, CO2Xf_pk, CO2Xf_o_h, CO2Xf_sum), CO2Change = c(CO2Xdelta_i_h, CO2Xdelta_pk, CO2Xdelta_o_h, CO2Xdelta_sum), NOXInitial = c(NOXX0_i_h, NOXX0_pk, NOXX0_o_h, NOXX0_sum), NOXFinal = c(NOXXf_i_h, NOXXf_pk, NOXXf_o_h, NOXXf_sum), NOXChange = c(NOXXdelta_i_h, NOXXdelta_pk, NOXXdelta_o_h, NOXXdelta_sum), NOXChangeDAC = c(0, NOxDACXdelta_pk, 0, 0))
  #Note: there's got to be a cleaner way to calculate these totals.  
  
  #Calculate cost reduction from NOX reduction (only doing it for the change)
  Emissions_Table <- Emissions_Table %>% 
    mutate(NOXChangeCost = NOXChange*NOXcost) %>% #this is in dollars
    mutate(CO2ChangeCost = CO2Change*CO2cost)
  
  #Curtailment Cost Reduction
  Xcurtpotential <- sum(Emissions$Xcurt) #eek. this 75% is weird and doesn't match the solar fuel mix %. we need another column for solar reduction potential in the fuel mix. 
  Xcurtextra <- sum(Emissions$Xextra) 
  Xcurtproportion <- Xcurtpotential/sum(Emissions$curt)
  
  Curtailmentcost <- Xcurtpotential*-Curtailmentcost
  #add to output table
  Emissions_Table <- Emissions_Table %>% 
    mutate(RedCurtProp = c("NA", "NA", "NA", Xcurtproportion), SolarNonCurt= c("NA", "NA", "NA", Xcurtextra), ChangeCurtCost = c("NA", "NA", "NA", Curtailmentcost))
  
  return(list(Emissions=Emissions, Emissions_Table=Emissions_Table))
}

#Test_emissions <- emissions_fcn(sim1) or Test_emissions <- emissions_fcn(EVDemand = simulation())

#defaults below
#emissions_fcn defaults. peak_hours 17:21 and emissions year 2018. only change if need to make 2030 run. Then: emissions_year = 2030. Always run on the output from your simulation table. see example below with defaults. #PM1_output_table <- emissions_fcn(PM1) to run defaults
#PM1_output_table <- emissions_fcn(EVDemand = PM1, peak_hours = c(17:21), emissions_year = 2019)