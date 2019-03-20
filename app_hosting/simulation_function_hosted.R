
source("hourly_demand_function.R")



simulation <- function(simulations = 100, 
                       sim_method = mthd,
                       sim_avg_elasticity = avg_elst,
                       sim_seg = sg,
                       sim_month = mth, #baseline month
                       sim_year = yr, #baseline year
                       sim_include_wknds = wknds,
                       sim_charger_power = pwr,
                       sim_elasticity_schedule = sch, 
                       sim_price_change = p_c, 
                       sim_intervention_hours = i_h,
                       sim_intervention_chargers = int_ch,
                       sim_price_change_2 = p_c_2,
                       sim_intervention_hours_2 = i_h_2,
                       sim_int_equals_baseline = int_e_b, 
                       sim_throttle_amount = t_a, 
                       sim_throttle_hours = t_h,
                       sim_air_pollution_comm = a_p_co,
                       sim_price_comm = p_co,
                       sim_curt_year=c_yr,
                       sim_new_tou = n_tou) {
  
  ##create lists to sample from for each variable to sample from
  #method
  method_draws <- sample(c(1:4), simulations, replace = TRUE)
  
  #self and cross elasticities 
  #eliminate cross elasticities from 0 to 1
  if(sim_method == 1) {
    Elasticities <- Elasticities_cross
  } else{
    Elasticities <- Elasticities_no_cross
  }
  
  if(sim_include_wknds == TRUE) {
    Elasticities <- Elasticities
  } else {
    Elasticities <- Elasticities[1:8] # filters for first 6 elasticities (first two columsn are hour and period)
  }
  
  elasticity_draws <- sample(3:length(Elasticities),simulations,replace = TRUE)
  
  #average elasticty 
  avg_elasticity_possibilities <- seq(from = -0.8, to = -0.04, by = 0.01)
  avg_elasticity_draws <- sample(avg_elasticity_possibilities, simulations, replace = TRUE)
  
  #comm (not set to be a sample, but could be)
  comm_possibilities <- seq(from=0.2, to=1, by=.01)
  comm_draws <-  sample(comm_possibilities,simulations, replace = TRUE)
  
  #create empty data frame for simulation fcn to populate
  sim_result <- hourly_demand()
  sim_result_EV_Demand <- sim_result$EV_Demand %>% 
    mutate(method_draw = 0, elasticity_draw = 0) %>% 
    mutate(avg_elasticity_draw = 0)
  sim_result_EV_Demand <- sim_result_EV_Demand[0,]
  
  
  for(i in seq(simulations)){
    run_i <- hourly_demand(method = method_draws[i],
                           avg_elasticity = avg_elasticity_draws[i],
                           seg = sim_seg,
                           month = sim_month, 
                           year = sim_year,
                           include_wknds = sim_include_wknds,
                           charger_power = sim_charger_power,
                           elasticity_schedule = elasticity_draws[i], 
                           price_change = sim_price_change, 
                           intervention_hours = sim_intervention_hours,
                           intervention_chargers = sim_intervention_chargers,
                           price_change_2 = sim_price_change_2,
                           intervention_hours_2 = sim_intervention_hours_2,
                           int_equals_baseline = sim_int_equals_baseline, 
                           throttle_amount = sim_throttle_amount, 
                           throttle_hours = sim_throttle_hours,
                           air_pollution_comm = sim_air_pollution_comm,
                           price_comm = sim_price_comm,
                           curt_year=sim_curt_year,
                           new_tou=sim_new_tou)
    
    
    run_i_EV_Demand <- run_i$EV_Demand %>% 
      mutate(run_number = i,method_draw = method_draws[i], elasticity_draw = elasticity_draws[i]) %>% 
      mutate(avg_elasticity_draw = elasticity_draws[i])
    
    sim_result_EV_Demand <- rbind(sim_result_EV_Demand,run_i_EV_Demand)
    
    
  }
  
  sim_result_summary <- group_by(sim_result_EV_Demand, Hr) %>% 
    summarise_at(vars(Xf, P0, Xi, X0, P1, X1, Xint_effect, Xf, Curt, I01),funs(mean,min,max))
  
  names(sim_result_summary[-1]) <- "NULL" #to avoid a dumb error
  
  
  return(list(sim_result_EV_Demand = sim_result_EV_Demand, sim_result_summary = sim_result_summary))
}
