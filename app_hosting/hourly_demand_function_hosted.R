

#test
#load libraries
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(lubridate)
library(AER)
library(forcats) #added for as_factor

#LOAD DATA
#Price Schedule
price_schedule <- read_csv("Model_Map/2018_Summer_TOU_EV_4.csv")

#Price Schedule Options (All in Model_Map Folder)

#2018 Summer 3
read_csv("Model_Map/2018_Summer_TOU_EV_3.csv")

#2018 Summer 4
TOU_2018_summer_4 <- read_csv("Model_Map/2018_Summer_TOU_EV_4.csv")

#2018 Winter 3
read_csv("Model_Map/2018_Winter_TOU_EV_3.csv")

#2018 Winter 4
TOU_2018_winter_4 <- read_csv("Model_Map/2018_Winter_TOU_EV_4.csv")

#2018 Winter D
read_csv("Model_Map/2018_Winter_TOU_EV_D.csv")

#2019 Summer 8
TOU_2019_summer_8 <- read_csv("Model_Map/2019_Summer_TOU_EV_8.csv")

#2019 Winter 8
TOU_2019_winter_8 <- read_csv("Model_Map/2019_Winter_TOU_EV_8.csv")


#Baseline Usage

##~~~~~~~~~~REMOVING WEEKENDS~~~~~~~~~~##

Workplace_Daily_Usage <- read_csv("Workplace_Daily_Usage.csv")
Multi_Unit_Dwelling_Daily_Usage <- read_csv("Multi_Unit_Dwelling_Daily_Usage.csv")
Fleet_Daily_Usage <- read_csv("Fleet_Daily_Usage.csv")
Destination_Center_Daily_Usage <- read_csv("Destination_Center_Daily_Usage.csv")


#Event Usage
DC_Event_Total_Usage <- read_csv("Model_Map/DC_Event_Total_Usage.csv")
Workplace_Event_Total_Usage <- read_csv("Model_Map/Workplace_Event_Total_Usage.csv")
Fleet_Event_Total_Usage <- read_csv("Model_Map/Fleet_Event_Total_Usage.csv")
MUD_Event_Total_Usage <- read_csv("Model_Map/MUD_Event_Total_Usage.csv")

#Putting all the hourly baseline in one place

WP_gathered <- gather(Workplace_Daily_Usage,"Hour","Demand",3:26, factor_key = TRUE) %>% 
  mutate(segment = "Workplace")

MUD_gathered <- gather(Multi_Unit_Dwelling_Daily_Usage,"Hour","Demand",3:26, factor_key = TRUE) %>% 
  mutate(segment = "Multi Unit Dwelling")

F_gathered <- gather(Fleet_Daily_Usage,"Hour","Demand",3:26, factor_key = TRUE) %>% 
  mutate(segment = "Fleet")

DC_gathered <- gather(Destination_Center_Daily_Usage,"Hour","Demand",3:26, factor_key = TRUE) %>% 
  mutate(segment = "Destination Center")



hourly_baseline <- rbind(WP_gathered,MUD_gathered,F_gathered,DC_gathered)

#Change class of Date to Date
#hourly_baseline$Hour <- as.numeric(hourly_baseline$Hour)
hourly_baseline$Date <- as.Date(hourly_baseline$Date, "%m/%d/%Y")



hourly_baseline <- hourly_baseline %>% 
  mutate(weekday = wday(Date, label = TRUE)) %>% 
  mutate(weekday = ifelse(weekday == "Sun" |weekday == "Sat", "Weekend","Weekday")) %>% #Add weekday column to label if weekend or weekday
  mutate(price = ifelse(month(Date) %in% seq(6,9,1), 
                        ifelse(Hour %in% c(seq(1,8,1),24),
                               0.05, 
                               ifelse(Hour %in% c(seq(9,12,1),seq(19,23,1)),
                                      0.12,
                                      0.29)),
                        ifelse(Hour %in% c(seq(1,8,1),24),
                               0.06, 
                               ifelse(Hour %in% c(seq(9,12,1),seq(19,23,1)),
                                      0.09,
                                      0.11)
                        )))#Then add price column (TOU EV 4) with complicated nested ifelse statement to decide price at each hour based on if month is summer or winter and which rate period the hour is



#Gathering Event usage into one place

MUD_event_gathered <- gather(MUD_Event_Total_Usage[-c(1,2),],"Date","Demand",2:9) %>% 
  mutate(segment = "Multi Unit Dwelling",
         event_type = rep(
    unlist(
    filter(MUD_Event_Total_Usage, Hour == "event_type")[-1]),each = 24), participating_chargers = rep(
      unlist(
        filter(MUD_Event_Total_Usage, Hour == "participating_chargers")[-1]),each = 24))
  

F_event_gathered <- gather(Fleet_Event_Total_Usage[-c(1,2),],"Date","Demand",2:9) %>% 
  mutate(segment = "Fleet",
         event_type = rep(
    unlist(
      filter(Fleet_Event_Total_Usage, Hour == "event_type")[-1]),each = 24), participating_chargers = rep(
        unlist(
          filter(Fleet_Event_Total_Usage, Hour == "participating_chargers")[-1]),each = 24))

DC_event_gathered <- gather(DC_Event_Total_Usage[-c(1,2),],"Date","Demand",2:9) %>%
  mutate(segment = "Destination Center",
         event_type = rep(
    unlist(
      filter(DC_Event_Total_Usage, Hour == "event_type")[-1]),each = 24), participating_chargers = rep(
        unlist(
          filter(DC_Event_Total_Usage, Hour == "participating_chargers")[-1]),each = 24))

WP_event_gathered <- gather(Workplace_Event_Total_Usage[-c(1,2),],"Date","Demand",2:9) %>% 
  mutate(segment = "Workplace",
         event_type = rep(
    unlist(
      filter(Workplace_Event_Total_Usage, Hour == "event_type")[-1]),each = 24), participating_chargers = rep(
        unlist(
          filter(Workplace_Event_Total_Usage, Hour == "participating_chargers")[-1]),each = 24))


event_data <- rbind(WP_event_gathered,MUD_event_gathered,F_event_gathered, DC_event_gathered) 
event_data$Demand <- as.numeric(event_data$Demand)
event_data$participating_chargers <- as.numeric(event_data$participating_chargers)
#change event data to numeric



#add intervention prices
#adds normal price with nested if statement and then adds intervention price based on type of event and hours

event_data$Date <- as.Date(event_data$Date, "%m/%d/%Y")


event_data <- event_data %>% 
  mutate(price = ifelse(month(Date) %in% seq(6,9,1), 
                        ifelse(Hour %in% c(seq(1,8,1),24),
                               0.05, 
                               ifelse(Hour %in% c(seq(9,12,1),seq(19,23,1)),
                                      0.12,
                                      0.29)),
                        ifelse(Hour %in% c(seq(1,8,1),24),
                               0.06, 
                               ifelse(Hour %in% c(seq(9,12,1),seq(19,23,1)),
                                      0.09,
                                      0.11)
                        ))) %>% 
  mutate(int_price = ifelse(event_type == "LS", ifelse(Hour %in% c(12:15), price-0.05, price), ifelse(Hour %in% c(17:21), price + 0.1, price)))


#Format the event data to be like hourly demand

event_data_for_merge <- event_data %>% 
  mutate(event = 1) %>% 
  mutate(weekday = wday(Date, label = TRUE)) %>%
  mutate(weekday = ifelse(weekday == "Sun" |weekday == "Sat", "Weekend","Weekday")) %>%
  select(Date, Ports = participating_chargers, Hour, Demand, segment, weekday,price = int_price, event)




#55584 entries

#replaces all hourly baseline data with the baselines used in the events (in case we want to run analysis on this)
hourly_baseline_with_events <- hourly_baseline %>% 
  mutate(event = 0) %>% 
  filter(!(Date %in% event_data_for_merge$Date)) %>% 
  rbind(event_data_for_merge) 

hourly_baseline_with_events$Hour <- as_factor(hourly_baseline_with_events$Hour)
hourly_baseline_with_events$Demand <- as.numeric(hourly_baseline_with_events$Demand)
hourly_baseline_with_events$Ports <- as.numeric(hourly_baseline_with_events$Ports)

hourly_baseline_with_events <- hourly_baseline_with_events %>% 
  mutate(demand_per_port = Demand/Ports, cost = Demand*price)

daily_baseline_with_events <- hourly_baseline_with_events %>% 
  group_by(Date,segment) %>% 
  summarise(Ports = mean(Ports), Demand = sum(Demand), price = sum(cost)) 

daily_baseline_with_events <- as.data.frame(daily_baseline_with_events) %>%  
  mutate(avg_price = price/Demand, demand_per_port = Demand/Ports,weekday = wday(Date, label = TRUE)) %>% 
  mutate(weekday = ifelse(weekday == "Sun" |weekday == "Sat", "Weekend","Weekday")) %>% 
  na.omit()
  

#REGRESSION (NO LONGER USING)
plot_weekdays <- filter(daily_baseline_with_events, weekday == "Weekday")

#& month(Date) %in% c(1:5, 10:12)

ggplot(plot_weekdays, aes(x = avg_price)) +
  geom_point(aes(y = demand_per_port, color = month(Date) %in% c(6:9))) +
  facet_wrap(~segment, scales = "free") +
  theme_classic()

daily_baseline_with_events$segment <- as_factor(daily_baseline_with_events$segment)

daily_baseline_with_events$weekday <- as_factor(daily_baseline_with_events$weekday)

EV_daily_lm <- lm(demand_per_port~ avg_price + segment + weekday, data = daily_baseline_with_events)

EV_lm <- lm(exp(demand_per_port) ~ exp(price) + Hour + segment, data = hourly_baseline_with_events)

EV_IV <- ivreg(exp(demand_per_port) ~ exp(price) + Hour + segment | event + Hour + segment, data = hourly_baseline_with_events)


Workplace_Daily_Usage$Date <- as.Date(Workplace_Daily_Usage$Date, "%m/%d/%Y")
Workplace_Daily_Usage <- Workplace_Daily_Usage %>% 
  mutate(weekday = wday(Date, label=TRUE), month = month(Date,label = TRUE), Year = year(Date)) #add columns to the end of sheet to identify day of week, month, year
#as a check for the chunk below



# DATA EXPLORATION (CAN REMOVE)
Workplace_Weekday_Usage <- Workplace_Daily_Usage %>% 
  filter(!wday(Date) %in% c(1, 7) & month(Date) == 11 & year(Date) == 2018 )#keep everything that's nopt sunday(1) and satuday(7)
Workplace_Weekday_Average <- apply(select(Workplace_Weekday_Usage, '1':'24'),2,mean) 
#View(Workplace_Weekday_Average)


## LOAD ALL OTHER CONTEXTUAL DATA (Chargers, elasticities, curtailment)
# Number of Chargers by Segment
#chargers <- read_csv("Model_Map/Chargers_Installed_03-18.csv")
Chargers <- read_csv("Model_Map/Chargers.csv")
Event_Chargers <- read_csv("Model_Map/Event_Chargers.csv")

add_baseline_chargers <- Chargers %>% 
  filter(Market_Segment!= "Total") %>% 
  slice(rep(1:n(),each=24))

#Elasticities with format 9X3 with columns Base_Hr, Changed_Hr, and Elasticity
#Changed_Hr is the Hour where the price change occurs, Base_Hr is the hour in which demand changes
Elasticities_cross <- read_csv("SDGE_Elasticities.csv")
Elasticities_no_cross <- read_csv("SDGE_Elasticities_no_cross.csv")
SDGE_P_SOP_Ratios <- read_csv("SDGE_P_SOP_Ratios.csv") #gets used if we don't tell it what elasticity to use (it runs it on the closest ratio)

#Ratio for selecting Default Elasticities
P_SOP_Ratio <- max(price_schedule$P0)/min(price_schedule$P0)
#Matches our closest Ratio to Inputted Ratio
closest_schedule <- SDGE_P_SOP_Ratios$Rate_Schedule[which.min(abs(SDGE_P_SOP_Ratios$P_SOP_Ratio - P_SOP_Ratio))]
closest_elasticities <- match(closest_schedule, names(Elasticities_cross))
#Uses Elasticities of rate schedule with closest ratio

#Curtailment Data 
curtailment_2018 <- read_csv("Curtailment_2018.csv")
curtailment_2030 <- read_csv("Curtailment_2030.csv")















## MODEL STARTS HERE##

mthd <- 1 #method
avg_elst <- -0.4 #average elasticity
sg <- "Workplace" #segment choose from "Workplace", "Destination Center", "Fleet", "Multi Unit Dwelling"
mth <- 11 #month for baseline data
yr <- 2018 #baseline data year
wknds <- TRUE #include weekends in the baseline data and in the elasticity options
pwr <- 6.6 #charger power
sch <- closest_elasticities #elasticities to use for price intervention (column in the elasticities dataframe) -  Non PV Summer Weekday EPEV L. The default now picks from the ratio
p_c <- -0.05 #price change
p_c_2 <- 0
i_h <- c(12:15) #intervention hours
i_h_2 <- c(17:21)
int_ch <- filter(Chargers, Market_Segment == sg) %>% 
  select(mth) %>% 
  as.numeric() # number of chargers. default is to MARCH 2018
int_e_b <- TRUE # if true, number of intervention chargers equals baseline number of chargers
t_a <- 0 #throttling amount
t_h <- c(7:11) #throttling hours. 
a_p_co <- FALSE #air pollution communication
p_co <- TRUE #price communication
c_yr <- 2018 #default year for curtailment and emissions factors
n_tou <- 2018 #new tou year (options are 2019 or 2018)
pk <- c(17:21) #target window to shift out off (this is only used in the output calculations below, not for the function)


hourly_demand <- function(method = mthd,
                          avg_elasticity = avg_elst,
                          seg = sg, 
                          month = mth,
                          year = yr,
                          include_wknds = wknds,
                          charger_power = pwr,
                          elasticity_schedule = sch,
                          price_change = p_c,
                          price_change_2 = p_c_2,
                          intervention_hours = i_h, 
                          intervention_hours_2 = i_h_2,
                          intervention_chargers = int_ch,#if intervention_chargers is ever changed, have to also set int_equals_baseline = FALSE
                          int_equals_baseline = int_e_b, 
                          throttle_amount = t_a,
                          throttle_hours = t_h, 
                          air_pollution_comm = a_p_co,
                          price_comm = p_co,
                          curt_year = c_yr,
                          new_tou = n_tou
                         ){
  
  library(tidyverse)
  library(lubridate)
  
  #CONTEXT#####
  
  #This section puts the Hr, initial price (P0), period, initial unscaled load (Xi), and scaled load (XO) into EV_Demand
  
  #Price Schedule is read in above
  
  # Elasticity 
  if(method == 1) {
    Elasticities <- Elasticities_cross
  } else{
    Elasticities <- Elasticities_no_cross
  }
  
  
  if(include_wknds == TRUE) {
    Elasticities <- Elasticities
  } else {
    Elasticities <- Elasticities[1:8] # filters for first 6 elasticities (first two columsn are hour and period)
  }
  
  chosen_elasticities <- Elasticities[c(1,2,elasticity_schedule)] #this pulls out columns 1, 2, and the designated elasticity (from the parameter elasticity_schedule)  (from row 74 into a new dataframe)
  colnames(chosen_elasticities) <- c("Base_Hr","Changed_Hr","Elasticity")
  
  if(month%in%c(6:9)) {
    price_schedule <- TOU_2018_summer_4
  } else {
    price_schedule <- TOU_2018_winter_4
    
  }
   
  #price_schedule$period <- factor(price_schedule$period, levels = c("P","MP","OP"))
  
  #Baseline
  #filter the number of chargers by market segment and month, change to numeric (have to set the month and segment otherwise it will take the default, workplace and March 2018)
  #baseline_chargers <-filter(Chargers, Market_Segment == segment) %>% 
  #  select(month) %>% 
   # as.numeric()
  
  #baseline demand
if(include_wknds == TRUE) {
  Xi_choose_weekends <- filter(hourly_baseline, weekday == "Weekday" | weekday == "Weekend")
} else {
  Xi_choose_weekends <- filter(hourly_baseline, weekday == "Weekday")
}

  #calculate Xi for each hour
Xi <- Xi_choose_weekends %>% 
  filter(segment == seg) %>% 
  filter(month(Date) == month & year(Date) == year) %>% 
  group_by(Hour) %>% 
  summarise(Demand = mean(Demand)) %>% 
  select(Demand) %>% 
  unlist()
  
  #baseline chargers (gets one number)
  baseline_chargers <- hourly_baseline %>% 
    filter(segment == seg) %>% 
    filter(month(Date) == month & year(Date) == year) %>% 
    summarise(Ports = mean(Ports)) %>% 
    select(Ports) %>% 
    unlist()
   
  #intervention chargers 
  intervention_chargers <- ifelse(int_equals_baseline == TRUE, baseline_chargers, intervention_chargers)
  
  #Create a new table (EV_Demand) that lists initial price schedule, month, scaled by # of chargers
  EV_Demand <- mutate(price_schedule, I01 = 0 ,Xi = Xi, X0 = Xi/baseline_chargers*intervention_chargers) #I01 refers to the hours where there is an intervention. 
  
 # EV_Demand$I01[intervention_hours] <-1
  
  
  
  #MAX_THEORETICAL#### 
  #Theoretical max is based on the current number of chargers in the SCE Charge Ready pilot program, multiplied by the average power rating of Level 2 EV chargers (6.6 kW), multiplied by 1 hour.  This gives us the total number of kWh for each hour that could be achieved if every charger were utilized during the target load shift window of 11 AM - 3 PM.
  
  Max_Theory <- intervention_chargers*charger_power


  #CURTAILMENT###
  
  if (curt_year == 2030) {
    curtailment <- curtailment_2030
  } else{
    curtailment <- curtailment_2018
  }
  
  curtailment_test <- curtailment %>% 
    select(month) #works because removed hours
  
  colnames(curtailment_test) <- c("Curt")
  
  
  #SPLINING / ELASTICITY MATRIX CREATION ####
  
  #This makes a table for each hour that lists the midpoint hours that will be splined, hours as <24, rate period, and elasticity relative to the hour.
  
  x <- c(1:24) #used for the 24 hours in for loops (24 elasticity columns)
  
  #Finds the hours in the rate schedule just before the period changes
  change_points <- which(price_schedule$Period != dplyr::lag(price_schedule$Period)) - 1
  
  #Finds the midpoints of each "chunk" of rate periods unless the "chunk" spans over the end of the day
  mid_points <- change_points[-length(change_points)] +diff(change_points)/2
  
  #finds the midpoint of the "chunk" of rate period that spans over the end of the day
  rollover_midpoint <- (change_points[1]+24 + change_points[length(change_points)])/2 -24
  
  #adds rollover_midpoint but only if there is actually a rate period "chunk" that rolls over the day
  if(price_schedule$Period[1] == price_schedule$Period[length(price_schedule$Period)]) {
    mid_points <- append(mid_points, rollover_midpoint)
  }
  
  #create a dataframe of the own and cross elasticities of the midpoints and self point (i.e., put the stepwise elasticities into one)
  for(i in x) {
    nam <- paste("Midpoints", i, sep = ".")
    Hrs <- append(mid_points, i)
    Hrs <- Hrs[-match(price_schedule$Period[i],price_schedule$Period[mid_points])]
    #The loop above selects a set of midpoints that leaves out one midpoint based on the hour that a table is being made for (it excludes the midpoint that is in the same period as the hour of the table)
    
    Hrs24 <- append(Hrs,i) #adds the end point (the starting hour 24 hours later)
    Hrs <- if_else(Hrs<i,Hrs+24,Hrs) %>% 
      append(i+24)
    #lists "real hours" from the starting point, adding 24 to any hours before the start point
    
    periods <- price_schedule$Period[c(Hrs24)] 
    #retrieves the rate periods of each hour listed
    
    own_period <- price_schedule$Period[i]
    #retrieves rate period of the current hour
    own_period_elasticities <- filter(chosen_elasticities, Base_Hr == own_period)
    midpoint_elasticities <- own_period_elasticities$Elasticity[match(periods, Elasticities$Changed_Hr)]
    
    assign(nam,data.frame(Hour=Hrs,Hrs24=Hrs24, Period=periods,Elasticity = midpoint_elasticities))
    #makes a data frame named after the current hour with each of the above variables
  }
  
  if(method == 4) {
    #spline the midpoint table
    for (i in x) {
      current_hr <- eval(parse(text = sub("XX", i, "Midpoints.XX"))) #calls current hours midpoint table
      
      Y = spline(x=current_hr$Hour,y=current_hr$Elasticity,xout=seq(min(current_hr$Hour),max(current_hr$Hour)))
      #splines elasticities to smoooth
      
      HR = Y$x
      
      ELAST = Y$y
      
      nam <- paste("Elasticities", i, sep = ".")
      
      assign(nam,data.frame(HR=HR,ELAST=ELAST,HR24 = if_else(HR<=24,HR,HR-24)))
      #makes a data frame with above variables: Hours, smoothed elasticities
    }
  }
  
  else {
    for (i in x) {
    current_hr <- eval(parse(text = sub("XX", i, "Midpoints.XX")))
    #calls current hours midpoint table
    
    HR24 <- seq(0,24,1)
    no_cross_elas <- rep(0,25)
    self <- current_hr %>% 
      filter(Hour == i) %>% 
      select(Elasticity) %>% 
      unlist()
    no_cross_elas[i+1] <- self
    
    nam <- paste("Elasticities", i, sep = ".")
    
    assign(nam,data.frame(HR = HR24, ELAST=no_cross_elas,HR24 = HR24))
    #makes a data frame with above variables: Hours, smoothed elasticities
    }
  }
  
  ####
  
  #MATRIX####
  
  #creates our matrix based on the 24 smoothed elasticities for each hour.
  #uses a for loop to call files rather than individually
  #NOTE this matrix has each COLUMN to be used for each hour. Our excel used each ROW if trying to compare.
  
  matrix <- data.frame(Hr = c(1:24))
  for (i in x) {
    El <- eval(parse(text = sub("YY", i, "Elasticities.YY")))
    El <- El[-1,]
    El <- El[order(El$HR24),]
    matrix <- cbind(matrix, El$ELAST)
  }
  matrix<-matrix[,-1]
  colnames(matrix) <- c(1:24)
  ####
  
  ###set matrix to no cross_elasticitities
  #matrix <- read_csv("No_Cross_Matrix.csv")
  
  
  #PRICE INTERVENTION & COMMUNICATION####
  
  #price_change <- -0.05
  #intervention_hours <- c(12:15)
  EV_Demand <- EV_Demand %>% 
    mutate(P1 = price_schedule$P0) #Copies the initial price schedule into a new column (P1) that can then be modified to reflect the intervention
  
  #new tou as an intervention. 
  #select the potential new tou based on month
  if(month%in%c(6:9)) {
    price_schedule_tou_int <- TOU_2019_summer_8
  } else {
    price_schedule_tou_int <- TOU_2019_winter_8
  }
  #if applying the tou intervention, make that the initial P1. If not, then keep it as the initial priace schedule (all of this then gets modified in the next chunck with the discount and rebate)
  if (new_tou == 2019) {
    EV_Demand$P1 <- price_schedule_tou_int$P0
  } 
  else {
    EV_Demand$P1 <- EV_Demand$P1
  }
  
  
  
  EV_Demand$P1[intervention_hours] <-EV_Demand$P1[intervention_hours] + price_change #updates intervention column to implement intervention
  
  EV_Demand$P1[intervention_hours_2] <- EV_Demand$P1[intervention_hours_2] + price_change_2 #updates intervention price column with option for a second price change. Default price change 2 is 0
  
  #Comms calculation (because air pollution impact is 8.2%, price is 3.5%)
  if (price_comm == TRUE & air_pollution_comm == TRUE) {
    intervention_comm_effect <- 1.082
  }
  else if (price_comm == FALSE & air_pollution_comm == TRUE) {
    intervention_comm_effect <- 1.047
  } 
  else if (price_comm == FALSE & air_pollution_comm == FALSE) {
    intervention_comm_effect <- 0.965
  }
  else {
    intervention_comm_effect <- 1 # price comm = true; air pollution false
  }
    
  #Adds percentage change in price (P1p)
  EV_Demand <- EV_Demand %>% 
    mutate(P1p = (P1-P0)/P0) %>% 
    mutate(P1pC = P1p*intervention_comm_effect)
  
  #Update I01 column for intervention hours (if P1p isn't zero, there was a price intervention in those hours)
  
  EV_Demand <- EV_Demand %>% 
    mutate(I01 = ifelse(P1p != 0 , 1, 0))
  
  #Find percent change in demand as a result of price (due to self and cross elasticities)
  X1p <- as.vector(0)
  for (val in x) {
    mat <- sub("XX",val, "matrix$`XX`")
    sum_prod <- crossprod(EV_Demand$P1pC,eval(parse(text = mat)))
    X1p<- append(X1p,sum_prod)
  } 
  #crossprod() multiplies sumproduct of the percent change in price with each column in the matrix. This is done 24 times by the for loop rather than 24 individual times
  
  X1p <- X1p[-1] # gets rid of the first dummy entry to the variable
  EV_Demand <- mutate(EV_Demand, X1p = X1p) #add percent change in demand due to price onto EV_Demand (X1p)
  
  #find magnitude of new demand
  EV_Demand <- mutate(EV_Demand, X1 = (1+X1p)*X0) #adds new demand in kW variable (X1)

  # for methods 2 and 3 amend new demand with alternative methods
  #method 2 = find non-intervention hours in order to make net zero (through forced load shifting)
  if(method == 2) {
    X1_method2 <- EV_Demand$X1
    X1_method2[-intervention_hours] <- X1_method2[-intervention_hours] -(EV_Demand$X0[-intervention_hours]/sum(EV_Demand$X0[-intervention_hours]))*sum(EV_Demand$X1-EV_Demand$X0)
    
    EV_Demand <- EV_Demand %>% 
      mutate(X1=X1_method2)
  }
  
  #method 3 = find non-intervention hours such that it ensures the net load change suggested by the average elasticity remains true
  else if(method == 3) {
    base_avg_price_mthd_3 <- crossprod(EV_Demand$P0, EV_Demand$X0)/sum(EV_Demand$X0) #average initial price
    int_avg_price_mthd_3 <- crossprod(EV_Demand$P1, EV_Demand$X0)/sum(EV_Demand$X0) #average new price (this still uses the base demand, which is weird)
    avg_price_change_mthd_3 <- int_avg_price_mthd_3 - base_avg_price_mthd_3 #average change in price
    avg_price_change_pct_mthd_3 <- avg_price_change_mthd_3/base_avg_price_mthd_3 #average percentage change in price
    
    net_change_demand_pct <- avg_price_change_pct_mthd_3*avg_elasticity #average change in demand due to change and price and elasticity
    net_change_demand <- net_change_demand_pct*sum(EV_Demand$X0) #average magnitude of the change in demand
    
    net_change_demand_out_int <- net_change_demand - sum(EV_Demand$X1 - EV_Demand$X0) #magnitude of demand change in the non-intervention windows
    
    X1_method3 <- EV_Demand$X1
    X1_method3[-intervention_hours] <- X1_method3[-intervention_hours] + (EV_Demand$X0[-intervention_hours]/sum(EV_Demand$X0[-intervention_hours])*c(net_change_demand_out_int))
    
    EV_Demand <- EV_Demand %>% 
      mutate(X1=X1_method3)
  }
  
  #for methods 1 and 2
  else{
    EV_Demand <- EV_Demand
  }
  ####
  
  
  #THROTTLING####
  
  #throttle_amount <- 0 #throttling amount -0.5 - 50%
  Tp <- rep(0,24)
  #throttle_hours <- c(7:11) #hours that throttling occurs
  Tp[throttle_hours] <- throttle_amount #Assigns the throttling intervention percentage chosen at the inputs at the beginning to the hours chosen then
  
  #Adds throttling percentage to each hour (Tp)
  
  EV_Demand <- EV_Demand %>% 
    mutate(Tp=Tp) %>% 
    mutate(Xt = (1+Tp)*X1)
  
  ####
  
  
  #SHIFTING/FINAL####
  
  #The variables below quantify the shift and net change in demand as a result of interventions, and need to be adjusted based on intervention (does not count throttling)
  
#  Total_x0 <- sum(EV_Demand$X0)
# Total_xt <-sum(EV_Demand$Xt)
  
#  Net_Change <- Total_xt-Total_x0
#  Change_intervention <- sum(EV_Demand$Xt[intervention_hours]) - sum(EV_Demand$X0[intervention_hours])
#  Change_outside_intervention <- sum(EV_Demand$Xt[-intervention_hours])- sum(EV_Demand$X0[-intervention_hours])
  
  
  
  
  EV_Demand <- mutate(EV_Demand, Xint_effect = Xt - X0)
  
  EV_Demand <- mutate(EV_Demand, MT = Max_Theory, Curt = curtailment_test$Curt, Xf = ifelse(Xt > MT, MT, Xt)) %>% 
    mutate(Xf = ifelse(Xf < 0 , 0, Xf)) 
  
  EV_Demand <- EV_Demand %>% 
    mutate(Xint_effect = ifelse(-Xint_effect >X0, -X0, Xint_effect))
  
  
  ####
  
  
  return(list(EV_Demand=EV_Demand,matrix=matrix)) #This makes the model output two data frames. To call each of thoes EV_Demand$EV_Demand or EV_Demand$matrix.  
  
}
