# 14.06.24 GC 
#This script processes the financial costs (protect) and finanacial profits (from harvest) of each
# of my scenarios. The inputs are the the HabByAgeCashflows produced in the CalculateHabCashFlows.R script. 


library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggpubr)
library(stringr) 
library(cowplot)
library(boot)
library(purrr)

#set params ####

#define discount rates
discount_2 <- 0.02
discount_4 <- 0.04
discount_6 <- 0.06


#Read in Inputs ####
#read in the scenario parametres containing conversion factors for converting from point to parcel/entire landscape  
source('Inputs/FixedScenarioParmams.R')

#add in scenarios 
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")

#get scenario compositions
scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition

# Read in DF showing cashflow outcomes - calculated in CalculateAllHabFlows.R
 cashflow <- read.csv("Outputs/HabByAgeCashflows.csv")

#Read in habitats by year
hab_by_year <- read.csv("Inputs/HabByYears.csv", strip.white = TRUE) %>%  
  rename(true_year = year, 
         functionalhabAge = functional_habAge, 
         habitat = transition_habitat) %>% select(-c(functionalhabAge,X))

#------add temporal information to scenarios ------ 
#nb, we don't need to add harvest delays because 
#becuase we already assume staggered application of harvests (1/30th a year) in cashflow calculations

scenarios 

addDelayFun <- function(x){
 scen <-  x %>% left_join(hab_by_year, by = c("original_habitat", "habitat"),
            relationship = "many-to-many")
 return(scen)

}

scenarios_list <- lapply(scenarios, addDelayFun)
scenarios <- bind_rows(scenarios_list)



#-----seperate cashflow into (1) harvest profits (2) protection costs  

#values are currently in USD/ha so bring to USD/10km2 
cashflow <- cashflow %>%  mutate(cashFlow = cashFlow*1000)

#Harvest profits ####
#cashflow over entire landscape, including revenues in harvested area, and costs of protection 
#both inside and outside the harvested area 
all_cashflow <- cashflow

harvest_cashflow <- cashflow %>%
  #if habitat is unharvested, no harvest revenues are accrued 
  mutate(cashFlow = ifelse(original_habitat == habitat, 0, cashFlow)) %>%  
  mutate(
    cashFlow = case_when(
  
      # If primary goes to once-logged, no harvest profits after yr 30 
      original_habitat == "primary" & habitat == "once-logged" & functionalhabAge > 29 ~ 0,
      
      # If once-logged goes to twice-logged, no harvest profits after yr 30 
      original_habitat == "once-logged" & habitat == "twice-logged" & functionalhabAge > 29 ~ 0,
      
      # If once-logged goes to restored, no harvest profits after yr 30 
      original_habitat == "once-logged" & habitat == "restored" & functionalhabAge > 29 ~ 0,
      
      
      TRUE ~ cashFlow
    )
  )
  

#Define the protection costs ####

#Assumptions (Summarised in Fig S11 in Manuscript)

#1. ENTIRELY UNHARVEST PROTECTION COSTS 
#Scenarios that stay the same habitat for all 60 yeas = society pays protection costs
# e.g. for primary -> primary, once-logged - > once-logged, twice-logged -> twice-logged 

#2. FUTURE PROTECTION COSTS
#Scenario where harvests end at yr 30 = loggers leave and we pay protection costs 
#eg, primary -> once logged = protection costs from yr 30 
#eg  once-logged -> twice-logged = protection costs from yr 30 

#3. NO PROTECTION COSTS
#Forest that stay under active managmenent does not incur societal protection costs
#e.g. Primary - > twicelogged
#eg Primary -> strip plantated
#e.g Once-logged -> strip-planted 

protection_cashflow <- all_cashflow %>% mutate(
  cashFlow = case_when(
    
    #ENTIRELY UNHARVEST PROTECTION COSTS 
    #already incorporated
    
    #FUTURE PROTECTION COSTS
    #Scenario where harvests end at yr 30 = loggers leave and we pay protection costs 
    
    # If primary goes to once-logged or restored, society pays the next 30 years of protection
    original_habitat == "primary" & habitat == "once-logged" & functionalhabAge < 30 ~ 0,
    original_habitat == "once-logged" & habitat == "twice-logged" & functionalhabAge < 30 ~ 0,
    
    #NO PROTECTION COSTS
    original_habitat == "primary" & habitat == "twice-logged"  ~ 0,
    original_habitat == "primary" & habitat == "restored"  ~ 0,
    original_habitat == "once-logged" & habitat == "restored"  ~ 0,
    
    # If habitat contains "eucalyptus" or "albizia," society pays no protection costs
    grepl("eucalyptus|albizia", habitat, ignore.case = TRUE) ~ 0,
    
    TRUE ~ cashFlow
  )
)


# plot undiscounted protection and harvest cashflows ####

#all cashflow 
ggplot(all_cashflow, aes(x = functionalhabAge, y = cashFlow)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "All cashflow",
       x = "Age",
       y = " cashflow") +
  theme_minimal() +
  facet_wrap(habitat ~ original_habitat, scales = "free")


#harvest revenue 
ggplot(harvest_cashflow, aes(x = functionalhabAge, y = cashFlow)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Harvest profits",
       x = "Age",
       y = " cashflow") +
  theme_minimal() +
  facet_wrap(habitat ~ original_habitat, scales = "free")

#protection
ggplot(protection_cashflow, aes(x = functionalhabAge, y = cashFlow)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Protection costs",
       x = "Age",
       y = " cashflow") +
  theme_minimal() +
  facet_wrap(habitat ~ original_habitat)


#discount cashflows ####

discountedCashflow_function  <- function(x) {
  x %>% mutate(
    cashflow_d2 = cashFlow* (1/(1+discount_2)^functionalhabAge), 
    cashflow_d4 = cashFlow*(1/(1+discount_4)^functionalhabAge), 
    cashflow_d6 = cashFlow*(1/(1+discount_6)^functionalhabAge))
}

protection_cashflow <- discountedCashflow_function(protection_cashflow)
harvest_cashflow <- discountedCashflow_function(harvest_cashflow)
all_cashflow <- discountedCashflow_function(all_cashflow)


# plot discounted cashflow
discountRate <- "cashflow_d2"

plot_discounted <- function(x){
  x %>% ggplot(aes_string(x = "functionalhabAge", y = discountRate)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "FunctionalhabAge vs. Cahsflow",
         x = "Age",
         y = "discounted cashflow") +
    theme_minimal() +
    facet_wrap(habitat ~ original_habitat, scales = "free")
}

plot_discounted(protection_cashflow)
plot_discounted(harvest_cashflow)
plot_discounted(all_cashflow)


#Calculate NPV ####
#calculate NPV of scenarios for each of three types of cashflow 
#one of all_cashflow, protection_cashflow or  harvest cashflow)

NPV_fun <- function(x){
   
   scenarios <- as.data.table(scenarios)
    #define which cashflow we're interested in (one of all_cashflow, protection_cashflow, harvest cashflow)
    
    cashflow <- as.data.table(x) %>% 
      rename(true_year = functionalhabAge )
    
    # # Join that scenarios (based on above set join keys) to cashflow data
    scen_bio <- scenarios[cashflow,
                         on = .(original_habitat == original_habitat,
                                functional_habitat == habitat,
                                true_year ==  true_year),
                         nomatch = NA,
                         allow.cartesian=TRUE] %>% na.omit %>% 
    
    #multiply the cashflow by the num parcels
           mutate(cashFlow_parcels = (num_parcels*cashFlow)) %>% 
      ungroup %>% 
      mutate(                                         
        cashflow_d2 = cashFlow_parcels* (1/(1+discount_2)^true_year),    #low discount rate = higher NPV, as we value timber returns from the future more
        cashflow_d4 = cashFlow_parcels*(1/(1+discount_4)^true_year), 
        cashflow_d6 = cashFlow_parcels*(1/(1+discount_6)^true_year)
      )       %>%  

    # calculate NPV 
      group_by(index,production_target) %>%   #this will summarise NPV for each scenario across
      #1. different harvest delays. #2 different habitat transitions in the scenario
      summarise(NPV2 = sum(cashflow_d2), 
                NPV4 = sum(cashflow_d4), 
                NPV6 = sum(cashflow_d6))
    
} 
 
NPV_all <- NPV_fun(all_cashflow) %>%  cbind(costType = "All_costs")
NPV_protection <- NPV_fun(protection_cashflow) %>%  cbind(costType = "ProtectionCosts")
NPV_harvest <- NPV_fun(harvest_cashflow) %>% cbind(costType = "HarvestProfits")


allcosts <- NPV_all %>% rbind(NPV_protection) %>% rbind(NPV_harvest) 


allCosts_composition <- scenario_composition %>% left_join(allcosts, by = c("index","production_target"), relationship = "many-to-many")

#-----EXPORT OUTCOME PERFORMANCE for consolidated figure of all outcomes -----
getwd()
names(allCosts_composition)
output <- allCosts_composition %>% select(index, production_target, scenarioName,scenarioStart,
                                          NPV2, NPV4, NPV6,
                                          costType) %>% cbind(outcome = "financial")
saveRDS(output, "Outputs/MasterFinancialPerformance.rds")

#--------------

