#13.06.2024

# create  cash flows for scenarios based on Runting et al 2020. 
# original code written by Rebecca Runting rebecca.runting@unimelb.edu.au
# subsequent modification by Gianluca cerull grcerullo@gmail.com

#---------------describe management scenarios ---------------------------------------------------------------

# OriginalHabitat	EndHabitat 	  Management_yrs0_30                            	      Management_yrs31_61

# 1	primary 	    primary	      strict protection	                                    strict protection 
# 2	once-logged	  once-logged 	strict protection                                     strict protection 
# 3	twice-logged	twice-logged	strict protection			                                strict protection

# 4	primary 	    once-logged 	first harvest -conventional logging	                  strict protection 
# 5	primary       strip-planted	first harvest- conventional logging- then strip-plant	second harvest-conventional logging 
# 6	primary 	    twice-logged	first harvest - conventional logging 	                second harvest-conventional logging 
# 7	once-logged	  strip-planted first harvest- conventional logging- then strip-plant	second harvest-conventional logging 
# 8	once-logged	  twice-logged	second harvest- conventional loggging                 strict protection 

# 9	primary	      albizia	clear forest, establish plantation,12 year rotations        continued plantations 
#10	primary	      eucalyptus 	  clear forest, establish plantation, 6 year rotations  continued plantations 
#11 once-logged	  albizia	      clear forest, establish plantation, 12 year rotations continued plantations 
#12	once-logged	  eucalyptus 	  clear forest, establish plantation, 6 year rotations 	continued plantations 
#13	twice-logged	albizia	      clear forest, establish plantation, 12 year rotations continued plantations 
#14	twice-logged	eucalyptus 	  clear forest, establish plantation, 6 year rotations 	continued plantations 
#15	deforested	  albizia	      12_year rotations (no clearance costs or revenues)	  continued plantations 
#16	deforested	  eucalyptus 	   6 year rotations (no clearance costs or revenues)	  continued plantations 

library(tidyverse)

#-------------- Specify cost/revenue parameters ---------------------------------------------------
#slope parameters
slope30 <- 0.89 # proportion slope <30 degrees
slope3045 <- 0.10 # proportion slope >30 and <45 degrees
slope45 <- 0.01 # proportion slope >45 degrees
mslope <- 16.97 # the mean percent slope in the potential area for Acacia plantations my dataset of the east Kalimantan forest estate

# distance parameters. May want to modify these for Sabah.
RoadDist <- 3365 #meters (mean from my dataset of the east Kalimantan forest estate of distance to roads)
LPdist <- 75.813 #km (mean from my dataset of the east Kalimantan forest estate of distance to log pond)
DistMillKm <-222.43  #222.43 #km (mean from my dataset of the east Kalimantan forest estate of distance to mill)

#gianluca edit####
DistMillKm_plnt <-30#2 distances to mill. 1.DistMillKm  Is for logging and for the 40% of veneer-quality logs
#2. DistMillKm_plnt  is for pulped timber (100% of eucalyyptus and 61% albizia), which we assume is pulped at an onsite facility, such as at SSB



#TO CHECK 
#1. what distance to apply to mill for my landscape - it might actually make sense to have a different average distance for 
#(i) plantion and (ii) logging mill, since plantations in my scenarios will always be consolidated around mills, whereas logging can 
#on average be further away. 
#2. What is going to be the price I set for veneer and pulpwood - including for FSC pulpwood?
##################### PROTECTED AREAS #####################################################

#=========================== NPV ===========================================================

paEst <- 50   #$/ha establishment cost from Wilson et al 2010
trans <-  9.84 #$/ha one off cost to move from current to effective management
paStrict <- 9.17 #$/ha/pa strict/effective management


cfprimaryPA <- rep(0,61)
cf1logPA <- rep(0,61)
cf2logPA <- rep(0,61)
for (j in 1:61){
  if (j==1){
    cfprimaryPA[j] <- -paStrict   #ADD OPPORTUNITY COSTS?
    cf1logPA[j] <- -paStrict - trans# - onceLOppCost
    cf2logPA[j] <- -paStrict  - paEst - trans 
  } else {
    cfprimaryPA[j] <- -paStrict      #ADD OPPORTUNITY COSTS?
    cf1logPA[j] <- -paStrict# - onceLOppCost
    cf2logPA[j] <- -paStrict 
  }
}

########################################### SELECTIVE LOGGING ##########################################################

###Forest management cost- fixed per ha (concession fee, planning, etc)
fm <- 385.8 # yr1

#========================Net Present value by forest management type================================================

#cash flow <- revenue of harv1 
#       + revenue of harv2 
#       - cost of harv1 
#       - cost of harv2
#       - cost of general forest management cost
#       -taxes and royalties
#       - planting and maintenance cost (for strip planting only)

# Harvest cost varies with distance from logpond and slope

#======================== CURRENT PRACTICE========================================================================
harv1 <- 113 #m3 per ha over 30 years first harvest
harv2 <- 32 #m3 per ha over 30 years second harvest
revharv1 <- harv1*105 #$105 price per m3
revharv2 <- harv2*105

##harvest cost with no variation spatially (felling, loading, etc)
fixcost <- 20.9 #$per m3
## skidding costs - varies with slope
skid <- (slope30*10.8) + (slope3045*10.8*1.5) + (slope45*10.8*2) #  0-30% slope x cost factor + # 30-45% slope x cost factor + # 45% up slope x cost factor
## Hauling cost. 
haul <- LPdist*0.1
## Timber taxes and royalties
tax1 <- 22

## Calculate profit (30 years)
prof1log <- revharv1 - harv1*(fixcost + skid + haul + tax1)
prof2log <- revharv2 - harv2*(fixcost + skid + haul + tax1)

## Calculate cash flows
cf4 <- rep(0,61)
cf6 <- rep(0,61)
cf8 <- rep(0,61)
for (j in 1:30){
  if (j==1){
    cf4[j]   <- prof1log/30 - fm
    cf6[j]   <- prof1log/30 - fm
    cf8[j]   <- prof2log/30 - fm
  } else {
    cf4[j]   <- prof1log/30
    cf6[j]   <- prof1log/30
    cf8[j]   <- prof2log/30
  }
}
for (j in 31:61){
  if (j==31){
    cf4[j]   <- -paStrict  - paEst - trans #included establishment and transaction costs where PA established after logging
    cf6[j]   <- prof2log/30
    cf8[j]   <- -paStrict  - paEst - trans #included establishment and transaction costs where PA established after logging
  } else {
    cf4[j]   <- -paStrict
    cf6[j]   <- prof2log/30
    cf8[j]   <- -paStrict
  }}

#========================   strip planting       ================================================
plantharv<- 40.76148 #m3 per ha. 0.286 cap area of strip planting already aplpied
plantrev <- plantharv*105

##Planting cost # to be applied in yr1. Change the following code to plant <- 418.8 if you want to reflect that strip planting is always close to rodes
plant <-0
if (RoadDist<=200){
  plant <- 418.8
} else if (RoadDist>200 & RoadDist<=400){
  plant <- 418.8*1.1
} else {
  plant <- 418.8*1.5
}


## Calculate profit
profplant <- plantrev - plantharv*(fixcost + 10.8 + haul + tax1) #10.8 = skid cost, lowest value as all on flattest part
prof1logs <- revharv1 - harv1*(fixcost + skid + haul + tax1)
prof2logs <- revharv2 - harv2*(fixcost + skid + haul + tax1)
##

## Calculate cash flows
cf5 <- rep(0,61)
cf7 <- rep(0,61)
for (j in 1:30){
  if (j==1){
    cf5[j]   <- prof1log/30 - fm - (plant/30)*0.286
    cf7[j]   <- prof2log/30 - fm - (plant/30)*0.286
  } else {
    cf5[j]   <- prof1log/30 - (plant/30)*0.286
    cf7[j]   <- prof2log/30 - (plant/30)*0.286
  }
}
for (j in 31:61){
  cf5[j]   <- (profplant/30)*0.286 + (prof2log/30)*(1-0.286)
  cf7[j]   <- (profplant/30)*0.286
}


##################### PLANTATIONS #####################################################

# Perhaps include an administrative establishment cost of $154 ha based on official guidelines: Republik Indonesia. Peraturan Menteri Kehutanan Republik Indonesia. Nomor: P.26/Menhut-II/2009. (Republik Indonesia: Jakarta, Indonesia, 2009).
plantArea <- slope30+slope3045 #not plant  (or clear) slope >45  

# LAND CLEARING -------------------------------
cfrw_primary <- plantArea*168.660  #roundwood volume (m3 per ha) for primary forest
cfpw_primary <- cfrw_primary*1.478649 #pulpwood volume (m3 per ha). see loggingRatios.R for info on ratio used

cfrw_log1 <- plantArea*55.770  #roundwood volume (m3 per ha) for once logged forest
cfpw_log1 <- cfrw_log1*2.567974 #pulpwood volume (m3 per ha). see loggingRatios.R for info on ratio used

cfrw_log2 <- plantArea*24.460  #roundwood volume (m3 per ha) for twice logged forest
cfpw_log2 <- cfrw_log2*2.567974 #pulpwood volume (m3 per ha). see loggingRatios.R for info on ratio used

#revenue/cost primary forest
cfrev_primary <- cfrw_primary*105 +cfpw_primary*31.84
cf_cost_primary <- plantArea*800 + plantArea*(mslope/100)*700 + cfrw_primary*(LPdist/1000)*0.2 + cfrw_primary*22 + cfpw_primary*DistMillKm*0.16 + cfpw_primary*5 #harvest cost + transport cost + timber royalties

#revenue/cost once-logged forest
cfrev_log1 <- cfrw_log1*105 +cfpw_log1*31.84
cf_cost_log1 <- plantArea*800 + plantArea*(mslope/100)*700 + cfrw_log1*(LPdist/1000)*0.2 + cfrw_log1*22 + cfpw_log1*DistMillKm*0.16 + cfpw_log1*5 #harvest cost + transport cost + timber royalties

#revenue/cost twice-logged forest
cfrev_log2 <- cfrw_log2*105 +cfpw_log2*31.84
cf_cost_log2 <- plantArea*800 + plantArea*(mslope/100)*700  + cfrw_log2*(LPdist/1000)*0.2 + cfrw_log2*22 + cfpw_log2*DistMillKm*0.16 + cfpw_log2*5 #harvest cost + transport cost + timber royalties


# PLANTATION HARVEST --------------------------
short_rotation_yields <- 93 #
#double yields 
#short_rotation_yields <- 186 #
#triple yields 
#short_rotation_yields <- 279

long_rotation_yields <- 210 #210 m3/ha over 12 years cutting cycle
#ddouble yields
#long_rotation_yields <- 420 #210 m3/ha over 12 years cutting cycle 
#triple yields 
#long_rotation_yields <- 630

sr_dollars_m3 <- 31.84 #$30 m3 pulp   #31.84 Lao Eucalyptus 2020 price from ( see along ->) -  https://www.forest-journal.com/index.php/JFBR/global-timberland-investment-returns
lr_dollars_m3 <- 70 #$70 m3 veneer

harv_short <- plantArea*short_rotation_yields # 91 m3/ha over 6 years cutting cycle 
harv_long  <- plantArea*long_rotation_yields# 201 m3/ha over 12 years cutting cycle 
rev_short  <- harv_short *sr_dollars_m3 # 160 m3/ha over 6 years cutting cycle at $30 m3 profit
rev_long   <- harv_long*0.6*sr_dollars_m3 + harv_long*0.4*lr_dollars_m3 # split between pulp (60% @ $30 m3) and veneer (40% @ $70 m3)

# PLANTING AND MAINTENANCE -----------------------
#  values from SSB                                        #backcast to 2018 
#plant_costsSR_in_2022_value =865.00 2022 convert to 2018 value to match runting = 742.20
#maint_costsSR_in_2022_value =1109.7864  2022 convert to 2018 value to match runting = 952.23
#plant_costsLR_in_2022_value =709.6032  2022 convert to 2018 value to match runting = 608.86
#maint_costsLR_in_2022_value =1140.4992  2022 convert to 2018 value to match runting = 978.58


plant_cost_short <- plantArea*742.20 
maint_cost_short <- plantArea*952.23/6 # divide by six as maintenance cost is given for entire rotation - apply it equally over individual years
plant_cost_long <- plantArea*608.86 
maint_cost_long <- plantArea*978.58/12  # divide by twelve as maintenance cost is given for entire rotation - apply it equally over individual years 

# HARVESTING ---------------------
harv_cost <- plantArea*(800+(mslope/100)*700) 
# TRANSPORT --------------------------------------
#gianluca edit#### 
trans_cost_long <- DistMillKm*0.08           #transport costs are different because short-rotation pulp is milled on site
trans_cost_short <- DistMillKm_plnt*0.08     #LR logs are transported as sawnlogs and processed to veneer off-site
# TAX AND ROYALTIES ------------------------------
tax2 <- 5
# HARVEST PROFIT --------------------------------

harv_prof_short <- rev_short - harv_cost - harv_short*(trans_cost_short + tax2)
harv_prof_long <- rev_long - harv_cost - harv_long*(trans_cost_long*0.4 +trans_cost_long*0.6 + tax2)    #40% logs transported offsite, 61% logs pulped onsite 

# CashFlows
cfplantShort_primary <- rep(0,61) 
cfplantShort_log1 <- rep(0,61) 
cfplantShort_log2 <- rep(0,61) 
cfplantShort_defor <- rep(0,61) 

cfplantLong_primary <- rep(0,61) 
cfplantLong_log1 <- rep(0,61) 
cfplantLong_log2 <- rep(0,61) 
cfplantLong_defor <- rep(0,61) 

for (j in 1:61){
  if (j<=6){
    #(clear fell revenue - clearfelling cost - planting cost) - Maintenance
    cfplantShort_primary[j] <- (cfrev_primary -  cf_cost_primary - plant_cost_short)/6 - (maint_cost_short/6)*(j-1) # maintenance costs increase cumaltively in first six years as more is planted
    cfplantShort_log1[j] <- (cfrev_log1 -  cf_cost_log1 - plant_cost_short)/6 - (maint_cost_short/6)*(j-1)
    cfplantShort_log2[j] <- (cfrev_log2 -  cf_cost_log2 - plant_cost_short)/6 - (maint_cost_short/6)*(j-1)
    cfplantShort_defor[j] <- (-plant_cost_short)/6 - (maint_cost_short/6)*(j-1)
  } else {
    #harvest profit /6      - maintenance
    cfplantShort_primary[j] <- harv_prof_short/6 -  maint_cost_short # maintenance costs now over entire concession
    cfplantShort_log1[j] <- harv_prof_short/6 -  maint_cost_short # maintenance costs now over entire concession
    cfplantShort_log2[j] <- harv_prof_short/6 -  maint_cost_short # maintenance costs now over entire concession
    cfplantShort_defor[j] <- harv_prof_short/6 -  maint_cost_short # maintenance costs now over entire concession
  }}

for (j in 1:61){
  if (j<=12){
    #(clear fell revenue - clearfelling cost - planting cost) - Maintenance
    cfplantLong_primary[j] <- (cfrev_primary -  cf_cost_primary - plant_cost_long)/12 - (maint_cost_long/12)*(j-1) # maintenance costs increase this year as more is planted
    cfplantLong_log1[j] <- (cfrev_log1 -  cf_cost_log1 - plant_cost_long)/12 - (maint_cost_long/12)*(j-1)
    cfplantLong_log2[j] <- (cfrev_log2 -  cf_cost_log2 - plant_cost_long)/12 - (maint_cost_long/12)*(j-1)
    cfplantLong_defor[j] <- (-plant_cost_long)/12 - (maint_cost_long/12)*(j-1)
  } else {
    #harvest profit /6      - maintenance
    cfplantLong_primary[j] <- harv_prof_long/12 -  maint_cost_long # maintenance costs now over entire concession
    cfplantLong_log1[j] <-    harv_prof_long/12 -  maint_cost_long # maintenance costs now over entire concession
    cfplantLong_log2[j] <-    harv_prof_long/12 -  maint_cost_long # maintenance costs now over entire concession
    cfplantLong_defor[j] <-   harv_prof_long/12 -  maint_cost_long # maintenance costs now over entire concession
  }}

#===================== bring to correct structure =================================
cfAll <- rbind(cfprimaryPA,cf1logPA,cf2logPA,cf4,cf5,cf6,cf7,cf8,cfplantLong_primary,cfplantShort_primary,
               cfplantLong_log1,cfplantShort_log1,cfplantLong_log2,cfplantShort_log2,cfplantLong_defor,cfplantShort_defor)
cfAll <- cbind(seq(1,16),cfAll)
cfAll_df <- as.data.frame(cfAll) %>% select(-1)

original_habitat  <- data.frame(
  original_habitat = c("primary", "once-logged", "twice-logged", "primary", "primary", "primary", "once-logged", "once-logged", "primary", "primary", "once-logged", "once-logged", "twice-logged", "twice-logged", "deforested", "deforested"))
habitat <- data.frame(
  habitat = c("primary", "once-logged", "twice-logged", "once-logged", "restored", "twice-logged", "restored", "twice-logged", "albizia", "eucalyptus", "albizia", "eucalyptus", "albizia", "eucalyptus", "albizia", "eucalyptus"))

hab <- cbind(original_habitat,habitat)

cfAll_df <- hab %>% cbind(cfAll_df)

cfAll_df <- cfAll_df %>%
  pivot_longer(
    cols = -c(habitat, original_habitat),
    names_to = "scenario",
    values_to = "cashFlow") %>%  
  mutate(             #remove characters and -1 to give functionalhabAge
    functionalhabAge = as.numeric(gsub("[^0-9]", "", scenario)) -2) %>% select(-scenario)

#===================== fill in missing habitat types =================================
#!!!!IF IF i do end up using improved varieties in the df, I will need to actually calculate npvs!

#add  current and improved plantations 
euc_current <- cfAll_df %>%  filter(habitat == "eucalyptus") %>% 
  mutate(habitat = "eucalyptus_current")
euc_improved <- cfAll_df %>%  filter(habitat == "eucalyptus") %>% 
  mutate(habitat = "eucalyptus_improved")
alb_current <- cfAll_df %>%  filter(habitat == "albizia") %>% 
  mutate(habitat = "albizia_current")
alb_improved <- cfAll_df %>%  filter(habitat == "albizia") %>% 
  mutate(habitat = "albizia_improved")

cfAll_df <- cfAll_df %>% filter(!(habitat == "eucalyptus")) %>% filter(!(habitat == "albizia")) %>% 
  rbind(alb_current,alb_improved,euc_improved,euc_current)

#keeping restored as restored costs the same as primary forest, as we incorporate the maintenance cost as part of the calculation 
restored <- cfAll_df %>% filter(habitat == "primary", original_habitat == "primary") %>%  mutate(
  habitat = "restored", 
  original_habitat = "restored"
)

#!!!assume no money from deforested land that stays deforested in the scenario
stays_deforested <- data.frame(original_habitat = "deforested", habitat = "deforested", functionalhabAge =0:60, cashFlow = 0)

cfAll_df<- cfAll_df %>% rbind(stays_deforested) %>% rbind(restored) %>% distinct()
# =================== write cashflow output  ======================================

write.csv(cfAll_df,"HabByAgeCashflows.csv",row.names = FALSE)

#write.csv(cfAll_df,"DoublePlantationYieldsHabByAgeCashflows.csv",row.names = FALSE)

#write.csv(cfAll_df,"TriplePlantationYieldsHabByAgeCashflows.csv",row.names = FALSE)





