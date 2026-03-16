# 14.06.24 GC
# This script processes the financial costs (protection) and finanacial profits (from harvest) of each
# of my scenarios. The inputs are the the HabByAgeCashflows produced in the CalculateHabCashFlows.R script.
#
# 2026-03-16 Nature Revision version:
# - Uses the single cashflow input table `Outputs/HabByAgeCashflows.csv` that now contains:
#     - cashFlow                (baseline)
#     - cashFlow_minus25pct     (-25% applied to ALL $-unit params in the cashflow model, then re-run)
#     - cashFlow_plus25pct      (+25% applied to ALL $-unit params in the cashflow model, then re-run)
# - Propagates these three cashflow variants through exactly the same harvest/protection assumptions
#   and NPV calculations, and exports an output that includes baseline/minus/plus NPVs.

library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggpubr)
library(stringr)
library(cowplot)
library(boot)
library(purrr)

# set params ####

# define discount rates
discount_2 <- 0.02
discount_4 <- 0.04
discount_6 <- 0.06

# Read in Inputs ####
# read in the scenario parametres containing conversion factors for converting from point to parcel/entire landscape
source("Inputs/FixedScenarioParmams.R")

# add in scenarios
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")

# get scenario compositions
scenario_composition <- rbindlist(scenarios, use.names = TRUE) # get scenario composition

# Read in DF showing cashflow outcomes - calculated in CalculateAllHabFlows.R
cashflow_raw <- read.csv("Outputs/HabByAgeCashflows.csv")

required_cashflow_cols <- c(
  "original_habitat",
  "habitat",
  "functionalhabAge",
  "cashFlow",
  "cashFlow_minus25pct",
  "cashFlow_plus25pct"
)
missing_cols <- setdiff(required_cashflow_cols, names(cashflow_raw))
if (length(missing_cols) > 0) {
  stop(paste0(
    "Missing required columns in Outputs/HabByAgeCashflows.csv: ",
    paste(missing_cols, collapse = ", ")
  ))
}

# Read in habitats by year
hab_by_year <- read.csv("Inputs/HabByYears.csv", strip.white = TRUE) %>%
  rename(
    true_year = year,
    functionalhabAge = functional_habAge,
    habitat = transition_habitat
  ) %>%
  select(-c(functionalhabAge, X))

# ------add temporal information to scenarios ------
# nb, we don't need to add harvest delays because
# because we already assume staggered application of harvests (1/30th a year) in cashflow calculations

annualiseScenarios <- function(x) {
  scen <- x %>%
    left_join(
      hab_by_year,
      by = c("original_habitat", "habitat"),
      relationship = "many-to-many"
    )
  return(scen)
}

scenarios_list <- lapply(scenarios, annualiseScenarios)
scenarios <- bind_rows(scenarios_list)

# --- helpers for sensitivity variants ---
cashflow_variants <- c(
  baseline = "cashFlow",
  minus25 = "cashFlow_minus25pct",
  plus25 = "cashFlow_plus25pct"
)

make_cashflow_variant <- function(cashflow_df, value_col, variant_label) {
  out <- cashflow_df %>%
    transmute(
      original_habitat = original_habitat,
      habitat = habitat,
      functionalhabAge = functionalhabAge,
      cashFlow = .data[[value_col]]
    ) %>%
    mutate(
      cashFlow = cashFlow * 1000, # values are currently in USD/ha so bring to USD/10km2
      cashflow_variant = variant_label
    )
  out
}

derive_cashflow_types <- function(cashflow) {
  # cashflow over entire landscape, including revenues in harvested area, and costs of protection
  # both inside and outside the harvested area
  all_cashflow <- cashflow

  # Harvest profits ####
  harvest_cashflow <- cashflow %>%
    # if habitat is unharvested, no harvest revenues are accrued
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

  # Define the protection costs ####
  # Assumptions (Summarised in Fig S11 in Manuscript)
  protection_cashflow <- all_cashflow %>%
    mutate(
      cashFlow = case_when(
        # FUTURE PROTECTION COSTS
        original_habitat == "primary" & habitat == "once-logged" & functionalhabAge < 30 ~ 0,
        original_habitat == "once-logged" & habitat == "twice-logged" & functionalhabAge < 30 ~ 0,

        # NO PROTECTION COSTS
        original_habitat == "primary" & habitat == "twice-logged" ~ 0,
        original_habitat == "primary" & habitat == "restored" ~ 0,
        original_habitat == "once-logged" & habitat == "restored" ~ 0,

        # If habitat contains "eucalyptus" or "albizia," society pays no protection costs
        grepl("eucalyptus|albizia", habitat, ignore.case = TRUE) ~ 0,

        TRUE ~ cashFlow
      )
    )

  list(
    all_cashflow = all_cashflow,
    harvest_cashflow = harvest_cashflow,
    protection_cashflow = protection_cashflow
  )
}

# Calculate NPV ####
# calculate NPV of scenarios for each of three types of cashflow
# (one of all_cashflow, protection_cashflow or harvest cashflow)
NPV_fun <- function(x) {
  scenarios_dt <- as.data.table(scenarios)

  cashflow_dt <- as.data.table(x) %>%
    rename(true_year = functionalhabAge)

  # Join scenarios to cashflow data
  scen_bio <- scenarios_dt[cashflow_dt,
    on = .(
      original_habitat == original_habitat,
      functional_habitat == habitat,
      true_year == true_year
    ),
    nomatch = NA,
    allow.cartesian = TRUE
  ] %>%
    na.omit %>%
    mutate(cashFlow_parcels = (num_parcels * cashFlow)) %>%
    ungroup %>%
    mutate(
      cashflow_d2 = cashFlow_parcels * (1 / (1 + discount_2) ^ true_year),
      cashflow_d4 = cashFlow_parcels * (1 / (1 + discount_4) ^ true_year),
      cashflow_d6 = cashFlow_parcels * (1 / (1 + discount_6) ^ true_year)
    ) %>%
    group_by(index, production_target) %>%
    summarise(
      NPV2 = sum(cashflow_d2),
      NPV4 = sum(cashflow_d4),
      NPV6 = sum(cashflow_d6),
      .groups = "drop"
    )

  scen_bio
}

run_variant_npvs <- function(variant_label, value_col) {
  cf <- make_cashflow_variant(cashflow_raw, value_col = value_col, variant_label = variant_label)
  cfs <- derive_cashflow_types(cf)

  NPV_all <- NPV_fun(cfs$all_cashflow) %>% mutate(costType = "All_costs", cashflow_variant = variant_label)
  NPV_protection <- NPV_fun(cfs$protection_cashflow) %>% mutate(costType = "ProtectionCosts", cashflow_variant = variant_label)
  NPV_harvest <- NPV_fun(cfs$harvest_cashflow) %>% mutate(costType = "HarvestProfits", cashflow_variant = variant_label)

  bind_rows(NPV_all, NPV_protection, NPV_harvest)
}

allcosts_sens <- imap_dfr(cashflow_variants, ~ run_variant_npvs(variant_label = .y, value_col = .x))

allCosts_composition_sens <- scenario_composition %>%
  left_join(allcosts_sens, by = c("index", "production_target"), relationship = "many-to-many")

output_long <- allCosts_composition_sens %>%
  select(
    index,
    production_target,
    scenarioName,
    scenarioStart,
    cashflow_variant,
    NPV2,
    NPV4,
    NPV6,
    costType
  ) %>%
  mutate(outcome = "financial") %>%
  unique()

# A wide version can be convenient for analysis/plotting
output_wide <- output_long %>%
  pivot_wider(
    names_from = cashflow_variant,
    values_from = c(NPV2, NPV4, NPV6),
    names_sep = "__"
  )

saveRDS(output_long, "Outputs/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds")
saveRDS(output_wide, "Outputs/MasterFinancialPerformance__Sensitivity25pctMoneyParams_WIDE.rds")

