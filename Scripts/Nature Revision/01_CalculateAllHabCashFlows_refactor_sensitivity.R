# 13.06.2024
#
# create  cash flows for scenarios based on Runting et al 2020.
# original code written by Rebecca Runting rebecca.runting@unimelb.edu.au
# subsequent modification by Gianluca cerullo grcerullo@gmail.com
#
# 2026-03-16
# Refactor of `Scripts/01_CalculateAllHabCashFlows.R`:
# - All assumptions in `params` (see `default_params()`)
# - Cashflow generation as pure functions
# - Optional ±25% sensitivity outputs that ONLY scale $ cashflows (see `run_cashflow_scaling_sensitivity()`)
#
#---------------describe management scenarios ---------------------------------------------------------------
#
# OriginalHabitat	EndHabitat 	  Management_yrs0_30                            	      Management_yrs31_61
#
# 1	primary 	    primary	      strict protection	                                    strict protection
# 2	once-logged	  once-logged 	strict protection                                     strict protection
# 3	twice-logged	twice-logged	strict protection			                                strict protection
#
# 4	primary 	    once-logged 	first harvest -conventional logging	                  strict protection
# 5	primary       strip-planted	first harvest- conventional logging- then strip-plant	second harvest-conventional logging
# 6	primary 	    twice-logged	first harvest - conventional logging 	                second harvest-conventional logging
# 7	once-logged	  strip-planted first harvest- conventional logging- then strip-plant	second harvest-conventional logging
# 8	once-logged	  twice-logged	second harvest- conventional loggging                 strict protection
#
# 9	primary	      albizia	clear forest, establish plantation,12 year rotations        continued plantations
#10	primary	      eucalyptus 	  clear forest, establish plantation, 6 year rotations  continued plantations
#11 once-logged	  albizia	      clear forest, establish plantation, 12 year rotations continued plantations
#12	once-logged	  eucalyptus 	  clear forest, establish plantation, 6 year rotations 	continued plantations
#13	twice-logged	albizia	      clear forest, establish plantation, 12 year rotations continued plantations
#14	twice-logged	eucalyptus 	  clear forest, establish plantation, 6 year rotations 	continued plantations
#15	deforested	  albizia	      12_year rotations (no clearance costs or revenues)	  continued plantations
#16	deforested	  eucalyptus 	   6 year rotations (no clearance costs or revenues)	  continued plantations

suppressPackageStartupMessages(library(tidyverse))

default_params <- function() {
  list(
    years = 61,

    #-------------- Specify cost/revenue parameters ---------------------------------------------------
    #slope parameters
    slope30 = 0.89,
    slope3045 = 0.10,
    slope45 = 0.01,
    mslope = 16.97,

    # distance parameters. May want to modify these for Sabah.
    RoadDist = 3365,      # meters
    LPdist = 75.813,      # km
    DistMillKm = 222.43,  # km
    #gianluca edit####
    DistMillKm_plnt = 30, #2 distances to mill.
    #NB: 1.DistMillKm  Is for logging and for the 40% of veneer-quality logs
    #    2. DistMillKm_plnt  is for pulped timber (100% of eucalyyptus and 61% albizia), which we assume is pulped at an onsite facility, such as at SSB

    ##################### PROTECTED AREAS #####################################################
    #=========================== NPV ===========================================================
    paEst = 50,     # $/ha
    trans = 9.84,   # $/ha
    paStrict = 9.17,# $/ha/year

    ########################################### SELECTIVE LOGGING ##########################################################
    ###Forest management cost- fixed per ha (concession fee, planning, etc)
    fm = 385.8,      # $/ha in year 1 only

    #========================Net Present value by forest management type================================================
    #cash flow <- revenue of harv1
    #       + revenue of harv2
    #       - cost of harv1
    #       - cost of harv2
    #       - cost of general forest management cost
    #       -taxes and royalties
    #       - planting and maintenance cost (for strip planting only)
    #
    # Harvest cost varies with distance from logpond and slope
    #
    #======================== CURRENT PRACTICE========================================================================
    harv1 = 113,     # m3/ha over 30 years
    harv2 = 32,      # m3/ha over 30 years
    log_price = 105, # $/m3
    ##harvest cost with no variation spatially (felling, loading, etc)
    fixcost = 20.9,  # $/m3
    ## skidding costs - varies with slope
    skid_base = 10.8,# $/m3 (0-30)
    ## Hauling cost.
    haul_per_km = 0.1,
    ## Timber taxes and royalties
    tax1 = 22,       # $/m3

    #========================   strip planting       ================================================
    plantharv = 40.76148, # m3/ha
    strip_prop = 0.286,   # proportion of area strip-planted
    ##Planting cost # to be applied in yr1. Change the following code to plant <- 418.8 if you want to reflect that strip planting is always close to roads
    strip_plant_base = 418.8,
    strip_rd_thresh1_m = 200,
    strip_rd_thresh2_m = 400,
    strip_mult2 = 1.1,
    strip_mult3 = 1.5,

    ##################### PLANTATIONS #####################################################
    # LAND CLEARING -------------------------------
    cfrw_primary_per_ha = 168.660,
    cfrw_log1_per_ha = 55.770,
    cfrw_log2_per_ha = 24.460,
    pulp_ratio_primary = 1.478649, # see loggingRatios.R for info on ratio used
    pulp_ratio_logged = 2.567974,  # see loggingRatios.R for info on ratio used
    pulp_price = 31.84, # $/m3
    clear_fell_cost_per_ha = 800,
    clear_fell_slope_cost_per_ha = 700, # multiplied by (mslope/100)
    lp_transport_per_m3_per_km = 0.2,    # with LPdist in km
    royalty_per_m3 = 22,
    mill_transport_per_m3_per_km = 0.16,
    tax2 = 5, # $/m3

    # PLANTATION HARVEST --------------------------
    short_rotation_yields = 93,   # m3/ha over 6y
    #double yields
    #short_rotation_yields <- 186 #
    #triple yields
    #short_rotation_yields <- 279
    long_rotation_yields = 210,   # m3/ha over 12y
    #ddouble yields
    #long_rotation_yields <- 420 #210 m3/ha over 12 years cutting cycle
    #triple yields
    #long_rotation_yields <- 630
    sr_dollars_m3 = 31.84,        # $/m3 pulp
    lr_dollars_m3 = 70,           # $/m3 veneer
    veneer_share_long = 0.4,
    pulp_share_long = 0.6,

    # PLANTING AND MAINTENANCE -----------------------
    #  values from SSB                                        #backcast to 2018
    #plant_costsSR_in_2022_value =865.00 2022 convert to 2018 value to match runting = 742.20
    #maint_costsSR_in_2022_value =1109.7864  2022 convert to 2018 value to match runting = 952.23
    #plant_costsLR_in_2022_value =709.6032  2022 convert to 2018 value to match runting = 608.86
    #maint_costsLR_in_2022_value =1140.4992  2022 convert to 2018 value to match runting = 978.58
    plant_cost_short = 742.20,
    maint_cost_short_total = 952.23, # divide by six as maintenance cost is given for entire rotation - apply it equally over individual years
    plant_cost_long = 608.86,
    maint_cost_long_total = 978.58,  # divide by twelve as maintenance cost is given for entire rotation - apply it equally over individual years

    # TRANSPORT --------------------------------------
    #gianluca edit####
    transport_cost_per_km = 0.08
  )
}

renormalize_proportions <- function(params, names_vec) {
  vals <- unlist(params[names_vec], use.names = FALSE)
  s <- sum(vals)
  if (!is.finite(s) || s <= 0) stop("Proportion group sum must be positive.")
  vals <- vals / s
  for (i in seq_along(names_vec)) params[[names_vec[[i]]]] <- vals[[i]]
  params
}

calc_strip_plant_cost_per_ha <- function(params) {
  rd <- params$RoadDist
  base <- params$strip_plant_base
  if (rd <= params$strip_rd_thresh1_m) {
    base
  } else if (rd > params$strip_rd_thresh1_m && rd <= params$strip_rd_thresh2_m) {
    base * params$strip_mult2
  } else {
    base * params$strip_mult3
  }
}

calc_protected_area_cashflows <- function(params) {
  years <- params$years
  cfprimaryPA <- rep(0, years)
  cf1logPA <- rep(0, years)
  cf2logPA <- rep(0, years)

  for (j in seq_len(years)) {
    if (j == 1) {
      cfprimaryPA[[j]] <- -params$paStrict
      cf1logPA[[j]] <- -params$paStrict - params$trans
      cf2logPA[[j]] <- -params$paStrict - params$paEst - params$trans
    } else {
      cfprimaryPA[[j]] <- -params$paStrict
      cf1logPA[[j]] <- -params$paStrict
      cf2logPA[[j]] <- -params$paStrict
    }
  }

  list(
    cfprimaryPA = cfprimaryPA,
    cf1logPA = cf1logPA,
    cf2logPA = cf2logPA
  )
}

calc_selective_logging_cashflows <- function(params) {
  years <- params$years

  # skid cost weighted by slope classes
  skid <- (params$slope30 * params$skid_base) +
    (params$slope3045 * params$skid_base * 1.5) +
    (params$slope45 * params$skid_base * 2)
  haul <- params$LPdist * params$haul_per_km

  revharv1 <- params$harv1 * params$log_price
  revharv2 <- params$harv2 * params$log_price

  prof1log <- revharv1 - params$harv1 * (params$fixcost + skid + haul + params$tax1)
  prof2log <- revharv2 - params$harv2 * (params$fixcost + skid + haul + params$tax1)

  # current practice cashflows
  cf4 <- rep(0, years)
  cf6 <- rep(0, years)
  cf8 <- rep(0, years)

  for (j in 1:30) {
    if (j == 1) {
      cf4[[j]] <- prof1log / 30 - params$fm
      cf6[[j]] <- prof1log / 30 - params$fm
      cf8[[j]] <- prof2log / 30 - params$fm
    } else {
      cf4[[j]] <- prof1log / 30
      cf6[[j]] <- prof1log / 30
      cf8[[j]] <- prof2log / 30
    }
  }
  for (j in 31:years) {
    if (j == 31) {
      cf4[[j]] <- -params$paStrict - params$paEst - params$trans
      cf6[[j]] <- prof2log / 30
      cf8[[j]] <- -params$paStrict - params$paEst - params$trans
    } else {
      cf4[[j]] <- -params$paStrict
      cf6[[j]] <- prof2log / 30
      cf8[[j]] <- -params$paStrict
    }
  }

  # strip planting
  plantrev <- params$plantharv * params$log_price
  plant_cost <- calc_strip_plant_cost_per_ha(params)

  profplant <- plantrev - params$plantharv * (params$fixcost + params$skid_base + haul + params$tax1)

  cf5 <- rep(0, years)
  cf7 <- rep(0, years)
  for (j in 1:30) {
    if (j == 1) {
      cf5[[j]] <- prof1log / 30 - params$fm - (plant_cost / 30) * params$strip_prop
      cf7[[j]] <- prof2log / 30 - params$fm - (plant_cost / 30) * params$strip_prop
    } else {
      cf5[[j]] <- prof1log / 30 - (plant_cost / 30) * params$strip_prop
      cf7[[j]] <- prof2log / 30 - (plant_cost / 30) * params$strip_prop
    }
  }
  for (j in 31:years) {
    cf5[[j]] <- (profplant / 30) * params$strip_prop + (prof2log / 30) * (1 - params$strip_prop)
    cf7[[j]] <- (profplant / 30) * params$strip_prop
  }

  list(cf4 = cf4, cf5 = cf5, cf6 = cf6, cf7 = cf7, cf8 = cf8)
}

calc_plantation_cashflows <- function(params) {
  years <- params$years

  plantArea <- params$slope30 + params$slope3045

  # land clearing volumes
  cfrw_primary <- plantArea * params$cfrw_primary_per_ha
  cfpw_primary <- cfrw_primary * params$pulp_ratio_primary

  cfrw_log1 <- plantArea * params$cfrw_log1_per_ha
  cfpw_log1 <- cfrw_log1 * params$pulp_ratio_logged

  cfrw_log2 <- plantArea * params$cfrw_log2_per_ha
  cfpw_log2 <- cfrw_log2 * params$pulp_ratio_logged

  # revenue/cost land clearing
  cfrev_primary <- cfrw_primary * params$log_price + cfpw_primary * params$pulp_price
  cf_cost_primary <- plantArea * params$clear_fell_cost_per_ha +
    plantArea * (params$mslope / 100) * params$clear_fell_slope_cost_per_ha +
    cfrw_primary * (params$LPdist / 1000) * params$lp_transport_per_m3_per_km +
    cfrw_primary * params$royalty_per_m3 +
    cfpw_primary * params$DistMillKm * params$mill_transport_per_m3_per_km +
    cfpw_primary * params$tax2

  cfrev_log1 <- cfrw_log1 * params$log_price + cfpw_log1 * params$pulp_price
  cf_cost_log1 <- plantArea * params$clear_fell_cost_per_ha +
    plantArea * (params$mslope / 100) * params$clear_fell_slope_cost_per_ha +
    cfrw_log1 * (params$LPdist / 1000) * params$lp_transport_per_m3_per_km +
    cfrw_log1 * params$royalty_per_m3 +
    cfpw_log1 * params$DistMillKm * params$mill_transport_per_m3_per_km +
    cfpw_log1 * params$tax2

  cfrev_log2 <- cfrw_log2 * params$log_price + cfpw_log2 * params$pulp_price
  cf_cost_log2 <- plantArea * params$clear_fell_cost_per_ha +
    plantArea * (params$mslope / 100) * params$clear_fell_slope_cost_per_ha +
    cfrw_log2 * (params$LPdist / 1000) * params$lp_transport_per_m3_per_km +
    cfrw_log2 * params$royalty_per_m3 +
    cfpw_log2 * params$DistMillKm * params$mill_transport_per_m3_per_km +
    cfpw_log2 * params$tax2

  # plantation harvest
  harv_short <- plantArea * params$short_rotation_yields
  harv_long <- plantArea * params$long_rotation_yields

  rev_short <- harv_short * params$sr_dollars_m3
  rev_long <- harv_long * params$pulp_share_long * params$sr_dollars_m3 +
    harv_long * params$veneer_share_long * params$lr_dollars_m3

  plant_cost_short <- plantArea * params$plant_cost_short
  maint_cost_short <- plantArea * (params$maint_cost_short_total / 6)
  plant_cost_long <- plantArea * params$plant_cost_long
  maint_cost_long <- plantArea * (params$maint_cost_long_total / 12)

  harv_cost <- plantArea * (params$clear_fell_cost_per_ha + (params$mslope / 100) * params$clear_fell_slope_cost_per_ha)

  trans_cost_long <- params$DistMillKm * params$transport_cost_per_km
  trans_cost_short <- params$DistMillKm_plnt * params$transport_cost_per_km

  harv_prof_short <- rev_short - harv_cost - harv_short * (trans_cost_short + params$tax2)
  harv_prof_long <- rev_long - harv_cost - harv_long * (trans_cost_long * 0.4 + trans_cost_long * 0.6 + params$tax2)

  cfplantShort_primary <- rep(0, years)
  cfplantShort_log1 <- rep(0, years)
  cfplantShort_log2 <- rep(0, years)
  cfplantShort_defor <- rep(0, years)

  cfplantLong_primary <- rep(0, years)
  cfplantLong_log1 <- rep(0, years)
  cfplantLong_log2 <- rep(0, years)
  cfplantLong_defor <- rep(0, years)

  for (j in seq_len(years)) {
    if (j <= 6) {
      cfplantShort_primary[[j]] <- (cfrev_primary - cf_cost_primary - plant_cost_short) / 6 - (maint_cost_short / 6) * (j - 1)
      cfplantShort_log1[[j]] <- (cfrev_log1 - cf_cost_log1 - plant_cost_short) / 6 - (maint_cost_short / 6) * (j - 1)
      cfplantShort_log2[[j]] <- (cfrev_log2 - cf_cost_log2 - plant_cost_short) / 6 - (maint_cost_short / 6) * (j - 1)
      cfplantShort_defor[[j]] <- (-plant_cost_short) / 6 - (maint_cost_short / 6) * (j - 1)
    } else {
      cfplantShort_primary[[j]] <- harv_prof_short / 6 - maint_cost_short
      cfplantShort_log1[[j]] <- harv_prof_short / 6 - maint_cost_short
      cfplantShort_log2[[j]] <- harv_prof_short / 6 - maint_cost_short
      cfplantShort_defor[[j]] <- harv_prof_short / 6 - maint_cost_short
    }
  }

  for (j in seq_len(years)) {
    if (j <= 12) {
      cfplantLong_primary[[j]] <- (cfrev_primary - cf_cost_primary - plant_cost_long) / 12 - (maint_cost_long / 12) * (j - 1)
      cfplantLong_log1[[j]] <- (cfrev_log1 - cf_cost_log1 - plant_cost_long) / 12 - (maint_cost_long / 12) * (j - 1)
      cfplantLong_log2[[j]] <- (cfrev_log2 - cf_cost_log2 - plant_cost_long) / 12 - (maint_cost_long / 12) * (j - 1)
      cfplantLong_defor[[j]] <- (-plant_cost_long) / 12 - (maint_cost_long / 12) * (j - 1)
    } else {
      cfplantLong_primary[[j]] <- harv_prof_long / 12 - maint_cost_long
      cfplantLong_log1[[j]] <- harv_prof_long / 12 - maint_cost_long
      cfplantLong_log2[[j]] <- harv_prof_long / 12 - maint_cost_long
      cfplantLong_defor[[j]] <- harv_prof_long / 12 - maint_cost_long
    }
  }

  list(
    cfplantLong_primary = cfplantLong_primary,
    cfplantShort_primary = cfplantShort_primary,
    cfplantLong_log1 = cfplantLong_log1,
    cfplantShort_log1 = cfplantShort_log1,
    cfplantLong_log2 = cfplantLong_log2,
    cfplantShort_log2 = cfplantShort_log2,
    cfplantLong_defor = cfplantLong_defor,
    cfplantShort_defor = cfplantShort_defor
  )
}

assemble_hab_by_age_cashflows <- function(params) {
  params <- renormalize_proportions(params, c("slope30", "slope3045", "slope45"))

  pa <- calc_protected_area_cashflows(params)
  sl <- calc_selective_logging_cashflows(params)
  pl <- calc_plantation_cashflows(params)

  cfAll <- rbind(
    pa$cfprimaryPA, pa$cf1logPA, pa$cf2logPA,
    sl$cf4, sl$cf5, sl$cf6, sl$cf7, sl$cf8,
    pl$cfplantLong_primary, pl$cfplantShort_primary,
    pl$cfplantLong_log1, pl$cfplantShort_log1,
    pl$cfplantLong_log2, pl$cfplantShort_log2,
    pl$cfplantLong_defor, pl$cfplantShort_defor
  )

  cfAll <- cbind(seq(1, 16), cfAll)
  cfAll_df <- as.data.frame(cfAll) %>% select(-1)

  original_habitat <- data.frame(
    original_habitat = c(
      "primary", "once-logged", "twice-logged",
      "primary", "primary", "primary", "once-logged", "once-logged",
      "primary", "primary", "once-logged", "once-logged", "twice-logged", "twice-logged",
      "deforested", "deforested"
    )
  )
  habitat <- data.frame(
    habitat = c(
      "primary", "once-logged", "twice-logged",
      "once-logged", "restored", "twice-logged", "restored", "twice-logged",
      "albizia", "eucalyptus", "albizia", "eucalyptus", "albizia", "eucalyptus",
      "albizia", "eucalyptus"
    )
  )

  hab <- cbind(original_habitat, habitat)
  cfAll_df <- hab %>% cbind(cfAll_df)

  cfAll_df <- cfAll_df %>%
    pivot_longer(
      cols = -c(habitat, original_habitat),
      names_to = "scenario",
      values_to = "cashFlow"
    ) %>%
    mutate(functionalhabAge = as.numeric(gsub("[^0-9]", "", scenario)) - 2) %>%
    select(-scenario)

  # fill in missing habitat types
  euc_current <- cfAll_df %>% filter(habitat == "eucalyptus") %>% mutate(habitat = "eucalyptus_current")
  alb_current <- cfAll_df %>% filter(habitat == "albizia") %>% mutate(habitat = "albizia_current")
  cfAll_df <- cfAll_df %>%
    filter(!(habitat %in% c("eucalyptus", "albizia"))) %>%
    rbind(alb_current, euc_current)

  restored <- cfAll_df %>%
    filter(habitat == "primary", original_habitat == "primary") %>%
    mutate(habitat = "restored", original_habitat = "restored")

  stays_deforested <- data.frame(
    original_habitat = "deforested",
    habitat = "deforested",
    functionalhabAge = 0:60,
    cashFlow = 0
  )

  cfAll_df <- cfAll_df %>% rbind(stays_deforested) %>% rbind(restored) %>% distinct()
  cfAll_df
}

write_cashflow_csv <- function(cf_df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # Write to a temp file then rename, to reduce partial-write risk.
  tmp <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(path)), "_"),
    tmpdir = dirname(path),
    fileext = ".tmp.csv"
  )

  write.csv(cf_df, tmp, row.names = FALSE)

  ok <- suppressWarnings(file.rename(tmp, path))
  if (!isTRUE(ok)) {
    # Common on Windows: target csv is open in Excel => "permission denied".
    alt <- file.path(
      dirname(path),
      paste0(
        tools::file_path_sans_ext(basename(path)),
        "__",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    )
    suppressWarnings(file.rename(tmp, alt))
    message("Could not overwrite `", path, "` (possibly open elsewhere). Wrote `", alt, "` instead.")
    return(invisible(alt))
  }

  invisible(path)
}

##################### SENSITIVITY ANALYSIS #####################################################
#
# Goal:
# - Create a single output table that contains baseline cashflows and a ±25% sensitivity where we
#   perturb ALL parameters with $ units, then re-run the full cashflow model.
#
# What it does:
# - Computes the baseline `HabByAgeCashflows.csv` exactly as in script one.
# - Creates two alternative parameter sets:
#     - "minus": every $-unit parameter is multiplied by (1 - pct)  e.g. 0.75 for -25%
#     - "plus":  every $-unit parameter is multiplied by (1 + pct)  e.g. 1.25 for +25%
# - Re-runs `assemble_hab_by_age_cashflows()` for baseline / minus / plus.
# - Joins the resulting cashflows into ONE table with three cashflow columns.
#
# What it does NOT do:
# - It does NOT change any non-$ assumptions (e.g., slopes, distances, yields, volumes, areas, rotation
#   length, strip-planting proportion). Only parameters that are in $ units are perturbed.
#
# Output:
# - ONE csv with the baseline cashflow column (`cashFlow`) plus two additional columns:
#   `cashFlow_minus25pct` and `cashFlow_plus25pct`.

money_param_names <- function() {
  # All parameters in `default_params()` that are in $ units.
  c(
    # Protected areas ($/ha or $/ha/year)
    "paEst", "trans", "paStrict",

    # Selective logging ($/ha, $/m3, $/km, royalties/taxes)
    "fm", "log_price", "fixcost", "skid_base", "haul_per_km", "tax1",

    # Strip planting ($/ha); multipliers/thresholds are not $ and should NOT be scaled
    "strip_plant_base",

    # Plantations - clearing and transport ($/ha, $/m3, $/(m3*km), $/km)
    "pulp_price", "clear_fell_cost_per_ha", "clear_fell_slope_cost_per_ha",
    "lp_transport_per_m3_per_km", "royalty_per_m3", "mill_transport_per_m3_per_km",
    "tax2", "transport_cost_per_km",

    # Plantations - harvest prices ($/m3)
    "sr_dollars_m3", "lr_dollars_m3",

    # Planting and maintenance ($/ha)
    "plant_cost_short", "maint_cost_short_total", "plant_cost_long", "maint_cost_long_total"
  )
}

scale_money_params <- function(params, factor, money_names = money_param_names()) {
  p <- params
  present <- intersect(names(p), money_names)
  for (nm in present) {
    if (!is.numeric(p[[nm]]) || length(p[[nm]]) != 1 || !is.finite(p[[nm]])) {
      stop(paste0("Money param `", nm, "` is not a finite numeric scalar."))
    }
    p[[nm]] <- p[[nm]] * factor
  }
  p
}

combine_baseline_and_money_sensitivity <- function(
    baseline_params,
    pct = 0.25,
    minus_col = "cashFlow_minus25pct",
    plus_col = "cashFlow_plus25pct") {

  baseline_cf <- assemble_hab_by_age_cashflows(baseline_params)

  down_factor <- 1 - pct
  up_factor <- 1 + pct

  params_minus <- scale_money_params(baseline_params, down_factor)
  params_plus <- scale_money_params(baseline_params, up_factor)

  cf_minus <- assemble_hab_by_age_cashflows(params_minus) %>%
    rename("{minus_col}" := cashFlow) %>%
    select(original_habitat, habitat, functionalhabAge, all_of(minus_col))

  cf_plus <- assemble_hab_by_age_cashflows(params_plus) %>%
    rename("{plus_col}" := cashFlow) %>%
    select(original_habitat, habitat, functionalhabAge, all_of(plus_col))

  baseline_cf %>%
    left_join(cf_minus, by = c("original_habitat", "habitat", "functionalhabAge")) %>%
    left_join(cf_plus, by = c("original_habitat", "habitat", "functionalhabAge"))
}

main <- function(
    write_baseline = TRUE,
    baseline_out_path = file.path("Outputs", "HabByAgeCashflows.csv"),
    run_sensitivity = TRUE,
    pct = 0.25) {

  params <- default_params()

  cf_df <- if (run_sensitivity) {
    combine_baseline_and_money_sensitivity(
      baseline_params = params,
      pct = pct,
      minus_col = paste0("cashFlow_minus", round(pct * 100), "pct"),
      plus_col = paste0("cashFlow_plus", round(pct * 100), "pct")
    )
  } else {
    assemble_hab_by_age_cashflows(params)
  }

  if (write_baseline) write_cashflow_csv(cf_df, baseline_out_path)

  invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}

