library(dplyr)
library(tidyr)
library(tibble)
library(data.table)
library(countrycode)
library(safirimmunity)

source("R/run_function_immunity_new.R")
source("R/utils.R")
source("R/vaccine_strategy.R")

scenarios <- readRDS("scenarios/scenarios_runs_calibrationrtb1mu.rds")
index <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
scenarios_in <- scenarios[index,]

run_scenario_new(
  name = "runs_calibrationrtb1mu",
  index = index,
  b1 = scenarios_in$b1,
  mu_ab_infection = scenarios_in$mu_ab_infection,
  ab_50 = scenarios_in$ab_50,
  ab_50_severe = scenarios_in$ab_50_severe,
  mu_ab_d1 = scenarios_in$mu_ab_d1,
  mu_ab_d2 = scenarios_in$mu_ab_d2,
  k = scenarios_in$k,
  income_group = scenarios_in$income_group,
  hs_constraints = scenarios_in$hs_constraints,
  scenario_name = scenarios_in$scenario_name,
  max_t = scenarios_in$max_t,
  rt = scenarios_in$rt,
  dt = scenarios_in$dt,
  seeding_cases = scenarios_in$seeding_cases,
  max_coverage = scenarios_in$max_coverage,
  vaccine_doses = scenarios_in$vaccine_doses,
  vfr = scenarios_in$vfr,
  vfr_time1 = scenarios_in$vfr_time1,
  vfr_time2 = scenarios_in$vfr_time2,
  vaccine = scenarios_in$vaccine,
  target_pop = scenarios_in$target_pop,
  age_groups_covered = scenarios_in$age_groups_covered,
  vacc_period = scenarios_in$vacc_period,
  vacc_start = scenarios_in$vacc_start,
  immune_escape = scenarios_in$immune_escape,
  vacc_on = scenarios_in$vacc_on,
  matched_vacc = scenarios_in$matched_vacc,
  matched_vacc_level = scenarios_in$matched_vacc_level,
  vfr2 = scenarios_in$vfr2,
  vfr2_time1 = scenarios_in$vfr2_time1,
  vfr2_time2 = scenarios_in$vfr2_time2,
  repetition = scenarios_in$repetition,
  hosp_scal = scenarios_in$hosp_scal
)
