library(tidyverse)

#### Set up simulation parameters ##############################################
name <- "scenario1"
rt <- c(2.5,3,3.5,4)
max_t <- 365*10
income_group <- "HIC"
target_pop <- 1e6
hs_constraints <- "Absent"
dt <- 0.25
seeding_cases <- 10
age_groups_covered <- 14
max_coverage <- 0.05
vacc_period <- 30
vacc_start <- 365*10-5
vaccine_doses <- 1
vfr <- 1
vfr2 <- 1
vfr_time1 <- 1
vfr_time2 <- 30
vfr2_time1 <- 1095 # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- 1095+90

vaccine <- "vaccine"
vacc_on <- 0

mu_ab_d1 = 1.13/5.2*3#log10(1.13)
mu_ab_d2 = 1.13/5.2*3#log10(1.13)
k = 2.5
hl_s = 35
hl_l = 1000
period_s <- 75
period_l <- 565 # not used
ab_50 = 0.091
ab_50_severe = 0.021
std10 = 0.44
immune_escape <- 0
matched_vacc <- 0
matched_vacc_level <- 1
max_ab <- 5

mu_ab_infection <- (1.13/5.2)*c(2,2.5,3,3.5,4)#log10(1.13)
std10_infection <- 0.44
mu_ab_inf_scal_vfr <- 1

age_groups_covered_d3 <- 14
age_groups_covered_d4 <- 14
age_groups_covered_d5 <- 14
b1 <- c(0.02, 0.04, 0.06,0.08)
hosp_scal <- 1
repetition <- 1:5

#### Create scenarios ##########################################################
scenarios <- expand_grid(
  vacc_on,
  immune_escape,
  matched_vacc,
  matched_vacc_level,
  vfr2,
  name,
  rt,
  max_t,
  income_group,
  target_pop,
  hs_constraints,
  dt,
  seeding_cases,
  age_groups_covered,
  age_groups_covered_d3,
  age_groups_covered_d4,
  age_groups_covered_d5,
  max_coverage,
  vacc_period,
  vacc_start,
  vaccine_doses,
  vfr,
  b1,
  vfr_time1,
  vfr_time2,
  vfr2_time1,
  vfr2_time2,
  vaccine,
  mu_ab_d1,
  k,
  hl_l,
  hl_s,
  period_s,
  period_l,
  ab_50,
  ab_50_severe,
  std10,
  max_ab,
  mu_ab_infection,
  std10_infection,
  mu_ab_inf_scal_vfr,
  hosp_scal,
  repetition) %>%
  mutate(vacc_start = if_else(vacc_on == 0, max_t, vacc_start)) %>%
  mutate(mu_ab_d2 = mu_ab_d1)

nrow(scenarios)

# name the scenarios
scenarios <- scenarios %>%
  mutate(scenario_name = "undefined")

scenarios$index <- 1:nrow(scenarios)

saveRDS(scenarios, "../covid_katana_runs/scenarios/scenarios_runs_calibrationrtb1mu.rds")

write_csv(scenarios, paste0("../covid_katana_runs/scenarios/scenarios_runs_calibrationrtb1mu.csv"))

#### Test on PC ###############################################################
# 
# source("R/run_function_immunity_new.R")
# source("R/utils.R")
# source("R/vaccine_strategy.R")
# 
# plan(multicore, workers = 4)
# system.time({out <- future_pmap(scenarios, run_scenario_new, .progress = TRUE)})
