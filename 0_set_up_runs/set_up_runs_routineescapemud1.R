library(tidyverse)

#### Set up simulation parameters ##############################################
name <- "scenario3"
rt <- c(3,3.5)
max_t <- 365*15
income_group <- "HIC"
target_pop <- 1e6
hs_constraints <- "Absent"
dt <- 0.25
seeding_cases <- 10
age_groups_covered <- 4
age_groups_covered_d2 <- 2
t_d2 <- 180
max_coverage <- 0.6
vacc_period <- 90
vfr <- 1
vfr_time1 <- 1
vfr_time2 <- 30
vaccine <- "vaccine"

k = 2.5
hl_s = 35
hl_l = 1000
period_s <- 75
period_l <- 565 # not used
ab_50 <- 0.091
ab_50_severe <- 0.021
std10 <- 0.44
immune_escape <- 1
matched_vacc <- 1
max_ab <- 5

mu_ab_infection <- 1.13/5.2*3
std10_infection <- 0.44
mu_ab_inf_scal_vfr <- 1

age_groups_covered_d3 <- 14
age_groups_covered_d4 <- 14
age_groups_covered_d5 <- 14

b1 <- 0.04
hosp_scal <- 1
ICU_scal <- 1

repetition <- 1:10
run <- 1:36

inputs <- read_csv("../covid_katana_runs/escape_inputs_mud1.csv") %>%
  mutate(mu_ab_d1 = mu_ab_d1*matched_vacc_level,
         mu_ab_d1 = mu_ab_d2*matched_vacc_level) %>%
  mutate(vfr2_time2 = vfr2_time1 + 60)


#### Create scenarios ##########################################################
sub <- expand_grid(
  immune_escape,
  matched_vacc,
  name,
  rt,
  max_t,
  income_group,
  target_pop,
  hs_constraints,
  dt,
  seeding_cases,
  age_groups_covered,
  age_groups_covered_d2,
  t_d2,
  age_groups_covered_d3,
  age_groups_covered_d4,
  age_groups_covered_d5,
  max_coverage,
  vacc_period,
  vfr,
  vfr_time1,
  vfr_time2,
  vaccine,
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
  repetition,
  b1,
  hosp_scal,
  ICU_scal,
  run)

scenarios <- left_join(sub, inputs) %>%
  select(-run)

nrow(scenarios)

scenarios$index <- 1:nrow(scenarios)

saveRDS(scenarios, "../covid_katana_runs/scenarios/scenarios_runs_routineescapemud1.rds")

write_csv(scenarios, "../covid_katana_runs/scenarios/scenarios_runs_routineescapemud1.csv")
