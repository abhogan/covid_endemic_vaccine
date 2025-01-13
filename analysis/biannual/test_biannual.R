max_t <- 365*8
income_group <- "HIC"
target_pop <- 1e5
hs_constraints <- "Absent"
dt <- 0.5
seeding_cases <- 10
vaccine_doses <- 1
vfr <- 1
vfr2 <- 1
vfr_time1 <- 1
vfr_time2 <- 30
vfr2_time1 <- 31 # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- 60
mu_ab_infection <- (1.13/5.1*3)
max_coverage <- 0.05
vaccine <- "vaccine"

mu_ab_d1 = (1.13/5.1*2)
mu_ab_d2 = (1.13/5.1*2)
k = 2.5
hl_s = 35
hl_l = 1000
period_s <- 75
period_l <- 565 # not used
ab_50 = 0.091#log10(0.04)
ab_50_severe = 0.021#log10(0.005)
std10 = 0.44
t_d2 = 365*3
t_d3 = 181
t_d4 = 181
t_d5 = 181
t_d6 = 181
t_d7 = 181
t_d8 = 181
t_d9 = 181
immune_escape <- 1
matched_vacc <- 1
matched_vacc_level <- 1
max_ab <- 5

std10_infection <- 0.44
mu_ab_inf_scal_vfr <- 1

scenario <- 1
age_groups_covered <- 2
age_groups_covered_d3 <- 14
age_groups_covered_d4 <- 14
age_groups_covered_d5 <- 14
repetition <- 1
vacc_period <- 30
vacc_start <- 30
ICU_scal_vfr <-  1
hosp_scal_vfr <- 1
ICU_scal_vfr2 <- 1
hosp_scal_vfr2 <- 1
vfr_drift_factor <- 2
drift_start <- 365*4

name <- "runbiannual"
rt <- 4
source("R/immunity_process_biannual.R")
source("R/utils.R")
source("R/vaccine_function.R")
source("R/vaccine_strategy.R")

run_model_biannual()
df <- readRDS("../covid_booster_strategies/raw_outputs/output_runbiannual/scenario_1.rds") %>%
  mutate(scenario = "runbiannual")


ggplot(data = df, aes(x = timestep, y = incidence, col = scenario)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "incidence", color = "model") 
  #scale_x_continuous(breaks = c(seq(0,365*5, by = 365)), limits = c(180, 1825)) +
  #scale_y_continuous(limits = c(0,1250)) +
  th
