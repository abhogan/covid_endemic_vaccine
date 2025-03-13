library(tidyverse)
library(furrr)
library(purrr)
library(countrycode)

# read in scenarios
name <- "runs_calibrationagertb1"
scenarios <- readRDS(paste0("../covid_katana_runs/scenarios/scenarios_", name, ".rds")) %>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 3))
start_sum <- unique(scenarios$max_t) - 2*365
end_sum <- start_sum + 365

# read in raw outputs
df_all <- list.files(path = paste0("../covid_katana_runs/raw_outputs/output_", name, "/"), pattern = ".rds")
df_all <- map(paste0("../covid_katana_runs/raw_outputs/output_", name, "/", df_all), readRDS)
scenario_num <- data.frame(scenario_num = list.files(path = paste0("../covid_katana_runs/raw_outputs/output_", name, "/"), pattern = ".rds")) %>%
    separate(scenario_num, c("A", "B", "C", "scenario_num"), sep = "_") %>%
    separate(scenario_num, c("scenario", "B"), sep = ".rds") %>%
    select(scenario) %>%
    mutate(scenario = as.double(scenario))

target_pop <- unique(scenarios$target_pop)
rep_country <- "Australia"
iso3c <- countrycode(rep_country, origin = "country.name", destination = "iso3c")
pop <- squire::get_population(country = rep_country)
pop_standardise <- target_pop / sum(pop$n)
pop$n <- as.integer(pop$n * pop_standardise)
n_vacc <- round(sum(pop$n[14:17]))


x <- df_all[[28]]
y <- x %>%
  mutate(age = as.numeric(age)) %>%
  group_by(timestep, age, scenario) %>%
  filter(compartment %in% c("IMild", "IAsymp", "ICase"), age >= 14) %>%
  summarise(value = sum(value))
ggplot(data = filter(y, timestep > 365*9), aes(x = timestep, y = value, col = factor(age))) +
  geom_line()

temp <- df_all

df_all_inf <- NULL
df_all_hosp <-NULL

# summarise each run
for (i in 1:length(df_all)){
  saf_reps_summarise <- df_all[[i]] %>%
    filter(timestep < max(timestep),
           timestep > (365*8+180)) %>%
    mutate(age = as.numeric(age)) %>%
    filter(compartment %in% c("E"),
           age>=14) %>%
    group_by(timestep) %>%
    summarise(infectious = sum(value)) %>%
    ungroup() %>%
    mutate(prev = infectious /n_vacc) %>%
    mutate(incidence = prev/4.6 *n_vacc)
  saf_reps_summarise$index <- scenario_num$scenario[i]
  df_all_inf[[i]] <- saf_reps_summarise
}

for (i in 1:length(df_all)){
  saf_reps_summarise_hosp <- df_all[[i]] %>%
    filter(timestep < max(timestep),
           timestep > (365*8+180)) %>%
    mutate(age = as.numeric(age)) %>%
    filter(compartment %in% c("ICase"),
           age>=14) %>%
    group_by(timestep) %>%
    summarise(hosp = sum(value)) %>%
    ungroup() %>%
    mutate(prev = hosp /n_vacc) %>%
    mutate(hosp = prev/7 *n_vacc)
  saf_reps_summarise_hosp$index <- scenario_num$scenario[i]
  df_all_hosp[[i]] <- saf_reps_summarise_hosp
}

# join the runs# join the runs# join the runs and link to parameters
df_all_inf <- do.call(rbind, df_all_inf)

df_inf <- left_join(df_all_inf, scenarios)

head(df_inf)

# join the runs# join the runs# join the runs and link to parameters
df_all_hosp <- do.call(rbind, df_all_hosp)

df_hosp <- left_join(df_all_hosp, scenarios)

head(df_hosp)

# summarise temporal dynamics over reptitions
df_summarise <- df_inf %>%
  # summarise across repetitions
  select(-c(index, repetition)) %>%
  group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name, b1) %>% 
  summarise(infectious = median(infectious),
            prev = median(prev),
            incidence = median(incidence)) %>%
  ungroup() %>%
  unique()

ggplot(data = df_summarise, aes(x = timestep, y = incidence, col = factor(vacc_on))) +
  geom_line() +
  facet_grid(mu_ab_d1~rt) +
  th


# summarise temporal dynamics over reptitions
df_summarise_hosp <- df_hosp %>%
  # summarise across repetitions
  select(-c(index, repetition)) %>%
  group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name) %>% 
  summarise(
            hosp = median(hosp)) %>%
  ungroup() %>%
  unique()

ggplot(data = df_summarise_hosp, aes(x = timestep, y = hosp, col = factor(vacc_on))) +
  geom_line() +
  facet_grid(mu_ab_d1~rt)


# estimate difference in attack rates over 6 months
df_summarise %>%
  group_by(vacc_on, mu_ab_d1, rt) %>%
  filter(timestep > 365*9+180 + 15,
         timestep < 365*9+180 + 15 + 183) %>%
  summarise(inc = sum(incidence)) %>%
  mutate(AR = inc / n_vacc) %>%
  select(vacc_on, mu_ab_d1, AR, rt) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(effectiveness = (`0` - `1`)/`0`*100)

df_summarise_hosp %>%
  group_by(vacc_on, mu_ab_d1, rt) %>%
  filter(timestep > 365*9+180 + 15,
         timestep < 365*9+180 + 15 + 183) %>%
  summarise(hosp = sum(hosp)) %>%
  mutate(AR = hosp / n_vacc) %>%
  select(vacc_on, mu_ab_d1, AR, rt) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(effectiveness = (`0` - `1`)/`0`*100)

saveRDS(df_summarise, paste0("../covid_katana_runs/processed_outputs/df_summarise_", name, ".rds"))
saveRDS(df_totals, paste0("../covid_katana_runs/processed_outputs/df_summarise_totals_", name, ".rds"))

