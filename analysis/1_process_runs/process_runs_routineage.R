library(tidyverse)
library(furrr)
library(purrr)
library(countrycode)

# read in scenarios
name <- "runs_routineage"
scenarios <- readRDS(paste0("../covid_katana_runs/scenarios/scenarios_", name, ".rds"))
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
n_vacc2 <- round(sum(pop$n[12:13]))

hosp_comps <- unique(x$compartment)[6:14]

x <- df_all[[28]]
y <- x %>%
  mutate(age = as.numeric(age)) %>%
  group_by(age, scenario) %>%
  filter(compartment %in% c("E")) %>%
  summarise(value = sum(value)/4.6)

y2 <- x %>%
  mutate(age = as.numeric(age)) %>%
  group_by(age, scenario) %>%
  filter(compartment %in% c("ICase")) %>%
  summarise(value_hosp = sum(value)/7)

y3 <- left_join(y, y2) %>%
  mutate(prop_hosp = value_hosp/value*100)
y3

ggplot(data = filter(y, timestep > 365*5, timestep < 365*6), aes(x = timestep, y = value, col = factor(age))) +
  geom_line()




temp <- df_all

df_all_inf <- NULL
df_all_hosp <-NULL
df_all_hosp2 <-NULL

# summarise each run
for (i in 1:length(df_all)){
  saf_reps_summarise <- df_all[[i]] %>%
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

s <- temp[[1]] %>%
  mutate(age = as.numeric(age)) %>%
  group_by(compartment, timestep) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(compartment != "S", compartment != "D", compartment != "R") %>%
  filter(timestep >500)
head(s)
ggplot(data = s, aes(x = timestep, y = value, col = compartment)) +
  geom_line()



for (i in 1:length(df_all)){
  saf_reps_summarise_hosp <- df_all[[i]] %>%
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


# for (i in 1:length(df_all)){
#   saf_reps_summarise_hosp2 <- df_all[[i]] %>%
#     mutate(age = as.numeric(age)) %>%
#     filter(compartment %in% c("ICase"),
#            age>=12, age<=13) %>%
#     group_by(timestep) %>%
#     summarise(hosp = sum(value)) %>%
#     ungroup() %>%
#     mutate(prev = hosp /n_vacc2) %>%
#     mutate(hosp = prev/7 *n_vacc2)
#   saf_reps_summarise_hosp2$index <- scenario_num$scenario[i]
#   df_all_hosp2[[i]] <- saf_reps_summarise_hosp2
# }

# join the runs# join the runs# join the runs and link to parameters
df_all_inf <- do.call(rbind, df_all_inf)

df_inf <- left_join(df_all_inf, scenarios)

head(df_inf)

# join the runs# join the runs# join the runs and link to parameters
df_all_hosp <- do.call(rbind, df_all_hosp)

df_hosp <- left_join(df_all_hosp, scenarios)

head(df_hosp)


# df_all_hosp2 <- do.call(rbind, df_all_hosp2)
# 
# df_hosp2 <- left_join(df_all_hosp2, scenarios)
# 
# head(df_hosp2)


# summarise temporal dynamics over repetitions
df_summarise <- df_inf %>%
  # summarise across repetitions
  select(-c(index, repetition)) %>%
  group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name) %>% 
  summarise(infectious = median(infectious),
            prev = median(prev),
            incidence = median(incidence)) %>%
  ungroup() %>%
  unique() %>%
  filter(timestep < max(timestep),
         timestep > (3860-365))

ggplot(data = df_summarise, aes(x = timestep, y = incidence, col = factor(vacc_on))) +
  geom_line() +
  facet_grid(mu_ab_d1~vacc_start)


# summarise temporal dynamics over reptitions
df_summarise_hosp <- df_hosp %>%
  # summarise across repetitions
  select(-c(index, repetition)) %>%
  group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name) %>% 
  summarise(
            hosp = median(hosp)) %>%
  ungroup() %>%
  unique() %>%
  filter(timestep < max(timestep),
         timestep > (3860-365))

# # summarise temporal dynamics over reptitions
# df_summarise_hosp2 <- df_hosp2 %>%
#   # summarise across repetitions
#   select(-c(index, repetition)) %>%
#   group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name) %>% 
#   summarise(
#     hosp = median(hosp)) %>%
#   ungroup() %>%
#   unique() %>%
#   filter(timestep < max(timestep),
#          timestep > (3860-365))



# attach no-vacc scenario
df_novacc <- df_summarise_hosp %>%
  filter(vacc_on == 0) %>%
  select(timestep, hosp, mu_ab_d1, vacc_on)
df_summarise_hosp <- df_summarise_hosp %>%
  select(timestep, vacc_on, hosp, mu_ab_d1, vacc_start)

x <- data.frame(vacc_start = sort(unique(df_summarise_hosp$vacc_start))[1:3])
y <- cross_join(df_novacc, x)

df_summarise_hosp <- df_summarise_hosp %>%
  rbind(y)

df_summarise_hosp <- df_summarise_hosp %>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 3)) %>%
  mutate(mu_ab_d1 = factor(mu_ab_d1, labels = c("unmatched", "partially-matched", "well-matched")))
ggplot(data = filter(df_summarise_hosp, vacc_start != 5475, timestep <= 5000), aes(x = timestep, y = hosp, col = factor(vacc_on))) +
  geom_line() +
  facet_grid(mu_ab_d1~vacc_start) +
  th +
  geom_vline(aes(xintercept = vacc_start), linetype = "dashed") +
  scale_x_continuous(breaks = seq(3500, 5500, by = 365), labels = seq(0,365*5, by = 365))
  

# attach no-vacc scenario
df_summarise_inc <- df_summarise
df_novacc <- df_summarise_inc %>%
  filter(vacc_on == 0) %>%
  select(timestep, incidence, mu_ab_d1, vacc_on)
df_summarise_inc <- df_summarise_inc %>%
  select(timestep, vacc_on, incidence, mu_ab_d1, vacc_start)

x <- data.frame(vacc_start = sort(unique(df_summarise_inc$vacc_start))[1:3])
y <- cross_join(df_novacc, x)

df_summarise_inc <- df_summarise_inc %>%
  rbind(y)%>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 3)) %>%
  mutate(mu_ab_d1 = factor(mu_ab_d1, labels = c("unmatched", "partially-matched", "well-matched")))

ggplot(data = filter(df_summarise_inc, vacc_start != 5475, timestep <= 5000), aes(x = timestep, y = incidence, col = factor(vacc_on))) +
  geom_line() +
  facet_grid(mu_ab_d1~vacc_start) +
  th +
  geom_vline(aes(xintercept = vacc_start), linetype = "dashed") +
  scale_x_continuous(breaks = seq(3500, 5500, by = 365), labels = seq(0,365*5, by = 365))

# estimate difference in attack rates: 6 months
df_summarise_inc %>%
  group_by(vacc_on, mu_ab_d1, vacc_start) %>%
  filter(timestep > vacc_start + 30,
         timestep < vacc_start + 30 + 183) %>%
  summarise(inc = sum(incidence)) %>%
  mutate(AR = inc / n_vacc) %>%
  select(-inc) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(effectiveness = (`0` - `1`)/`0`)


df_summarise_hosp %>%
  group_by(vacc_on, mu_ab_d1, vacc_start) %>%
  filter(timestep > vacc_start + 30,
         timestep < vacc_start + 30 + 183) %>%
  summarise(hosp = sum(hosp)) %>%
  mutate(AR = hosp / n_vacc) %>%
  select(-hosp) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(eff =(`0` - `1`) / `0`)



# estimate difference in attack rates: 12 months from start of wave
df_summarise_inc %>%
  group_by(vacc_on, mu_ab_d1, vacc_start) %>%
  filter(vacc_start != 5475) %>%
  filter(timestep > 3860,
         timestep < 3860 + 365) %>%
  summarise(inc = sum(incidence)) %>%
  mutate(AR = inc / n_vacc) %>%
  select(-inc) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(effectiveness = (`0` - `1`)/`0`)


df_summarise_hosp %>%
  group_by(vacc_on, mu_ab_d1, vacc_start) %>%
  filter(vacc_start != 5475) %>%
  filter(timestep > 3860,
         timestep < 3860 + 365) %>%
  summarise(hosp = sum(hosp)) %>%
  mutate(AR = hosp / n_vacc) %>%
  select(-hosp) %>%
  ungroup() %>%
  pivot_wider(names_from = vacc_on, values_from = AR) %>%
  mutate(eff =(`0` - `1`) / `0`)


