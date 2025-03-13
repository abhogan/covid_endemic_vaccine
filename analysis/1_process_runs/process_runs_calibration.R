library(tidyverse)
library(furrr)
library(purrr)

source("../covid_katana_runs/R/utils.R")
# read in scenarios
name <- "runs_calibrationmud1"
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
  
# summarise each run
for (i in 1:length(df_all)){
  saf_reps_summarise <- df_all[[i]] %>%
    # mutate(IMild_count = IMild_count + IAsymp_count) %>%
    # dplyr::select(-IAsymp_count) %>%
    #filter(timestep >= 365*7) %>%
    mutate(incidence = if_else(is.na(incidence), 0, incidence),
           hosp_all = if_else(is.na(hosp_all), 0, hosp_all)) %>%
    select(timestep, incidence, hosp_all, vax_ab_mean, nat_ab_mean, nat_ab_upper, nat_ab_lower, nat_mean, nat_lower, nat_upper, vax_ab_lower, vax_ab_upper, vax_ab_mean, X1_count, IMild_count, ICase_count, E_count) %>%
    pivot_longer(cols = c(incidence, hosp_all, vax_ab_mean, nat_ab_mean, nat_ab_upper, nat_ab_lower, nat_mean, nat_lower, nat_upper, vax_ab_lower, vax_ab_upper, vax_ab_mean, X1_count, IMild_count, ICase_count, E_count), names_to = "compartment") %>%
    group_by(compartment) %>%
    mutate(value = if_else(is.na(value), 0, value)) %>%
    ungroup() %>%
    pivot_wider(id_cols = timestep, names_from = "compartment", values_from = "value")  %>%
    mutate(cum_hosp_all = sum(hosp_all[timestep >= start_sum & timestep <= end_sum]),
           inc = sum(incidence[timestep >= start_sum & timestep <= end_sum])) %>%
    ungroup()
  
  saf_reps_summarise <- add_cols("X1_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X2_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X3_count", saf_reps_summarise)
  
  saf_reps_summarise <- saf_reps_summarise %>%
    mutate(doses = X1_count,
           total_doses = max(doses)) %>%
    nest(cols = c(timestep, X1_count, X2_count, X3_count, IMild_count, ICase_count, E_count, incidence, vax_ab_mean, vax_ab_lower, vax_ab_upper, nat_ab_mean, nat_ab_lower, nat_ab_upper, nat_mean, nat_lower, nat_upper, hosp_all, doses)) %>%
    unique()
  
  saf_reps_summarise$index <- scenario_num$scenario[i]
  df_all[[i]] <- saf_reps_summarise
}

# join the runs and link to parameters
df_all <- do.call(rbind, df_all)

df <- left_join(df_all, scenarios)

head(df)

# summarise totals over repetitions
df_totals <- df %>%
  # scale outputs to per-million-population
  select(-cols) %>%
  mutate(inc = inc / target_pop * 1e6,
         cum_hosp_all = cum_hosp_all / target_pop * 1e6,
         total_doses = total_doses / target_pop * 1e6) %>%
  group_by(across(-c(cum_hosp_all, inc, total_doses, index, repetition) )) %>%
 summarise(hosp_med = median(cum_hosp_all),
         hosp_lower = quantile(cum_hosp_all, 0.025),
         hosp_upper = quantile(cum_hosp_all, 0.975),
         inc_med = median(inc),
         inc_lower = quantile(inc, 0.025),
         inc_upper = quantile(inc, 0.975),
         total_doses_med = median(total_doses)) %>%
  ungroup() 

# summarise temporal dynamics over reptitions
df_summarise <- df %>%
  unnest(cols) %>%
  #filter(timestep > 1095) %>%
  # scale outputs to per-million-population
  mutate(X1_count = X1_count/ target_pop * 1e6,
         X2_count = X2_count/ target_pop * 1e6,
         X3_count = X3_count/ target_pop * 1e6,
         hosp_all = hosp_all / target_pop * 1e6,
         incidence = incidence / target_pop * 1e6,
         E_count = E_count / target_pop * 1e6,
         IMild_count = IMild_count/ target_pop * 1e6,
         ICase_count = ICase_count/ target_pop * 1e6)%>%
  # summarise across repetitions
  select(-c(cum_hosp_all, total_doses, inc, index, repetition)) %>%
  group_by(timestep, vacc_on, immune_escape, matched_vacc, matched_vacc_level, vfr2, name, rt, max_t, income_group, target_pop, hs_constraints, dt, seeding_cases, age_groups_covered, age_groups_covered_d3, age_groups_covered_d4, age_groups_covered_d5, max_coverage, vacc_period, vacc_start, vaccine_doses, vfr, vfr_time1, vfr_time2, vfr2_time1, vfr2_time2, vaccine, mu_ab_d1, mu_ab_d2, k, hl_l, hl_s, period_s, period_l, ab_50, ab_50_severe, std10, max_ab, mu_ab_infection, std10_infection, mu_ab_inf_scal_vfr, scenario_name, b1, hosp_scal) %>% 
  summarise(hosp_t = median(hosp_all),
            hosp_tmin = quantile(hosp_all, 0.025),
            hosp_tmax = quantile(hosp_all, 0.975),
            E_t = median(E_count),
            IMild_t = median(IMild_count),
            ICase_t = median(ICase_count),
            inc_t = median(incidence),
            inc_tmin = quantile(incidence, 0.025),
            inc_tmax = quantile(incidence, 0.975),
            vax_ab_med = median(vax_ab_mean),
            vax_ab_lower = median(vax_ab_lower),
            vax_ab_upper = median(vax_ab_upper),
            nat_ab_med = median(nat_ab_mean),
            nat_ab_lower = median(nat_ab_lower),
            nat_ab_upper = median(nat_ab_upper),
            nat_med = median(nat_mean),
            nat_lower = median(nat_lower),
            nat_upper = median(nat_upper),
            vaccines_t = median(X1_count + X2_count * 2 + X3_count * 3),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count)) %>%
  ungroup() %>%
  unique() %>%
  filter(timestep < max(timestep))

saveRDS(df_summarise, paste0("../covid_katana_runs/processed_outputs/df_summarise_", name, ".rds"))
saveRDS(df_totals, paste0("../covid_katana_runs/processed_outputs/df_summarise_totals_", name, ".rds"))

