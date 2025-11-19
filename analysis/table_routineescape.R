
offset <- 365*9+180

# plot totals
df_1y <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_totals_runs_routineescapemud1new.rds") %>%
  select(vfr2_time1, vfr2, vaccine_doses, scenario_name, vacc_on, hosp_med, inc_med, rt,  contains("_med"), contains("_upper"), contains("_lower")) %>%
  mutate(vfr2_time1 = vfr2_time1 - offset-365) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate( vfr2_time1_name = factor(vfr2_time1, levels = c(60, 120, 180), labels = c("2 months", "4 months", "6 months"))) %>%
  mutate(scenario_name = factor(scenario_name, levels = c("unmatched", "partially-matched", "partially-matched d1, well-matched d2")))

# replicate "no vaccine" across both doses
df_novacc <- filter(df_1y, scenario_name == "no vaccine") %>%
  mutate(vaccine_doses = "65+ annual, 75+ 6m")

df_1y <- rbind(df_1y, df_novacc)%>%
  mutate(timeframe = "1y")%>%
  mutate(scenario_name = factor(scenario_name, levels = c("no vaccine", "unmatched", "partially-matched", "partially-matched d1, well-matched d2"))) %>%
  mutate(vfr2_label = paste0("immune escape ", vfr2)) %>%
  filter(rt == 3,
         vfr2 == 1.5)

df_out <- df_1y %>%
  filter(vacc_on != "baseline") %>%
  arrange(vfr2_time1_name, vaccine_doses, scenario_name) %>%
  select(vaccine_doses, vfr2_time1_name, scenario_name, hosp_med, hosp_lower, hosp_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper, prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper) %>%
  pivot_longer(cols = c(hosp_med, hosp_lower, hosp_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper)) %>%
  mutate(value = round(value/10)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  pivot_longer(cols = c(prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper)) %>%
  mutate(value = round(value, 1)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(hosp_out = paste0(hosp_med, " (", hosp_lower, ", ", hosp_upper, ")")) %>%
  mutate(avert_out = paste0(cum_hosp_averted_med, " (", cum_hosp_averted_lower, ", ", cum_hosp_averted_upper, ")")) %>%
  mutate(prop_out = paste0(prop_hosp_averted_med, " (", prop_hosp_averted_lower, ", ", prop_hosp_averted_upper, ")")) %>%
  select(vaccine_doses, vfr2_time1_name, scenario_name, hosp_out, avert_out, prop_out)
df_out

write_csv(df_out, "../covid_endemic_vaccine/table_outputs_routineescape.csv")

df_1y %>%
  filter(vacc_on == "baseline") %>%
  select(vfr2_time1, vfr2, vacc_on, hosp_med, hosp_lower, hosp_upper) %>%
  mutate(hosp_med = round(hosp_med/10),
         hosp_lower = round(hosp_lower/10),
         hosp_upper = round(hosp_upper/10)) %>%
  mutate(hosp_out = paste0(hosp_med, " (", hosp_lower, ", ", hosp_upper, ")")) %>%
  select(vfr2_time1, vfr2, vacc_on, hosp_out)
