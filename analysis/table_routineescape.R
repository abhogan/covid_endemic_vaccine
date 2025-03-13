
offset <- 365*9+180

# plot totals
df_1y <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_totals_runs_routineescapemud1.rds") %>%
  select(vfr2_time1, vfr2, vaccine_doses, scenario_name, vacc_on, hosp_med, inc_med, rt) %>%
  mutate(vfr2_time1 = vfr2_time1 - offset-365) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  pivot_longer(c(inc_med, hosp_med)) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate( vfr2_time1_name = factor(vfr2_time1, levels = c(60, 120, 180), labels = c("2 months", "4 months", "6 months")))

# replicate "no vaccine" across both doses
df_novacc <- filter(df_1y, scenario_name == "no vaccine") %>%
  mutate(vaccine_doses = "65+ annual, 75+ 6m")

df_1y <- rbind(df_1y, df_novacc)%>%
  mutate(timeframe = "1y")%>%
  mutate(scenario_name = factor(scenario_name, levels = c("no vaccine", "unmatched", "partially-matched", "partially-matched d1, well-matched d2"))) %>%
  mutate(vfr2_label = paste0("immune escape ", vfr2)) %>%
  filter(rt == 3,
         vfr2 == 1.5)

#########################################################
# present as # hospitalisations averted per 100,000
df_novacc <- df_1y %>%
  filter(scenario_name == "no vaccine") %>%
  select(vfr2, vfr2_time1, vfr2_time1_name, vaccine_doses, name, value, timeframe, rt) %>%
  filter(timeframe == "1y") %>%
  select(-timeframe) %>%
  rename(value0 = value) %>%
  unique()

df_avert <- df_1y %>%
  filter(scenario_name != "no vaccine") %>%
  left_join(df_novacc) %>%
  mutate(averted = (value0 - value)/1e6*1e5)

df_1y_avert <- df_1y %>%
  filter(scenario_name != "no vaccine") %>%
  left_join(df_novacc) %>%
  mutate(averted = (value0 - value)/1e6*1e5) %>%
  mutate(scenario_name = factor(scenario_name, levels = c("unmatched", "partially-matched", "partially-matched d1, well-matched d2")))

df_out <- df_1y_avert %>%
  filter(name == "hosp_med") %>%
  arrange(vfr2_time1_name, vaccine_doses, scenario_name) %>%
  select(vaccine_doses, vfr2_time1_name, scenario_name, value0, value, averted) %>%
  mutate(value0 = round(value0/10),
         value= round(value/10),
         averted = round(averted),
         percent_averted = round( (value0-value)/value0*100,1))

write_csv(df_out, "../covid_endemic_vaccine/table_outputs_routineescape.csv")

