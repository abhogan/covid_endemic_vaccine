
#########################################################################################
# routine runs - varied max_coverage (3 levels), and vaccine either on or off (no immune escape)
# two delivery scenarios: one dose to 65+ plus, or option of additional dose to 75+ after 6 months

offset <- 365*9+180

# totals
df_1y <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_totals_runs_routinemud1.rds") %>%
  select(max_coverage, hosp_med, inc_med, vacc_start, vacc_on, mu_ab_d1, vaccine_doses, rt) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(max_coverage = if_else(vacc_on == "baseline", 0, max_coverage )) %>%
  pivot_longer(c(inc_med, hosp_med)) %>%
  mutate(vacc_start = vacc_start - offset) %>%
  mutate(
    mu_ab_d1 = round(mu_ab_d1, 3)) 
  

# include vaccine = 0 for all scenarios
novacc <- df_1y %>%
  filter(max_coverage == 0) %>%
  select(-vacc_start, -mu_ab_d1, -vaccine_doses)

vacc_start = sort(unique(df$vacc_start))[1:3]
mu_ab_d1 = sort(unique(df$mu_ab_d1))
vaccine_doses <- c(1,2)

x <- expand_grid(vacc_start, mu_ab_d1, vaccine_doses)

y <- cross_join(x, novacc)



# attach no vacc
df_novacc <- df_1y %>%
  filter(max_coverage == 0) %>%
  select(-vacc_start, -vacc_on, -max_coverage, -mu_ab_d1, -vaccine_doses) %>%
  rename(value0 = value) %>%
  unique()

df_1y <- df_1y %>%
  filter(max_coverage != 0) %>%
  rbind(y)


df_1y <- df_1y %>%
  left_join(df_novacc) %>%
  mutate(prop_averted = (value0-value)/value0*100,
         value0 = value0/1e6*1e5,
         value = value/1e6*1e5,
         number_averted = value0-value
  ) %>%
  filter(rt == 3,
         name == "hosp_med")%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate(vacc_start_adjust = vacc_start - 395) %>%
  mutate(vacc_start_label =  paste0("relative vaccination start ", vacc_start_adjust, " days"))

head(df_1y)

df_1y2 <- df_1y %>%
  mutate(vacc_start_adjust_label = paste0("relative vaccination\nstart ", vacc_start_adjust, " days")) %>%
  mutate(vacc_start_adjust_label = factor(vacc_start_adjust_label, levels = c("relative vaccination\nstart 0 days",
                                                                              "relative vaccination\nstart 60 days",
                                                                              "relative vaccination\nstart 120 days")))
df_out <- df_1y2 %>%
  filter(vacc_on != "baseline") %>%
  arrange(vaccine_doses, mu_ab_d1, max_coverage, vacc_start_adjust_label) %>%
  select(vaccine_doses, mu_ab_d1, max_coverage, vacc_start_adjust_label, value, number_averted, prop_averted, value0) %>%
  mutate(value = round(value),
         value0 = round(value0),
         number_averted = round(number_averted),
         prop_averted = round(prop_averted, 1))

write_csv(df_out, "../covid_endemic_vaccine/table_outputs_routine.csv")



