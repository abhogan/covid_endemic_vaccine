
#########################################################################################
# routine runs - varied max_coverage (3 levels), and vaccine either on or off (no immune escape)
# two delivery scenarios: one dose to 65+ plus, or option of additional dose to 75+ after 6 months

offset <- 365*9+180

# totals
df_1y <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_totals_runs_routinemud1_new.rds") %>%
  select(max_coverage, hosp_med, hosp_upper, hosp_lower, inc_med, inc_upper, inc_lower, vacc_start, vacc_on, mu_ab_d1, vaccine_doses, rt, contains("_med"), contains("_upper"), contains("_lower")) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(max_coverage = if_else(vacc_on == "baseline", 0, max_coverage )) %>%
  mutate(vacc_start = vacc_start - offset) %>%
  mutate(
    mu_ab_d1 = round(mu_ab_d1, 3)) %>%
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
  select(vaccine_doses, mu_ab_d1, max_coverage, vacc_start_adjust_label, hosp_med, hosp_lower, hosp_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper, prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper) %>%
  pivot_longer(cols = c(hosp_med, hosp_lower, hosp_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper)) %>%
  mutate(value = round(value/10)) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    pivot_longer(cols = c(prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper)) %>%
    mutate(value = round(value, 1)) %>%
    pivot_wider(names_from = name, values_from = value) %>%
  mutate(hosp_out = paste0(hosp_med, " (", hosp_lower, ", ", hosp_upper, ")")) %>%
  mutate(avert_out = paste0(cum_hosp_averted_med, " (", cum_hosp_averted_lower, ", ", cum_hosp_averted_upper, ")")) %>%
  mutate(prop_out = paste0(prop_hosp_averted_med, " (", prop_hosp_averted_lower, ", ", prop_hosp_averted_upper, ")")) %>%
  select(vaccine_doses, mu_ab_d1, max_coverage, vacc_start_adjust_label, hosp_out, avert_out, prop_out)
df_out

write_csv(df_out, "../covid_endemic_vaccine/table_outputs_routine.csv")

df_1y %>%
  filter(vacc_on == "baseline")
