# some plotting things
theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text.align = 0,
            legend.text=element_text(size=14),
            legend.title=element_text(size=16),
            axis.text=element_text(size=16),
            axis.title=element_text(size=16),
            strip.text.x = element_text(size = 16))

##############################################################
# illustrate burn-in
df <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_runs_calibrationrtb1mu.rds") %>%
  mutate(prev = inc_t /1e6 * 7 *100) %>%
  mutate(mu_ab_infection = round(mu_ab_infection, 3)) %>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 2)) %>%
  mutate(scenario_1 = if_else(vacc_on == "baseline", "baseline (no vaccine)", if_else(mu_ab_d1 == 0.22, "partially-matched vaccine", "well-matched vaccine"))) %>%
  dplyr::group_by(vacc_on, scenario_1, mu_ab_d1, b1, rt, mu_ab_infection) %>%
  mutate(prevmean = rollmean(prev, 7, na.pad = TRUE),
         incmean = rollmean(inc_t, 7, na.pad = TRUE),
         hospmean = rollmean(hosp_t, 7, na.pad = TRUE),
         incmean_upper = rollmean(inc_tmax, 7, na.pad = TRUE),
         incmean_lower = rollmean(inc_tmin, 7, na.pad = TRUE),
         hospmean_upper = rollmean(hosp_tmax, 7, na.pad = TRUE),
         hospmean_lower = rollmean(hosp_tmin, 7, na.pad = TRUE)) %>%
  filter(vacc_on == 0) %>%
  filter(timestep > 365*7+180,
         timestep<=365*9+180) %>%
  mutate(timestep = timestep - 365*7-180)

ggplot(data = df, aes(x = timestep, y = incmean)) +
  geom_line() +
  facet_grid(rt~b1+mu_ab_infection)+
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0,365*10,by=365*2), limits = c(0,365*2)) +
  labs(x = "time (years)", y = "infection incidence per million\n(rolling 7-day mean)\nlog10 scale") +
  th 

ggplot(data = df, aes(x = timestep, y = hospmean)) +
  geom_line() +
  facet_grid(rt~b1+mu_ab_infection)+
  scale_x_continuous(breaks = seq(0,365*10,by=365), limits = c(0,365*2)) +
  labs(x = "time (days)", y = "hospitalisations per million\n(rolling 7-day mean)\nlog10 scale") +
  th 


ggplot(data = filter(df, b1 == 0.04), aes(x = timestep, y = prevmean)) +
  geom_line() +
  facet_grid(rt~mu_ab_infection)+
  scale_x_continuous(breaks = seq(0,365*10,by=365), limits = c(0,365*2)) +
  labs(x = "time (days)", y = "prevalence (%)") +
  th 

# calculate annual attack rate for all scenarios
z <- df %>%
  filter(timestep < max(df$timestep),
         timestep > max(df$timestep)-365) %>%
  mutate(prev = inc_t /1e6 * 7 *100)

av_infections <- z %>%
  group_by(rt, mu_ab_infection, hosp_scal, vacc_on, mu_ab_d1, b1) %>%
  summarise(infect = sum(inc_t)/1e6) 
av_infections

# include attack rate on infections plot
ggplot(data = filter(df, b1 == 0.04), aes(x = timestep, y = incmean)) +
  geom_line() +
  facet_grid(rt~mu_ab_infection)+
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0,365*10,by=365), limits = c(0,365*2)) +
  labs(x = "time (days)", y = "infection incidence per million\n(rolling 7-day mean)\nlog10 scale") +
  th +
  geom_text(data = filter(av_infections, b1 == 0.04), aes(x = 500, y = 800, label = paste0("AR = ",round(infect,2))), col = c4)


x <- data.frame(mu_ab_infection = sort(unique(df$mu_ab_infection)),
                mu_ab_labels = (paste("delta[I]==", c(2,2.5,3,3.5,4), sep = "")))
y <- data.frame(rt = c(2.5,3,3.5,4),
                rt_labels = paste0("R[t]==", c(2.5,3,3.5,4)))
              
x
df2 <- left_join(df, x) %>%
  left_join(y)

av_infections <- av_infections %>%
  left_join(x) %>%
  left_join(y)

# include attack rate on hosp plot
ggplot(data = filter(df2, b1 == 0.04), aes(x = timestep, y = hospmean)) +
  geom_line() +
  geom_ribbon(aes(ymin = hospmean_lower, ymax = hospmean_upper), alpha = 0.2, col = NA) +
  facet_grid(rt_labels~mu_ab_labels, labeller = label_parsed)+
  scale_x_continuous(breaks = seq(0,365*10,by=365), limits = c(0,365*2)) +
  labs(x = "time (days)", y = "hospitalisations per million\n(rolling 7-day mean)") +
  th +
  geom_text(data = filter(av_infections, b1 == 0.04), aes(x = 500, y = 45, label = paste0("AR==",round(infect,2))), col = c4, parse = T) +
  scale_y_continuous(limits = c(0,50))

ggsave("../covid_endemic_vaccine/figures/initial_calibration_FigS3.png", height = 8, width = 10)


