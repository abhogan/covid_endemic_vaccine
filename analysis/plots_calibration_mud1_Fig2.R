
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
df <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_runs_calibrationmud1.rds") %>%
  mutate(prev = inc_t /1e6 * 7 *100) %>%
  mutate(mu_ab_infection = round(mu_ab_infection, 3)) %>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 2)) %>%
  filter(mu_ab_d1 %in% c(0.22, 0.43)) %>%
  mutate(scenario_1 = if_else(vacc_on == "baseline", "baseline (no vaccine)", if_else(mu_ab_d1 == 0.22, "partially-matched vaccine", "well-matched vaccine"))) %>%
  dplyr::group_by(vacc_on, scenario_1, mu_ab_d1, rt) %>%
  mutate(prevmean = rollmean(prev, 7, na.pad = TRUE),
         incmean = rollmean(inc_t, 7, na.pad = TRUE),
         hospmean = rollmean(hosp_t, 7, na.pad = TRUE),
         incmean_upper = rollmean(inc_tmax, 7, na.pad = TRUE),
         incmean_lower = rollmean(inc_tmin, 7, na.pad = TRUE),
         hospmean_upper = rollmean(hosp_tmax, 7, na.pad = TRUE),
         hospmean_lower = rollmean(hosp_tmin, 7, na.pad = TRUE)) %>%
  filter(vacc_on == 0, mu_ab_d1 == 0.43)

ggplot(data = df, aes(x = timestep, y = incmean)) +
  geom_line() +
  geom_ribbon(aes(ymin = incmean_lower, ymax = incmean_upper), alpha = 0.5) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0,365*10,by=365*2), labels = seq(0,10,by=2), limits = c(0,365*10)) +
  labs(x = "time (years)", y = "infection incidence per million\n(rolling 7-day mean)\nlog10 scale") +
  th 
ggsave("../covid_endemic_vaccine/figures/burnin_FigS2.png", height = 5, width = 8)

##############################################################
# plot calibration outputs
df <- readRDS("../covid_katana_runs/processed_outputs/df_summarise_runs_calibrationmud1.rds") %>%
  filter(timestep >= 365*10+180, timestep < 365*18+180) %>%
  mutate(timestep = timestep - 365*10-180,
         vacc_start = vacc_start - 365*10-180) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(prev = inc_t /1e6 * 7 *100,
         prev_upper = inc_tmax/1e6 * 7 * 100,
         prev_lower = inc_tmin / 1e6 * 7 * 100) %>%
  mutate(mu_ab_infection = round(mu_ab_infection, 3)) %>%
  mutate(mu_ab_d1 = round(mu_ab_d1, 2)) %>%
  filter(mu_ab_d1 %in% c(0.22, 0.43)) %>%
  mutate(scenario_1 = if_else(vacc_on == "baseline", "baseline (no vaccine)", if_else(mu_ab_d1 == 0.22, "partially-matched vaccine", "well-matched vaccine"))) %>%
    dplyr::group_by(vacc_on, scenario_1, mu_ab_d1, rt, b1) %>%
    mutate(prevmean = rollmean(prev, 7, na.pad = TRUE),
           prevmean_upper = rollmean(prev_upper, 7, na.pad = TRUE),
           prevmean_lower = rollmean(prev_lower, 7, na.pad = TRUE),
           incmean = rollmean(inc_t, 7, na.pad = TRUE),
           hospmean = rollmean(hosp_t, 7, na.pad = TRUE),
           incmean_upper = rollmean(inc_tmax, 7, na.pad = TRUE),
           incmean_lower = rollmean(inc_tmin, 7, na.pad = TRUE),
           hospmean_upper = rollmean(hosp_tmax, 7, na.pad = TRUE),
           hospmean_lower = rollmean(hosp_tmin, 7, na.pad = TRUE))

vs <- min(unique(df$vacc_start))

# incidence over time
p1 <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = incmean, col = scenario_1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  scale_y_continuous(limits = c(0,3200)) +
  labs(x = "time (days)", y = "infections", col = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th +
  scale_color_manual(values = c(c1, c5, c4))
p1

# severe disease over time
p2 <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = hospmean, col = scenario_1)) +
  geom_line()+
  #geom_ribbon(aes(ymin = hospmean_lower, ymax = hospmean_upper, fill = scenario_1), alpha = 0.3, col = NA) +
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  labs(x = "time (days)", y = "hospitalisations", col = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th +
  scale_color_manual(values = c(c1, c5, c4)) +
  scale_fill_manual(values = c(c1, c5, c4))
p2

# prevalence over time
prev <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = prevmean, col = scenario_1)) +
  geom_line()+
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  labs(x = "time (days)", y = "prevalence (%)", col = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th  +
  scale_color_manual(values = c(c1, c5, c4)) +
  scale_y_continuous(limits = c(0,2.3))

prev

# inc, hosp and prev over time with stochastic variability illustrated
p1_var <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = incmean, col = scenario_1)) +
  geom_line() +
  geom_ribbon(aes(ymin = incmean_lower, ymax = incmean_upper, fill = scenario_1), alpha = 0.2, col = NA) +
  facet_wrap(~scenario_1, nrow = 1) +
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  scale_y_continuous(limits = c(0,3500)) +
  labs(x = "time (days)", y = "infections", col = "scenario", fill = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th +
  scale_color_manual(values = c(c1, c5, c4)) +
  scale_fill_manual(values = c(c1, c5, c4))

p1_var

p2_var <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = hospmean, col = scenario_1)) +
  geom_line()+
  geom_ribbon(aes(ymin = hospmean_lower, ymax = hospmean_upper, fill = scenario_1), alpha = 0.2, col = NA) +
  facet_wrap(~scenario_1, nrow = 1) +
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  labs(x = "time (days)", y = "hospitalisations", col = "scenario", fill = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th +
  scale_color_manual(values = c(c1, c5, c4)) +
  scale_fill_manual(values = c(c1, c5, c4))

p2_var

prev_var <- ggplot(data = filter(df, (vacc_on == "baseline" & mu_ab_d1 == 0.22) | vacc_on == "vaccine"), aes(x = timestep, y = prevmean, col = scenario_1)) +
  geom_line()+
  geom_ribbon(aes(ymin = prevmean_lower, ymax = prevmean_upper, fill = scenario_1), alpha = 0.2, col = NA) +
  facet_wrap(~scenario_1, nrow = 1) +
  scale_x_continuous(breaks = seq(0,365*4,by=365)) +
  labs(x = "time (days)", y = "prevalence (%)", col = "scenario", fill = "scenario")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  th  +
  scale_color_manual(values = c(c1, c5, c4)) +
  scale_y_continuous(limits = c(0,2.3))+
  scale_fill_manual(values = c(c1, c5, c4))

prev_var

# immune recognition over time
df2 <- df %>%
  ungroup() %>%
  #filter(mu_ab_d1 ==0.43) %>%
  select(timestep, vacc_on, nat_ab_med, nat_med, vax_ab_med, scenario_1) %>%
  pivot_longer(cols = c(nat_ab_med, nat_med, vax_ab_med)) %>%
  mutate(name = factor(name, levels = c("nat_med", "nat_ab_med", "vax_ab_med"), labels = c("combined", "infection", "vaccine"))) %>%
  unique()

imm <- ggplot(data = df2, aes(x = timestep, y = value, col = factor(scenario_1))) +
  facet_wrap(~name) +
  geom_line() +
  scale_x_continuous( breaks = c(seq(0,365*20, by = 365))) +
  th +
  labs(x = "time (days)", y = "immune recognition", col = "scenario", linetype = "immune\nrecognition type")+
  geom_vline(xintercept = vs, linetype = "dashed", col = "grey4") +
  scale_color_manual(values = c(c1, c5, c4))

imm

#prevalence and attack rate
z <- df %>%
  filter(timestep < max(df$timestep),
         timestep > max(df$timestep)-365) %>%
mutate(prev = inc_t /1e6 * 7 *100)

av_infections <- z %>%
  group_by(rt, mu_ab_infection, hosp_scal, vacc_on, mu_ab_d1) %>%
  summarise(infect = sum(inc_t)/1e6)
av_infections

# assemble plots
leg <- get_legend(p1)
p1 <- p1 +
  guides(fill="none", color="none", shape = "none", linetype = "none", size = "none")
p2 <- p2 +
  guides(fill="none", color="none", shape = "none", linetype = "none", size = "none")
prev <- prev +
  guides(fill="none", color="none", shape = "none", linetype = "none", size = "none")
imm <- imm +
  guides(fill="none", color="none", shape = "none", linetype = "none", size = "none")

figure <- ggarrange(
  ggarrange(p1, p2, prev, ncol = 3, labels = c("A", "B", "C")),
  ggarrange(imm, leg, ncol = 2, widths = c(3,1), labels = c("D", "")),
  nrow = 2)
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_v1.png", height = 7, width = 12)


p <- readRDS("../covid_endemic_vaccine/figures/p.RDS")
q <- readRDS("../covid_endemic_vaccine/figures/q.RDS")

figure <- ggarrange(
  ggarrange(p, q,
            common.legend = TRUE, legend = "right", labels = c("A",""),
            widths = c(1.1, 2) ),
  ggarrange(p1, p2, prev, ncol = 3, labels = c("B", "C", "D")),
  ggarrange(imm, leg, ncol = 2, widths = c(3,1), labels = c("E", "")),
  nrow = 3)
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_Fig2.png", height = 11, width = 12)

# plots with stochastic variation around outputs
figure_var <- ggarrange(
  p1_var,
  p2_var,
  prev_var,
  nrow = 3,
  labels = c("A", "B", "C"),
  common.legend = T
)
figure_var
ggsave("../covid_endemic_vaccine/figures/combined_figure_Fig2_variation.png", figure_var, height = 11, width = 12)
