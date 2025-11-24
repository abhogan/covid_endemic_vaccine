library(zoo)

# some plotting things
theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text.align = 0,
            axis.text=element_text(size=15),
            axis.title=element_text(size=16),
            strip.text.x = element_text(size = 15),
            strip.text.y = element_text(size = 15))

#########################################################################################
# routine runs - varied max_coverage (3 levels), and vaccine either on or off (no immune escape)
# two delivery scenarios: one dose to 65+ plus, or option of additional dose to 75+ after 6 months

offset <- 365*9+180
df <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_runs_routinemud1_new.rds") %>%
  filter(timestep >= offset, timestep < offset +365*3) %>%
  mutate(timestep = timestep - offset,
         vacc_start = vacc_start - offset) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(prev = inc_t /1e6 * 7 *100) %>%
  mutate(mu_ab_infection = round(mu_ab_infection, 3),
         mu_ab_d1 = round(mu_ab_d1, 3)) %>%
  mutate(max_coverage = if_else(vacc_on == "baseline", 0, max_coverage )) %>%
  dplyr::group_by(vacc_on, vacc_start, max_coverage, mu_ab_d1, vaccine_doses, rt) %>%
  mutate(prevmean = rollmean(prev, 7, na.pad = TRUE),
         incmean = rollmean(inc_t, 7, na.pad = TRUE),
         hospmean = rollmean(hosp_t, 7, na.pad = TRUE),
         incmean_upper = rollmean(inc_tmax, 7, na.pad = TRUE),
         incmean_lower = rollmean(inc_tmin, 7, na.pad = TRUE),
         hospmean_upper = rollmean(hosp_tmax, 7, na.pad = TRUE),
         hospmean_lower = rollmean(hosp_tmin, 7, na.pad = TRUE)) %>%
  ungroup()

# include vaccine = 0 for all scenarios
novacc <- df %>%
  filter(max_coverage == 0) %>%
  select(-vacc_start, -mu_ab_d1, -vaccine_doses)
vacc_start = sort(unique(df$vacc_start))[1:3]
mu_ab_d1 = sort(unique(df$mu_ab_d1))
vaccine_doses <- c(1,2)

x <- expand_grid(vacc_start, mu_ab_d1, vaccine_doses)

y <- cross_join(x, novacc)

df <- df %>%
  filter(max_coverage != 0) %>%
  rbind(y)

# check how vaccines being rolled out
df_doses <- df %>%
  filter(mu_ab_d1 == 0.435, vacc_start == 395, rt ==3, vacc_on == "vaccine", max_coverage == 0.4, vaccine_doses == 2) %>%
  select(timestep, dose1_t, dose2_t) %>%
  pivot_longer(cols = c(dose1_t, dose2_t))

ggplot(data = df_doses, aes(x = timestep, y = value, col = name)) +
  geom_line()

# incidence over time
ggplot(data = filter(df, mu_ab_d1 == 0.435, rt == 3), aes(x = timestep, y = inc_t, col = vacc_on, fill = vacc_on)) +
  geom_line() +
  facet_grid(vacc_start~max_coverage + vaccine_doses)

# hosps over time
df2 <- df %>%
  mutate(vacc_start_adjust = vacc_start - 395) %>%
  mutate(vacc_start_label =  paste0("relative vaccination start ", vacc_start_adjust, " days")) %>%
  mutate(max_coverage = max_coverage * 100) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))

# include in supplement
# show line plot with high cov and 0 cov, high level matching, single dose only, different vaccine start times
p1 <- ggplot(data = filter(df2,
                           max_coverage %in% c(0,40),
                           mu_ab_d1 == 0.435,
                           rt == 3), aes(x = timestep, y = hospmean, col = factor(max_coverage))) +
  facet_grid(vacc_start_adjust~vaccine_doses) +
  labs(x = "time (days)", y = "hospitalisations", col = "coverage (%)", fill = "coverage (%)") +
  annotate("rect", xmin = 390, xmax = 390+365, ymin = 0, ymax = Inf,
           alpha = .3, fill = "pink")+
  geom_vline(aes(xintercept = vacc_start), linetype = "dashed", col = "grey4") +
  th +
  geom_line() +
  geom_ribbon(aes(ymin = hospmean_lower, ymax = hospmean_upper, fill = factor(max_coverage)), alpha = 0.3, col= NA) +
  scale_x_continuous(breaks = seq(0,365*4, by = 365))

p1

ggsave("../covid_endemic_vaccine/figures/routine1_FigS5.png", height = 6, width = 10)
# subset plot to include in main

p1_sub <- ggplot(data = filter(df2,
                           max_coverage %in% c(0,40),
                           mu_ab_d1 == 0.435, vacc_start_adjust == 0,
                           rt == 3), aes(x = timestep, y = hospmean, col = factor(max_coverage))) +
  facet_grid(~vaccine_doses) +
  labs(x = "time (days)", y = "hospitalisations", col = "coverage (%)") +
  annotate("rect", xmin = 390, xmax = 390+365, ymin = 0, ymax = Inf,
           alpha = .3, fill = "pink")+
  geom_vline(aes(xintercept = vacc_start), linetype = "dashed", col = "grey4") +
  th +
  geom_line() +
  scale_x_continuous(breaks = seq(0,365*4, by = 365)) +
  theme(legend.position = "top")

p1_sub

ggsave("../covid_endemic_vaccine/figures/routine2_Fig3A.png", height = 3, width = 10)

# plot totals
df_1y <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_totals_runs_routinemud1_new.rds") %>%
  select(max_coverage, hosp_med, inc_med, vacc_start, vacc_on, mu_ab_d1, vaccine_doses, rt, inc_averted_med, inc_averted_lower, inc_averted_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper, prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(max_coverage = if_else(vacc_on == "baseline", 0, max_coverage )) %>%
  filter(max_coverage != 0) %>%
  pivot_longer(c(inc_med, hosp_med, inc_averted_med, inc_averted_lower, inc_averted_upper, cum_hosp_averted_med, cum_hosp_averted_lower, cum_hosp_averted_upper, prop_hosp_averted_med, prop_hosp_averted_lower, prop_hosp_averted_upper)) %>%
  mutate(vacc_start = vacc_start - offset) %>%
  mutate(
    mu_ab_d1 = round(mu_ab_d1, 3)) 

df_1y <- df_1y %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate(vacc_start_adjust = vacc_start - 395) %>%
  mutate(vacc_start_label =  paste0("relative vaccination start ", vacc_start_adjust, " days"))%>%
  mutate(vacc_start_adjust_label = paste0("relative vaccination\nstart ", vacc_start_adjust, " days")) %>%
  mutate(vacc_start_adjust_label = factor(vacc_start_adjust_label, levels = c("relative vaccination\nstart 0 days",
                                                                              "relative vaccination\nstart 60 days",
                                                                              "relative vaccination\nstart 120 days")))

df_1y_prophosp <- filter(df_1y, max_coverage != 0, name %in% c("prop_hosp_averted_med", "prop_hosp_averted_upper", "prop_hosp_averted_lower"), mu_ab_d1 == 0.435, rt == 3) %>%
  pivot_wider(names_from = name, values_from = value)
  
p2 <- ggplot(data = df_1y_prophosp, aes(x = factor(max_coverage*100), y = prop_hosp_averted_med, fill = factor(max_coverage*100))) +
  geom_col() +
  geom_errorbar(aes(ymin = prop_hosp_averted_lower, ymax = prop_hosp_averted_upper), width = 0.2) +
  facet_grid(vaccine_doses~vacc_start_adjust_label) +
  labs(x = "coverage (%)", y = "proportion hospitalisations\n averted (%)", fill = "coverage in 65+ (%)", alpha = "timeframe") +
  scale_fill_brewer(palette = "YlGnBu", type = "seq") +
  #scale_alpha_discrete(range = c(0.5,1)) +
  theme(legend.position = "top") +
  th

p2

df_1y_prophosp2 <- filter(df_1y, max_coverage != 0, name %in% c("prop_hosp_averted_med", "prop_hosp_averted_upper", "prop_hosp_averted_lower"), rt == 3, max_coverage == 0.4) %>%
  pivot_wider(names_from = name, values_from = value)%>%
  mutate(mu_ab_d1 = factor(mu_ab_d1, labels = c("unmatched\nvaccine", "partially-matched\nvaccine", "well-matched\nvaccine")))

p3 <- ggplot(data = df_1y_prophosp2, aes(x = factor(vacc_start_adjust), y = prop_hosp_averted_med, fill = vaccine_doses)) +
  geom_col(position = "dodge") +
  facet_grid(~mu_ab_d1) +
  geom_errorbar(aes(ymin = prop_hosp_averted_lower, ymax = prop_hosp_averted_upper), width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "relative vaccination start", y = "proportion hospitalisations\n averted (%)", fill = "vaccine scenario", alpha = "timeframe") +
  th
p3

df_1y_prophosp3 <- filter(df_1y, max_coverage != 0, name %in% c("prop_hosp_averted_med", "prop_hosp_averted_upper", "prop_hosp_averted_lower"), rt == 3, max_coverage == 0.4, vacc_start_adjust == 0) %>%
  pivot_wider(names_from = name, values_from = value)%>%
  mutate(mu_ab_d1 = factor(mu_ab_d1, labels = c("unmatched\nvaccine", "partially-matched\nvaccine", "well-matched\nvaccine")))

p4 <- ggplot(data = df_1y_prophosp3, aes(x = mu_ab_d1, y = prop_hosp_averted_med, fill = vaccine_doses)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = prop_hosp_averted_lower, ymax = prop_hosp_averted_upper), width = 0.2, position = position_dodge(width = 0.9)) +
  labs(x = "degree of vaccine matching", y = "proportion hospitalisations\n averted (%)", fill = "vaccine scenario", alpha = "timeframe") +
  theme(legend.position = "top") +
  th
p4

library(ggpubr)

p2  <- p2 +
  theme(legend.position = "right")
figure <- ggarrange(ggarrange(p1_sub,p4, nrow = 1, labels = c("A", "B")),ggarrange(p2, ncol = 2, widths = c(4,1)),nrow = 2,labels = c("","c"), heights = c(1,1.5))
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_3_v3.png", height = 10, width = 12)

