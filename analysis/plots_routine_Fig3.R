
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
df <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_runs_routinemud1.rds") %>%
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
         hospmean = rollmean(hosp_t, 7, na.pad = TRUE)) %>%
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
  labs(x = "time (days)", y = "hospitalisations", col = "coverage (%)") +
  annotate("rect", xmin = 390, xmax = 390+365, ymin = 0, ymax = Inf,
           alpha = .3, fill = "pink")+
  geom_vline(aes(xintercept = vacc_start), linetype = "dashed", col = "grey4") +
  th +
  geom_line() +
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

df_1y <- df_1y %>%
  filter(max_coverage != 0) %>%
  rbind(y)%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate(vacc_start_adjust = vacc_start - 395) %>%
  mutate(vacc_start_label =  paste0("relative vaccination start ", vacc_start_adjust, " days"))

# attach no vacc
df_novacc <- df_1y %>%
  filter(max_coverage == 0) %>%
  select(-vacc_start, -vacc_on, -max_coverage) %>%
  rename(value0 = value) %>%
  unique()

df_1y <- df_1y %>%
  left_join(df_novacc) %>%
  mutate(prop_averted = (value0-value)/value0*100,
         number_averted = (value0-value)/1e6*1e5)

head(df_1y)

# plots showing % events averted -  probably more useful
df_1y2 <- df_1y %>%
  mutate(vacc_start_adjust_label = paste0("relative vaccination\nstart ", vacc_start_adjust, " days")) %>%
  mutate(vacc_start_adjust_label = factor(vacc_start_adjust_label, levels = c("relative vaccination\nstart 0 days",
                                                                              "relative vaccination\nstart 60 days",
                                                                              "relative vaccination\nstart 120 days")))

p2 <- ggplot(data = filter(df_1y2, max_coverage != 0, name == "hosp_med", mu_ab_d1 == 0.435, rt == 3), aes(x = factor(max_coverage*100), y = prop_averted, fill = factor(max_coverage*100))) +
  geom_col() +
  facet_grid(vaccine_doses~vacc_start_adjust_label) +
  labs(x = "coverage (%)", y = "proportion hospitalisations\n averted (%)", fill = "coverage in 65+ (%)", alpha = "timeframe") +
  scale_fill_brewer(palette = "YlGnBu", type = "seq") +
  #scale_alpha_discrete(range = c(0.5,1)) +
  theme(legend.position = "top") +
  
  th
p2

p2b <- ggplot(data = filter(df_1y2, max_coverage != 0, name == "hosp_med", mu_ab_d1 == 0.435, rt == 3.5), aes(x = factor(max_coverage*100), y = prop_averted, fill = factor(max_coverage*100))) +
  geom_col() +
  facet_grid(vaccine_doses~vacc_start_adjust_label) +
  labs(x = "coverage (%)", y = "proportion hospitalisations\n averted (%)", fill = "coverage in 65+ (%)", alpha = "timeframe") +
  scale_fill_brewer(palette = "YlGnBu", type = "seq") +
  #scale_alpha_discrete(range = c(0.5,1)) +
  theme(legend.position = "top") +
  
  th
p2b


df_1y <- df_1y %>%
  mutate(mu_ab_d1 = factor(mu_ab_d1, labels = c("unmatched\nvaccine", "partially-matched\nvaccine", "well-matched\nvaccine")))

out <- df_1y %>%
  filter(rt == 3,
         max_coverage != 0,
         vacc_start == 395,
         name == "hosp_med") %>%
  arrange(vaccine_doses, max_coverage)


p3 <- ggplot(data = filter(df_1y, max_coverage == 0.4, name == "hosp_med", rt == 3), aes(x = factor(vacc_start_adjust), y = prop_averted, fill = vaccine_doses)) +
  geom_col(position = "dodge") +
  facet_grid(~mu_ab_d1) +
  labs(x = "relative vaccination start", y = "proportion hospitalisations\n averted (%)", fill = "vaccine scenario", alpha = "timeframe") +
  th
p3

p4 <- ggplot(data = filter(df_1y, max_coverage == 0.4, name == "hosp_med", vacc_start_adjust == 0, rt == 3), aes(x = mu_ab_d1, y = prop_averted, fill = vaccine_doses)) +
  geom_col(position = "dodge") +
#  facet_grid(~mu_ab_d1) +
  labs(x = "degree of vaccine matching", y = "proportion hospitalisations\n averted (%)", fill = "vaccine scenario", alpha = "timeframe") +
  theme(legend.position = "top") +
  th

p4

library(ggpubr)
figure <- ggarrange(
  ggarrange(p1_sub,p4,nrow = 1, labels = c("A","B"), widths = c(1.3,1)),
  p2, nrow = 2, labels = c("", "C"), heights = c(1,1.3), widths = c(1,0.5))
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_3.png", height = 10, width = 13)

figure <- ggarrange(p1_sub,p4,p2,nrow = 2, ncol = 2,labels = c("A","B","C"), widths = c(1.3,1),
  heights = c(1,1.5))
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_3_v2.png", height = 10, width = 12)

p2  <- p2 +
  theme(legend.position = "right")
figure <- ggarrange(ggarrange(p1_sub,p4, nrow = 1, labels = c("A", "B")),ggarrange(p2, ncol = 2, widths = c(4,1)),nrow = 2,labels = c("","c"), heights = c(1,1.5))
figure
ggsave("../covid_endemic_vaccine/figures/combined_figure_3_v3.png", height = 10, width = 12)

