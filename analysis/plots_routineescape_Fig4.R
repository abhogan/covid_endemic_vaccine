
cols<- hue_pal()(4)

# some plotting things
theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text.align = 0,
            axis.text=element_text(size=14),
            axis.title=element_text(size=16),
            strip.text.x = element_text(size = 15),
            strip.text.y = element_text(size = 15))

#########################################################################################
# routine runs - varied max_coverage (3 levels), and vaccine either on or off (no immune escape)

offset <- 365*9+180
df <- readRDS("../covid_endemic_vaccine/processed_outputs/df_summarise_runs_routineescapemud1.rds") %>%
  filter(timestep >= offset, timestep < offset +365*4) %>%
  mutate(timestep = timestep - offset,
         vacc_start = vacc_start - offset,
         vfr2_time1 = vfr2_time1 - offset,
         vfr2_time2 = vfr2_time2 - offset) %>%
  mutate(vacc_on = factor(vacc_on, levels = c(0,1), labels = c("baseline", "vaccine"))) %>%
  mutate(prev = inc_t /1e6 * 7 *100) %>%
  dplyr::group_by(vacc_on, scenario_name, vfr2_time1, vfr2_time2, vaccine_doses, vacc_start, vfr2, rt) %>%
  mutate(prevmean = rollmean(prev, 7, na.pad = TRUE),
         incmean = rollmean(inc_t, 7, na.pad = TRUE),
         hospmean = rollmean(hosp_t, 7, na.pad = TRUE)) %>%
  ungroup() %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c(1,2), labels = c("65+ annual", "65+ annual, 75+ 6m")))%>%
  mutate( vfr2_time1_name = factor(vfr2_time1, levels = c(425, 485, 545), labels = c("2 months", "4 months", "6 months")))

# replicate "no vaccine" across both doses
df_novacc <- filter(df, scenario_name == "no vaccine") %>%
  mutate(vaccine_doses = "65+ annual, 75+ 6m")

df <- rbind(df, df_novacc) %>%
  mutate(scenario_name = factor(scenario_name, levels = c("no vaccine", "unmatched", "partially-matched", "partially-matched d1, well-matched d2")))

# incidence over time
ggplot(data = filter(df, vaccine_doses == "65+ annual", scenario_name == "no vaccine", rt == 3), aes(x = timestep, y = inc_t, col = factor(vfr2))) +
  geom_line() +
  facet_grid(~vfr2_time1_name) +
  theme(legend.position = "bottom") +
  th +
  labs(col = "immune escape (fold change in immune recognition)", x = "time (days)", y = "incidence") +
  scale_color_viridis_d(option="C", begin = 0.2, end = 0.8, direction =1)

ggsave("../covid_endemic_vaccine/figures/routineescape_incidence_FigS7.png", height = 4.2, width = 8)

# hosps over time
rectdata <- df %>%
  select(vfr2_time1, vaccine_doses, vacc_start, scenario_name, vfr2, rt) %>%
  unique() %>%
  mutate( vfr2_time1_name = factor(vfr2_time1, levels = c(425, 485, 545), labels = c("2 months", "4 months", "6 months")))

p1 <- ggplot() +
  geom_rect(data = filter(rectdata, vfr2 == 1.5, rt == 3), aes(xmin = vfr2_time1, xmax = vfr2_time1+365, ymin = 0, ymax = Inf),
            alpha = .2, fill = "pink") +
  facet_grid(vfr2_time1_name~vaccine_doses) +
  geom_line(data = filter(df, vfr2 == 1.5, rt == 3), aes(x = timestep, y = hospmean, col = scenario_name)) +
  geom_vline(data = df, aes(xintercept = vacc_start), linetype = "dashed", col = "grey4") +
  th +
  labs(x = "time (days)", y = "hospitalisations", col = "vaccine scenario") +
  scale_x_continuous(limits = c(0,365*4), breaks = c(1:4)*365)

p1

# plot totals
df_1y <- readRDS("../covid_katana_runs/processed_outputs/df_summarise_totals_runs_routineescapemud1.rds") %>%
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
  mutate(vfr2_label = paste0("immune escape ", vfr2))

# plot of total events
p4 <- ggplot(data = filter(df_1y, name == "hosp_med", rt == 3, vfr2== 1.5), aes(fill = scenario_name, y = vfr2_time1_name, x = value/1e6*1e5)) +
  geom_col(position = "dodge") +
  facet_grid(~vaccine_doses) +
  labs(y = "timing of escape relative\nto vaccination start", x = "hospitalisations per 100,000", fill = "vaccine scenario")+
  th
p4

ggsave("../covid_endemic_vaccine/figures/routineescape_totals_FigS8.png", height = 4, width = 10)

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

p5 <- ggplot(data = filter(df_1y_avert, vfr2 == 1.5, name == "hosp_med", rt == 3), aes(fill = scenario_name, y = vfr2_time1_name, x = averted)) +
  geom_col(position = "dodge") +
  facet_wrap(~vaccine_doses, nrow = 2) +
  labs(y = "timing of escape relative\nto vaccination start", x = "averted hospitalisations\nper 100,000", fill = "vaccine scenario") +
  th +
  scale_fill_manual(values = cols[2:4])
p5

# plot of % averted

# attach no vacc
df_novacc <- df_1y %>%
  filter(scenario_name == "no vaccine") %>%
  select(-scenario_name, -vacc_on) %>%
  rename(value0 = value)

df_1y <- df_1y %>%
  left_join(df_novacc) %>%
  filter(scenario_name != "no vaccine") %>%
  mutate(prop_averted = (value0-value)/value0*100)

head(df_1y)

x <- df_1y %>%
  filter(name == "hosp_med", vfr2 == 1.5, rt == 3) %>%
  filter(scenario_name != "partially-matched d1, well-matched d2") %>%
select(-value0, -value) %>%
  
  pivot_wider(names_from = vaccine_doses, values_from = prop_averted) %>%
  mutate(diff = `65+ annual, 75+ 6m`- `65+ annual`)
x

p6 <- ggplot(data = filter(df_1y, name == "hosp_med", vfr2 == 1.5, rt == 3), aes(y = factor(vfr2_time1_name), x = prop_averted, fill = scenario_name)) +
  geom_col(position = "dodge") +
  facet_wrap(~vaccine_doses, nrow = 2) +
  labs(y = "timing of escape relative\nto vaccination start", x = "% hospitalisations averted", fill = "vaccine scenario", alpha = "timeframe") +
  scale_fill_manual(values = cols[2:4])+
  th

p6

ggarrange(p1, p6, nrow= 2, labels = c("A", "B"))
ggsave("../covid_endemic_vaccine/figures/Figure4.png", height = 10, width = 9)
