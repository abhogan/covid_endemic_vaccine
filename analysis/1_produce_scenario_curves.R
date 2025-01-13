source("../covid_endemic_vaccine/R/vaccine_function.R")
c6 <- "#363432"
c7 <- "#90A19D"
c8 <- "#F0941F"
# set up parameters - from Nature Comms paper (moderna vaccine values) with appropriate transforms applied

il_t0 <- c(log10(1.13/5.1*1), log10(1.13/5.1*2), log10(1.13/5.1*3)) # starting immune level
boost <- log10(1.13) # moderna booster (previously called d3)
ab_50       <- log10(0.091)
ab_50_severe <-  log10(0.021)
ab_50_death  <- log10(0.021)
k           <- 2.5#3.1
hl_s        <- 35
hl_l        <- 1000 #581
period_s    <- 75
std10 <- 0.44 # Pooled standard deviation of antibody level on log10 scale (from Khoury et al Nat Med 2020)

future_vfr <- 0 # set to log10(1) i.e. 0 if no future variant
immune_escape <- 0 # binary value of 0 or 1 for future immune escape

vaccine <- 0
matched_vacc <- 0
matched_vacc_level <- 0

# transforms
dr_s <- -log(2)/hl_s  # Corresponding decay rate in days for half life above
dr_l <- -log(2)/hl_l

# Timing of doses
max_t <- 365*2+5 # number of days to model
t_boost <- max_t-1
t_future_vfr <- 100

scenarios <-
  expand_grid(
    il_t0,
    boost,
    ab_50,
    ab_50_severe,
    ab_50_death,
    k,
    dr_s,
    dr_l,
    period_s,
    future_vfr,
    immune_escape,
    vaccine,
    matched_vacc,
    matched_vacc_level,
    t_boost,
    t_future_vfr,
    max_t
  ) %>%
  # dont need matched vaccine if no additional immune escape
  filter(!(matched_vacc == 1 & future_vfr == 0)) %>%
  mutate(name = c("partially-matched vaccine", "well-matched vaccine", "infection"))

nrow(scenarios)

#### Run the model #############################################################
#plan(multiprocess, workers = 2)
system.time({out <- future_pmap(scenarios, vaccine_profile, .progress = TRUE)})
################################################################################

### Save output ################################################################
saveRDS(out, "output/1_main_curve.rds")
################################################################################
df <- out
df <- readRDS("output/1_main_curve.rds")

df <- bind_rows(df) %>%
  rename(scenario = name) %>%
  pivot_longer(cols = c("immune_level", "ef_infection", "ef_severe", "ef_death")) %>%
  mutate( name = factor(name, levels = c("immune_level", "ef_infection", "ef_severe", "ef_death"), labels = c("immune recognition", "infection", "severe disease", "death")),
          scenario = factor(scenario),
          future_vfr = 10^(future_vfr),
          future_vfr = factor(future_vfr),
          matched_vacc = factor(matched_vacc)) %>%
  mutate(scenario = factor(scenario, levels = c("infection", "well-matched vaccine", "partially-matched vaccine")))

# create plots
p <- ggplot(data = filter(df, name == "immune recognition"), aes(x = t, y = value, col = scenario)) + 
  geom_line(linewidth = 0.8) +
  facet_wrap(~name)+
  scale_y_log10(limits = c(1e-2,1e00)) +
  scale_x_continuous(breaks = c(0,180,360, 360+180, 720), limits = c(0,720)) +
  th +
  labs(x = "time (days)", y = "immune recognition", col = "profile") +
  scale_color_manual(values = c(c6, c7, c8))
p

q <- ggplot(data = filter(df, name != "immune recognition", name != "death"), aes(x = t, y = value, col= scenario)) + 
  geom_line(linewidth = 0.8) +
  facet_wrap(~name) +
  lims(y = c(0,1)) +
  scale_x_continuous(breaks = c(0,180,360, 360+180, 720), limits = c(0,720)) +
  th +
  labs(x = "time (days)", y = "efficacy (%)", col = "profile")+
  scale_color_manual(values = c(c6, c7, c8))
q

# save plots
ggarrange(p, q,
          common.legend = TRUE, legend = "right", labels = "AUTO",
          widths = c(1.1, 2) ) %>%
  ggexport(filename = "figures/fig_parameterise_curves.png", ncol = 2, nrow = 1, width = 800,height = 250, pointsize = 11, res = 100)

saveRDS(p, "figures/p.RDS")
saveRDS(q, "figures/q.RDS")
