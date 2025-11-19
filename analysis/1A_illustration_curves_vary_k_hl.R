# produce illustration curves in panel 1A

# set up parameters - from Nature Comms paper (moderna vaccine values) with appropriate transforms applied

# Panel A
# vaccination

il_t0 <- log10(1.13/5.1*3)#c(log10(1.13/5.1*1), log10(1.13/5.1*2), log10(1.13/5.1*3)) # starting immune level
boost <- log10(1.13/5.1*2) # moderna booster (previously called d3)
ab_50       <- log10(0.091)
ab_50_severe <-  log10(0.021)
ab_50_death  <- log10(0.021)
k           <- c(2.5, 3.1)
hl_s        <- 35
hl_l        <- 1000 #581
period_s    <- 75
std10 <- 0.44 # Pooled standard deviation of antibody level on log10 scale (from Khoury et al Nat Med 2020)

future_vfr <- 0 # set to log10(1) i.e. 0 if no future variant
immune_escape <- 0 # binary value of 0 or 1 for future immune escape

vaccine <- 1
matched_vacc <- 1
matched_vacc_level <- 0.666

# transforms
dr_s <- -log(2)/hl_s  # Corresponding decay rate in days for half life above
dr_l <- -log(2)/hl_l

# Timing of doses
max_t <- 365*3+5 # number of days to model
t_boost <- 365*3
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
  mutate(name = "vaccine",
         matched_vacc_multiplier = 0) %>%
  mutate(dr_l = if_else(k == 3.1, (-log(2)/581), dr_l))

system.time({out <- future_pmap(scenarios, vaccine_profile, .progress = TRUE)})

d1 <- bind_rows(out) %>%
  mutate(scenario_name = if_else(k == 2.5, "updated", "original"))

# infection
boost <- log10(1.13/5.1*3) # moderna booster (previously called d3)
t_boost <- 365*3

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
  mutate(name = "infection",
         matched_vacc_multiplier = 0)%>%
  mutate(dr_l = if_else(k == 3.1, (-log(2)/581), dr_l))

system.time({out <- future_pmap(scenarios, vaccine_profile, .progress = TRUE)})

d2 <- bind_rows(out) %>%
  mutate(scenario_name = if_else(k == 2.5, "updated", "original"))

x <- rbind(d1, d2) %>%
  group_by(t, k, scenario_name) %>%
  filter(immune_level == max(immune_level)) %>%
  ungroup() %>%
  mutate(name = "combined")

y <- d2 %>%
  mutate(name = factor(name, levels = c("infection", "vaccine", "combined"))) %>%
  rename(`efficacy against infection` = "ef_infection",
         `efficacy against severe disease` = "ef_severe") %>%
  pivot_longer(cols = c(`efficacy against infection`, `efficacy against severe disease`), names_to = "outcome")
  
ggplot(data = y, aes(x = t, y = immune_level, color = name)) +
  facet_wrap(outcome~scenario_name) +
  geom_line()

ggplot(data = y, aes(x = t, y = value*100, color = scenario_name))+
  facet_wrap(~outcome, nrow = 1)+
  geom_line(size = 1) +
  th +
  labs(x = "time (days)", y = "efficacy (%)", col = "parameters") +
  lims(y = c(0,100)) +
  scale_x_continuous(breaks = c(0,360, 720,1095), limits = c(0,(365*3-1)))

ggsave("figures/illustrate_k_hl_l.png", height = 5, width = 12)

