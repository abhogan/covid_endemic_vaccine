
dat <- read_csv("data/ihr.csv") %>%
  select(-c(`UK data`)) %>%
  pivot_longer(cols = c(`Naive`, Modelled)) %>%
  mutate(age_mid = (age_upper - age_lower)/2 + age_lower)

dat <- dat %>%
  na.omit(value)
labs <- unique(dat$age_name)

dat_UK <- read_csv("data/uk_data.csv") %>%
  mutate(age_mid = (age_upper - age_lower)/2 + age_lower)
  

ggplot() +
  geom_line(data = dat, aes(x = age_mid, y = value, col = name), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  th +
  #theme(axis.text.x = element_text(angle = -90, hjust = 1, vjust = 0.5)) +
  labs(x = "age group (years)", col = "", y = "IHR (%)") +
  geom_point(data = dat_UK, aes(x = age_mid, y = mean), col = "green4") +
  geom_errorbar(data = dat_UK, aes(ymin=lower, ymax=upper, x = age_mid), width = 1,
                position=position_dodge(.9), col = "green4")
  
ggsave("figures/IHR_plot.png", height = 5, width = 10)