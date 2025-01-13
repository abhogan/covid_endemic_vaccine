library(tidyverse)
dat <- read_csv("data/ihr.csv") %>%
  pivot_longer(cols = c(`Inputs`, `UK data`, Modelled))

dat <- dat %>%
  na.omit(value)
labs <- unique(dat$age_name)

ggplot(data = filter(dat, name != "UK data"), aes(x = age_lower, y = value, col = name)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 0:16*5,labels = labs) +
  geom_point(data = filter(dat, name == "UK data"), size = 5, shape = "o") +
  th +
  theme(axis.text.x = element_text(angle = -90, hjust = 1, vjust = 0.5)) +
  labs(x = "age group", col = "", y = "IHR (%)")
  
ggsave("figures/IHR_plot.png", height = 5, width = 10)
  #scale_x_continuous(limits = c(0,80)) +
  geom_smooth(data = filter(dat, name == "UK_data"), method = 'glm',se = FALSE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0,90)) +
  
geom_line() 
  #coord_cartesian(xlim=c(0,80),ylim=c(0,25))

