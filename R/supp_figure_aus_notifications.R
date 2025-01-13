dat <- read_csv("data/aus_notification_data.csv")

ggplot(data = dat, aes(x = time, y = y/1000, col = factor(year))) +
  geom_line() +
  scale_y_continuous(limits = c(0,20000/1000)) +
  labs(x = "week of diagnosis", y = "COVID-19 cases ('000)", col = "year") +
  th +
  theme(legend.position = "bottom")

ggsave("figures/covid_data.png", height = 4, width = 8)
