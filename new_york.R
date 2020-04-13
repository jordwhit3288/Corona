times_corona_data  <- read_csv("ny_times_data/covid-19-data/us-states.csv")
View(times_corona_data)

head(times_corona_data)
#Filtering df to NY data

ny_data <- filter(times_corona_data, state == 'New York')

head(ny_data)

ny_data <- ny_data %>% 
  mutate(daily_increase = cases - lag(cases)) # dplyr has its own version of lag

ny_data$daily_increase <- replace_na(ny_data$daily_increase, 0) 
View(usa_cases)


ny_daily_increase_plot <- ggplot(data = ny_data) +
  geom_smooth(mapping = aes(x=date, y=daily_increase)) +
  labs(title = "NY Daily Case Increase", subtitle = "4/8/2020", x="Date", y = "Case Increase") +
  scale_x_date(breaks = "7 days") + theme(axis.ticks = element_line(colour = "gray4"), 
  panel.grid.major = element_line(linetype = "dotted"), 
  axis.text.x = element_text(vjust = 0.5, 
  angle = 45), panel.background = element_rect(fill = "aliceblue", 
  linetype = "longdash"), plot.background = element_rect(fill = "white"))
ny_daily_increase_plot

plotly_build(ny_daily_increase_plot)

ggplotly(ny_daily_increase_plot)

