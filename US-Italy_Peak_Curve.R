

world_confirmed <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
View(world_confirmed)

# ---------------------------------

#          USA PLOT

# ---------------------------------

usa_cases <- filter(world_confirmed, world_confirmed$`Country/Region` == 'US')


usa_cases <- usa_cases %>% ###use today - 1
  gather(`1/22/20`:`5/10/20`, key = 'date', value = 'cases')

usa_cases <- usa_cases %>% 
  mutate(daily_increase = cases - lag(cases)) # dplyr has its own version of lag

usa_cases$daily_increase <- replace_na(usa_cases$daily_increase, 0) 
# View(usa_cases)


usa_cases$date <- as.Date(usa_cases$date, format='%m/%d/%y')
class(usa_cases$date)

usa_cases$date

usa_daily_increase_plot <- ggplot(data = usa_cases) +
  geom_smooth(mapping = aes(x=date, y=daily_increase)) +
  labs(title = "USA Daily Case Increase", x="Date", y = "Case Increase") +
  scale_x_date(breaks = "5 days") + theme(axis.ticks = element_line(colour = "gray4"), 
  panel.grid.major = element_line(linetype = "dotted"), 
  axis.text.x = element_text(vjust = 0.5, angle = 45), panel.background = element_rect(fill = "aliceblue", 
  linetype = "longdash"), plot.background = element_rect(fill = "white"))

usa_daily_increase_plot


install.packages("rbokeh")
library(rbokeh)


# ---------------------------------

#           ITALY PLOT

# ---------------------------------

italy_cases <- filter(world_confirmed, world_confirmed$`Country/Region` == 'Italy')

italy_cases <- italy_cases %>% ## use today -1
  gather(`1/22/20`:`5/10/20`, key = 'date', value = 'cases')

italy_cases <- italy_cases %>% 
  mutate(daily_increase = cases - lag(cases))

italy_cases$date <- as.Date(italy_cases$date, format='%m/%d/%y')
class(italy_cases$date)


ggplot(data = italy_daily_increase, mapping=aes(x=date, y=daily_increase)) + 
  geom_point() +
  geom_smooth()

italy_daily_increase_plot <- ggplot(data = italy_cases) +
  geom_smooth(mapping = aes(x=date, y=daily_increase)) +
  labs(title = "Italy Daily Case Increase", x="Date", y = "Case Increase") +
  scale_x_date(breaks = "7 days") + theme(axis.ticks = element_line(colour = "gray4"), 
    panel.grid.major = element_line(linetype = "dotted"), 
    axis.text.x = element_text(vjust = 0.5, 
        angle = 45), panel.background = element_rect(fill = "aliceblue", 
        linetype = "longdash"), plot.background = element_rect(fill = "white"))

italy_daily_increase_plot

italy_daily_increase_plot <- italy_daily_increase_plot + theme_tufte()
plotly_build(daily_increase_plot)

#-------------------
# CHINA

#------------------

china_cases <- filter(world_confirmed, world_confirmed$`Country/Region` == 'China')
head(china_cases)

china_cases <- china_cases %>% ## use today -1
  gather(`1/22/20`:`4/23/20`, key = 'date', value = 'cases')

china_cases <- china_cases %>% 
  mutate(daily_increase = cases - lag(cases))

china_cases <- china_cases %>%
  group_by(china_cases$date)

china_cases

china_cases$date <- as.Date(china_cases$date, format='%m/%d/%y')
class(china_cases$date)


ggplot(data = china_cases, mapping=aes(x=date, y=daily_increase)) + 
  geom_point() +
  geom_smooth()

china_daily_increase_plot <- ggplot(data = china_cases) +
  geom_smooth(mapping = aes(x=date, y=daily_increase)) +
  labs(title = "Italy Daily Case Increase", subtitle= "April 23", x="Date", y = "Case Increase") +
  scale_x_date(breaks = "7 days") + theme(axis.ticks = element_line(colour = "gray4"), 
                                          panel.grid.major = element_line(linetype = "dotted"), 
                                          axis.text.x = element_text(vjust = 0.5, 
                                                                     angle = 45), panel.background = element_rect(fill = "aliceblue", 
                                                                                                                  linetype = "longdash"), plot.background = element_rect(fill = "white"))

head(china_cases)
View(china_cases)



              