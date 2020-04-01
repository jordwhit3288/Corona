

world_confirmed <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
View(world_confirmed)


italy_cases <- filter(world_confirmed, world_confirmed$`Country/Region` == 'Italy')
View(italy_cases)


italy_cases <- italy_cases %>%
  gather(first_date:last_date, key = 'date', value = 'cases')


first_date <- colnames(italy_cases)[5]
last_date <- colnames(italy_cases)[73]

head(italy_cases)


ggplot(italy_cases, aes(x=date, y=cases)) +
  geom_point() +
  geom_smooth(se= FALSE)

library(ggplot2)

daily_increase <- diff(italy_cases$cases)

date <- italy_cases$date
date <- date[1:68]
date

italy_daily_increase <- data.frame(date, daily_increase)
italy_daily_increase <- filter(italy_daily_increase, daily_increase > 10)

italy_daily_increase

class(italy_daily_increase$date)
italy_daily_increase$date <- as.Date(italy_daily_increase$date, format='%m/%d/%y')
class(italy_daily_increase$daily_increase)

  
ggplot(data = italy_daily_increase, mapping=aes(x=date, y=daily_increase)) + 
  geom_point() +
  geom_smooth()

ggplot(data = italy_daily_increase) +
  geom_smooth(mapping = aes(x=date, y=daily_increase))

              