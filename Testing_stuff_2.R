install.packages("tmap")
library(tmap)
usa_cases <- filter(world_confirmed, world_confirmed$`Country/Region` == 'US')


usa_cases <- usa_cases %>% ###use today - 1
  gather(`1/22/20`:`4/11/20`, key = 'date', value = 'cases')


usa_cases

tm_shape(usa_union)


