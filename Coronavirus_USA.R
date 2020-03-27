library(dplyr)
library(tidyr)
library(ggplot2)
library(mapdata)
library(RColorBrewer)
library(readr)


jhk_corona_data <- read_csv("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-26-2020.csv")
View(jhk_corona_data)


corona_data <- jhk_corona_data
head(corona_data)

colnames(corona_data)[2] <- "city"
colnames(corona_data)[3] <- "state"
colnames(corona_data)[4] <- "country"
colnames(corona_data)[5] <- "date"


head(corona_data)


new_usa_data <- filter(corona_data, country == 'US')
head(new_usa_data)

current_case_count <- new_usa_data %>%
  group_by(state) %>%
  summarize(cases = sum(Confirmed))


# new_usa_data$date <as.Date(new_usa_data$date, "%m/%d/%y")
# new_usa_data
# 
# current_case_count <- new_usa_data %>%
#   filter(Date == max(Date))

states_v <- c('Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Minor Outlying Islands', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Northern Mariana Islands', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'U.S. Virgin Islands', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
states_v

#usa_new <- usa_data[usa_data$State %in% states_v,]
current_case_count <- current_case_count[current_case_count$state %in% states_v,]

current_case_count


states <- map_data("state")
states


state_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
state_base


current_case_count[[1]] <- tolower(current_case_count[[1]])

current_case_count

colnames(states)[5] <- "state"
states

# current_case_count <- current_case_count[,c (1,6)]
# current_case_count

joined_states <- inner_join(states, current_case_count, by = 'state')
joined_states



plot_clean_background <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


case_count_country_plot <- state_base + 
  geom_polygon(data = joined_states, aes(fill = cases), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  plot_clean_background

case_count_country_plot 

#log 10 helps 
case_count_country_plot + scale_fill_gradient(trans = "log10")

#picking an appropriate color scheme
display.brewer.all()
mypalette<-brewer.pal(4,"Spectral")
mypalette


case_count_country_plot_addition <- case_count_country_plot + 
  scale_fill_gradientn(colours = rev(mypalette),
                       #  breaks = c(100, 500, 1500, 5000, 7500, 10000))
                       breaks = c(100, 500, 1500, 7500, 15000, 30000), 
                       trans="log10")

case_count_country_plot_addition + ggtitle("Cases Per State")



##Population Percentage 
library(readr)
usa_population <- read_delim("usa_population.csv", 
                             "|", escape_double = FALSE, trim_ws = TRUE)
View(usa_population)

usa_population <- usa_population[-c(3:10)]
usa_population

trimws(usa_population$state, "left")
Encoding(usa_population$state) <- "UTF-8"
usa_population$state <- iconv(usa_population$state, "UTF-8", "UTF-8",sub='')

usa_population$state
usa_population

usa_population[[1]] <- tolower(usa_population[[1]])


inner_join_pop <- inner_join(joined_states, usa_population, key=states)
inner_join_pop


inner_join_pop$percent_infected <- inner_join_pop$cases/inner_join_pop$population
inner_join_pop$percent_infected <-inner_join_pop$percent_infected * 100



percent_infected_plot <- state_base + 
  geom_polygon(data = inner_join_pop, aes(fill = percent_infected), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  plot_clean_background

percent_infected_plot 
percent_infected_plot + scale_fill_gradient(trans = "log10")

percent_infected_plot

mypalette<-brewer.pal(4,"Spectral")
mypalette

display.brewer.all()

percent_infected_plot_addition <- percent_infected_plot + 
  scale_fill_gradientn(colours = rev(mypalette),
                       #  breaks = c(100, 500, 1500, 5000, 7500, 10000))
                       breaks = c(.003,.010, .030, 0.100)
                       ,trans="log10")

percent_infected_plot_addition + ggtitle("Percent of State Population Infected")






