install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("mapdata")
install.packages("RColorBrewer")
install.packages("readr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(mapdata)
library(RColorBrewer)
library(readr)
library(widgetframe)
library(plotly)
library(hrbrthemes)
library(colormap)
library(plotly)
#Loading dataset .. for newest day, change /m-dd-yyyy.csv to the MOST CURRENT DATA (current_day -1)
jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")

#jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-27-2020.csv")
#jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")
View(jhk_corona_data)

#renaming dataframe to friendly name
corona_data <- jhk_corona_data
head(corona_data)


#renaming specific column indices to friendly names
colnames(corona_data)[2] <- "city"
colnames(corona_data)[3] <- "state"
colnames(corona_data)[4] <- "country"
colnames(corona_data)[5] <- "date"


head(corona_data)

#Filtering df to united states data
new_usa_data <- filter(corona_data, country == 'US')
head(new_usa_data)
View(new_usa_data)

#grouping by state to aggregate the num of confirmed cases
current_case_count <- new_usa_data %>%
  group_by(state) %>%
  summarize(cases = sum(Confirmed))

head(current_case_count)

# States vector for further filtration 
states_v <- c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',  'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
states_v

#usa_new <- usa_data[usa_data$State %in% states_v,]

#using states_v vector to filter out the current_case_count df
current_case_count <- current_case_count[current_case_count$state %in% states_v,]

current_case_count

#creating an object that will hold the USA map outline
states <- map_data("state")
states


state_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
state_base

#lower casing the state field for joining purposes.
current_case_count[[1]] <- tolower(current_case_count[[1]])

current_case_count

#looking at max cases :o scary 
max(current_case_count$cases)

#renaming the column to ensure a join will occur (case matters. lame.)
colnames(states)[5] <- "state"
states

# current_case_count <- current_case_count[,c (1,6)]
# current_case_count

#creating a new df based off of the states df and the current_case_count df :)
joined_states <- inner_join(states, current_case_count, by = 'state')
joined_states

# ...again... scary
max(joined_states$cases)

#getting rid of the messy background 
plot_clean_background <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

### doing some manipulation of the plot


case_count_country_plot <- state_base + (aes(text= paste("Reported Cases:", joined_states$cases))) + 
  geom_polygon(data = joined_states, aes(fill = cases)) +
  geom_polygon(color = "black", fill = NA) +
  theme_set(theme_bw(base_size =  15, base_family = 'Times New Roman')) +
  plot_clean_background


case_count_country_plot



#log 10 helps 
# case_count_country_plot <- case_count_country_plot + scale_fill_gradient2(trans = "log10")
# case_count_country_plot
#picking an appropriate color scheme
#display.brewer.all()
mypalette<-brewer.pal(8,"Spectral")
mypalette



case_count_country_plot_addition <- case_count_country_plot +
  scale_fill_gradientn(colours = rev(mypalette),
                       breaks = c(500, 1500, 6500, 20000, 80000),
                       trans="log10",
                       label=scales::comma)

case_count_country_plot_addition

#BOOM!
case_count_country_plot_addition <- case_count_country_plot_addition + ggtitle("April 2 Cases Per State")
dynamic_label_plot <- plotly_build(case_count_country_plot_addition)
ggplotly(dynamic_label_plot, tooltip = c("text", "x"))

plotly_build(dynamic_label_plot)



-----------------------------------------


-------------------------------
  
  
#########  Population Percentage  ##############
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

colnames(inner_join_pop)[9] <- "Percent_Infected"



percent_infected_plot <- state_base + 
  geom_polygon(data = inner_join_pop, aes(fill = Percent_Infected), color = "white") +
  geom_polygon(color = "black", fill = NA) +
 # theme_set(theme_bw(base_size =  15, base_family = 'Times New Roman')) 
  plot_clean_background

 percent_infected_plot 
# percent_infected_plot + scale_fill_gradient(trans = "log10")

percent_infected_plot

mypalette<-brewer.pal(6,"Spectral")
mypalette

display.brewer.all()

percent_infected_plot_addition <- percent_infected_plot + 
  scale_fill_gradientn(colours = rev(mypalette),
                         breaks = c(.015, .035, 0.1, .25,  .40)
                       #breaks = c(.01,.03, .1, .30)
                       ,trans="log10")

percent_infected_plot_addition + ggtitle("April 1 Percent of State Population Infected")







############### NEW TESTING ##################

####testing dots for cases

library(ggmap)
library(sp)
library(ggfortify)
library(usmap)
library(maptools)
library(rgdal)
#example

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
usa_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 
usa_map
class(usa_map)

jp <-  map('world2', 'japan', plot = FALSE, fill = TRUE)
class(jp)

p <- autoplot(jp, geom = 'polygon', fill = 'subregion') + 
  theme(legend.position="none")
p

df <- data.frame(long = c(139.691704, 135.519711),
                 lat = c(35.689521, 34.686316),
                 label = c('Tokyo', 'Osaka'),
                 population = c(1335, 886))
coordinates(joined_states) <- ~ long + lat
class(df)

autoplot(df, p = p, colour = 'red', size = 'population') 

###usa map
data <- data.frame(
  lon = c(-74.01, -95.36, -118.24, -87.65, -134.42, -157.86),
  lat = c(40.71, 29.76, 34.05, 41.85, 58.30, 21.31),
  pop = c(8398748, 2325502, 3990456, 2705994, 32113, 347397)
)
# Transform data
transformed_data <- usmap_transform(data)
# Plot transformed data on map
plot_usmap() + geom_point(
  data = transformed_data,
  aes(x = lon.1, y = lat.1, size = pop),
  color = "red", alpha = 0.5
)


##need to filter for current date..
library(dplyr)
View(new_usa_data)
#reordering to see if this helps.... 

cities_current_cases <- new_usa_data[,c(2,3,6,7,8)]
View(cities_current_cases)

colnames(cities_current_cases)[1] <- "county"
colnames(cities_current_cases)[2] <- "state"
colnames(cities_current_cases)[3] <- "lat"
colnames(cities_current_cases)[4] <- "long"
colnames(cities_current_cases)[5] <- "cases"

View(cities_current_cases)

county_df <- usmap::us_map(regions = "counties")
View(county_df)
library(stringr)
county_df$county <- gsub("[ County]", "", county_df$county)

county_df <- str_replace(county_df$county, "([ County])", '')
View(county_df)

joined_counties_with_map_data <- inner_join(county_df, cities_current_cases, key=lat)

View(joined_counties_with_map_data)


#joined_counties_with_map_data <- joined_counties_with_map_data[joined_counties_with_map_data$state %in% states_v,]

map_data <- joined_counties_with_map_data[,c(2,1,14)]

View(map_data)
transformed_data <- usmap_transform(map_data)

library(usmap)
library(ggplot2)


plot_usmap("states") +
  geom_point( data= transformed_data,
              aes(x=long.1, y=lat.1, size = cases),
              color="red", alpha = 0.5) +
  labs(title = "US Earthquakes",
        subtitle = "Source: USGS, Jan 1 to Jun 30 2019",
  
              size = "Magnitude") +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0))



####

new_map_data <- new_usa_data[,c(7,6,8)]
head(new_map_data)
new_map_data <- data.frame(new_map_data)
new_map_data <- new_map_data[new_map_data$Confirmed > 50, ]

head(new_map_data)

transformed_data <- usmap_transform(new_map_data)
head(transformed_data)


plot_usmap("states") + geom_point(
  data= transformed_data,
  aes(x=Long_.1, y=Lat.1, size = Confirmed),
  color="red"
)

####Density test
qmplot(long, lat, data = joined_states, geom = "blank", 
       maptype = "toner-background", legend = "topleft"
) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2(joined_states$cases, low = "white", mid = "yellow", high = "red", midpoint = 650)





## my work 

head(joined_states)

coordinates(joined_states) <- ~ long + lat
test_class_joined_states <- class(joined_states)

autoplot(joined_states, p= state_base, color = 'red', size = joined_states$cases) 










