jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/06-03-2020.csv")

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

nc_data <- filter(new_usa_data, state == 'North Carolina')
head(nc_data)
colnames(nc_data)[1] <- 'fips'
head(nc_data)

#ggplot(nc_data) + geom_sf(aes(fill=Confirmed))
states <- map_data("state")
states

counties <- map_data("county")
counties

nc_outline <- subset(states, region == 'north carolina')
nc_counties <- subset(counties, region == 'north carolina')

#getting rid of the messy background 
plot_clean_background <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
plot_clean_background

nc_base  <- ggplot(data = nc_outline, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
nc_base + plot_clean_background

nc_base <- nc_base + plot_clean_background + 
  geom_polygon(data = nc_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

nc_base

nc_data$city <- tolower(nc_data$city)
colnames(nc_data)[2] <- 'county'
colnames(nc_counties)[6] <- 'county'

head(nc_data)
head(nc_counties)

joined_nc_counties <- inner_join(nc_data, nc_counties, by='county')
head(joined_nc_counties)

colnames(joined_nc_counties)[8] <- 'Cases'


display.brewer.all(n=6)
nc_palette<-brewer.pal(5,"Spectral")


country_death_plot <- state_base + (aes(text= paste("State:", joined_states_death$state, "\n", "Deaths:", joined_states_death$deaths))) + 
  geom_polygon(data = joined_states_death, aes(fill = deaths), color="grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_set(theme_bw(base_size =  15, base_family = 'Times New Roman')) +
  plot_clean_background

View(joined_nc_counties)

joined_nc_counties <- joined_nc_counties[c(2,8,13,14,15,16)]

nc_confirmed_cases <- nc_base + (aes(text=  paste("County", joined_nc_counties$county, "\n", "Cases", joined_nc_counties$Cases))) +
  geom_polygon(data = joined_nc_counties, aes(fill = Cases), color = "grey")

nc_confirmed_by_county <- nc_base + 
  geom_polygon(data = joined_nc_counties, aes(fill = Cases), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradientn(colours = nc_palette,
                       breaks = c(10, 100, 250, 350, 500, 800)) +
  geom_text(label = row.names(joined_nc_counties))
  theme_bw() +
  plot_clean_background +
  ggtitle("Confirmed COVID-19 Cases per County - April 4")

nc_confirmed_by_county + scale_y
plotly_build(nc_confirmed_by_county)


################## PLOTTING DEATHS NOW #######################


nc_deaths_by_county <- nc_base  + 
  # (aes(text= paste("County:", joined_nc_counties$county, "\n", "Reported Cases:", joined_nc_counties$Cases))) +
  geom_polygon(data = joined_nc_counties, aes(fill = Deaths), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradientn(colours = rev(nc_palette),
                       breaks = c(0, 1, 2, 3, 4,5)) +
  theme_bw() +
  plot_clean_background +
  ggtitle("Confirmed COVID-19 Deaths per County - April 4")

nc_confirmed_by_county
plotly_build(nc_deaths_by_county)



######################## LOOKING AT PEAK #########################
times_corona_data  <- read_csv("ny_times_data/covid-19-data/us-states.csv")
View(times_corona_data)

head(times_corona_data)
#Filtering df to NY data
max(times_corona_data$date)
times_nc_data <- filter(times_corona_data, state == 'North Carolina')

head(times_nc_data)

times_nc_data <- times_nc_data %>% 
  mutate(daily_increase = cases - lag(cases)) # dplyr has its own version of lag

times_nc_data$daily_increase <- replace_na(times_nc_data$daily_increase, 0) 
View(usa_cases)

View(times_nc_data)

ggplot(data = times_nc_data, aes(x=date, y=daily_increase)) +
  geom_line() +
  geom_smooth()

nc_daily_increase_plot <- ggplot(data = times_nc_data, aes(x=date, y=daily_increase)) +
  geom_line() +
  geom_smooth() +
  labs(title = "North Carolina Daily Case Increase", subtitle = "June 3", x="Date", y = "Case Increase") +
  scale_x_date(breaks = "5 days") + theme(axis.ticks = element_line(colour = "gray4"), 
                                          panel.grid.major = element_line(linetype = "dotted"), 
                                          axis.text.x = element_text(vjust = 0.5, 
                                           angle = 45), panel.background = element_rect(fill = "aliceblue", 
                                          linetype = "longdash"), plot.background = element_rect(fill = "white")) + 
                                          theme(panel.background = element_rect(fill = "gray97", 
                                          linetype = "blank"), plot.background = element_rect(linetype = "dashed")) +
                                          labs(y = "Daily Case Increase", caption = "Source: New York Times")
nc_daily_increase_plot

plotly_build(nc_daily_increase_plot)

















