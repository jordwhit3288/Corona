
library(readr)
us_states <- read_csv("ny_times_data/covid-19-data/us-states.csv")

View(us_states)

class(us_states$date)
colnames(us_states)[3] <- "state_fips"

### CASES PER DAY
total_us_cases <- aggregate(us_states$cases, by=list(Date=us_states$date), FUN=sum)
colnames(total_us_cases)[2] <- "Cases"

View(total_us_cases)

usa_total_plot <- ggplot(data=total_us_cases, aes(x=Date, y=Cases)) +
  geom_point(color="red") +
  ggtitle("USA Cases")

plotly_build(usa_total_plot)

### NORTH CAROLINA PLOT
north_carolina <- filter(us_states, us_states$state == 'North Carolina')

max_date <- max(us_states$date)



nc_plot <- ggplot(data=north_carolina, mapping=aes(x=date, y=cases)) +
  geom_point() +
  ggtitle("NC Cases") 

plot_ly(data=north_carolina, aes(x=date, y=cases)) +
  geom_point()


### CASES PER EACH STATES
current_state_cases <- filter(us_states, us_states$date == max_date)

head(current_state_cases)


states_v <- c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',  'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
current_state_cases <- current_state_cases[current_state_cases$state %in% states_v,]
View(current_state_cases)


all_states <- ggplot(data=current_state_cases, mapping=aes(x=state, y=cases)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle =60, vjust = 0.5)) +
  ggtitle("All States Reported Cases")

plotly_build(all_states)

class(us_states$fips)


fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  locations = usmap::plot_usmap(),
  z=us_states$cases,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= -95.71, lat=37.09))
)
fig



library(urbnmapr)

states_urban <- urbnmapr::states

joined_urban_nytimes <- inner_join(us_states, states_urban, by="state_fips")

joined_urban_nytimes %>%
  ggplot(aes(long, lat, fill = cases)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0= 39, lat1 =45) +
  labs(fill = "Cases By State")

ggplot(data = joined_urban_nytimes, aes(x=long, y=lat, fill = cases)) +
  geom_polygon(color = NA)








