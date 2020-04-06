jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/04-04-2020.csv")

View(jhk_corona_data)

#renaming dataframe to friendly name
corona_data <- jhk_corona_data
head(corona_data)


#renaming specific column indices to friendly names
colnames(kansas_data)[2] <- "county"
colnames(corona_data)[3] <- "state"
colnames(corona_data)[4] <- "country"
colnames(corona_data)[5] <- "date"


head(corona_data)

#Filtering df to united states data
new_usa_data <- filter(corona_data, country == 'US')
head(new_usa_data)
View(new_usa_data)

kansas_data <- filter(new_usa_data, state == 'Kansas')
head(kansas_data)
colnames(kansas_data)[1] <- 'fips'
head(kansas_data)

#ggplot(kansas_data) + geom_sf(aes(fill=Confirmed))
states <- map_data("state")
states

counties <- map_data("county")
counties

kansas_outline <- subset(states, region == 'kansas')
kansas_counties <- subset(counties, region == 'kansas')

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

kansas_base  <- ggplot(data = kansas_outline, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
kansas_base + plot_clean_background

kansas_base <- kansas_base + plot_clean_background + 
  geom_polygon(data = kansas_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

kansas_base

kansas_data$county <- tolower(kansas_data$county)
colnames(kansas_data)[2] <- 'county'
colnames(kansas_counties)[6] <- 'county'

head(kansas_data)
head(kansas_counties)

joined_kansas_counties <- inner_join(kansas_data, kansas_counties, by='county')
head(joined_kansas_counties)

colnames(joined_kansas_counties)[8] <- 'Cases'

max(joined_kansas_counties$Cases)
mean(joined_kansas_counties$Cases)
display.brewer.all(n=6)
kansas_palette<-brewer.pal(5,"Spectral")


kansas_confirmed_by_county <- kansas_base  + 
  # (aes(text= paste("County:", joined_kansas_counties$county, "\n", "Reported Cases:", joined_kansas_counties$Cases))) +
  geom_polygon(data = joined_kansas_counties, aes(fill = Cases), color = "gray90") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradientn(colours = rev(kansas_palette),
                       breaks = c(-5, 0, 5, 30, 50, 100, 150)) +
  theme_bw() +
  plot_clean_background +
  ggtitle("Confirmed COVID-19 Cases per County - April 4")


kansas_confirmed_by_county
plotly_build(kansas_confirmed_by_county, registerFrames = FALSE)

ggplotly(kansas_confirmed_by_county)
################## PLOTTING DEATHS NOW #######################


kansas_deaths_by_county <- kansas_base  + 
  # (aes(text= paste("County:", joined_kansas_counties$county, "\n", "Reported Cases:", joined_kansas_counties$Cases))) +
  geom_polygon(data = joined_kansas_counties, aes(fill = Deaths), color = "gray90") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradientn(colours = rev(kansas_palette),
                       breaks = c(0, 1, 2, 3, 4,5)) +
  theme_bw() +
  plot_clean_background +
  ggtitle("Confirmed COVID-19 Deaths per County - April 4")

kansas_confirmed_by_county
plotly_build(kansas_deaths_by_county)




















