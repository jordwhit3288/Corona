
ihme_data <- read_csv("ihme_data/2020_04_05.05.us/Hospitalization_all_locs.csv")
View(ihme_data)


test_ihme_plot <- ggplot(ihme_data, aes(date, deaths_mean)) +
  geom_point() +
  geom_smooth()

plotly_build(test_ihme_plot)


sum(ihme_data$deaths_mean)

ihme_death_data <-ihme_data[c(2,3,13,14,15)]
library(CGPfunctions)
class(ihme_death_data$date)
ihme_death_data$date <- as.character(ihme_death_data$date)

ihme_states <- filter(ihme_data, ihme_death_data$location_name != 'United States of America')
head(ihme_states)
View(ihme_states)

ihme_lm <- lm(deaths_mean ~ admis_mean + allbed_mean + ICUbed_mean + InvVen_mean,
              data = ihme_states)

ihme_lm

visreg(ihme_lm, "InvVen_mean", gg = TRUE)

newggslopegraph(ihme_death_data, date, deaths_mean, location_name)

ihme_usa <- filter(ihme_death_data, ihme_death_data$location_name == 'United States of America')
ihme_usa  



#### RAW PLOT
p2 <- ggplot(data=ihme_usa, aes(x=date)) +
  geom_line(aes(y=deaths_mean)) +
  geom_line(aes(y=deaths_lower)) +
  geom_line(aes(y=deaths_upper)) 
p2

plotly_build(p2)


######## IHME STYLE PLOT ######

ihme_nc <- filter(ihme_states, location_name == 'North Carolina'& date > '2020-03-25' & date < '2020-05-10')
View(ihme_nc)
ihme_nc <- filter(ihme_states, date < '2020-05-01' )
class(ihme_nc$date)

colnames(ihme_nc)[3] <- 'Date'
colnames(ihme_nc)[13] <- 'Mean Projected Deaths'
colnames(ihme_nc)[15] <- 'Upper Limit Projected Deaths'
ihme_nc$`Mean Projected Deaths`
ihme_nc$`Upper Limit Projected Deaths`


ihme_nc_plot <- ggplot(ihme_nc, aes(x=Date)) +
  geom_area(aes(y=`Upper Limit Projected Deaths`), fill = "blue", alpha= 0.2) +
  geom_line(aes(y=`Mean Projected Deaths`)) +
  scale_x_date(date_breaks = "5 days") +
  labs(title = "NC Deaths per Day",  subtitle = "Upper Limit VS Projected Mean", x = "Date", y ="Deaths")

ihme_nc_plot

ihme_nc_plot <-  ihme_nc_plot + theme_clean()

ihme_nc_plot <- plotly_build(ihme_nc_plot)

ihme_nc_plot <- ihme_nc_plot %>%
  title(main = "NC Deaths per Day", sub = "Upper Limit VS Projected Mean")
ihme_nc_plot


fig <- plot_ly(ihme_nc, x = ~Date, y = ~`Upper Limit Projected Deaths`, name = 'Upper Limit', type = 'scatter', mode = 'lines',
               hoverinfo = 'text',
               text = ~paste('</br> Date: ', Date,
                             '</br> Upper Limit: ', `Upper Limit Projected Deaths`),
               fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
            showlegend = FALSE)
fig <- fig %>% add_trace( y = ~`Mean Projected Deaths`, name = 'Projected Mean', mode = 'lines',
                          hoverinfo = 'text',
                          text = ~paste('</br> Date: ', Date,
                                        '</br> Projected Deaths Per Day: ', `Mean Projected Deaths`),
                          line = list(color='rgb(0,100,80)'))


f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "black"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)
a <- list(
  title = "Deaths Per Day",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f2,
  exponentformat = "E"
)

b <- list(
  title = "Date",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f2,
  exponentformat = "E"
)


fig <- fig %>%
  layout(title = "Projected COVID-19 Deaths per Day in North Carolina", xaxis = b, yaxis = a)
fig


##### DISREGARD
# ihme_nc_test <- ihme_nc[c(3, 13, 14, 15)]
# ihme_nc_test$date <- ymd(ihme_nc_test$date)
# ihme_nc_test$date


install.packages("ggThemeAssist")
install.packages("ggthemes")
library(ggThemeAssist)
library(ggthemes)


