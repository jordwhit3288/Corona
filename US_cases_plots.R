
library(readr)
us_states <- read_csv("ny_times_data/covid-19-data/us-states.csv")

View(us_states)

class(us_states$date)


### CASES PER DAY
total_us_cases <- aggregate(us_states$cases, by=list(Date=us_states$date), FUN=sum)
colnames(total_us_cases)[2] <- "Cases"

View(total_us_cases)

ggplot(data=total_us_cases, aes(x=Date, y=Cases)) +
  geom_point(color="red") +
  ggtitle("USA Cases")


### NORTH CAROLINA PLOT
north_carolina <- filter(us_states, us_states$state == 'North Carolina')

max_date <- max(us_states$date)



ggplot(data=north_carolina, mapping=aes(x=date, y=cases)) + 
  geom_point(color = "blue") +
  ggtitle("NC Cases") 
  


### CASES PER EACH STATES
current_state_cases <- filter(us_states, us_states$date == max_date)

head(current_state_cases)


states_v <- c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',  'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
current_state_cases <- current_state_cases[current_state_cases$state %in% states_v,]
View(current_state_cases)


ggplot(data=current_state_cases, mapping=aes(x=state, y=cases)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle =60, vjust = 0.5)) +
  ggtitle("All States Reported Cases")

