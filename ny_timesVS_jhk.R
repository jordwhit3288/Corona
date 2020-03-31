#### ny times data prep

ny_state_data <- read_csv("ny_times_data/covid-19-data/us-states.csv")

head(ny_state_data)
View(ny_state_data)

max_date <- max(ny_state_data$date)
max_date
ny_times_current_case_count <- filter(ny_state_data, date == max_date)

View(ny_times_current_case_count)
ny_times_current_case_count <- ny_times_current_case_count[ny_times_current_case_count$state %in% states_v,]


### jhk data prep 

jhk_corona_data <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv")
head(jhk_corona_data)


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

states_v <- c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',  'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
states_v

#usa_new <- usa_data[usa_data$State %in% states_v,]

#using states_v vector to filter out the current_case_count df
current_case_count <- current_case_count[current_case_count$state %in% states_v,]

current_case_count


ny_times_current_case_count <- ny_times_current_case_count[,c(2,4)]
colnames(ny_times_current_case_count)[1] <- "ny_times_state"
colnames(ny_times_current_case_count)[2] <- "ny_times_cases"

colnames(current_case_count)[1] <- "jhk_state"
colnames(current_case_count)[2] <- "jhk_cases"
current_case_count

combined_case_data <- cbind(current_case_count, ny_times_current_case_count) 

View(combined_case_data)

combined_case_data$diff <- combined_case_data$jhk_cases - combined_case_data$ny_times_cases

View(combined_case_data)


