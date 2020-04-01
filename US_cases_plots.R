
library(readr)
usa_confirmed <- read_csv("jhk_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
View(time_series_covid19_confirmed_US)


head(usa_confirmed)
View(usa_confirmed)

usa_confirmed[,ncol(usa_confirmed)]

usa_confirmed <- usa_confirmed[ , c(7, 13:length( names(usa_confirmed)) -1 ) ]

usa_confirmed_new <- usa_confirmed %>% 
  group_by(usa_confirmed$Province_State)

head(usa_confirmed)
View(usa_confirmed)


states_v <- c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',  'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
usa_confirmed <- usa_confirmed[usa_confirmed %in% states_v,]


colnames(usa_confirmed) <- usa_confirmed[1, ]
colnames(usa_confirmed)

usa_confirmed <- data.frame(t(usa_confirmed[-1]))
colnames(usa_confirmed) <- usa_confirmed[, 1]

View(usa_confirmed)


usa_confirmed$
colnames(usa_confirme
