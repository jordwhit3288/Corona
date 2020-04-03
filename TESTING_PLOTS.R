library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))

head(df)
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(df, locationmode = 'USA-states')
fig
fig <- fig %>% add_trace(
  z = ~total.exports, text = ~hover, locations = ~code,
  color = ~total.exports, colors = 'Purples'
)
fig
fig <- fig %>% colorbar(title = "Millions USD")
fig <- fig %>% layout(
  title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
  geo = g
)

fig


new_df <- joined_states
head(new_df)
new_df$hover <- with(new_df, paste(state, '<br>', "Cases", cases))

head(new_df)
new_l <- list(color = toRGB("white"), width = 2)

new_g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

new_df

new_fig <- plot_geo(new_df, locationmode = 'USA-states')
new_fig
new_fig <- new_fig %>% add_trace(
  z = new_df$cases, text = new_df$hover,
  color = new_df$cases, colors = 'Purples'
)
new_fig


View(new_df)

new_df <- new_df[c(5,7,8)]
head(new_df)

new_df <- new_df[!duplicated(new_df),]
View(new_df)

library(Hmisc)
capitalize(new_df$state)
new_df$state <- capitalize(new_df$state)

head(new_df)
plot_ly(type="choropleth", locations=new_df$state, locationmode="USA-states",
        z=new_df$cases) %>% layout(geo=list(scope="usa"),)

plot_ly(type="choropleth", locations=new_df$state, locationmode="USA-states", z=new_df$cases) %>%
  layout(geo=list(scope="usa"))


p <- plot_geo(sample, locationmode='USA-states')

state_base
plot_ly(new_df, color= new_df$cases)



