library(RCurl)
library(XML)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(scales)
library(rjson)
library(dplyr)
library(crosstalk)

# The URL for the JSON changes each day. So today's URL, we want to find it in the current day's page.
# We get the top-level page

ny = htmlParse(getURLContent("https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html?action=click&module=Top%20Stories&pgtype=Homepage"))

# Then we find the HTML elements that have an immediate child text node that contains the string USA.json.
js = xpathSApply(ny, "//*[contains(./text(), 'USA.json')]", xmlValue)

#These are <script> elements containing JavaScript, not JSON.
# But we can find the URL with a regular expression in these
u = gsub('.*"(https://[^"]+USA.json)".*', "\\1", js)
u = unique(u)
# There is only 1 URL repeated multiple times.

# So now we have these

tt = getURLContent(u, verbose = TRUE, followlocation = TRUE)
library(RJSONIO)
us = fromJSON(tt)

length(us$data)


#time series
ca_id = us$data[grep("(state.*California|California.*state)", us$data)][[1]]$geoid
pattern = sprintf("county.*%s|county.*%s", ca_id, ca_id)
county_data = us$data[grep(pattern, us$data)]
county_data = county_data[1:58]
range = as.Date(us$range)
dates = seq(range[1], range[2], by = "day")

createTS = function(dates, mydata){
  Cases = mydata$cases
  Deaths = mydata$deaths
  
  New_Cases = c(Cases[1], rep(0, (length(Cases)-1)))
  New_Deaths = c(Deaths[1], rep(0, (length(Deaths)-1)))
  
  for (i in c(2:length(Cases))){
    New_Cases[i] = Cases[i] - Cases[(i-1)]
    New_Deaths[i] = Deaths[i] - Deaths[(i-1)]
  }
  
  case_ts = data.frame(County = rep(mydata$display_name, length(dates)), Cases = Cases, Deaths = Deaths, Population = mydata$population, New_Cases = New_Cases, New_Deaths=New_Deaths)
  
  return(case_ts)
}

all_ts = rbind(lapply(county_data, function(x) createTS(dates, x)))

#combine all data into one dataframe for ploting
ts = data.frame(
  County = unlist(lapply(all_ts, function(x) return(x$County))),
  Date = rep(dates, length(all_ts)),
  Cases = unlist(lapply(all_ts, function(x) return(x$Cases))),
  Deaths = unlist(lapply(all_ts, function(x) return(x$Deaths))),
  Cases_Per = unlist(lapply(all_ts, function(x) return(x$Cases)))/unlist(lapply(all_ts, function(x) return(x$Population/1000))),
  Deaths_Per = unlist(lapply(all_ts, function(x) return(x$Deaths)))/unlist(lapply(all_ts, function(x) return(x$Population/1000))),
  New_Cases = unlist(lapply(all_ts, function(x) return(x$New_Cases))),
  New_Deaths = unlist(lapply(all_ts, function(x) return(x$New_Deaths))),
  Population = unlist(lapply(all_ts, function(x) return(x$Population)))
)


generateText = function(ts){
  County = ts$County
  Date = ts$Date
  Cases = ts$Cases
  Text = rep(NA, length(nrow(ts)))
  for (i in c(1:nrow(ts))){
    Text[i] = sprintf("%s <br>Date: %s <br>New Cases: %i <br>New Deaths: %i <br>Cases Per Thousand: %.2f <br>Deaths Per Thousand: %.2f <br>Population: %i <br>Cases: %i <br>Deaths: %i", 
                      County[i], Date[i], ts$New_Cases[i], ts$New_Deaths[i], ts$Cases_Per[i], ts$Deaths_Per[i], ts$Population[i], Cases[i], ts$Deaths[i])
  }
  df = data.frame(County = County, Date = Date, Cases = Cases, Text = Text, Cases_Per = ts$Cases_Per, Deaths_Per = ts$Deaths_Per, Population = ts$Population)
  return(df)
}

mydata = generateText(ts)

# obtain the geo information from plotly
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

#only keep counties in California 
ca_counties = counties$features[grep('US06', counties$features)]
counties$features = ca_counties

date = tail(dates,1)
counties_name = unique(mydata$County)
geoid = list()
for (i in c(1:length(counties_name))){
  id = counties$features[grep(counties_name[i], counties$features)][[1]]$id
  geoid = unlist(c(geoid, rep(id, length(dates))))
}

mydata = cbind(mydata, geoid)

mydata <- highlight_key(mydata, ~County)

case_plot = ggplot(data = mydata, aes(x = Date, y = Cases, group = County, color = County, text = Text)) +
  geom_line() +
  labs(x = "Date",
       y = "Total Cases")+
  scale_y_continuous(labels = comma)

cases = ggplotly(case_plot, tooltip = c("text"),
                 dynamicTicks = TRUE)

cases = cases %>%
  layout(
    title = 'California Covid-19 Time Series and Map',
    
    annotations = list(
      list(x = 0, y = -0.25, text = "Source: New York Times.", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='left', yanchor='top', xshift=0, yshift=0,
           font=list(size=10)),
      
      list(x = 0, y = 1.2, text = "Click to highlight a county; Double click to disable highlighting. <br>Hover to see details", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='left', yanchor='botton', xshift=0, yshift=0,
           font=list(size=10))
    ),
    
    autosize = T,
    margin = list(
      l= 50,
      r= 50,
      b= 100,
      t= 100
    ),
    
    updatemenus = list(
      list(
        type = "buttons",
        direction ="right",
        showactive = FALSE,
        xanchor = 'left',
        yanchor = 'top',
        x = 0.95,
        y = -0.3,
        buttons = list(
          
          list(method = "restyle",
               args = list("visible", "legendonly"),
               label = "Hide All"),
          
          list(method = "restyle",
               args = list("visible", T),
               label = "Show All")))
    )
  )%>%
  highlight(on = "plotly_click", off = "plotly_relayout", 
            persistent = TRUE,  selected = attrs_selected(
              showlegend = FALSE), color = "red")

#map
#create data for mapping
df = generateText(ts)

df = cbind(df, geoid)

index = seq(1, length(dates), 5)

if (tail(index,1) != length(dates)){
  index= c(index, length(dates))
}

#subset the data to reduce the size.
map_data = filter(df, df$Date %in% dates[index])

csl = list(c(0.2, 4), c("pink","red"))

default = filter(df, df$Date == date)
#obtain all data for current date

fig = plot_ly(data = default,
              type='choropleth',
              colorscale = csl,
              colorbar = list(title = "Cases Per 1000 Population"),
              geojson = counties, 
              locations = default$geoid,
              showscale = T,
              z = default$Cases_Per,
              zmax = max(default$Cases_Per), 
              zmin = 0,
              text = default$Text,
              hoverinfo = 'text',
              marker = list(     # for the lines separating states
                line = list (
                  color = 'rgb(255,255,255)', 
                  width = 2)
              ))%>%
  add_trace(
    #add all data
    data = map_data,
    type='choropleth',
    frame = map_data$Date,
    colorscale = csl,
    geojson = counties,
    locations = map_data$geoid,
    showscale = F,
    #using cases per thousands to color the map since some counties have very high population, leading to very high number of cases.
    z = map_data$Cases_Per,
    #set static colorscale across all days
    zmax = max(map_data$Cases_Per), 
    zmin = 0,
    
    text = ~Text,
    hoverinfo = 'text',
    marker = list(     # for the lines separating states
      line = list (
        color = 'rgb(255,255,255)', 
        width = 2)
    )
  )%>%
  animation_opts(1, easing = "elastic", redraw = TRUE)%>%
  animation_slider(
    active = 0,
    currentvalue = list(prefix = "Date: ")
  )%>%
  animation_button(
    x = 1.02, xanchor = "left", y = 0, yanchor = "top"
  )

fig = fig%>%layout(
  title = list(text = "Map of Covid-19 Cases, California"),
  legend = list(
    title = list(text = "Cases Per 1000")
  ),
  
  margin = list(
    l= 50,
    r= 50,
    b= 100,
    t= 100
  ),
  geo = list(
    scope = "usa",
    center = list(lon=c(-120),lat=c(37)),
    projection = list(scale = 2, type='albers usa' )
  )
)


widget = bscols(
  widths = c(12,12),
  cases, fig)

widget