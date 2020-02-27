library(tidyverse)
library(rgdal)

###############################################

data <- read_csv("Police_Department_Incidents_-_Previous_Year__2016_.csv")
data <- data %>%
  rename(DISTRICT = PdDistrict,
         lat = Y, long = X,
         Day = DayOfWeek) %>% 
  mutate(Datetime = as.POSIXct(paste(str_sub(Date, 1, 10), Time), format = "%m/%d/%Y %H:%M:%S"),
         Date = as.Date(str_sub(Date, 1, 10), format = "%m/%d/%Y"),
         Resolution = as.factor(Resolution),
         DISTRICT = as.factor(DISTRICT),
         Category = as.factor(Category),
         Day = as.factor(Day))
  

geojson <- readOGR("san-francisco.geojson")

###############################################

geomap <- function(start = min(data$Date), end = max(data$Date), palette = "YlOrRd"){
  #' retourne un leaflet des incidents pendant la période
  #' start, end: dates limites de la période
  #' palette: palette de couleur dans fillColor
  
  pal <- colorNumeric(palette, NULL)
  
  gdata <- data %>% 
    filter(Date >= start & Date <= end) %>% 
    group_by(DISTRICT) %>% 
    summarize(
      Incidents = n()
    )
  
  gjson <- geojson
  gjson@data <- merge(gjson@data, gdata)
  
  plot <- leaflet(gjson) %>% addTiles() %>%
    addPolygons(
      stroke = F,
      smoothFactor = 0.3,
      fillOpacity = 0.6,
      fillColor = ~pal(Incidents),
      label = ~paste0(DISTRICT, ": ",
                      formatC(Incidents, big.mark = ","),
                      " incidents.")
    ) %>% 
    addLegend(pal = pal, values = ~Incidents)
  
  return(plot)
}
###############################################

ts_plot <- function(start = min(data$Date), end = max(data$Date)){
  #' retourne un amPlot des incidents pendant une période
  #' start, end: limites période
  
  Seq <- tibble(Date = seq.POSIXt(from = as.POSIXct(start, format = "%Y-%m-%d"),
                                  to = as.POSIXct(end, format = "%Y-%m-%d"),
                                  by = 'day'))
  
  pdata <- data %>%
    filter(Date >= start & Date <= end) %>% 
    group_by(Date) %>%
    summarise(Incidents = n()) %>% 
    mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
    right_join(Seq, by = "Date") %>% 
    mutate(Incidents = replace_na(Incidents, 0))
  
  plot <- amPlot(x = format(pdata$Date),
                 y = pdata$Incidents, type = 'sl',
                 parseDates = TRUE, dataDateFormat = "YYYY-MM-DD",
                 main = paste("Incidents du", start, "au", end),
                 ylab = "Incidents", xlab = "")
  
  return(plot)
}
###############################################

clustermap <- function(start = min(data$Date), end = max(data$Date)){
  #' retourne un leaflet de la répartition pendant la période
  #' start, end: limites période
  
  plot <- data %>% 
    filter(Date >= start & Date <= end) %>%
    leaflet() %>% addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions(),
               label = ~paste0(Date, ": ", tolower(Descript)))
  return(plot)
}