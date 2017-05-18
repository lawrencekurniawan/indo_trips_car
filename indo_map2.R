library(leaflet)
library(tidyverse)
library(stringr)
library(htmltools)

file <- '/Users/lawrencewong/Documents/Projects/Trip_map_autoupdate/allhands_trip.csv'

data <- read_csv(file)

#need to update the list below based on the mapping code (addAwesomeMarkers will show which are clustered, put the cities below)
cities_clustered <- c("Jakarta", "Bandung", "Cirebon", "Semarang", "Yogyakarta",
                      "Surakarta", "Surabaya", "Malang", "Bali")

data <- data %>%
  mutate(type = ifelse(is.na(car_wow) == TRUE, "new", 
                       ifelse(car_wow >= 0, "growth", "decline")), #type is currently not used
         color = ifelse(type == "new", "blue",
                        ifelse(type == "growth", "green", "red")), #used for icon color
         clustered = ifelse(City %in% cities_clustered, T, F)) #create column indicating whether a row (City) is clustered by addAwesomeMarker

for (i in 1:nrow(data)) {
  data$label[i] <- htmltools::HTML(paste0(data$City[[i]], '<br/>', '<font color = \"', data$color[[i]], '\">', data$Car[[i]], "</font>"))
}

data_non_clustered <- data[data$clustered == F, ] #contains data of city that are not clustered by addAwesomeMarkers

#creates icon with variable color (specified above)
icons <- awesomeIcons(
  icon = 'ion-model-s',
  iconColor = 'black',
  library = 'ion',
  markerColor = data$color
)

map_clustered <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.DarkMatter") %>% #adds dark skin to the map
  addAwesomeMarkers(
    lng = data$longitude, 
    lat = data$latitude, 
    clusterOptions = markerClusterOptions(), #will cluster based on zoom level
    icon = icons) %>%
    {
      for(i in 1:nrow(data_non_clustered)){ #only adds labels for non clustered cities
        . <- addLabelOnlyMarkers(
          .,
          lng = data_non_clustered$longitude[[i]],
          lat = data_non_clustered$latitude[[i]],
          label = htmltools::HTML(data_non_clustered$label[[i]]),
          labelOptions = labelOptions(direction = 'auto', noHide = T)
          )
      } 
      return(.)
    }

map_show_all_label <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.DarkMatter") %>% #adds dark skin to the map
  addAwesomeMarkers(
    lng = data$longitude, 
    lat = data$latitude, #removed the cluster option
    icon = icons) %>%
    {
      for(i in 1:nrow(data)){ #show all label
        . <- addLabelOnlyMarkers(
          .,
          lng = data$longitude[[i]],
          lat = data$latitude[[i]],
          label = htmltools::HTML(data$label[[i]]),
          #surakarta's label will be forced to be on the right
          labelOptions = labelOptions(direction = ifelse(data$City[[i]] == 'Surakarta', 'right', 'auto'), noHide = T)
        )
      } 
      return(.)
    }

map_clustered
map_show_all_label
