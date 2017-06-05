library(leaflet)
library(tidyverse)
library(stringr)
library(htmltools)
library(sitools)

source("/Users/lawrencewong/Documents/Projects/Trip_map_autoupdate/format_number.R") #formats number

file <- '/Users/lawrencewong/Documents/Projects/Trip_map_autoupdate/allhands_trip.csv'

data <- read_csv(file)


# Manual input ------------------------------------------------------------
#need to update the list below based on the mapping code (addAwesomeMarkers will show which are clustered, put the cities below)
cities_clustered <- c("Jakarta", "Bandung", "Cirebon", "Semarang", "Yogyakarta",
                      "Surakarta", "Surabaya", "Malang", "Bali")

label_direction_right <- c()

label_direction_left <- c()

label_direction_bottom <- c()

label_direction_top <- c()


# End manual input --------------------------------------------------------


data <- data %>%
  mutate(type = ifelse(is.na(car_wow) == TRUE, "new", 
                       ifelse(car_wow >= 0, "growth", "decline")), #type is currently not used
         marker_color = ifelse(type == "new", "darkblue",
                        ifelse(type == "growth", "darkgreen", "darkred")), #used for marker color
         color = ifelse(type == "new", "#108188",
                        ifelse(type == "growth", "green", "red")), #used for number color
         clustered = ifelse(City %in% cities_clustered, T, F), #create column indicating whether a row (City) is clustered by addAwesomeMarker
         Car = f2si(Car, digits = 4),
         Moto = f2si(Moto, digits = 4),
         car_wow = ifelse(is.na(car_wow) == F, 
                          paste0(ifelse(car_wow > 0, '+', ''), car_wow, "%"), 
                          '100%'),
         moto_wow = ifelse(is.na(moto_wow) == F, 
                           paste0(ifelse(moto_wow > 0, '+', ''), moto_wow, "%"), 
                           '100%')) 

for (i in 1:nrow(data)) {
  data$label[i] <- htmltools::HTML(paste0('<font style = "font-size: 120%;">', data$City[[i]], '</font>',
                                          '<br/>', 
                                          '<font style = "font-size: 120%;">', data$Car[[i]],
                                          '<br/>',
                                          ' (',
                                          '<font color = \"', data$color[[i]], '\">', 
                                          data$car_wow[[i]],
                                          "</font>", ')', '</font>'))
}

data_non_clustered <- data[data$clustered == F, ] #contains data of city that are not clustered by addAwesomeMarkers


# Function to generate indo trip map -----------------------------------------------

indo_trip_map <- function(data_full, data_non_clustered, vehicle_view = car_wow) {
  
#creates icon with variable color (specified above)
icons <- awesomeIcons(
  icon = 'ion-model-s',
  iconColor = 'black',
  library = 'ion',
  markerColor = data_full$marker_color
  )

map_clustered <<- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.DarkMatter") %>% #adds dark skin to the map
  addAwesomeMarkers(
    lng = data_full$longitude, 
    lat = data_full$latitude, 
    clusterOptions = markerClusterOptions(), #will cluster based on zoom level
    icon = icons) %>%
    {
      for(i in 1:nrow(data_non_clustered)){ #only adds labels for non clustered cities
        . <- addLabelOnlyMarkers(
          .,
          lng = data_non_clustered$longitude[[i]],
          lat = data_non_clustered$latitude[[i]],
          label = htmltools::HTML(data_non_clustered$label[[i]]),
          labelOptions = labelOptions(direction = ifelse(data_non_clustered$City[[i]] %in% label_direction_top,
                                                         'top', 
                                                         ifelse(data_non_clustered$City[[i]] %in% label_direction_right, 
                                                                'right', 
                                                                ifelse(data_non_clustered$City[[i]] %in% label_direction_bottom, 
                                                                       'bottom',
                                                                       ifelse(data_non_clustered$City[[i]] %in% label_direction_left,
                                                                              'left', 'auto')))), noHide = T)
          )
      } 
      return(.)
    }

map_show_all_label <<- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.DarkMatter") %>% #adds dark skin to the map
  addAwesomeMarkers(
    lng = data_full$longitude, 
    lat = data_full$latitude, #removed the cluster option
    icon = icons) %>%
    {
      for(i in 1:nrow(data_full)){ #show all label
        . <- addLabelOnlyMarkers(
          .,
          lng = data_full$longitude[[i]],
          lat = data_full$latitude[[i]],
          label = htmltools::HTML(data_full$label[[i]]),
          labelOptions = labelOptions(direction = ifelse(data_full$City[[i]] %in% label_direction_top,
                                                         'top', 
                                                         ifelse(data_full$City[[i]] %in% label_direction_right, 
                                                                'right', 
                                                                ifelse(data_full$City[[i]] %in% label_direction_bottom, 
                                                                       'bottom',
                                                                       ifelse(data_full$City[[i]] %in% label_direction_left,
                                                                              'left', 'auto')))), noHide = T)
        )
      } 
      return(.)
    }

map_clustered
map_show_all_label

}

indo_trip_map(data, data_non_clustered)
