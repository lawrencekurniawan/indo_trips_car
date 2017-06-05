library(leaflet)
library(tidyverse)
library(stringr)
library(htmltools)
#library(sitools)

source("/Users/lawrencewong/Documents/Projects/Trip_map_autoupdate/format_number.R") #formats number

file <- '/Users/lawrencewong/Documents/Projects/Trip_map_autoupdate/20170529_allhands_trip.csv'

data <- read_csv(file)


# Manual input ------------------------------------------------------------
#need to update the list below based on the mapping code
cities_clustered <- c("Jakarta", "Bandung", "Cirebon", "Semarang", "Yogyakarta",
                      "Surakarta", "Surabaya", "Malang", "Bali", 'Banyuwangi')

label_direction_right <- c('Makassar', 'Surabaya', 'Makassar')

label_direction_left <- c('Medan', 'Pekanbaru', 'Semarang', 'Yogyakarta', 'Jakarta', 'Bandung'
                          , 'Lampung', 'Palembang', 'Pontianak', 'Manado', 'Lampung', 'Malang')

label_direction_bottom <- c() #doesn't really work because labels are multiline

label_direction_top <- c('Surakarta', 'Bali', 'Cirebon') #doesn't really work because labels are multiline


# End manual input --------------------------------------------------------


# Car ---------------------------------------------------------------------
# ----------------------------------- -------------------------------------

data <- data %>% #replaces all the car_wow % for all new cities to NA
  mutate(car_wow = replace(car_wow, car_wow > 10000, NA))

data <- data %>%
  mutate(type = ifelse(is.na(car_wow) == TRUE, "new", 
                       ifelse(car_wow >= 0, "growth", "decline")), #type is currently not used
         marker_color = ifelse(type == "new", "blue",
                        ifelse(type == "growth", "green", "red")), #used for marker color
         color = ifelse(type == "new", "#108188",
                        ifelse(type == "growth", "green", "red")), #used for number color
         clustered = ifelse(City %in% cities_clustered, T, F), #create column indicating whether a row (City) is clustered by addAwesomeMarker
         Moto = f2si2(Moto, digits = 4),         
         Car = f2si2(Car, digits = 4),
         car_wow = ifelse(is.na(car_wow) == F, 
                          paste0(ifelse(car_wow > 0, '+', ''), car_wow, "%"), 
                          '100%'),
         moto_wow = ifelse(is.na(moto_wow) == F, 
                           paste0(ifelse(moto_wow > 0, '+', ''), moto_wow, "%"), 
                           '100%')) 

for (i in 1:nrow(data)) {
  data$label[i] <- htmltools::HTML(paste0('<font style = "font-size: 350%;">', data$City[[i]], '</font>',
                                          '<br/>', 
                                          '<font style = "font-size: 350%;">', data$Car[[i]],
                                          '<br/>',
                                          ' (',
                                          '<font color = \"', data$color[[i]], '\">', 
                                          data$car_wow[[i]],
                                          "</font>", ')', '</font>'))
}

data_non_clustered <- data[data$clustered == F, ] #contains data of city that are not clustered by addAwesomeMarkers


# Function to generate indo trip map -----------------------------------------------

car_trip_map <- function(data_full, data_non_clustered) {
  
#creates icon with variable color (specified above)
icons <- awesomeIcons(
  icon = 'ion-model-s',
  iconColor = 'black',
  library = 'ion',
  markerColor = data_full$marker_color
  )

car_clustered <<- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>% #adds dark skin to the map
  addProviderTiles("Stamen.TonerBackground",
                   options = providerTileOptions(opacity = 0.4)) %>% 
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

car_show_all_label <<- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>% #adds dark skin to the map
  addProviderTiles("Stamen.TonerBackground",
                   options = providerTileOptions(opacity = 0.4)) %>% 
  addRectangles(
    lng1= 105.043262, lat1=-5.465551, 
    lng2= 115.846386, lat2=-9.420447, 
    fillColor = "transparent",
    color = "#FFFFFF", weight = 2, opacity = 0.5
  ) %>%
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

#car_clustered
#car_show_all_label

}



car_trip_map(data, data_non_clustered)



# Motor trip map ----------------------------------------------------------

# ------------------------------------- -----------------------------------

data <- read_csv(file)

data <- data %>%
  mutate(type = ifelse(is.na(moto_wow) == TRUE, "new", 
                       ifelse(moto_wow >= 0, "growth", "decline")), #type is currently not used
         marker_color = ifelse(type == "new", "blue",
                               ifelse(type == "growth", "green", "red")), #used for marker color
         color = ifelse(type == "new", "#108188",
                        ifelse(type == "growth", "green", "red")), #used for number color
         clustered = ifelse(City %in% cities_clustered, T, F), #create column indicating whether a row (City) is clustered by addAwesomeMarker
         Moto = f2si2(Moto, digits = 4),         
         Car = f2si2(Car, digits = 4),
         car_wow = ifelse(is.na(car_wow) == F, 
                          paste0(ifelse(car_wow > 0, '+', ''), car_wow, "%"), 
                          '100%'),
         moto_wow = ifelse(is.na(moto_wow) == F, 
                           paste0(ifelse(moto_wow > 0, '+', ''), moto_wow, "%"), 
                           '100%')) %>%
  filter(Moto != 0) 


for (i in 1:nrow(data)) {
  data$label[i] <- htmltools::HTML(paste0('<font style = "font-size: 300%;">', data$City[[i]], '</font>',
                                          '<br/>', 
                                          '<font style = "font-size: 300%;">', data$Moto[[i]],
                                          '<br/>',
                                          ' (',
                                          '<font color = \"', data$color[[i]], '\">', 
                                          data$moto_wow[[i]],
                                          "</font>", ')', '</font>'))
}

data_non_clustered <- data[data$clustered == F, ]

moto_trip_map <- function(data_full, data_non_clustered) {
  
  #creates icon with variable color (specified above)
  icons <- awesomeIcons(
    icon = 'fa-motorcycle',
    iconColor = 'black',
    library = 'fa',
    markerColor = data_full$marker_color
  )
  
  moto_show_all_label <<- leaflet() %>%
    addProviderTiles("CartoDB.DarkMatterNoLabels") %>% #adds dark skin to the map
    addProviderTiles("Stamen.TonerBackground",
                     options = providerTileOptions(opacity = 0.4)) %>% 
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
  
  moto_clustered <<- leaflet() %>%
    addProviderTiles("CartoDB.DarkMatterNoLabels") %>% #adds dark skin to the map
    addProviderTiles("Stamen.TonerBackground",
                     options = providerTileOptions(opacity = 0.4)) %>% 
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
  
  
  
  #moto_clustered
  #moto_show_all_label
  
}

moto_trip_map(data, data_non_clustered)

car_show_all_label
car_clustered
moto_show_all_label
