library(leaflet)
library(dplyr)
library(ggmap)
library(tidyr)
library(lubridate)
library(stringr)
library(ggflags)
library(devtools)
library(gganimate)
library(countrycode)
library(extrafont)
library(tidyverse)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
pump = pumpkins%>%
  filter(!is.na(weight_lbs) == TRUE,!is.na(city) == TRUE,
         !is.na(place) == TRUE, !is.na(seed_mother) == TRUE, !is.na(pollinator_father) == TRUE,
         grepl("damage|\\.|\\`|\\^|xxx", city) == FALSE,
         grepl("EXH|DMG", place) == FALSE,
         state_prov != "Other")%>%
  select(-ott, -est_weight, -pct_chart, -variety)%>%
  separate(id, c("Year","type"), sep='-',remove = FALSE)%>%
  mutate(weight_lbs = as.numeric(gsub("\\,","",weight_lbs)),
         place = as.numeric(place),
         type = ifelse(grepl("F|G", type),"pumpkin",
                ifelse(grepl("S", type),"squash",
                ifelse(grepl("W", type),"watermelon",
                ifelse(grepl("L", type),"gourd","tomato")))),
         myPopup = paste(sep='<br/>',
                         paste('<b>Grower</b>:', grower_name),
                         paste('<b>Weight</b>:', weight_lbs,'lbs'),
                         paste('<b>Mother</b>:', seed_mother),
                         paste('<b>Father</b>:', pollinator_father)),
         Year = as.Date(Year, '%Y'))
pumpkin = subset(pump, grepl("F", id) == TRUE)
squash = subset(pump, grepl("S", id) == TRUE)
wat = subset(pump, grepl("W", id) == TRUE)
gourd = subset(pump, grepl("L", id) == TRUE)
tomato = subset(pump, grepl("T", id) == TRUE)

register_google(key = "AIzaSyAornhYTNeaWsF9W9CE0CJ7X5yGRx9AYwA")

puloc = geocode(pumpkin$city)
pumpkin <- cbind(pumpkin, puloc)
sqloc = geocode(squash$city)
squash <- cbind(squash, sqloc)
watloc = geocode(wat$city)
wat <- cbind(wat, watloc)
goloc = geocode(gourd$city)
gourd <- cbind(gourd, goloc)
toloc = geocode(tomato$city)
tomato <- cbind(tomato, toloc)

mypump = makeIcon(
  iconUrl = '/Users/celiawang/Downloads/pump.png',
  iconWidth = pumpkin$weight_lbs / 5,
  iconHeight = pumpkin$weight_lbs / 5)
mysquash = makeIcon(
  iconUrl = '/Users/celiawang/Downloads/squash.jpeg',
  iconWidth = squash$weight_lbs / 20,
  iconHeight = squash$weight_lbs / 20)
mywat = makeIcon(
  iconUrl = '/Users/celiawang/Downloads/wat.png',
  iconWidth = wat$weight_lbs /5,
  iconHeight = wat$weight_lbs /5)
mygourd = makeIcon(
  iconUrl = '/Users/celiawang/Downloads/gourd.png',
  iconWidth = gourd$weight_lbs / 10,
  iconHeight = gourd$weight_lbs / 10)
mytomato = makeIcon(
  iconUrl = '/Users/celiawang/Downloads/tomato.png',
  iconWidth = tomato$weight_lbs * 5,
  iconHeight = tomato$weight_lbs * 5)


leaflet() %>%
  addProviderTiles(providers$Stamen.Watercolor)%>%
  addMarkers(data=pumpkin,icon = mypump,group = "Pumpkin",
             clusterOptions = markerClusterOptions(),
             label = ~id,
             popup = ~myPopup)%>%
  addMarkers(data=squash,icon = mysquash,group = "Squash",
             clusterOptions = markerClusterOptions(),
             label = ~id,
             popup = ~myPopup)%>%
  addMarkers(data=wat,icon = mywat,group = "Watermelon",
             clusterOptions = markerClusterOptions(),
             label = ~id,
             popup = ~myPopup)%>%
  addMarkers(data=gourd,icon = mygourd,group = "Gourd",
             clusterOptions = markerClusterOptions(),
             label = ~id,
             popup = ~myPopup)%>%
  addMarkers(data=tomato,icon = mytomato,group = "Tomato",
             clusterOptions = markerClusterOptions(),
             label = ~id,
             popup = ~myPopup)%>%
  addLayersControl(overlayGroups = c('Pumpkin', 'Squash', 'Watermelon', 'Gourd','Tomato'),
                   position='bottomright')



newpump = pumpkin%>%
  mutate(country = ifelse(country == "Austria","au",
                   ifelse(country == "United States","us",
                   ifelse(country == "Germany","de",
                   ifelse(country == "Canada","ca",
                   ifelse(country == "Finland","fi",
                   ifelse(country == "Italy","it",
                   ifelse(country == "The Netherlands","nl","other"))))))))%>%
  filter(country != "other")%>%
  group_by(Year,country)%>%
  summarise(max_weight = max(weight_lbs))

my_theme <- theme(text =element_text(face = "bold"),
                  plot.background = element_rect(fill = "gray95"),
                  panel.background = element_rect(fill = "gray95"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text( size = 10, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text( size = 10),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.background = element_rect(fill = "gray95"),
                  legend.position = "top", 
                  axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

a = ggplot(newpump,aes(x = Year,y = max_weight,color = country))+
  geom_point(alpha = .6)+
  geom_flag(aes(country = country))+
  labs(title = "Max weight of product in each country from 2013 to 2012",
       subtitle = "Pumpkins",
       y = "Max Weight") +
  my_theme+
  transition_time(as.integer(Year)) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none")  
a
 

b = ggplot(newpump, aes(x=reorder(country, max_weight), y=max_weight,)) + 
  geom_col(fill = "orange3", width = .8)+
  geom_flag(y = -2, aes(country = country)) +
  coord_flip() + expand_limits(y = -2)  +
  labs(title = "Max weight of product in each country from 2013 to 2012",
       subtitle = paste("Pumpkins"),
       x = "Country",
       y = "Max Weight") +
  my_theme +
  transition_time(as.integer(Year)) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = "none")  
b


