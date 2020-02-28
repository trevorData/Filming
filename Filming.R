setwd("~/Projects/Filming")

library(RSocrata)
library(dplyr)
library(ggmap)
library(stringr)
library(grid)
library(ggmapstyles)

# Read in data
url <- 'https://data.cityofchicago.org/resource/c2az-nhru.json'
raw.df <- read.socrata(url)

# Format data
raw.df$longitude <- raw.df$longitude %>% as.numeric
raw.df$latitude  <- raw.df$latitude %>% as.numeric

df <- raw.df[!is.na(raw.df$comments) & !is.na(raw.df$longitude) & !is.na(raw.df$latitude), ] %>%
          select(c("comments", "longitude", "latitude"))

# Load Map Backgrounds from Google
register_google(Sys.getenv('gmaps_key'))

mapNum <- 253319

mapImage <- get_snazzymap(center = c(lon = -87.57, lat = 41.90),
                          mapRef = mapNum,
                          zoom = 11)

zoomImage <- get_snazzymap(center = c(lon = -87.63, lat = 41.881),
                           mapRef = mapNum,
                           zoom = 15)

# Set plot styles
inset_themes <- theme(axis.title = element_blank(),
                axis.ticks = element_blank(), 
                axis.text = element_blank(),
                plot.background = element_rect(size = 2, color = 'gray40'),
                plot.margin = unit(c(3,2,1,1), "points"))

main_themes <- theme(axis.title = element_blank(),
                     axis.ticks = element_blank(), 
                     axis.text = element_blank())

ann_x   <- -87.511
title_y <- 42.04

# Plot Maps
keyword <- 'chase' %>% regex(ignore_case = T)

loop_inset <- 
  (ggmap(zoomImage) +
  geom_point(data = df[df$comments %>% str_detect(keyword), ], 
             aes(x=longitude, y=latitude), 
             color = 'mediumpurple4', 
             size = 3.5) + 
  inset_themes) %>% 
    ggplotGrob %>% 
    inset(xmin = -87.52, xmax = -87.35, 
          ymin = 41.744, ymax = 41.87)

ggmap(mapImage) +
  geom_point(data = df[df$comments %>% str_detect(keyword), ], 
             aes(x=longitude, y=latitude), 
             color = 'mediumpurple4',
             size=2) + 
  main_themes +
  loop_inset +
  annotate('text', label = 'Chicago Bike Rentals',  
           family = 'serif', fontface='bold', size = 15, color = 'gray40', 
           x = ann_x, y = title_y)

colors <- 
  c('dodgerblue4',
    'firebrick',
    'darkgreen',
    'mediumpurple4')

keywords <-
  c('Empire', 'Exorcist','Shameless', 'Batwoman', 'Violent', 'Gotham',  'Church', 'Music Video', 
    'Museum', 'Documentary', 'Chase', 'Drone', 'Bar', 'Hospital', 'Hotel', 'Bridge')

for (i in 1:(keywords %>% length)){
  print(keywords[i])
  print(colors[i%%4 + 1])
}

