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

# Load Map Background from Google
register_google(Sys.getenv('gmaps_key'))

inset_themes <- theme(axis.title = element_blank(),
                axis.ticks = element_blank(), 
                axis.text = element_blank(),
                plot.background = element_rect(size = 2, color = 'gray40'),
                plot.margin = unit(c(3,2,1,1), "points"))

main_themes <- theme(axis.title = element_blank(),
                     axis.ticks = element_blank(), 
                     axis.text = element_blank())

mapImage <- get_map(location = c(lon = -87.57, lat = 41.88),
                    #color = "bw",
                    maptype = "hybrid",
                    source = 'google',
                    darken = 1,
                    zoom = 11)

mapImag2 <- get_snazzymap(center = c(lon = -87.57, lat = 41.88),
                          mapRef = 21086,
                          zoom = 11)

ggmap(mapImag2)

zoomImage <- get_map(location = c(lon = -87.63, lat = 41.88),
                     color = "bw",
                     maptype = "toner-lines",
                     source = 'stamen',
                     zoom = 15)

zoomImag2 <- get_snazzymap(center = c(lon = -87.63, lat = 41.88),
                           mapRef = 21086,
                           zoom = 15)

keyword <- 'documentary' %>% regex(ignore_case = T)

loop_map <- 
  ggmap(zoomImag2) +
  geom_point(data = df[df$comments %>% str_detect(keyword), ], 
             aes(x=longitude, y=latitude), 
             color = 'firebrick', 
             size = 4) + 
  inset_themes

ggmap(mapImag2) +
  geom_point(data = df[df$comments %>% str_detect(keyword), ], 
             aes(x=longitude, y=latitude), 
             color = 'firebrick',
             size=2) + main_themes +
  loop_map %>% 
    ggplotGrob %>% 
    inset(xmin = -87.53, xmax = -87.36, 
          ymin = 41.725, ymax = 41.86)

"empire 
exorcist 
shameless 
batwoman 
gotham 
violent 
church 
music video 
museum
documentary 
chase 
basketball 
drone 
bar 
hospital 
hotel 
bridge 
"








