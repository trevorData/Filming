library(dplyr)
library(tm)
library(ggmap)
library(stringr)

setwd("~/Projects/Filming")

raw.df <- read.csv("Filming_Permits.csv")

# Clean the text
corpus <- VCorpus(VectorSource(raw.df$COMMENTS))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stripWhitespace)

raw.dtm <- corpus %>% DocumentTermMatrix

# Filter non-frequent words
dtm <- raw.dtm %>% removeSparseTerms(.9999)

# Create dataframe with word count columns
dataset <- dtm %>% as.matrix %>% as.data.frame
dataset$LAT <- raw.df$LATITUDE
dataset$LON <- raw.df$LONGITUDE

dataset <- dataset[!(dataset$LAT %>% is.na),]

colSums(dataset) %>% View

# Load Map Background from Google
register_google(Sys.getenv('gmaps_key'))

mapImage <- get_map(location = c(lon = -87.68, lat = 41.9),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 11)

zoomImage <- get_map(location = c(lon = -87.625, lat = 41.88),
                     color = "bw",
                     maptype = "toner",
                     source = 'stamen',
                     zoom = 14)

zoomImage <- get_map(location = c(lon = -87.625, lat = 41.88),
                              color = "bw",
                              maptype = "terrain-lines",
                              source = 'stamen',
                              zoom = 11)

ggmap(zoomImage) +
  geom_point(data = raw.df[raw.df$COMMENTS %>% str_detect(regex('museum', ignore_case = T)),], 
             aes(x=LONGITUDE, y=LATITUDE), 
             color = 'blue')

get

"empire
restaurant
office
bridge
bar
hospital
exorcist
shameless
documentary
church
chase
violent
hotel
music video
drone
advertisement
basketball
museum
theater
cops
pedestrians
batwoman
gotham
beach
"








