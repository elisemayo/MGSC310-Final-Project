library(tidyverse)

setwd("C:/Users/elise may/Documents/MGSC310")
spotify_data <- read.csv("Final Project/song_data.csv")
spotify_data2 <- read.csv("Final Project/song_info.csv")

spotify_data <- full_join(x = spotify_data,
                            y = spotify_data2,
                            by = "song_name")
spotify_data <- spotify_data %>% distinct()

#factor popularity into 10 or so levels

summary(spotify_data)

cormat <- cor(spotify_data %>% select_if(is.numeric) %>% drop_na()) %>% 
  round(3)
cormat

library(corrplot)
corrplot(cormat)

ggplot(spotify_data, aes(x = energy, y = loudness)) + 
  geom_point() +
  geom_smooth()

ggplot(spotify_data, aes(x = danceability, y = song_popularity)) + 
  geom_point() +
  geom_smooth()

top_hits <- spotify_data %>% filter(song_popularity > 80)
#facet wrap by playlist if possible for small enough top sample

