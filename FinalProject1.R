library(tidyverse)
library(devtools)
install.packages("sentimentr")
library(sentimentr)
install.packages("geniusr")
library(geniusr)
# ------------------------------------------------------------------

# data
spotify_data <- read.csv("song_data.csv")
spotify_data2 <- read.csv("song_info.csv")

# ------------------------------------------------------------------

# data cleaning 
spotify_data <- full_join(x = spotify_data,
                          y = spotify_data2,
                          by = "song_name")
# merging song data and song info

spotify_data <- spotify_data %>% distinct()
# getting rid of duplicates

spotify_data <- spotify_data %>% 
  mutate(top_playlist = fct_infreq(playlist))
table(spotify_data$playlist, spotify_data$top_playlist)
levels(spotify_data$top_playlist)
top_playlist <- spotify_data %>% group_by("All out 00s", "other")
# reordering factor levels by frequency to see top playlists

top_hits <- spotify_data %>% filter(song_popularity > 80)
# finding the most popular songs

# sentiment stuff
# scrape album lyrics
lyrics <- map_df(tracklist$song_lyrics_url, get_lyrics_url)

# counting negative / positive words
sentiment <- lyrics %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# --------------------------------------------------------------------

# summary statistics
summary(spotify_data)

# --------------------------------------------------------------------

# graphs
cormat <- cor(spotify_data %>% select_if(is.numeric) %>% drop_na()) %>% 
  round(3)
cormat

library(corrplot)
corrplot(cormat)
# graph 1

ggplot(spotify_data, aes(x = energy, y = loudness)) + 
  geom_point() +
  geom_smooth()
# graph 2

ggplot(spotify_data, aes(x = danceability, y = song_popularity)) + 
  geom_point() +
  geom_smooth()
# graph 3

ggplot(spotify_data, aes(x = danceability, y = song_popularity)) + 
  geom_point() + facet_wrap(~top_playlist) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
# graph 4 

ggplot(spotify_data, aes(x = energy, y = song_popularity, fill = top_playlist)) +
  geom_density_ridges()
# graph 5 

