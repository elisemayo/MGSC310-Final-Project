library(tidyverse)
library(devtools)
library(sentimentr)
library(geniusr)
library(forcats)
library(dplyr)
library(tidytext)
library(purrr)
library(readr)

spotify_data <- read_csv("song_data.csv")
spotify_data2 <- read_csv("song_info.csv")

spotify_data <- full_join(spotify_data %>% group_by(song_name) %>% mutate(id = row_number()),
                          spotify_data2 %>% group_by(song_name) %>% mutate(id = row_number()), 
                          by = c("song_name", "id"))

spotify_rows <- nrow(spotify_data)
subset_idx <- sample(1:spotify_rows, size = 0.05*spotify_rows)
spotify_subset <- spotify_data %>% slice(subset_idx)

Sys.setenv(GENIUS_API_TOKEN = "4_BzjOh_yAd-EUJnRtmIo14sgluBiT20ERey8PpJ3OlWAcYvV3oolNZy9DUTnt-n")
bing <- get_sentiments("bing")

sentiment_scores <- rep(NA, nrow(spotify_subset))
rows <- nrow(spotify_subset)

for(i in 1:rows){
  song_search <- search_song(spotify_subset$song_name[i],10)
  
  song <- song_search %>% filter(artist_name == spotify_subset$artist_name[i])
  
  if(nrow(song)>0){
    lyrics <- map_df(song$song_lyrics_url,
                     get_lyrics_url)
    
    sentiment <- lyrics %>%
      unnest_tokens(word, line) %>%
      anti_join(stop_words) %>%
      inner_join(bing) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    sentiment <- data.frame(sentiment)
    sentiment <- sentiment %>% mutate(word = word,
                                      sentiment = sentiment,
                                      n = n,
                                      binary = ifelse(sentiment == "negative",0,1),
                                      N = ifelse(sentiment == "negative",-n,n))
    sentiment_total <- sum(sentiment$N)
    
    sentiment_scores[i] <- sentiment_total
  } else {
    NULL
  }
}

sentiment_scores <- data.frame(sentiment_scores)
spotify_subset$sentiment_scores <- NA
spotify_subset$sentiment_scores <- sentiment_scores$sentiment_scores

ggplot(spotify_subset, aes(x = sentiment_scores, y = song_popularity)) + 
  geom_point(color = "green") +
  xlim(-100,100)
