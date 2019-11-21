library(tidyverse)
library(devtools)
library(sentimentr)
library(geniusr)
library(forcats)
library(dplyr)
library(tidytext)
library(purrr)

spotify_data <- read.csv("song_data.csv")
spotify_data2 <- read.csv("song_info.csv")

spotify_data <- full_join(spotify_data %>% group_by(song_name) %>% mutate(id = row_number()),
                          spotify_data2 %>% group_by(song_name) %>% mutate(id = row_number()), 
                          by = c("song_name", "id"))

Sys.setenv(GENIUS_API_TOKEN = "4_BzjOh_yAd-EUJnRtmIo14sgluBiT20ERey8PpJ3OlWAcYvV3oolNZy9DUTnt-n")
genius_token()

bing <- get_sentiments("bing")

genius_token(force = FALSE)
tracklist <- get_album_tracklist_search(artist_name = spotify_data$artist_name.y[2],
                                        album_name = spotify_data$album_names.y[2])

song_search <- search_song(spotify_data$song_name[45],
                    n_results = 10)
song_search <- data.frame(song_search)

song <- song_search %>% filter(artist_name == spotify_data$artist_name[45])

# scrape album lyrics
lyrics <- map_df(tracklist$song_lyrics_url, get_lyrics_url)
lyrics2 <- map_df(song$song_lyrics_url, get_lyrics_url)


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

sentiment2 <- lyrics2 %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

sentiment <- data.frame(sentiment)
sentiment <- sentiment %>% mutate(word = word,
                                  sentiment = sentiment,
                                  n = n,
                                  binary = ifelse(sentiment == "negative",0,1),
                                  N = ifelse(sentiment == "negative",-n,n))

sentiment_total <- sum(sentiment$N)
sentiment_total

sentiment2 <- data.frame(sentiment2)
sentiment2 <- sentiment2 %>% mutate(word = word,
                                  sentiment = sentiment,
                                  n = n,
                                  binary = ifelse(sentiment == "negative",0,1),
                                  N = ifelse(sentiment == "negative",-n,n))

sentiment_total2 <- sum(sentiment2$N)
sentiment_total2

#------------------------------------------------------------
sentiment_scores <- rep(NA, nrow(spotify_data))
rows <- nrow(spotify_data)

for(i in 2:rows){
  song_search <- search_song(spotify_data$song_name[i],10)
  song_search <- data.frame(song_search)
  
  song <- song_search %>% filter(artist_name == spotify_data$artist_name[i])
  
  lyrics <- map_df(song$song_lyrics_url, get_lyrics_url)
  
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
}
