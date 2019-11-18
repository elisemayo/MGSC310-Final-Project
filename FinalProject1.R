library(tidyverse)
library(devtools)
install.packages("sentimentr")
library(sentimentr)
install.packages("geniusr")
library(geniusr)
library(forcats)
library(ggridges)
# ------------------------------------------------------------------

# data
spotify_data <- read.csv("song_data.csv")
spotify_data2 <- read.csv("song_info.csv")

# ------------------------------------------------------------------

# data cleaning 
spotify_data <- full_join(spotify_data %>% group_by(song_name) %>% mutate(id = row_number()),
          spotify_data2 %>% group_by(song_name) %>% mutate(id = row_number()), 
          by = c("song_name", "id"))

# merging song data and song info
# getting rid of duplicates
spotify_data <- spotify_data %>% group_by(title, artist_name) %>% filter(n()>1)

top_hits <- spotify_data %>% filter(song_popularity > 80) %>% 
  group_by(playlist)
  
top_hits_levels <- fct_lump(top_hits$playlist, n = 9)

top_hits_df <- data.frame(
  top_hits,
  levels = top_hits_levels
)
# finding the most popular songs
#factor(top_hits$playlist)
# reordering factor levels by frequency to see top playlists

# sentiment stuff

Sys.setenv(GENIUS_API_TOKEN = "4_BzjOh_yAd-EUJnRtmIo14sgluBiT20ERey8PpJ3OlWAcYvV3oolNZy9DUTnt-n")
genius_token()

help("geniusr")

rows <- nrow(spotify_data)

bing <- get_sentiments("bing")
for(i in 1:rows){# scrape album tracklist
  tracklist <- get_album_tracklist_search(artist_name = spotify_data$artist_name[i],
                                        album_name = spotify_data$album_names[i])

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

sentiment_DF <- data.frame(spotify_data, sentiment = sentiment)
}
token <- create_token(access_token = "4_BzjOh_yAd-EUJnRtmIo14sgluBiT20ERey8PpJ3OlWAcYvV3oolNZy9DUTnt-n")
tracklist <- get_album_tracklist_search(artist_name = "Green Day",
                                        album_name = "Greatest Hits: God's Favorite Band",
                                        access_token)
# scrape album lyrics
#Install genius r from github page and not from cran we need to reinstall
#Get API Key and register with genius (can do this if you have a twitter)--> Edit system environment variable(hard coded information, tells you about this instance of r
#store within this the API key--> genius token, set env var in r)
#sys.setenv(Genius_API_Token = "put unique key here")
#sys.getenv()--> Make sure the genius api token is there
#genius_token("")
#Search for songs to get id--> put access_token = genius_token() in each function 
#Get_lyrics_id(search_results$id[1], token code)
#lyrics <- map_df(tracklist$song_lyrics_url, get_lyrics_url)
# scrape album lyrics
#lyrics <- map_df(tracklist$song_lyrics_url, get_lyrics_url)

# counting negative / positive words
#sentiment <- lyrics %>%
 # unnest_tokens(word, line) %>%
  # remove stop words
  #anti_join(stop_words) %>%
  # join afinn score
#  inner_join(bing) %>%
  # count negative / positive words
 # count(word, sentiment, sort = TRUE) %>%
#  ungroup()

# --------------------------------------------------------------------

# summary statistics
summary(spotify_data)

# --------------------------------------------------------------------

# test and train data sets 
train_idx <- sample(1:nrow(spotify_data), size = 0.75 * nrow(spotify_data))
spotify_train <- spotify_data %>% slice(train_idx)
spotify_test <- spotify_data %>% slice(-train_idx)

# at least 3 predictive models so we are doing 5 (bc we want an A)
# linear regression
mod1 <- lm(song_popularity ~ .,
           data = spotify_data)
summary(mod1)

mod2 <- lm(song_popularity ~ instrumentalness + energy + loudne,
           data = spotify_data)
summary(mod2)

mod3 <- lm(energy ~ acousticness + loudness + instrumentalness,
           data = spotify_data)
summary(mod3)

# lasso
lasso_fit <- cv.glmnet(song_popularity ~ .,
                       alpha = 1, 
                       nfolds = 10,
                       data = spotify_train)
plot(lasso_fit)

# ridge regression
ridge_fit <- cv.glmnet(song_popularity ~ . ,
                       data = spotify_data,
                       nfolds = 10,
                       alpha = 0)
ridge_fit$lambda.min
ridge_fit$lambda.1se

# randomForest
rf_fit <- randomForest(song_popularity ~ .,
                       data = spotify_data,
                       type = classification,
                       ntree = 1000,
                       mtry = 3,
                       importance = TRUE,
                       localImp = TRUE)
rf_fit

# LOOCV
preds_LOOCV <- rep(NA, nrow(spotify_train))
num_rows <- nrow(spotify_train)
LOOCV_mods <- list()
for(i in 1:num_rows){
  logit_mod <- glm(song_popularity ~ .,
                   data = spotify_train %>% slice(-i), family = binomial)
  preds_LOOCV[i] <- predict(logit_mod, spotify_train %>% slice(i),
                            type = "response")
}

# --------------------------------------------------------------------

# graphs
cormat <- cor(spotify_data %>% select_if(is.numeric) %>% drop_na()) %>% 
  round(3)
cormat

library(corrplot)
corrplot(cormat)
# graph 1

p <- ggplot(spotify_data, aes(x = energy, y = loudness)) + 
  geom_point() +
  geom_smooth() 

p +  transition_reveal(energy)
# graph 2

ggplot(spotify_data, aes(x = danceability, y = song_popularity)) + 
  geom_point() +
  geom_smooth()
# graph 3

ggplot(top_hits_df, aes(x = danceability, y = song_popularity)) + 
  geom_point() + facet_wrap(~levels) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
# graph 4 

ggplot(top_hits_df, aes(x = energy, y = levels, fill = song_popularity)) +
  geom_density_ridges()
# graph 5 

# to do
# gg animate?
# sentiment analysis
