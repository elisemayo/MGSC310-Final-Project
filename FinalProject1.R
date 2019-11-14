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
spotify_data <- full_join(x = spotify_data,
                          y = spotify_data2,
                          by = "song_name")
# merging song data and song info

spotify_data <- spotify_data %>% distinct()
# getting rid of duplicates

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

ggplot(spotify_data, aes(x = energy, y = loudness)) + 
  geom_point() +
  geom_smooth()
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
