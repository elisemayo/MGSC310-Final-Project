library(tidyverse)
library(devtools)
library(sentimentr)
library(geniusr)
library(forcats)
library(ggridges)
library(glmnet)
library(glmnetUtils)
library(randomForest)
library(randomForestExplainer)
library(ElemStatLearn)
library(dplyr)
library(tidytext)
library(purrr)
library(rtweet)
library(gganimate)
library(magrittr)
# ------------------------------------------------------------------

# data
spotify_data <- read.csv("song_data.csv")
spotify_data2 <- read.csv("song_info.csv")

# ------------------------------------------------------------------

# data cleaning 
spotify_data <- full_join(spotify_data %>% group_by(song_name) %>% mutate(id = row_number()),
          spotify_data2 %>% group_by(song_name) %>% mutate(id = row_number()), 
          by = c("song_name", "id"))

top_hits <- spotify_data %>% filter(song_popularity > 80) %>% 
  group_by(playlist)
  
top_hits_levels <- fct_lump(top_hits$playlist, n = 9)

top_hits_df <- data.frame(
  top_hits,
  levels = top_hits_levels)

# --------------------------------------------------------------------

# summary statistics
summary(spotify_data)

# --------------------------------------------------------------------

# test and train data sets 
train_idx <- sample(1:nrow(spotify_data), size = 0.75 * nrow(spotify_data))
spotify_train <- spotify_data %>% slice(train_idx)
spotify_test <- spotify_data %>% slice(-train_idx)

# predictive models 
# linear regression 1 
mod1 <- lm(song_popularity ~ instrumentalness + energy + loudness,
           data = spotify_data)
summary(mod1)

preds_mod1 <- data.frame(
  preds = predict(mod1),
  spotify_data)

# linear regression 2
mod2 <- lm(energy ~ acousticness + loudness + instrumentalness,
           data = spotify_data)
summary(mod2)

preds_mod2 <- data.frame(
  preds = predict(mod2),
  spotify_data)
head(preds_mod2)

# lasso 1
lasso_fit1 <- cv.glmnet(song_popularity ~ instrumentalness + energy + loudness,
                       alpha = 1, 
                       nfolds = 10,
                       data = spotify_train)
plot(lasso_fit1)

lasso_train1 <- data.frame(
  preds_lasso = predict(lasso_fit1, newdata = spotify_train, s = 
                          lasso_fit1$lambda.min)
) %>% rename(lasso = 1)

lasso_test1<- data.frame(
  preds_lasso = predict(lasso_fit1, newdata = spotify_test, s = 
                          lasso_fit1$lambda.min)
) %>% rename(lasso = 1)

caret::RMSE(lasso_train1$lasso, spotify_train$song_popularity)
caret::RMSE(lasso_test1$lasso, spotify_test$song_popularity)
caret::R2(lasso_train1$lasso, spotify_train$song_popularity)
caret::R2(lasso_test1$lasso, spotify_test$song_popularity)
 
# lasso 2
lasso_fit2 <- cv.glmnet(song_popularity ~ acousticness + loudness + instrumentalness,
                        alpha = 1, 
                        nfolds = 10,
                        data = spotify_train)
plot(lasso_fit2)

lasso_train2 <- data.frame(
  preds_lasso = predict(lasso_fit2, newdata = spotify_train, s = 
                          lasso_fit2$lambda.min)
) %>% rename(lasso = 1)

lasso_test2 <- data.frame(
  preds_lasso = predict(lasso_fit2, newdata = spotify_test, s = 
                          lasso_fit2$lambda.min)
) %>% rename(lasso = 1)

caret::RMSE(lasso_train2$lasso, spotify_train$song_popularity)
caret::RMSE(lasso_test2$lasso, spotify_test$song_popularity)
caret::R2(lasso_train2$lasso, spotify_train$song_popularity)
caret::R2(lasso_test2$lasso, spotify_test$song_popularity)

# ridge regression 1
ridge_fit1 <- cv.glmnet(song_popularity ~ instrumentalness + energy + loudness,
                       data = spotify_data,
                       nfolds = 10,
                       alpha = 0)
summary(ridge_fit1)
ridge_fit1$lambda.min
ridge_fit1$lambda.1se
plot(ridge_fit1)

ridge_train1 <- data.frame(
  preds_ridge = predict(ridge_fit1, newdata = spotify_train, s = 
                          ridge_fit1$lambda.min)
) %>% rename(ridge = 1)

ridge_test1 <- data.frame(
  preds_ridge = predict(ridge_fit1, newdata = spotify_test, s = 
                          ridge_fit1$lambda.min)
) %>% rename(ridge = 1)

caret::RMSE(ridge_train1$ridge, spotify_train$song_popularity)
caret::RMSE(ridge_test1$ridge, spotify_test$song_popularity)
caret::R2(ridge_train1$ridge, spotify_train$song_popularity)
caret::R2(ridge_test1$ridge, spotify_test$song_popularity)

# ridge regression 2
ridge_fit2 <- cv.glmnet(song_popularity ~ acousticness + loudness + instrumentalness,
                        data = spotify_data,
                        nfolds = 10,
                        alpha = 0)
plot(ridge_fit2)
ridge_fit2$lambda.min
ridge_fit2$lambda.1se

ridge_train2 <- data.frame(
  preds_ridge = predict(ridge_fit2, newdata = spotify_train, s = 
                          ridge_fit2$lambda.min)
) %>% rename(ridge = 1)

ridge_test2 <- data.frame(
  preds_ridge = predict(ridge_fit2, newdata = spotify_test, s = 
                          ridge_fit2$lambda.min)
) %>% rename(ridge = 1)

caret::RMSE(ridge_train2$ridge, spotify_train$song_popularity)
caret::RMSE(ridge_test2$ridge, spotify_test$song_popularity)
caret::R2(ridge_train2$ridge, spotify_train$song_popularity)
caret::R2(ridge_test2$ridge, spotify_test$song_popularity)

# randomForest OK WE NEED TO FIGURE OUT MTRY???? and maybe do another random forest of the 
# other three variables
rf_fit1 <- randomForest(song_popularity ~ instrumentalness + energy + loudness,
                       data = spotify_data,
                       type = classification,
                       ntree = 1000,
                       mtry = 3,
                       importance = TRUE,
                       localImp = TRUE)
rf_fit

test_preds <- predict(rf_fit, spotify_test)
summary(test_preds)

oob_preds <- predict(rf_fit)
summary(oob_preds)

ib_preds <- predict(rf_fit, spotify_train)
summary(ib_preds)

plot_min_depth_distribution(rf_fit)
plot_predict_interaction(rf_fit1, spotify_train, "energy", "song_popularity")

rf_fit2 <- randomForest(song_popularity ~ instrumentalness + acousticness + loudness,
                       data = spotify_data,
                       type = classification,
                       ntree = 1000,
                       mtry = 3,
                       importance = TRUE,
                       localImp = TRUE)
plot_predict_interaction(rf_fit2, spotify_train, "acousticness", "song_popularity")


test_preds <- predict(rf_fit2, spotify_test)
summary(test_preds)

oob_preds <- predict(rf_fit2)
summary(oob_preds)

ib_preds <- predict(rf_fit2, spotify_train)
summary(ib_preds)

# --------------------------------------------------------------------

# graphs
ggplot(spotify_data, aes(x = energy, y = loudness)) + 
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
  geom_density_ridges(fill = "darkred")
# graph 5 
