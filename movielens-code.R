# PH125.9 capstone 
# movielens project
# R code file, submitted by Johannes Resch (jr@xor.at)

library(dslabs)
library(tidyverse)
library(caret)
library(plyr, include.only = c("round_any"))
library(dplyr)
library(recosystem)


# function to calculate RMSE (root mean square error) for predicted vs. true rating.
# This function was reused from prior course exercise material of PH125.8 course module
RMSE <- function(true_quality, predicted_score){
  sqrt(mean((true_quality - predicted_score)^2))
}

# create train and test dataset from edx partition, 85/15 split
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# ensure all movies in trainset also exist in test set
# this snippet was taken from prior PH128.8 course exercise material
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# regularized movie + user effect model - only used to provide baseline RMSE
# based on prior PH128.8 course material, code refactored and improved (lambda values)

# function to calculate mu, b_i and b_u parameters for a dataset using a given lambda value
penalized_estimates_train <- function(dataset, l){
  mu <- mean(dataset$rating)
  b_i <- dataset %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- dataset %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  return(list("mu" = mu, "b_i" = b_i, "b_u" = b_u))
}

# function to predict rating for a dataset, based on provided mu, b_i and b_u values
penalized_estimates_predict <- function(dataset, b_i, b_u, mu) {
  predicted_ratings <- 
    dataset %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
}

# tuned lambda values for optimization
lambdas <- seq(4, 6, 0.05)

# based on train set data, calculate optimal lambda value
rmses <- sapply(lambdas, function(l) {
  params <- penalized_estimates_train(train_set, l)
  predicted_ratings <- penalized_estimates_predict(test_set, params$b_i, params$b_u, params$mu)
  return(RMSE(predicted_ratings, test_set$rating))
})
lambda <- lambdas[which.min(rmses)]

rmse_results <- data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses))

# calculate RMSE for holdout data set

# ensure common set of movieID 
final_set <- final_holdout_test %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#calculate parameters based on optimized lambda
params <- penalized_estimates_train(train_set, lambda)

#predict for final holdout set
y_hat_simple <- penalized_estimates_predict(final_set%>%select(-rating), params$b_i, params$b_u, params$mu)
# RMSE for regularized movie + user effect model (final holdout data)
RMSE(y_hat_simple, final_set$rating)



# Advanced matrix factorization based model, using "recosystem" package (https://github.com/yixuan/recosystem)
# define required datastructures for "recosystem" package

train_reco <- with(train_set, data_memory(user_index = userId, 
                                               item_index = movieId,
                                               rating = rating))
test_reco <- with(test_set, data_memory(user_index = userId, 
                                              item_index = movieId, 
                                              rating = rating))

# we do not include "rating" values for final holdout data structure on purpose (as this column is not supposed to be used during prediction)
final_holdout_reco <- with(final_holdout_test, data_memory(user_index = userId, 
                                                 item_index = movieId))
# instantiate model
set.seed(2025)
reco_system <- Reco()

# tune model based on "default" values provided on https://github.com/yixuan/recosystem
# ensure to use suitable number of CPU cores depending on CPU of system where the model is trained on
tuning <- reco_system$tune(train_reco, 
                           opts = list(dim = c(10, 20, 30),
                                lrate = c(0.1, 0.2),
                                nthread  = 12,
                                niter = 20))

# function for training model based on n iterations
reco_train <- function(n) {
  reco_system$train(train_reco, opts = c(tuning$min, nthread = 12, niter = n))
}
# find optimal iterations for model training based on test set RMSE
mf_iterations <- seq(5, 40, 1)
rmses_mf <- sapply(mf_iterations, function(n) {
  reco_train(n)
  y_hat_mf <-  reco_system$predict(test_reco, out_memory())
  return(RMSE(y_hat_mf, test_set$rating))
})
MF_best_iter <- mf_iterations[which.min(rmses_mf)]

# use model with the determined best iteration value
reco_train(MF_best_iter)
# predict on final holdout set 
y_hat_mf_final <- reco_system$predict(final_holdout_reco, out_memory())
RMSE(y_hat_mf_final, final_holdout_test$rating)
