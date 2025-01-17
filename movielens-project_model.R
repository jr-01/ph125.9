# Note: 
# this code assumes the provided R code was run already, and "edx" and "final_holdout_test" dataframe objects exist

# data normalization: in order to run PCA analysis, we need to normalize the rating data
edx_scaled <- edx %>% select(!title) %>% select(!genres) %>% mutate(normalized_rating = scale(rating)) 
pca <- edx_scaled %>% prcomp(normalized_rating)
summary(pca)
