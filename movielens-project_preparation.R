dim(edx)
str(edx)
# Q2
# How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% summarize(count=n())

# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% summarize(count=n())

# reference solution

edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()

# Q3
# How many different movies are in the edx dataset?

edx %>% select(movieId) %>% distinct(movieId) %>% tally()

# reference solution

n_distinct(edx$movieId)

# Q4
# How many different users are in the edx dataset?

n_distinct(edx$userId)

# Q5
# How many movie ratings are in each of the following genres in the edx dataset?

edx %>% filter(grepl("Drama", genres)) %>% tally()
edx %>% filter(grepl("Comedy", genres)) %>% tally()
edx %>% filter(grepl("Thriller", genres)) %>% tally()
edx %>% filter(grepl("Romance", genres)) %>% tally()


# reference solution

# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Q6
# Which movie has the greatest number of ratings?

edx %>% group_by(movieId, title) %>% summarize(ratings_count = n()) %>% arrange(desc(ratings_count))

# Q7
# What are the five most given ratings in order from most to least?

edx %>% group_by(rating) %>% summarize(count = n()) %>% arrange(desc(count))

# reference solution

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
