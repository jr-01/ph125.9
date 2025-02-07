# PH125.9 CYO project - Johannes Resch
# code to download dataset

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(magick)) install.packages('magick')
library(magick)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(lsr)) install.packages('lsr')
library(lsr)

### download of chosen raw dataset

# US real estate dataset:
# downloaded from https://www.kaggle.com/datasets/ahmedshahriarsakib/usa-real-estate-dataset
# included in github repo: # https://github.com/jr-01/ph125.9/blob/main/project%20CYO/realtor-data.csv.zip
# raw download link: https://raw.githubusercontent.com/jr-01/ph125.9/refs/heads/main/project%20CYO/realtor-data.csv.zip

# download dataset from git repo

dataset_dl <- "realtor-data.csv.zip"
if(!file.exists(dataset_dl))
  download.file("https://raw.githubusercontent.com/jr-01/ph125.9/refs/heads/main/project%20CYO/realtor-data.csv.zip", dataset_dl)

# unzip and get the dataset CSV 

dataset_file <- "realtor-data.csv"
if(!file.exists(dataset_file))
  unzip(dataset_dl, dataset_file)

# create dataframe from CSV
realtor_df_orig <- read.csv(dataset_file)

### data cleaning and pre-filtering
# check structure of data columnes

str(realtor_df_orig)

# find possible values for status
realtor_df_orig %>% select(status) %>% unique()

# find format for US state names used in data
realtor_df_orig %>% select(state) %>% unique() 
# sanity check for number of US states in report
realtor_df_orig %>% select(state) %>% unique() %>% count()

# check format of "prev_sold_date"
realtor_df_orig %>% select(prev_sold_date) %>% unique() 


# check how many entries have "prev_sold_date" set
realtor_df_orig %>% filter(grepl("^$", prev_sold_date)) %>% nrow() / nrow(realtor_df_orig)

# drop "street" column, and filter out overseas states/terretories
realtor_df_mod <- realtor_df_orig %>% select(!street) %>% select(!prev_sold_date) %>% 
  filter(!grepl("Puerto Rico|Virgin Islands|Guam|Hawaii|^$", state)) %>% filter(!is.na(price)) %>%
  filter(!is.na(bed)) %>% filter(bed > 0) %>% filter(!is.na(bath)) %>% filter(bath > 0) %>%
  filter(!grepl("ready_to_build", status))

# check results
realtor_df_mod %>% select(state) %>% unique() 
realtor_df_mod %>% select(state) %>% unique() %>% count()
str(realtor_df_mod)



### exploratory data analysis

# distribution of records per state

realtor_df_mod %>% group_by(state) %>% summarize(count=n()) %>% arrange(desc(count))

# barplot showing distribution
realtor_df_mod %>% group_by(state) %>% summarize(count=n()) %>% arrange(desc(count)) %>% 
  ggplot(aes(x=reorder(state, -count), y=count)) + geom_col()  + xlab("states") + theme(axis.text.x = element_text(angle = 90))

# evaluate the distribution of rows per zip_code# evaluate the distribution of rows per zip_code
zip_applicable <- realtor_df_mod %>% group_by(zip_code) %>% summarize(count=n()) %>% arrange(desc(count)) %>% filter(count >= 50) %>% nrow()

realtor_df_mod %>% group_by(zip_code) %>% summarize(count=n()) %>% arrange(desc(count)) %>% pull(count) %>% hist(main=NULL)

zip_applicable <- realtor_df_mod %>% group_by(zip_code) %>% summarize(count=n()) %>% arrange(desc(count)) %>% filter(count >= 50) %>% pull(zip_code)



# filter for ZIP codes which have at least 50 records each and drop NA values

realtor_df_mod4 <- realtor_df_mod %>% filter(zip_code %in% zip_applicable) %>% filter(!is.na(zip_code))
summary(realtor_df_mod4)

# filter N/A for response and all planned predictor columns
realtor_df_mod2 <- realtor_df_mod %>% filter(!is.na(acre_lot)) %>% filter(!is.na(zip_code)) %>% filter(!is.na(house_size))

# minimal NA filtering, only for response, and 2 main predictors ("bed", "zip_code")
realtor_df_mod3 <- realtor_df_mod %>% filter(!is.na(zip_code)) 


### check data quality / number of "N/A"

summary(realtor_df_mod)
summary(realtor_df_mod2)
summary(realtor_df_mod3)

# minimal filtering, resulting set of rows and relative decrease
nrow(realtor_df_mod3)
1-(nrow(realtor_df_mod3)/nrow(realtor_df_mod))

# full N/A filtering, resulting set of rows and relative decrease
nrow(realtor_df_mod2)
1-(nrow(realtor_df_mod2)/nrow(realtor_df_mod))

# dataset with limit to zipcodes with at least 50 transactions each
nrow(realtor_df_mod4)
1-(nrow(realtor_df_mod4)/nrow(realtor_df_mod))

realtor_df_mod5 <- realtor_df_mod4 %>% filter(!is.na(house_size))
nrow(realtor_df_mod5)

# distribution of prices per state
realtor_df_mod5 %>% select(price, state) %>%
  ggplot(aes(x=state, y=price)) + geom_boxplot() + scale_y_log10(labels = scales::comma) + theme(axis.text.x = element_text(angle = 90))
         
# prices per zip_code
realtor_df_mod5 %>% select(price, zip_code) %>%
  ggplot(aes(x=zip_code, y=price)) + geom_point() + scale_y_log10(labels = scales::comma)

# number of rows with price < 5000

realtor_df_mod5 %>% filter(price < 5000) %>% nrow()

# check highest number of bathrooms 
realtor_df_mod5 %>% group_by(bath) %>% summarize(count=n()) %>% arrange(desc(bath)) 

# check highest number of bedrooms 
realtor_df_mod5 %>% group_by(bed) %>% summarize(count=n()) %>% arrange(desc(bed)) 

realtor_df_mod5 %>% group_by(bath) %>% summarize(count=n()) %>% arrange((desc(count))) %>% 
  ggplot(aes(x=bath, y= count)) + geom_col() + xlim(1,20) + xlab("# of bathrooms") + theme(axis.text.x = element_text(angle = 90))

realtor_df_mod5 %>% group_by(bed) %>% summarize(count=n()) %>% arrange((desc(count))) %>% 
  ggplot(aes(x=bed, y= count)) + geom_col() + xlim(1,20) + xlab("# of bedrooms") + theme(axis.text.x = element_text(angle = 90))

# filter data based on threshold 10 bedrooms and 8 bathrooms

realtor_df_mod6 <- realtor_df_mod5 %>% filter(bed <= 10) %>% filter(bath <= 8)
nrow(realtor_df_mod5)

# resulting distribution of bed and bath

realtor_df_mod6 %>% group_by(bath) %>% summarize(count=n()) %>% arrange((desc(count))) %>% 
  ggplot(aes(x=bath, y= count)) + geom_col() + xlim(1,max(realtor_df_mod6$bath)) + xlab("# of bathrooms") + 
  theme(axis.text.x = element_text(angle = 90))

realtor_df_mod6 %>% group_by(bed) %>% summarize(count=n()) %>% arrange((desc(count))) %>% 
  ggplot(aes(x=bed, y= count)) + geom_col() + xlim(1,max(realtor_df_mod6$bed)) + xlab("# of bedrooms") + 
  theme(axis.text.x = element_text(angle = 90))
 
# find duplicate rows and create new df without duplicates
duplicates <- realtor_df_mod6 %>% group_by_all() %>% filter(n() > 1) %>% ungroup()
nrow(duplicates)
realtor_df_mod7 <- realtor_df_mod6 %>% distinct()
nrow(realtor_df_mod7)
summary(realtor_df_mod7)

# analyse correlations

# price vs. # bedrooms

realtor_df_mod6 %>% summarize(cor(price, bed))
realtor_df_mod6 %>% summarize(cor(price, bath))
realtor_df_mod6 %>% summarize(cor(price, house_size))             
realtor_df_mod7 %>% summarize(cor(price, zip_code))    

# to manage memory/compute need, for prediction we limit to a single state
realtor_df_mod8 <- realtor_df_mod7 %>% filter(!is.na(acre_lot)) %>% filter(!is.na(brokered_by)) %>% filter(state == "California")


# create correlation matrix (from https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi)
library(lsr)

# function to get chi square p value and Cramers V
f = function(x,y) {
  tbl = realtor_df_mod8 %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 4)
  cramV = round(cramersV(tbl), 4) 
  data.frame(x, y, chisq_pval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(names(realtor_df_mod8)), 2)), stringsAsFactors = F)

# apply function to each variable combination
df_res = map2_df(df_comb$X1, df_comb$X2, f)

# plot results
df_res %>%
  ggplot(aes(x,y,fill=cramV))+
  geom_tile(color="black")+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) + 
  coord_fixed() + theme_classic()

### building models

# implement test/training split

test_index <- createDataPartition(y = realtor_df_mod8$price, times = 1, p = 0.2, list = FALSE)
train_set <- realtor_df_mod8[-test_index,]
test_set <- realtor_df_mod8[test_index,]


# implement random forest model

#train_rf <- randomForest(price ~ bed + bath + zip_code + house_size, data=train_set)
#y_hat_rf <- predict(train_rf, test_set)
#confusionMatrix(y_hat_rf, test_set$price)
#train_rf

