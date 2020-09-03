##
## This is all my code to analyze the IMDB database
##

## Libraries I need
library(tidyverse)
library(ggplot2)

## Read in the data
imdb.test <- read_csv("IMDBTest.csv")
imdb <- read_csv("IMDBTrain.csv")

##
## Exploratory Data Analysis
##

## Scatterplot of Budget vs. Score
ggplot(data = imdb, mapping = aes(x = budget, y = imdb_score)) +
  geom_point()
imdb %>% filter(budget > 100000000, country == "USA") %>% 
  select(movie_title)

## Missing Values (what do you do?)
# Delete, Fill (Mean, Median, Regression), or Ignore
## Scatterplot of groww vs imdb
ggplot(data = imdb, mapping = aes(x = gross, y = imdb_score)) +
  geom_point()
with(imdb, cor(gross, imdb_score, use = "complete.obs"))


## Tasks
## Convert data to US dollars

## Analyze my Columns
my.col <- imdb[, 13:16]
names(my.col)
head(my.col)

## Note that there are quite a few outliers here. 
# ggplot(data = imdb, mapping = aes(y = num_voted_users))+
#   geom_boxplot()
# ggplot(data = imdb, mapping = aes(y = cast_total_facebook_likes))+
#   geom_boxplot()
ggplot(data = imdb, mapping = aes(y = facenumber_in_poster))+
  geom_boxplot()

## Note both are skewed right
# ggplot(data = imdb, mapping = aes(x = num_voted_users))+
#   geom_histogram()
# ggplot(data = imdb, mapping = aes(x = cast_total_facebook_likes))+
#   geom_histogram()
# ggplot(data = imdb, mapping = aes(x = facenumber_in_poster))+
#   geom_histogram()

## How many Na's are there in all of the columns
is.na(my.col) %>% apply(2, sum)
## Not many Na's... probably because the first two columns are counts
## so it makes sense that if nobody liked them there is just a 0
## The second two columns do have NA's but there are relatively few 
## compared to the dataset. The first NA's column is a categorical 
## variable so there is no adjusting for that. The second is a number
## for the number of people on the movie poster. There are 10 NA's which
## likely means there is no movie poster. One possible solution could be
## to fill those in with 0's because there technically were no faces on
## the poster.

head(imdb[, 17:28])
names(imdb[, 17:28])

imdb[which(is.na(my.col[, 3:4])),]$country

imdb %>% filter(is.na(facenumber_in_poster)) %>% 
  select(movie_title)
