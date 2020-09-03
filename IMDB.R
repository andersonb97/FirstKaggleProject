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
