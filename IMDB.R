##
## This is all my code to analyze the IMDB database
##

## Libraries I need
library(tidyverse)
library(ggplot2)

## Read in the data
imdb.test <- read_csv("IMDBTest.csv")
imdb.train <- read_csv("IMDBTrain.csv")

## Merge the two datasets together so when I clean the 
## training dataset I also treat teh test set the same way
names(imdb.test)[names(imdb.test) == "ID"] <- "movie_title"
imdb <- bind_rows(train = imdb.train, test = imdb.test, .id = "Set")

##
## Exploratory Data Analysis
##

## Scatterplot of Budget vs. Score
ggplot(data = imdb, mapping = aes(x = budget, y = imdb_score)) +
  geom_point()
imdb.train %>% filter(budget > 100000000, country == "USA") %>% 
  select(movie_title)

## Missing Values (what do you do?)
# Delete, Fill (Mean, Median, Regression), or Ignore
## Scatterplot of groww vs imdb
ggplot(data = imdb.train, mapping = aes(x = gross, y = imdb_score)) +
  geom_point()
with(imdb.train, cor(gross, imdb_score, use = "complete.obs"))

############## In class Cloumns ############
imdb.train[, c("genres", "plot_keywords")] %>% is.na() %>% apply(2, sum)



##### KEYWORDS

keywords <- imdb$plot_keywords %>% sapply(function(x){unlist(str_split(x, "[\\| ]"))}) %>% 
  unlist() %>% unname() %>%  sort(decreasing = TRUE)

#list out words and frequency
useless <- stopwords::stopwords("english")

positions <- c()
for(i in 1:length(keywords)){
  positions[i] <- keywords[i] %in% useless
}

meaningful <- keywords[-which(positions==1)]
meaningful.freq <- meaningful %>% table() %>%  sort(decreasing = TRUE) %>% data.frame()

keywords.list <- imdb$plot_keywords %>% sapply(function(x){unlist(str_split(x, "[\\| ]"))})
  
score.list <- lapply(keywords.list, function(x){
  points <- 0
  x <- unique(x)
  for(i in 1:length(x)){
    if(x[i] %in% meaningful.freq[, 1]){
      points <- points + meaningful.freq[which(meaningful.freq[, 1] == x[i]), 2]
    }
  }
  points / 5
})

scores <- score.list %>% unlist() %>% unname()

# this way of looking at it yields skewed results even
# with a log transformation
#if not doing a transformation... replace NA's with 0
is.na(scores)  %>% which()
hist(scores)
boxplot(scores)

#if doing a log transform... replace NA's with 0.01
scores <- ifelse(scores < 1, 1, scores)
hist(log(scores))
boxplot(log(scores))

# this is the best transforation that I could find
# to normalize the scores.
cube.scores <- scores^(1/3)
hist(cube.scores)
boxplot(cube.scores)
median(cube.scores)
mean(cube.scores)

# another way of looking at it is to give it a score
# from 1 - 10 that is based on according to it's percentile 
digit.score <- as.numeric(cut_number(scores, n = 10))


keywords.adjust <- data.frame(imdb$imdb_score, imdb$movie_title, scores, cube.scores, digit.score)
keywords.adjust.narm <- keywords.adjust[-which(is.na(keywords.adjust$imdb.imdb_score)), ]

# there really is no correlation so this may have just been 
# a waste of time
cor(keywords.adjust.narm$imdb.imdb_score, keywords.adjust.narm$scores)


################################################################
############################# Genres ###########################
################################################################

# We can do the same thing with the genre scores... but there is 
# similarly no correlation.

genre.freq <- imdb$genres %>% sapply(function(x){unlist(str_split(x, "[\\| ]"))}) %>% 
  unlist() %>% unname() %>% table() %>%  sort(decreasing = TRUE) %>% data.frame()

genre.list <- imdb$genres %>% sapply(function(x){unlist(str_split(x, "[\\| ]"))})

genre.score.list <- lapply(genre.list, function(x){
  points <- 0
  for(i in 1:length(x)){
    if(x[i] %in% genre.freq[, 1]){
      points <- points + genre.freq[which(genre.freq[, 1] == x[i]), 2]
    }
  }
  points / length(x)
})

scores.genre <- genre.score.list %>% unlist() %>% unname()

genre.adjust <- data.frame(imdb$imdb_score, imdb$movie_title, scores.genre)
genre.adjust.narm <- genre.adjust[-which(is.na(genre.adjust$imdb.imdb_score)), ]

cor(genre.adjust.narm$imdb.imdb_score, genre.adjust.narm$scores)


imdb$genres %>% sapply(function(x){unlist(str_split(x, "[\\| ]"))}) %>% 
   qdapTools::mtabulate()
