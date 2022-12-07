#Step III: Classification
#Part I: K-nearest neighbors
#A. Show the code you used to run your model, and the code you used to assess your model.

#Step 1: Clean the data so it use the selected neighborhood with no NA.
library(tidyverse)
library(dplyr)
library(tidyr)

VL <- van_listing
MP1 <- filter(VL, host_neighbourhood == 'Mount Pleasant')
MP2 <- filter(VL, host_neighbourhood == 'Riley Park')
MP3 <- filter(VL, host_neighbourhood == 'Fairview')
MP <- rbind(MP1,MP2,MP3)

MP$license <- MP$license %>% replace_na('Unkown') 
MP$reviews_per_month <- MP$reviews_per_month %>% replace_na(0) 
MP$bedrooms <- MP$bedrooms %>% replace_na(1) 
MP$neighborhood_overview <- MP$neighborhood_overview %>% replace_na('No comments') 
MP$host_about <- MP$host_about %>% replace_na('No comments')
MP$beds[is.na(MP$beds)] <- median(MP$beds, na.rm = T)
MP$review_scores_rating[is.na(MP$review_scores_rating)] <- median(MP$review_scores_rating, na.rm = T)
MP$review_scores_accuracy[is.na(MP$review_scores_accuracy)] <- median(MP$review_scores_accuracy, na.rm = T)
MP$review_scores_cleanliness[is.na(MP$review_scores_cleanliness)] <- median(MP$review_scores_cleanliness, na.rm = T)
MP$review_scores_communication[is.na(MP$review_scores_communication)] <- median(MP$review_scores_communication, na.rm = T)
MP$review_scores_location[is.na(MP$review_scores_location)] <- median(MP$review_scores_location, na.rm = T)
MP$review_scores_value[is.na(MP$review_scores_value)] <- median(MP$review_scores_value, na.rm = T)
MP$review_scores_checkin[is.na(MP$review_scores_checkin)] <- median(MP$review_scores_checkin, na.rm = T)

#alternative methods to replace NA values.
is.na(MP)

colSums(is.na(MP))

which(colSums(is.na(MP))>0)

names(which(colSums(is.na(MP))>0))
is.na(MP$host_response_time)
mget(ls())

anyNA(MP)
is.na(MP)

#Step2: Create a new table called amenity.predictor, include the useful numeric variables and amenities in it.Clean the NA value.
str(MP)
amenity.predictor <- data.frame(MP$minimum_maximum_nights,MP$minimum_minimum_nights,
                                MP$minimum_nights,MP$maximum_maximum_nights,MP$maximum_nights,MP$maximum_minimum_nights,MP$maximum_nights_avg_ntm,
                                MP$minimum_nights_avg_ntm, MP$latitude,MP$longitude,MP$bedrooms,MP$beds,MP$amenities)


View(amenity.predictor)
sum(which(colSums(is.na(amenity.predictor))>0))
which(colSums(is.na(amenity.predictor))>0)
amenity.predictor$MP.bedrooms[is.na(amenity.predictor$MP.bedrooms)] <- mean(amenity.predictor$MP.bedrooms, na.rm = TRUE)
amenity.predictor$MP.beds[is.na(amenity.predictor$MP.beds)] <- mean(amenity.predictor$MP.beds, na.rm = TRUE)


#Step 3: Name someone who is going to rent at this place.
#The new renter's name is Harry.
#Step 4: Assign random value to Harry.
harry_minimum_maximum_nights <- runif(1,2,1225)
harry_minimum_minimum_nights <- runif(1,1,365)
harry_minimum_nights <- runif(1,1,365)
harry_maximum_maximum_nights <- runif(1,4,1225)
harry_maximum_nights <-runif(1,4,1225)
harry_maximum_minimum_nights <-runif(1,1,365)
maximum_nights_avg_ntm <-runif(1,4,1225)
minimum_nights_avg_ntm <-runif(1,1,365)
latitude <- runif(1,49.22046,49.28378)
longitude <- runif(1,-123.1563,-123.0318)
bedrooms <- runif(1,1,5)
beds <- runif(1,1,7)


harry_renting<- data.frame(harry_minimum_maximum_nights,harry_minimum_minimum_nights,
                           harry_minimum_nights,harry_maximum_maximum_nights,harry_maximum_nights,harry_maximum_minimum_nights ,
                           maximum_nights_avg_ntm,minimum_nights_avg_ntm,latitude ,longitude ,bedrooms ,beds)
View(harry_renting)

#find baby monitor in dataset
install.packages("data.table")
library("data.table")
library(dplyr)
library(tibble)
library(tidyr)

amenity.predictor[amenity.predictor$MP.amenities %like% "Baby monitor", ]
amenity.predictor <- amenity.predictor %>%
  add_column(baby.monitor = NA)
View(amenity.predictor)
amenity.predictor$baby.monitor[is.na(amenity.predictor$baby.monitor)] <- 0
#changing 0 to 1 for rows contains baby monitor
amenity.predictor[15,14]=1
amenity.predictor[41,14]=1
amenity.predictor[45,14]=1
amenity.predictor[100,14]=1
amenity.predictor[232,14]=1
amenity.predictor[349,14]=1
amenity.predictor[371,14]=1


#Step 5: Partition the number into training and validation set.
set.seed(20)

train.index <- sample(c(1:nrow(amenity.predictor)), nrow(amenity.predictor)*0.6)
train.df <- amenity.predictor[train.index, ]
valid.df <- amenity.predictor[-train.index, ]

#Step6: Normalize the dataset.
library(ggplot2)
library(lattice)
library(caret)
amenity.predictor$baby.monitor <- as.factor(amenity.predictor$baby.monitor)
amenity.predictor[nrow(amenity.predictor) + 1,] <- c(harry_minimum_maximum_nights,
                                                     harry_minimum_minimum_nights,harry_minimum_nights,harry_maximum_maximum_nights,
                                                     harry_maximum_nights,harry_maximum_minimum_nights ,maximum_nights_avg_ntm,
                                                     minimum_nights_avg_ntm,latitude ,longitude ,bedrooms ,beds,NA,0)

train.norm.df <- train.df
valid.norm.df <- valid.df
renting.norm.df <- amenity.predictor

norm.values <- preProcess(train.df[,1:12], method=c("center", "scale"))
train.norm.df[,1:12] <- predict(norm.values, train.df[,1:12])
valid.norm.df[,1:12] <- predict(norm.values, valid.df[,1:12])
renting.norm.df[,1:12] <- predict(norm.values,amenity.predictor[,1:12])
new.norm.df <- predict(norm.values, renting.norm.df[386,1:12])
View(new.norm.df)

#Step 7: find best k
library(caret)
library(FNN)
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[, 1:12],
                  cl = train.norm.df[, 14], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 14])$overall[1]
}

accuracy.df

ggplot(accuracy.df, aes(x=k, y=accuracy)) + geom_point()

#change factor so that the levels are same factor

valid.norm.df[,14] <- factor(c("0","1"))

#Use k=5 to predict

library(FNN)
nn <- knn(train = train.norm.df[, 1:12], test = new.norm.df[,1:12],
          cl = train.norm.df[,14], k = 5)
row.names(train.df)[attr(nn, "nn.index")]
nn





