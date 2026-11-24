# Tsithembile Tembo & 
# Predicting edible (e)  vs. poisonous (p) mushrooms 

#packages
library(dplyr)

#dataset
mushrooms <- read.csv("~/Desktop/secondary_data_shuffled.csv", sep = ";", 
                      na.strings = "",
                      stringsAsFactors = TRUE)

# examine dataset 
View(mushrooms)
sapply(mushrooms, class)

summary(mushrooms)

# missing values
sum(is.na(mushrooms))
colSums(is.na(mushrooms))

#percentage in each column of missing values 
round(colMeans(is.na(mushrooms)) * 100, 2)

#remove missing columns, over 50 percent of it had missing values 
#columns contained insufficient info for initial model
mushrooms$spore.print.color <- NULL; 
mushrooms$stem.surface <- NULL; 
mushrooms$stem.root <- NULL; 
mushrooms$veil.type <- NULL; 
mushrooms$veil.color <- NULL; 
View(mushrooms)

sum(is.na(mushrooms))

#replacing remaining N/A with Missing 
missing_col <- c("cap.surface", "gill.attachment", "gill.spacing", "ring.type")
for (col in missing_col){
  #turn into characters 
  mushrooms[[col]] <- as.character(mushrooms[[col]])
  #N/A to missing, m is for missing
  mushrooms[[col]][is.na(mushrooms[[col]])] <- "m"
  #back to factor 
  mushrooms[[col]] <- as.factor(mushrooms[[col]])
}

#check if any NA's 
sum(is.na(mushrooms))

#deal with duplication 
sum(duplicated(mushrooms))
mushrooms <- distinct(mushrooms)
sum(duplicated(mushrooms))


# class, number of observations 
count <- table(mushrooms$class)
barplot(count, main = "Mushroom Class Distribution", 
        xlab = "Type of Mushroom", ylab = "Total Amount")


#baseline classification model

#Split data into training and testing sets
set.seed(13)
train <- sample(1:nrow(mushrooms), nrow(mushrooms) * 0.8)
train.data <- mushrooms[train, ]
test.data <- mushrooms[-train, ]

#baseline model - all 
glm_all <- glm(class ~ ., data = train.data, family = binomial)
summary(glm_all)
#predict 
glm_probs <- predict(glm_all, newdata = test.data, type = "response")
glm_preds1 <- ifelse(glm_probs > 0.5, "p", "e")

conf_matrix1 <- table( Predicted =  glm_preds1, Actual = test.data$class)
print(conf_matrix1)
accuracy1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)
print(accuracy1)

glm_preds2 <- ifelse(glm_probs > 0.3, "p", "e")

conf_matrix2 <- table( Predicted =  glm_preds2, Actual = test.data$class)
print(conf_matrix2)
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
print(accuracy2)

glm_preds3 <- ifelse(glm_probs > 0.2, "p", "e")

conf_matrix3 <- table( Predicted =  glm_preds3, Actual = test.data$class)
print(conf_matrix3)
accuracy3 <- sum(diag(conf_matrix3)) / sum(conf_matrix3)
print(accuracy3)

#visualization 
correct3 <- glm_preds3 == test.data$class
pie(table(correct3),
    labels = c("Incorrect", "Correct"),
    main = "Baseline Model Prediction Results (Threshold 0.2)",
    col = c("tomato", "steelblue"))

correct2 <- glm_preds2 == test.data$class
pie(table(correct2),
    labels = c("Incorrect", "Correct"),
    main = "Baseline Model Prediction Results (Threshold 0.3)",
    col = c("tomato", "steelblue"))

correct1 <- glm_preds1 == test.data$class
pie(table(correct1),
    labels = c("Incorrect", "Correct"),
    main = "Baseline Model Prediction Results (Threshold 0.5)",
    col = c("tomato", "steelblue"))







