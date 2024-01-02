
#############################################
# Author:     Shahal Algunaid               #              
# Date:       07/18/2023                    #              
# Subject:    Project3                      #              
# Class:      DSCI412                       #                                           
# Instructor: Nengbing Tao                  #              
# File Name:  Project3_Algunaid_Shahal.R    #
#############################################

# PART I
# 1.1 Read the dataset in  mtcars.xlsx into R.
#     Call the loaded data  mtcars.
library(readxl)
mtcars <- read_excel("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/repetition/mtcars.xlsx")
View(mtcars)

# 1.2  convert column am to a factor using factor() function

mtcars$am  <- factor(mtcars$am)
str(mtcars)

# 2.1 Split the data into training set and test set. 
#The training set contains the first 35 observations, 
#the test set containing the remaining observations

train_set <- mtcars[1:35, ]
test_set <- mtcars[36:nrow(mtcars), ]

# 3.1  Build a logistic regression model with the response is am 
# and the predictors are mpg, cyl, hp, and wt using glm() function

model_glm <- glm(am ~ mpg + cyl + hp + wt , data = mtcars , family = binomial)
summary(model_glm)

# 4.1 Compute the test error on the test data set using a confusion matrix. 

glm.probs <- predict(model_glm , test_set , type = "response")
threshold <- 0.5 
predict_am <- ifelse(glm.probs > threshold, 1 , 0)
confusion_matrix <- table(predict_am , test_set$am)
test_error <- 1-sum(diag(confusion_matrix ))/sum(confusion_matrix )
print(confusion_matrix )
print(test_error)

# 4.2 Is it a good model based on test error?
# Answer: 
# yes it is a good model based on test error

#Part II
#      Read the dataset in   bike.csv. into R.
#     Call the loaded data  bike.
Bike <- read.csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/Bike.csv")
  View(Bike)
  
# 1.1 Build a linear model to forecast number of total rentals (count) using potential predictors
#     season, holiday, workingday, weather, atemp, and registered. 
  
  linear_model <- lm(count ~ season + holiday + workingday + weather + atemp + registered , data = Bike)
  summary(linear_model)

# 2.1  Perform best subset selection using bestglm() function based on BIC. 

  library(caret)
  library(leaps)
  library(bestglm)
  XY <- Bike[ ,c("season", "holiday" , "workingday" , "weather" , "atemp" , "registered" ,"count")]
  XY <- as.data.frame(XY)
  best_subset <- bestglm(XY, IC = "BIC")
  print(best_subset)
  
  
# 2.2  What’s the best model based on BIC?
# Answer:  
# the best model is (season, holiday , workingday , weather , atemp , registered)
  
  
  
# 3.1  Compute the test error of the best model based on BIC using LOOCV?
  
  library(caret)
  library(leaps)
  library(MASS)
  library(bestglm)
  XY <- Bike[, c("season", "holiday", "workingday", "weather", "atemp", "registered", "count")]
  XY <- as.data.frame(XY)
  best_subset <- bestglm(XY, IC = "BIC")
  
  train_control <- trainControl(method="LOOCV")
  model <- train(count ~ season + holiday + workingday + weather + atemp + registered ,
                 data = XY ,
                 trControl = train_control,
                 method = "lm" , metric = "RMSE")
  print(model)
  
  test_predict <- predict(model, XY)
  test_error <- mean (test_predict != XY$count)
  print(test_error)
  

# 4.1  Calculate the test error of the best model based on BIC using 10-fold CV.
  
  library(caret)
  library(leaps)
  library(MASS)
  library(bestglm)
  XY <- Bike[, c("season", "holiday", "workingday", "weather", "atemp", "registered", "count")]
  XY <- as.data.frame(XY)
  best_subset <- bestglm(XY, IC = "BIC")
  
  train_control <- trainControl(method="cv", number = 10)
  model <- train(count ~ season + holiday + workingday + weather + atemp + registered ,
                 data = XY ,
                 trControl = train_control,
                 method = "lm" , metric = "RMSE")
  print(model)
  test_predict <- predict(model, XY)
  test_error <- mean (test_predict != XY$count)
  print(test_error)
  
  
# 5.1 Perform best subset selection using bestglm() function based on CV. What’s the best model based on CV?  
  
  library(caret)
  library(leaps)
  library(bestglm)
  XY <- Bike[ ,c("season", "holiday" , "workingday" , "weather" , "atemp" , "registered" ,"count")]
  XY <- as.data.frame(XY)
  best_subset <- bestglm(XY, IC = "CV")
  print(best_subset)
  
# 5.2  What’s the best model based on CV?
# Answer:    
#  the best model is (season, holiday , workingday , weather , atemp , registered)
  
  
# 6.1  Perform the backward stepwise selection using stepAIC() function. What’s the best model?
  
  library(MASS)
  full_model <- lm(count ~ season + holiday + workingday + weather + atemp + registered , data = Bike)
  best_model <- stepAIC(full_model, direction = "backward")
  summary(best_model)
  
# 6.2  What’s the best model
# Answer:   
#  the best model is (season, holiday , workingday , weather , atemp , registered)











