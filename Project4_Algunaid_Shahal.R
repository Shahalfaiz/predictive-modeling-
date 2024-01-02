#############################################
# Author:     Shahal Algunaid               #              
# Date:       07/24/2023                    #              
# Subject:    Project4                      #              
# Class:      DSCI412                       #                                           
# Instructor: Nengbing Tao                  #              
# File Name:  Project4_Algunaid_Shahal.R    #
#############################################


# 1.1 Read the dataset in Boston.csv  into R.
#     Call the loaded data  Boston.

library(readr)
Boston <- read_csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/Boston.csv")
View(Boston)

# 2.1 The response is nox and the predictor is dis. 
# Use the poly() function to fit a cubic polynomial regression to predict nox using dis. 
# Report the regression output.

poly_model <- lm(nox ~ poly(dis, 3), data = Boston)
print(poly_model)

# 2.2 Report the regression output.
# Answer:
# we can see the regression has Intercept <- 0.5547
# and have first degree <-  -2.0031
# and have second degree <-   0.8563 
# and have third degree  <- -0.3180

# 3.1 proposes models from degree 5, degree 4, and degree 3, and degree 2 polynomial regression. 
#   perform cross-validation using caret package to select the optimal degree for the polynomial 
#   and justify your answer.

library(caret)
train_control = trainControl(method="CV", number=10)
set.seed(1)
cv2 <- train(nox ~ poly(dis, 2), data = Boston ,  trControl=train_control , method = "lm" )
print(cv2)
cv3 <- train(nox ~ poly(dis, 3), data = Boston ,  trControl=train_control , method = "lm" )
print(cv3)
cv4 <- train(nox ~ poly(dis, 4), data = Boston ,  trControl=train_control , method = "lm" )
print(cv4)
cv5 <- train(nox ~ poly(dis, 5), data = Boston ,  trControl=train_control , method = "lm" )
print(cv5)

# 3.2 justify my answer
# Answer:
#the best model is degree 3 (cv3) because have a small test error 0.0619946

# 4.1 perform the following GAM analysis
# Predict nox using a smoothing spline of degree 3 in dis and a smoothing spline of degree 2 in medv
library(gam)
gam1 <- gam(nox ~ s(dis,3)+ s(medv,2) , data =Boston)
summary(gam1)

# 4.2 Predict nox using a smoothing spline of degree 2 in dis and a smoothing spline of degree 1 in medv
gam2 <- gam(nox ~ s(dis,2)+ s(medv,1) , data =Boston)
summary(gam2)

# 4.3 Perform anova analysis. Recommend the best model and justify your answer
anova(gam1,gam2)

# 4.4  Recommend the best model and justify my answer
# the best model is gam2 because have smallest p-value less than 5% (1.151e-08)


