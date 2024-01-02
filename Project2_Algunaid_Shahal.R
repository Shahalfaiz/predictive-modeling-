#############################################
# Author:     Shahal Algunaid               #              
# Date:       07/11/2023                    #              
# Subject:    Project2                      #              
# Class:      DSCI412                       #                                           
# Instructor: Nengbing Tao                  #              
# File Name:  Project2_Algunaid_Shahal.R    #
#############################################

# Read the dataset in  mtcars.xlsx into R.
#     Call the loaded data  mtcars.
library(readxl)
mtcars <- read_excel("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/mtcars.xlsx")
View(mtcars)



# 1.1 Use the lm() function to perform a simple linear regression with 
#the response mpg and the predictor hp?
model_lm <- lm(mpg ~ hp , data = mtcars)
summary(model_lm)

# 2.1 Is there a relationship between the target mpg and  predictor hp?
  # Answer: yes there is relationship between the target and  predictor hp
  #p-value = 2.723e-09 it is less than 5% that indicate there is relationship between them

# 3.1 How strong is the relationship between the response and predictor?
  # Answer: 
  # we look at R-squared it is equal  0.6006 and RSE it is equal 3.5 
  # that mean the relationship is a moderate to strong 

# 4.1 Is the relationship between mpg and hp positive or negative?
  # Answer: 
  # we look at the estimate number (hp) 
  #(Intercept) 29.297928   1.314115  22.295  < 2e-16 ***
  #hp          -0.064548   0.008429  -7.658 2.72e-09 ***
  # that mean the relationship is negative

# 5.1 What is the predicted mpg associated with a horsepower (hp) of 100?
  # Answer: 
  model_lm <- lm(mpg ~ hp, data = mtcars )
  summary(model_lm)
  # Make the prediction
  predict(model_lm, data.frame(hp =c(100)),interval = "confidence")

# 5.2 What’s the 95% confidence interval for the predicted mpg?
  # Answer: 
  predict(model_lm, data.frame(hp =c(100)) , interval = "confidence", level = 0.95)
  
# 6.1 Plot the response and the predictor and add the regression line using abline()?
  # Answer:
  model_lm <- lm(mpg ~ hp, data = mtcars )
  plot (mtcars$hp , mtcars$mpg ,xlab = "hp", ylab = "mpg", main = " Response and the Predictor")
  abline(model_lm , lwd = 5 , col = "red")
  
# 7.1 Perform a multiple linear regression with mpg as the response
#and the predictors cyl, disp, hp, wt, vs, and gear? 
#Print out the results using summary() function?   
  model_lm2 <- lm(mpg ~ cyl + disp + hp + wt + vs + gear, data = mtcars)
  summary(model_lm2)
  
# 8.1 Is there a relationship between the predictors and the response? 
  # Answer: yes there is a relationship between the predictors and the response
  #p-value = 2.568e-12 it is less than 5% which indicate there is relationship between them
  
# 9.1 Which predictors appears to have a statistically significant relationship to the response?
  # Answer:
  # we look at p-value associated with each predictors
  
  #(Intercept) p-value = 1.45e-05 less than 0.05
  #yes/have a statistically significant relationship 
  
  #cyl(Number of cylinders)  p-value = 0.32133  greater than 0.05
  #no/does not have a statistically significant relationship
  
  #disp(Displacement) p-value = 0.11981 greater than 0.05
  #no/does not have a statistically significant relationship
  
  #hp(Gross horsepower) p-value = 0.02061 less than 0.05
  #yes/have a statistically significant relationship
  
  #wt(Weight) p-value = 0.00012 less than 0.05
  #yes/have a statistically significant relationship
  
  #vs(Engine) p-value = 0.61420  greater than 0.05
  #no/does not have a statistically significant relationship
  
  #gear(Number of forward gears)  p-value = 0.24002 greater than 0.05
  #no/does not have a statistically significant relationship
  
# 10.1 Use * symbols to fit linear regression models with interaction effects between hp and wt?
  # Answer:
  model_lm3 <- lm(mpg ~ hp * wt, data = mtcars)
  summary(model_lm3)

# 10.2 Does this interaction appear to be statistically significant?
  # Answer:
  # we look at p-value
  #hp:wt   p-value = 0.000362  yes/have a statistically significant relationship 
  #because The p-value of 0.000362 is less than 0.05
  
  

