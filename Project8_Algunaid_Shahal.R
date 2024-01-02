#############################################
# Author:     Shahal Algunaid               #              
# Date:       08/17/2023                    #              
# Subject:    Project8                      #              
# Class:      DSCI412                       #                                           
# Instructor: Nengbing Tao                  #              
# File Name:  Project8_Algunaid_Shahal.R    #
#############################################


# 1.1 Read the dataset in  wine.csv  into R.
#     Call the loaded data wine.

library(readr)
wine <- read_csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/wine.csv")
View(wine)

# 2.1 Preprocess the inputs
# 2.a/ Standardize the inputs using the scale() function?

  wine_scale <-  scale(wine)

# 2.b/ Convert the standardized inputs to a data frame using the as.data.frame() function.
  
wine_scale <- as.data.frame(wine_scale)

# 2.c/ Split the data into a training set containing 3/4 of the original data 
#      (test set containing the remaining 1/4 of the original data)

set.seed(1)
train_set <- sample(nrow(wine_scale ) , floor(2/3 * nrow(wine_scale )))
train <- wine_scale[train_set,]
test <- wine_scale[-train_set,]


# 3.1 Build a neural networks model
# 3.a/ The response is quality and the inputs are: volatile.acidity, density, pH, 
#      and alcohol. Please use 1 hidden layer with 1 neuron.

library(neuralnet)
neural_model <- neuralnet(quality ~ volatile.acidity + density + pH + alcohol,
                          data=train, hidden=c(1,1))

# 3.b/ Plot the neural networks.
plot(neural_model)

# 3.c/ Forecast the wine quality in the test dataset.
predict_neural <- compute(neural_model,
                          test[, c( "volatile.acidity", "density", "pH", "alcohol")])

# 3.d/Get the observed wine quality of the test dataset.
observ_test <- test$quality

#3.e/ Compute test error (MSE)?
mean((observ_test- predict_neural$net.result)^2)

#Answer:
# MSE =  0.7109411








