

# 1.1 Read the dataset in  bike.csv  into R.
#     Call the loaded data   bike.

library(readr)
Bike <- read_csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/databike/Bike.csv")
View(Bike)

# 1.2 Convert holiday to a factor using factor() function
Bike$holiday <- factor(Bike$holiday)
str(Bike)

# 1.3 split the data into training set containing 2/3 of 
#     the original data (test set containing remaining 1/3 of the original data).

set.seed(1)
train_set <- sample(nrow(Bike) , floor(2/3 * nrow(Bike)))
test <- Bike[-train_set , ]

# 2.1 Build a support vector machine model
# 2.a/ The response is holiday and the predictors are: season, workingday, casual,
#      and registered. Please use svm() function with radial kernel and gamma=10 and cost = 100.
library(e1071)
svm_model <- svm(holiday ~ season + workingday + casual + registered, data= Bike[train_set, ],
              kernel = "radial", gamma =10, cost =100)
summary(svm_model)

# 2.b/ Perform a grid search to find the best model with potential cost: 1, 10, 50, 100 and potential
#      gamma: 1, 3, and 5 and using radial kernel and training dataset.

tune_model <- tune(svm,holiday ~ season + workingday + casual + registered , data= Bike[train_set, ], kernel = "radial", 
                ranges = list(cost = c(1,10,50,100), gamma= c(1,3,5)))

# 2.c/ Print out the model results. What’s the best model parameters?
summary(tune_model)
# The best parameters when  cost  = 100 and gamma = 1

# 2.d/ Forecast holiday using the test dataset and the best model found in c).
pred <- predict(tune_model$best.model, newdata = Bike[-train_set, ])

# 2.e/ Get the true observations of holiday in the test dataset?
true_bservation <- Bike[-train_set, "holiday"]

# 2.f/ Compute the test error by constructing the confusion matrix. Is it a good model?
table(true_bservation$holiday, pred)
# Is it a good model?
# pred
#     0    1
#0 3529    3
#1   89    8

# 3529+8+89+3
# 3629

# 3529+8
# 3537

#3537/3629
# 0.9746487

# it is equal 0.9746487 so it is a good model















