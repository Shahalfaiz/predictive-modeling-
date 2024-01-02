


# 1.1 Read the dataset in  bike.csv  into R.
#     Call the loaded data   bike.

library(readr)
Bike <- read_csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/PREDICTIVE MODELING/data set/repetition/Bike.csv")
View(Bike)


# 1.2  split the data into a training set containing 2/3 of the original data 
#     (test set containing remaining 1/3 of the original data)?

set.seed(1)
train_set <- sample(nrow(Bike) , floor(2/3 * nrow(Bike)))
test <- Bike[-train_set , ]

# 2.1 Build a tree model using function tree():-
# 2.a/ The response is count and the predictors are season, holiday, workingday,
#     temp, atemp, humidity, windspeed, casual, and registered

library(tree)
tree_bike <- tree(count ~ season + holiday + workingday + temp + atemp + 
                    humidity + windspeed + casual +  registered ,data = Bike , subset= train_set )
summary(tree_bike)

# 2.b/ Perform cross-validation to choose the best tree by calling cv.tree()
cv.bike <- cv.tree(tree_bike)

# 2.c/ Plot the model results of b) 
plot(cv.bike$size, cv.bike$dev , ttpe = 'b')
# and determine the best size of the optimal tree
cv.bike$size[which.min(cv.bike$dev)]
# the best size of the optimal tree is 8 


# 2.d/ Prune the tree by calling prune.tree() function with the best size found in c).
prune_bike <- prune.tree(tree_bike, best =8)

# 2.e/ Plot the best tree model

plot(prune_bike)
text(prune_bike, pretty = 0)

# 2.f/ Compute the test error using the test data set
best_test_error <- predict(prune_bike, newdata = Bike[-train_set, ])
bike_test <- Bike[-train_set, "count"]
mean((best_test_error-bike_test$count)^2)

# 3.1 Build a random forest model using function randomForest()
# 3.a/ The response is count and the predictors are season, holiday, workingday,
#     temp, atemp, humidity, windspeed, casual, and registered.
library(randomForest)
rf_bike <- randomForest(count ~ season + holiday + workingday + temp + atemp + humidity + windspeed + 
                           casual +  registered ,  data = Bike, subset = train_set, importance = TRUE)

# 3.b/ Compute the test error using the test data set?
test_error_rf <- predict(rf_bike, newdata = Bike[-train_set,])
bike_test <- Bike[-train_set, "count"]
mean((test_error_rf-bike_test$count)^2)

# 3.c/ Extract variable importance measure using importance() function?
importance(rf_bike)

# 3.d/ Plot the variable importance using function varImpPlot(). 
varImpPlot(rf_bike)

# Which are the top 2 important predictors in this model?
# Answer:
# registered and casual 



