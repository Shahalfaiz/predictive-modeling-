#############################################
# Author:     Shahal Algunaid               #              
# Date:       07/07/2023                    #              
# Subject:    Project 0                     #              
# Class:      DSCI412                       #                                           
# Instructor: Nengbing Tao                  #              
# File Name:  Project1_Algunaid_Shahal.R    #
#############################################

# 1.1 Read the dataset in  Boston.csv into R.
#     Call the loaded data  Boston.

Bostonnew <- read.csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/Bostonnew.csv")
  View(Bostonnew)
  
# 2.1 How many rows are in the data frame? 
  nrow(Bostonnew)
#Answer:  506
  
# 2.2 How many columns? 
  ncol(Bostonnew)
# Answer: 13
  
# 2.3 What do the rows and columns represent?
#     Answer: Each row represent the suburbs of Boston,
#             Each column represents a variables
#             for  suburbs, which include the
#             variables  crim,zn,indus,chas,nox,rm,
#             age,dis,rad,tax,ptratio,lstat,medv  
  
# 3.1 Select the 1st, 100th, and 500th rows with
#columns  tax and medv.  
  Bostonnew[c(1,100,500),c("tax", "medv")]
  #Answer:  tax medv
  #1   296 24.0
  #100 276 33.2
  #500 391 17.5
  
  
# 4.1 Look at the data using cor function.
  cor(Bostonnew)
# 4.2 Are any of the predictors associated with per capita crime rate?
  #Answer:
  #the correlation matrix shows predictors associated with per capita crime rate
  #indus ( proportion of non-retail business acres per town),
  #nox   (nitrogen oxides concentration (parts per 10 million)),
  #rad   (index of accessibility to radial highways), 
  #tax   (full-value property-tax rate per \$10,000), 
  #lstat (lower status of the population (percent)) 
  #have correlation coefficients greater than 0.4 and positive
  #it is strong relationship

# 4.3 If so, explain the relationship based on correlation coefficents.  
  #Answer:
  # zn(proportion of residential land zoned for lots over 25,000 sq.ft)
  #The correlation coefficient of -0.20046922 it's a weak negative relationship.
  #This means that larger zn lower per capita crime rates.
  
  #indus(proportion of non-retail business acres per town)
  #The correlation coefficient of  0.40658341 it's a strong positive relationship
  #this mean that larger indus high per capita crime rates.
  
  # chas (Charles River dummy variable (= 1 if tract bounds river; 0 otherwise))
  #The correlation coefficient of -0.055891582 it's a weak negative relationship.
  #This means that weak relationship between chas and crime rates.
  #This means that larger chas lower per capita crime rates
  
  #nox(nitrogen oxides concentration (parts per 10 million))
  #The correlation coefficient of  0.42097171 it's a strong positive relationship
  #this mean that larger nox high per capita crime rates.
  
  # rm(average number of rooms per dwelling)
  #The correlation coefficient of -0.21924670 it's a weak negative relationship.
  #This means that larger rm lower per capita crime rates.
  
  #age(proportion of owner-occupied units built prior to 1940)
  #The correlation coefficient of  0.35273425 it's a weak positive relationship
  #this mean that larger age slightly higher per capita crime rates.
  #but the relation is weak
  
  #dis(weighted mean of distances to five Boston employment centres)
  #The correlation coefficient of -0.37967009 it's a weak negative relationship.
  #This means that larger dis lower per capita crime rates.
  
  #rad(index of accessibility to radial highways)
  #The correlation coefficient of  0.625505145  it's a strong positive relationship
  #this mean that larger rad high per capita crime rates.
  
  #tax(full-value property-tax rate per \$10,000)
  #The correlation coefficient of  0.58276431  it's a strong positive relationship
  #this mean that larger tax high per capita crime rates.
  
  
  #ptratio(pupil-teacher ratio by town)
  #The correlation coefficient of   0.2899456 it's a weak positive relationship
  #this mean that larger ptratio slightly higher per capita crime rates.
  #but the relation is weak
  
  
  #lstat(lower status of the population (percent))
  #The correlation coefficient of 0.4556215  it's a strong positive relationship
  #this mean that larger lstat high per capita crime rates.
  
  
  #medv(median value of owner-occupied homes in \$1000s)
  #The correlation coefficient of -0.3883046 it's a weak negative relationship.
  #This means that larger medv lower per capita crime rates.
  
  
  
# 5.1 Make some pairwise scatterplots of the predictors, crim, rad, tax, indu 
  # and lstat in this data set
  pairs(Bostonnew[ , c("crim" , "rad" , "tax" , "indus" , "lstat")])
# 5.2  Describe your findings.
  #Answer:
  #In this scatterplot,I can observe that there is a positive relationship
  #between crim and rad 
  #and positive relationship between crim and tax
  #and positive relationship between crim and indus
  #and positive relationship between crim and lstat
  
# 6.1 Do any of the suburbs of Boston appear to have particularly high crime rates 
  #by looking at the histogram of crim?  
  hist(Bostonnew$crim , main = "Histogram of crim",xlab= " crim", ylab = "suburbs",col = "green")
  #Answer:
  #Yes, the histogram appears to have particularly high crime rates in a few suburbs of Boston
  
# 6.2  What is the range of crim by using range() function in R?
  range(Bostonnew$crim)
 # Answar:
  #the range of crim is 0.00632 to  88.97620
  
  
# 7.1 How many of the suburbs in this dataset bound the Charles River?
  sum(Bostonnew$chas)
  # Answar: 35
  
# 8.1 What is the median pupil-teach ratio among the towns in this dataset?
  median(Bostonnew$ptratio)
  # Answar: 19.05
# 8.2 What’s the mean?  
  mean(Bostonnew$ptratio)
  # Answar: 18.45553
  
# 9.1 In this dataset,how many of the suburbs average more than seven rooms per dwelling?
  sum(Bostonnew$rm > 7)
  # Answar: 64
# 9.2 More than eight rooms per dwelling?   
  sum(Bostonnew$rm > 8)
  # Answar: 13
# 9.3 Comment on the suburbs that average more than eight rooms per dwelling.
  # it is 13 suburbs that average more than eight rooms per dwelling
  
# 10.1 Convert chas to a factor?
  Bostonnew$chas <- factor(Bostonnew$chas)
  str(Bostonnew)
# 10.2 Boxplot the medv against chas?
  boxplot(medv ~ chas, data = Bostonnew,
          col = c("pink", "orange"),
          xlab = "Charles River dummy variable(chas)",
          ylab = "median value of owner-occupied homes(medv)",
          main = "Boxplot of medv against chas")
# 10.3  Are houses around the Charles River more expensive?  
  #Yes the house around the charles river is more expensive
  
  
#End assignment  
  
  
  
  