#############################################
#                                           #
# Author:     Shahal Algunaid               #
# Date:       07/06/2023                    #
# Subject:    Project 0                     #
# Class:      DSCI412                       #                              
# Instructor: Nengbing Tao                  #
# File Name:  Project0_Algunaid_Shahal.R    #
#                                           #
#############################################

# 1.1 Read the dataset in titanic2.csv into R.
#     Call the loaded data titanic.
titanic2 <- read.csv("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/titanic2.csv")
  View(titanic2)
  
# 1.2 Make sure that you have the directory set 
#     to the correct location for the data.
  setwd("C:/Users/aldul/OneDrive/Desktop/MASTER SHAHAL/titanic2.csv") 
  
# 2.1 How many rows are in the data frame? 
  nrow(titanic2)
  #Answer: 14
  
# 2.2 How many columns? 
  ncol(titanic2)
  # Answer: 5
  
# 2.3 What do the rows and columns represent?
  #     Answer: Each row represents class of passenger or crew,
  #             Each column represents a characteristic
  #             for each passenger, which include the
  #             characteristics Clas, Sex, Age, Survived, Died.  
  
# 3.1 Select the 1st, 5th, and 10th rows with
  #columns Class and Age.  
  titanic2[c(1,5,10),c("Class", "Age")]
  
  
# 4.1 Regress Survived and Died on the predictors Class, Sex and Age
  #using the two-column form for the dependent variable.  
  fit <- glm(cbind(Survived, Died) ~ Class + Sex + Age, family=binomial,  data=titanic2)
  summary(fit) 
  
# 4.2 Are any of the predictors associated with survival?
  # Answer: AgeChild is positively associated with survival.
  # Class2nd, Class3rd, ClassCrew and SexMale are
  # all negatively associated with survival.
  
# 4.3 If so, explain the relationship based on the t-statistics.
  # Answer: The t-statistics for AgeChild, Class2nd, Class3rd, ClassCrew
  # and SexMale are all well below the significance threshold
  # of 0.05.  
  
  
# 4.4 Explain the chances of survival in terms of odds, giving the 
  # precise numbers and giving in terms a non-expert can understand.
  odds <- exp(fit$coefficients)[-1]
  odds
  # Answer: 
  #  Class2nd   Class3rd  ClassCrew    SexMale   AgeChild 
  # 0.3612825  0.1690159  0.4241466  0.3459219 11.2465380  
  
  # Second Class and Male each have an approximate survival
  # rate of 1 in 3. If you are in Third Class, the odds of your
  # survival is approximately 1 to 6, and if you are in the crew,
  # your odds of survival are approximately 4 to 10. But if you
  # are a child, the odds of your survival are better than
  # 11 to 1.
  
  
  
# 5.1 What are the odds of  a male 3rd-class
  # child surviving the Titanic? Show the precise odds and
  # explain in a way a non-expert can understand.  
  titanic2
  p3 <- 76/(76+89)
  
  p3.odds <- p3/(1 - p3)
  
  results <- c(`3rd-class Male Child`=p3.odds)
  round(results, 2)
  
  # third-class male child, has 5 to 6 odds of
  # surviving.
  
  
# 6.1 Show a histogram of the Chi-Square distribution with 8 (upper) 
  # and 13 (lower) degrees of freedom.
  cdist <- qchisq((1:999)/1000, 8,13)
  hist(cdist, breaks=20, col="lavender", probability = T,
       xlab="Chi-square, with df=(8,13)",
       main="Histogram of Chi-Square Distribution\nWith 8, 13 degrees freedom")
  points(density(cdist), col="red", type="l", lwd=3)
  
# 6.2 On the same graphic, draw a vertical dotted line to
  # show the critical test value (0.05 significance level).
  # Make sure your graphic is properly titled and labeled.
  
  critical.value <- qchisq(0.95, 8,13)
  abline(v=critical.value, type="l",lty=2,col="green",lwd=3)
  text(critical.value -4, 0.045, "0.95\nsignificance\nlevel")
  
# End assignment  
  
  
  
  