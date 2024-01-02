

# 1.1 Read the dataset in   CreditCards.csv  into R.
#     Call the loaded  CreditCards.

library(readr)
CreditCards <- read_csv("data set/CreditCards.csv")
View(CreditCards)
   
# 2.1 Perform the k-means cluster analysis
# 2.a Remove the first column: 
#     CUST_ID since it doesn’t provide any info for cluster. 
   
   CreditCards <- CreditCards [ , -1 ]
   
# 2.b Determine the optimal number of clusters. Justify your answer.
   library("cluster")
   library("factoextra") 
   set.seed(1)
   cluster <- fviz_nbclust(CreditCards, kmeans, method = "gap_stat")
   print(cluster)
   
# Justify my answer:-  
# from the chart I see the optimal number of clusters  is 2   
  
# 2.c Perform k-means clustering using the optimal number of clusters
   
   km_cluster <- kmeans(CreditCards, 2 , nstart = 25)
   
# 2.d Visualize the clusters in different colors
   
   fviz_cluster(km_cluster, data = CreditCards)
   