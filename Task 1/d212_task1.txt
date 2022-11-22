## Author: Stephen E. Porter
## Title: D212 Task 1 Clustering Analysis
## Course: WGU D212: Data Mining II
## Instructor: Dr.Keiona Middleton

# Libraries
library(tidyverse)
library(dplyr)
library(factoextra)
library(ggplot2)
library(cluster)

# Set seed
set.seed(22)

# Import CSV as data frame
df <- read.csv(file = 'C:/WGU/D212 Data Mining II/churn_clean.csv')

# Checking for nulls
sapply(df, function(x) sum(is.na(x)))

str(df)
summary(df)

# Renaming unclear columns named Item1 through Item8 for improved readability &
# confirming they have been renamed correctly

df <- df %>%
  rename(
    Response = Item1,
    Fix = Item2,
    Replacement = Item3,
    Reliability = Item4,
    Options = Item5,
    Respectful = Item6,
    Courteous = Item7,
    Listening = Item8
  )

colnames(df)

dfNumeric <- select_if(df, is.numeric)
str(dfNumeric)

to_drop <- c('CaseOrder', 'Zip', 'Lat', 'Lng', 'Population')

dfDropped = dfNumeric[,!(names(dfNumeric) %in% to_drop)]
str(dfDropped)

# Normalize Function
normalize = function(x) {
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
}

# Normalize data set
dfNorm <- dfDropped
for (i in colnames(dfNorm)) {
  dfNorm[i] <- normalize(dfNorm[i])
}

# Export data set for analysis
write.csv(dfNorm, 'C:/WGU/D212 Data Mining II/churn_kmeans.csv', row.names = FALSE)

# Identify optimal number of clusters
fviz_nbclust(dfNorm, FUNcluster = kmeans)

# k-means: 2 centers, 50 starting assignments
clusters <-kmeans(dfNorm, centers=2, nstart=50)
clusters

# View Means for each cluster
aggregate(dfDropped, by=list(cluster=clusters$cluster), mean)

# View clusters in plot
fviz_cluster(object=clusters, data=dfDropped)

# Add clusters to original dataframe
df$cluster <- as.factor(clusters$cluster)

# View clusters based on Churn
ggplot(data=df, aes(x=Churn, fill=cluster)) +
  geom_bar(stat="count")

# accuracy check
clusters$betweenss/clusters$tots

################################################################################
# Three Clusters
clusters3 <-kmeans(dfNorm, 3, nstart = 50)
clusters3

aggregate(dfDropped, by=list(cluster=clusters3$cluster), mean)

fviz_cluster(object=clusters3, data=dfDropped)

df$cluster3 <- as.factor(clusters3$cluster)

ggplot(data=df, aes(x=Churn, fill=cluster3)) +
  geom_bar(stat="count")

# accuracy check
clusters3$betweenss/clusters3$tots

################################################################################
# Four Clusters
clusters4 <-kmeans(dfNorm, 4, nstart = 50)
clusters4

aggregate(dfDropped, by=list(cluster=clusters4$cluster), mean)

fviz_cluster(object=clusters4, data=dfDropped)

df$cluster4 <- as.factor(clusters4$cluster)

ggplot(data=df, aes(x=Churn, fill=cluster4)) +
  geom_bar(stat="count")

# accuracy check
clusters4$betweenss/clusters4$tots

################################################################################
# Five Clusters
clusters5 <-kmeans(dfNorm, 5, nstart = 50)
clusters5

aggregate(dfDropped, by=list(cluster=clusters5$cluster), mean)

fviz_cluster(object=clusters5, data=dfDropped)

df$cluster5 <- as.factor(clusters5$cluster)

ggplot(data=df, aes(x=Churn, fill=cluster5)) +
  geom_bar(stat="count")

# accuracy check
clusters5$betweenss/clusters5$tots
sil5 <- silhouette(clusters5$cluster, dist(dfNorm))
fviz_silhouette(sil5)
