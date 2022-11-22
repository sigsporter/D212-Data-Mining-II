############################### TITLE ##########################################

## Author: Stephen E. Porter
## Title: D212 Task 1 Clustering Analysis
## Course: WGU D212: Data Mining II
## Instructor: Dr.Keiona Middleton


############################### LIBRARIES ######################################
options(warn = -1)
library(tidyverse)
library(caret)
library(dplyr)
library(factoextra)
library(ggplot2)
library(cluster)


############################### DATA PREPARATION ###############################

# Import CSV as data frame
df <- read.csv(file = 'C:/WGU/D212 Data Mining II/churn_clean.csv')

# Checking for nulls
sapply(df, function(x) sum(is.na(x)))

str(df)
summary(df)

# Keeping desired columns
to_keep <- c('Tenure', 'Bandwidth_GB_Year', 'Outage_sec_perweek',
             'MonthlyCharge', 'Income')

dfDropped = df[to_keep]
str(dfDropped)


############################### OUTLIER DETECTION ##############################

#Check for outliers in boxplot
boxplot(dfDropped$Tenure, dfDropped$Bandwidth_GB_Year, 
        dfDropped$Outage_sec_perweek, dfDropped$MonthlyCharge, dfDropped$Income,
        main = "Boxplots",
        names = c("Ten", "Ban", "Otg", "Chg", "Inc"),
        horizontal = FALSE)

# Check each column for outliers
tenOut <- boxplot(dfDropped$Tenure, plot=FALSE)$out
tenOut
banOut <- boxplot(dfDropped$Bandwidth_GB_Year, plot=FALSE)$out
banOut
otgOut <- boxplot(dfDropped$Outage_sec_perweek, plot=FALSE)$out
otgOut

# Outage seconds per week has outliers. Create temp data frame & remove outliers
temp <- dfDropped
temp <- temp[-which(temp$Outage_sec_perweek %in% otgOut),]
str(temp)

chgOut <- boxplot(temp$MonthlyCharge, plot=FALSE)$out
chgOut

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

# Income has many outliers - remove them
temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

# View boxplot for outliers
boxplot(temp$Tenure, temp$Bandwidth_GB_Year, 
        temp$Outage_sec_perweek, temp$MonthlyCharge, temp$Income,
        main = "Boxplots",
        names = c("Ten", "Ban", "Otg", "Chg", "Inc"),
        horizontal = FALSE)

# Income still has outliers. Repeat process until none remain
incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

# Income has no outliers. Check boxplots
boxplot(temp$Tenure, temp$Bandwidth_GB_Year, 
        temp$Outage_sec_perweek, temp$MonthlyCharge, temp$Income,
        main = "Boxplots",
        names = c("Ten", "Ban", "Otg", "Chg", "Inc"),
        horizontal = FALSE)

# Removing rows has created outliers in Outage - repeat process
otgOut <- boxplot(temp$Outage_sec_perweek, plot=FALSE)$out
otgOut

temp <- temp[-which(temp$Outage_sec_perweek %in% otgOut),]
str(temp)

otgOut <- boxplot(temp$Outage_sec_perweek, plot=FALSE)$out
otgOut

temp <- temp[-which(temp$Outage_sec_perweek %in% otgOut),]
str(temp)

otgOut <- boxplot(temp$Outage_sec_perweek, plot=FALSE)$out
otgOut

boxplot(temp$Tenure, temp$Bandwidth_GB_Year, 
        temp$Outage_sec_perweek, temp$MonthlyCharge, temp$Income,
        main = "Boxplots",
        names = c("Ten", "Ban", "Otg", "Chg", "Inc"),
        horizontal = FALSE)

# Removing rows has caused outliers in Income - repeat process
incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

temp <- temp[-which(temp$Income %in% incOut),]
str(temp)

incOut <- boxplot(temp$Income, plot=FALSE)$out
incOut

# Check boxplots for outliers
boxplot(temp$Tenure, temp$Bandwidth_GB_Year, 
        temp$Outage_sec_perweek, temp$MonthlyCharge, temp$Income,
        main = "Boxplots",
        names = c("Ten", "Ban", "Otg", "Chg", "Inc"),
        horizontal = FALSE)

# No outliers remain. Ready for kmeans


############################### NORMALIZE & EXPORT #############################

# Normalize Function
normalize = function(x) {
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
}

# Normalize data set
dfNorm <- temp
for (i in colnames(dfNorm)) {
  dfNorm[i] <- normalize(dfNorm[i])
}
  
# Export data set for analysis
write.csv(dfNorm, 'C:/WGU/D212 Data Mining II/churn_kmeans.csv', 
          row.names = FALSE)


############################### KMEANS #########################################

# Identify optimal number of clusters
fviz_nbclust(dfNorm, FUNcluster = kmeans, method = "wss")
fviz_nbclust(dfNorm, FUNcluster = kmeans, method = "silhouette")

# k-means: 2 centers, 50 starting assignments
clusters2 <-kmeans(dfNorm, centers=2, nstart=50)
clusters2$centers
clusters2$betweenss / clusters2$totss

# View clusters in plot
fviz_cluster(object=clusters2, data=dfNorm, geom = "point")


############################### TENURE & BANDWIDTH #############################

dfFinal <-dfNorm[c('Tenure', 'Bandwidth_GB_Year')]
fviz_nbclust(dfFinal, FUNcluster = kmeans, method = "silhouette")
clusters2Final <- kmeans(dfFinal, centers=2, n=50)
clusters2Final$centers
clusters2Final$betweenss / clusters2Final$totss
fviz_cluster(object=clusters2Final, data=dfFinal, geom = "point")
