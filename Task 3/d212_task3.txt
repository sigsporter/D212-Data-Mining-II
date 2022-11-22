## Author: Stephen E. Porter
## Title: D212 Task 3 Market Basket Analysis
## Course: WGU D212: Data Mining II
## Instructor: Dr.Keiona Middleton

# Libraries
library(tidyverse)
library(arules)
library(RColorBrewer)

# Import CSV as data frame
df <- read.csv(file = 'C:/WGU/D212 Data Mining II/teleco_market_basket.csv')

# Checking for nulls
sapply(df, function(x) sum(is.na(x)))
str(df)
glimpse(df)

# Every other row in the dataframe is blank. Dropping blank columns
toDelete <- seq(0, nrow(df), 2)
df <- df[toDelete,]
glimpse(df)
head(df, n=1)

write.csv(df, 'C:/WGU/D212 Data Mining II/churn_mba.csv', row.names = FALSE)

tr <- read.transactions('C:/WGU/D212 Data Mining II/churn_mba.csv', 
                        format="basket", sep=',')
summary(tr)


itemFrequencyPlot(tr, topN=20, type="absolute", col=brewer.pal(10,'Set3'), 
                  main="Absolute Item Frequency Plot")


all.rules <- apriori(tr, parameter=list(supp=0.01, conf=0.4))
summary(all.rules)
inspect(head(all.rules, by="lift", n=18))
