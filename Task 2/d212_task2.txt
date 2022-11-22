## Author: Stephen E. Porter
## Title: D212 Task 1 Clustering Analysis
## Course: WGU D212: Data Mining II
## Instructor: Dr.Keiona Middleton

# Libraries
library(tidyverse)
library(corrplot)
library(factoextra)

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

# Getting numeric columns only
dfNumeric <- select_if(df, is.numeric)
str(dfNumeric)

# Dropping unuseful numeric columns
to_drop <- c('CaseOrder', 'Zip', 'Lat', 'Lng')

dfDropped = dfNumeric[,!(names(dfNumeric) %in% to_drop)]
str(dfDropped)

# Scaling data & exporting to CSV for submission
dfScaled <- as.data.frame(scale(dfDropped, center=TRUE, scale=TRUE))
summary(dfScaled)

write.csv(dfScaled, 'C:/WGU/D212 Data Mining II/churn_pca.csv', row.names = FALSE)

# Correlation matrix
corr_df <- round(cor(dfScaled),3)

corrplot(corr_df, method="circle", type="upper", order="hclust", tl.col="black",
         tl.srt=45)


pca_results <- prcomp(dfDropped, center=TRUE, scale.=TRUE)
summary(pca_results)

pca_results$rotation <- -1*pca_results$rotation
pca_results$rotation

# Scree plot
fviz_eig(pca_results, choice = "variance", ncp=19)
fviz_eig(pca_results, choice = "eigenvalue", ncp=19)

fviz_pca_var(pca_results,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE)

fviz_pca_biplot(pca_results, col.var = "#2E9FDF", col.ind = "#696969")

eigen_val <- get_eigenvalue(pca_results)
eigen_val
