##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(caret)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("~/github/DataAnalytics2025_Izaak_King/lab4/")


## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


###

# creating another dataframe from wine dataset that contains the identifier column 1,and another with all other attributes
X <- wine[,-1]
Y <- wine[,1]



####### PCA #######

Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = T)

principal_components <- princomp(Xc)

summary(principal_components)

principal_components$loadings

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)




# knn model based on all attributes,
model0Input <- scale(wine[, c("Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")])
label <- wine$Type

knnModel0 <- knn(train = model0Input, test = model0Input, cl = label, k = 3)

# Print performance statistics
confusionMatrix0 <- table(Predicted = knnModel0, Actual = label)
correct <- sum(diag(confusionMatrix0)) 
total <- length(wine$Type)
accuracy <- correct / total
print(confusionMatrix0)
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))

#Getting F1
cm <- confusionMatrix(knnModel0, as.factor(label), mode = "everything")
precision <- cm$byClass[, "Precision"]
recall <- cm$byClass[, "Recall"]
f1 <- 2 * (precision * recall) / (precision + recall)
print(f1)




# knn model using the first 2 PCs (Score)

pc_scores <- principal_components$scores[, 1:2]

knnModel1 <- knn(train = pc_scores, test = pc_scores, cl = label, k = 3)

# Print performance statistics
confusionMatrix1 <- table(Predicted = knnModel1, Actual = label)
correct <- sum(diag(confusionMatrix1)) 
total <- length(wine$Type)
accuracy <- correct / total
print(confusionMatrix1)
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))

#Getting F1
cm <- confusionMatrix(knnModel1, as.factor(label), mode = "everything")
precision <- cm$byClass[, "Precision"]
recall <- cm$byClass[, "Recall"]
f1 <- 2 * (precision * recall) / (precision + recall)
print(f1)

