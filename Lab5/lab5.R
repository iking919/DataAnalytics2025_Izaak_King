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

## change the data type of the "Type" column from character to factor
wine$Type <- as.factor(wine$Type)


# ## split train/test
N <- nrow(wine)
train.indexes <- sample(N,0.8*N)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

# First tune for linear kernel
Cost.range <- seq(1,20, 1)

tuned.svm0 <- tune.svm(
  Type ~ Flavanoids + `Total phenols` + `Color Intensity` + Alcohol,
  data = wine,
  kernel = 'linear',
  cost = Cost.range
)
tuned.svm0$best.parameters

## train SVM model - linear kernel 
svm.mod0 <- svm(Type ~ Flavanoids + `Total phenols` + `Color Intensity` + Alcohol, data = train, kernel = 'linear', cost = 1)

svm.mod0

# Testing svm accuracy
wine.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Type, Predicted = wine.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

# Test set prediction 
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


### SECOND SVM


# First tune for polynomial kernel
gamma.range <- seq(0.1,10, .1)
Cost.range <- seq(1,20, 1)

tuned.svm1 <- tune.svm(
  Type ~ Flavanoids + `Total phenols` + `Color Intensity` + Alcohol,
  data = wine,
  kernel = 'polynomial',
  gamma = gamma.range,
  cost = Cost.range
)
tuned.svm1$best.parameters

## train SVM model - polynomial kernel 
svm.mod1 <- svm(Type ~ Flavanoids + `Total phenols` + `Color Intensity` + Alcohol, data = train, kernel = 'polynomial', gamma = 0.2, cost = 2)

# Testing svm accuracy
wine.pred <- predict(svm.mod1, train)

cm = as.matrix(table(Actual = train$Type, Predicted = wine.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

# Test set prediction 
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)




### KNN MODEL
train.inputs <- train[, c("Flavanoids", "Total phenols", "Color Intensity", "Alcohol")]
test.inputs  <- test[, c("Flavanoids", "Total phenols", "Color Intensity", "Alcohol")]

train.labels <- train$Type
test.labels  <- test$Type

#  Train with k = 3 for each type
knn.pred <- knn(train = train.inputs, test = test.inputs, cl = train.labels, k = 3)

cm <- table(Actual = test.labels, Predicted = knn.pred)
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

