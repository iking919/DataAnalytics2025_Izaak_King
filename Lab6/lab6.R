library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)

## Read in house data
NY_House_Dataset <- read_csv("~/github/DataAnalytics2025_Izaak_King/lab6/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

## Data cleaning
dataset <- dataset[-which(dataset$PROPERTYSQFT==2184.207862 | dataset$PRICE>100000000),]
dataset$LOG_PRICE <- log10(dataset$PRICE)
dataset$LOG_SQFT <- log10(dataset$PROPERTYSQFT)

ggplot(dataset, aes(x = LOG_SQFT, y = LOG_PRICE)) +
  geom_point()


## linear model with clean data
lin.mod1 <- lm(LOG_PRICE ~ LOG_SQFT, dataset)
summary(lin.mod1)

ggplot(dataset, aes(x = LOG_SQFT, y = LOG_PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)



## SVM Model 1 - Linear Kernel
svm.mod1 <- svm(LOG_PRICE ~ LOG_SQFT, dataset, kernel="linear")
summary(svm.mod1)
svm.pred1 <- data.frame(real = dataset$LOG_PRICE, predicted=predict(svm.mod1, dataset))

ggplot(dataset, aes(x = LOG_SQFT, y = LOG_PRICE)) +
  geom_point() +
  geom_line(aes(x= LOG_SQFT, y=svm.pred1$predicted), col="green")


ggplot(svm.pred1, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)


## SVM Model 2 - Radial Kernel

# First tuning parameters from a subset region of 1000
dataset_sub <- dataset[sample(nrow(dataset), 500), ]
gamma.range <- 10^seq(-3,2,1)
gamma.range

C.range <- 10^seq(-3,2,1)
C.range

tuned.svm <- tune.svm(LOG_PRICE ~ LOG_SQFT, data=dataset_sub, kernel="radial", gamma = gamma.range, cost = C.range, tune.control=tune.control(cross = 5))

tuned.smv

opt.gamma <- tuned.svm$best.parameters$gamma
opt.C <- tuned.svm$best.parameters$cost


# Creating the model itself 
svm.mod2 <- svm(LOG_PRICE ~ LOG_SQFT, dataset, kernel="radial", gamma=opt.gamma, cost=opt.C)
summary(svm.mod2)
svm.pred2 <- data.frame(real = dataset$LOG_PRICE ,predicted=predict(svm.mod2, dataset))

ggplot(dataset, aes(x = LOG_SQFT, y = LOG_PRICE)) +
  geom_point() +
  geom_line(aes(x=LOG_SQFT, y=svm.pred2$predicted), col="red")

ggplot(svm.pred2, aes(x = predicted, y = real-predicted)) +
  geom_point() +
  geom_hline(yintercept = 0)



## Evaluate Model Performance

# Split into train/test
train.indexes <- sample(nrow(dataset), 0.75*nrow(dataset))
train <- dataset[train.indexes, ]
test  <- dataset[-train.indexes, ]

## Helper function to compute metrics
compute_metrics <- function(real, predicted){
  err <- predicted - real
  mae  <- mean(abs(err))
  mse  <- mean(err^2)
  rmse <- sqrt(mse)
  return(c(MAE=mae, MSE=mse, RMSE=rmse))
}

## Linear Regression
lin.pred <- predict(lm(LOG_PRICE ~ LOG_SQFT, train), test)
metrics_lin <- compute_metrics(test$LOG_PRICE, lin.pred)

## SVM - Linear
svm_lin.pred <- predict(svm(LOG_PRICE ~ LOG_SQFT, train, kernel="linear"), test)
metrics_svm_lin <- compute_metrics(test$LOG_PRICE, svm_lin.pred)

## SVM - Radial
svm_rad.pred <- predict(svm(LOG_PRICE ~ LOG_SQFT, train, kernel="radial", gamma=opt.gamma, cost=opt.C), test)
metrics_svm_rad <- compute_metrics(test$LOG_PRICE, svm_rad.pred)

## Combine results
results <- rbind(
  Linear_Regression = metrics_lin,
  SVM_Linear       = metrics_svm_lin,
  SVM_Radial       = metrics_svm_rad
)

results

