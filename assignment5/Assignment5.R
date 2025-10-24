library(ggplot2)
library(class)
library(dplyr)
library(lubridate)
library(randomForest)

### DATASET CLEANING ###
NYC.data <- read.csv("C:/Users/kingi/Documents/school/Data Analytics/assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

Manhattan.uncleaned.data <- NYC.data[NYC.data$BOROUGH == 'MANHATTAN', ]
Manhattan.data <- Manhattan.uncleaned.data

# Convert square feet to numeric values
Manhattan.data$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", Manhattan.data$LAND.SQUARE.FEET))
Manhattan.data$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", Manhattan.data$GROSS.SQUARE.FEET))

# Convert sale date to numeric
Manhattan.data$SALE.DATE <- mdy(Manhattan.data$SALE.DATE)
Manhattan.data$SALE.DATE.NUM <- as.numeric(Manhattan.data$SALE.DATE)

# Remove general outliers
Manhattan.data <- Manhattan.data %>%
  filter(!is.na(YEAR.BUILT) & !is.na(LAND.SQUARE.FEET) & 
           !is.na(SALE.DATE) & !is.na(SALE.PRICE) & !is.na(NEIGHBORHOOD)&
         !is.na(GROSS.SQUARE.FEET)& YEAR.BUILT > 1850 & LAND.SQUARE.FEET > 0 & 
           GROSS.SQUARE.FEET > 0 & 1000000 >= LAND.SQUARE.FEET & 
           1000000 >= GROSS.SQUARE.FEET & 900000000 >= SALE.PRICE & SALE.PRICE > 10000)

# Log cleaning for sale price, square feet attributes
Manhattan.data$LOG.SALE.PRICE <- log10(Manhattan.data$SALE.PRICE)
Manhattan.data$LOG.LAND.SQUARE.FEET <- log10(Manhattan.data$LAND.SQUARE.FEET)
Manhattan.data$LOG.GROSS.SQUARE.FEET <- log10(Manhattan.data$GROSS.SQUARE.FEET)




### EXPLORATORY DATA ANALYSIS ###

# Histogram for distribution of uncleaned Manhattan Sale Prices 
ggplot(Manhattan.uncleaned.data, aes(x = SALE.PRICE)) +
  geom_histogram(bins = 50, fill = "blue", color = "white") +
  labs(title = "Distribution of Manhattan Sale Prices (Before Cleaning)",
       x = "Sale Price (USD, log scale)",
       y = "Count")

# Histogram for distribution of clean Manhattan Sale Prices 
ggplot(Manhattan.data, aes(x = LOG.SALE.PRICE)) +
  geom_histogram(bins = 50, fill = "blue", color = "white") +
  labs(title = "Distribution of log10 Sale Price (After Cleaning)",
       x = "Sale Price (USD, log scale)",
       y = "Count")

# Boxplot of Sale price including outliers
boxplot(Manhattan.uncleaned.data$SALE.PRICE,
        main = "Boxplot of Sale Prices (Before Cleaning)",
        ylab = "Sale Price (USD)")

# Log-scale boxplot for better readability
boxplot(Manhattan.data$LOG.SALE.PRICE,
        main = "Boxplot of log10 Sale Price (After Cleaning)",
        ylab = "log10(Sale Price)")

# Sale Price vs Land Area
ggplot(Manhattan.data, aes(x = LOG.GROSS.SQUARE.FEET, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Sale Price vs Gross Square Feet (Manhattan)",
       x = "Gross Square Feet (log10 scale)",
       y = "Sale Price (log10 scale)")

# Sale Price vs Land Square Feet
ggplot(Manhattan.data, aes(x = LOG.LAND.SQUARE.FEET, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Sale Price vs Land Square Feet (Manhattan)",
       x = "Land Square Feet (log10 scale)",
       y = "Sale Price (log10 scale)")

# Sale Price vs Year Built
ggplot(Manhattan.data, aes(x = YEAR.BUILT, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Sale Price vs Year Built (Manhattan)",
       x = "Year Built",
       y = "Sale Price (log10 scale)") 

# Sale Price vs Sale Date
ggplot(Manhattan.data, aes(x = SALE.DATE.NUM, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.4, color = "purple") +
  labs(title = "Sale Price vs Sale Date (Manhattan)",
       x = "Sale Date (Numeric)",
       y = "Sale Price (log10 scale)") 



### REGRESSION ANALYSIS ###

#linear models using land square feet as predictor for sale price
lin.mod0 <- lm(LOG.SALE.PRICE~LOG.LAND.SQUARE.FEET,Manhattan.data)
summary(lin.mod0)

# linear models using gross square feet as predictor for sale price
lin.mod1 <- lm(LOG.SALE.PRICE~LOG.GROSS.SQUARE.FEET,Manhattan.data)
summary(lin.mod1)

# linear models using year built as predictor for sale price
lin.mod2 <- lm(LOG.SALE.PRICE~YEAR.BUILT,Manhattan.data)
summary(lin.mod2)

# linear models using numeric sale date as predictor for sale price
lin.mod3 <- lm(LOG.SALE.PRICE~SALE.DATE.NUM,Manhattan.data)
summary(lin.mod3)

# Linear model using both land square feet and gross square feet
lin.mod4 <- lm(LOG.SALE.PRICE ~ LOG.LAND.SQUARE.FEET + LOG.GROSS.SQUARE.FEET, data = Manhattan.data)
summary(lin.mod4)

# Linear model using both numeric sale date and year built
lin.mod5 <- lm(LOG.SALE.PRICE ~ SALE.DATE.NUM + YEAR.BUILT, data = Manhattan.data)
summary(lin.mod5)


# Linear model using land square feet, gross square feet, numeric sale date and year built
lin.mod6 <- lm(LOG.SALE.PRICE ~ LOG.LAND.SQUARE.FEET + LOG.GROSS.SQUARE.FEET + SALE.DATE.NUM + YEAR.BUILT, data = Manhattan.data)
summary(lin.mod6)

# Linear model using land square feet, gross square feet, numeric sale date and year built
lin.mod7 <- lm(LOG.SALE.PRICE ~ LOG.GROSS.SQUARE.FEET, data = Manhattan.data)
summary(lin.mod6)

# Linear model using land square feet, gross square feet, numeric sale date and year built
lin.mod8 <- lm(SALE.PRICE ~ LOG.LAND.SQUARE.FEET + LOG.GROSS.SQUARE.FEET + SALE.DATE.NUM + YEAR.BUILT, data = Manhattan.data)
summary(lin.mod6)

# Now we'll plot a scatterplot with regression line, and residuals for our best performing models, linmod1 and linmod6

# Scatterplot with regression line for linmod1
ggplot(data.frame(Actual = Manhattan.data$LOG.SALE.PRICE,
                  Fitted = lin.mod1$fitted.values),
       aes(x = Fitted, y = Actual)) +
  geom_point() +
  stat_smooth(method = "lm", )

# Residuals vs Fitted Values for linmod1
ggplot(data.frame(Fitted = lin.mod1$fitted.values, Residuals = lin.mod1$residuals),
       aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted Values (Model 1)",
       x = "Fitted Values",
       y = "Residuals")


# Scatterplot with regression line for linmod6
ggplot(data.frame(Actual = Manhattan.data$LOG.SALE.PRICE,
                  Fitted = lin.mod6$fitted.values),
       aes(x = Fitted, y = Actual)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "blue") +
  labs(title = "Actual vs Fitted Sale Prices (Model 6)",
       x = "Fitted Sale Price",
       y = "Actual Sale Price")

# Residuals vs Fitted Values for linmod6
ggplot(data.frame(Fitted = lin.mod6$fitted.values,
                  Residuals = lin.mod6$residuals),
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values (Model 6)",
       x = "Fitted Values",
       y = "Residuals")


### SUPERVISED LEARNING MODELS ###

# Knn Models

# First, knn model based on log sale price, 
model0Input <- scale(Manhattan.data[, c("LOG.SALE.PRICE")])
label <- Manhattan.data$NEIGHBORHOOD

knnModel0 <- knn(train = model0Input, test = model0Input, cl = label, k = 38)

# Print performance statistics
confusionMatrix0 <- table(Predicted = knnModel0, Actual = label)
correct <- sum(diag(confusionMatrix0)) 
total <- length(Manhattan.data$NEIGHBORHOOD)
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# Now doing knn model based on log gross square feet, log land square feet
model1Input <- scale(Manhattan.data[, c("LOG.GROSS.SQUARE.FEET", "LOG.LAND.SQUARE.FEET")])
label <- Manhattan.data$NEIGHBORHOOD
knnModel1 <- knn(train = model1Input, test = model1Input, cl = label, k = 38)

confusionMatrix1 <- table(Predicted = knnModel1, Actual = label)
correct <- sum(diag(confusionMatrix1)) 
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# Now doing knn model based on all 3
model2Input <- scale(Manhattan.data[, c("LOG.SALE.PRICE","LOG.GROSS.SQUARE.FEET", "LOG.LAND.SQUARE.FEET")])
label <- Manhattan.data$NEIGHBORHOOD
knnModel2 <- knn(train = model2Input, test = model2Input, cl = label, k = 38)

confusionMatrix2 <- table(Predicted = knnModel2, Actual = label)
correct <- sum(diag(confusionMatrix2)) 
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# Random Forest Models

# Convert NEIGHBORHOOD to factor
Manhattan.data$NEIGHBORHOOD <- as.factor(Manhattan.data$NEIGHBORHOOD)

# First random forest using log sale price
rf.model0 <- randomForest(NEIGHBORHOOD ~ LOG.SALE.PRICE, data = Manhattan.data, proximity=TRUE)
pred0 <- predict(rf.model0, Manhattan.data)
confusionMatrix0 <- table(Predicted = pred0, Actual = Manhattan.data$NEIGHBORHOOD)
correct <- sum(diag(confusionMatrix0))
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("RF Model 0 Accuracy:", round(accuracy, 3)))


# Next random forest using log gross square feet and log land square feet
rf.model1 <- randomForest(NEIGHBORHOOD ~ LOG.GROSS.SQUARE.FEET + LOG.LAND.SQUARE.FEET,
                          data = Manhattan.data, proximity = TRUE)
pred1 <- predict(rf.model1, Manhattan.data)
confusionMatrix1 <- table(Predicted = pred1, Actual = Manhattan.data$NEIGHBORHOOD)
correct1 <- sum(diag(confusionMatrix1))
accuracy1 <- correct1 / total
print(paste("Amount Correct:", correct1))
print(paste("RF Model 1 Accuracy:", round(accuracy1, 3)))

# Now doing using all 3
rf.model2 <- randomForest(NEIGHBORHOOD ~ LOG.SALE.PRICE + LOG.GROSS.SQUARE.FEET + LOG.LAND.SQUARE.FEET,
                          data = Manhattan.data, proximity = TRUE)
pred2 <- predict(rf.model2, Manhattan.data)
confusionMatrix2 <- table(Predicted = pred2, Actual = Manhattan.data$NEIGHBORHOOD)
correct2 <- sum(diag(confusionMatrix2))
accuracy2 <- correct2 / total
print(paste("Amount Correct:", correct2))
print(paste("RF Model 2 Accuracy:", round(accuracy2, 3)))



### QUESTION 2 ###

### DATASET CLEANING ###
Queens.uncleaned.data <- NYC.data[NYC.data$BOROUGH == 'QUEENS', ]
Queens.data <- Queens.uncleaned.data

# Convert square feet to numeric values
Queens.data$LAND.SQUARE.FEET <- as.numeric(gsub(",", "", Queens.data$LAND.SQUARE.FEET))
Queens.data$GROSS.SQUARE.FEET <- as.numeric(gsub(",", "", Queens.data$GROSS.SQUARE.FEET))

# Convert sale date to numeric
Queens.data$SALE.DATE <- mdy(Queens.data$SALE.DATE)
Queens.data$SALE.DATE.NUM <- as.numeric(Queens.data$SALE.DATE)

# Remove general outliers
Queens.data <- Queens.data %>%
  filter(!is.na(YEAR.BUILT) & !is.na(LAND.SQUARE.FEET) & 
           !is.na(SALE.DATE) & !is.na(SALE.PRICE) & !is.na(NEIGHBORHOOD)&
           !is.na(GROSS.SQUARE.FEET)& YEAR.BUILT > 1850 & LAND.SQUARE.FEET > 0 & 
           GROSS.SQUARE.FEET > 0 & 1000000 >= LAND.SQUARE.FEET & 
           1000000 >= GROSS.SQUARE.FEET & 900000000 >= SALE.PRICE & SALE.PRICE > 10000)

# Log cleaning for sale price, square feet attributes
Queens.data$LOG.SALE.PRICE <- log10(Queens.data$SALE.PRICE)
Queens.data$LOG.LAND.SQUARE.FEET <- log10(Queens.data$LAND.SQUARE.FEET)
Queens.data$LOG.GROSS.SQUARE.FEET <- log10(Queens.data$GROSS.SQUARE.FEET)

### APPLYING THE REGRESSION MODELS ###

#Applying lin.mod1
predictions <- predict(lin.mod1, newdata = Queens.data)
residuals <- Queens.data$LOG.SALE.PRICE - predictions


ggplot(Queens.data, aes(x = predictions, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "blue") +
  labs(title = "Predicted vs Actual Sale Price (Queens)",
       x = "Predicted LOG.SALE.PRICE",
       y = "Actual LOG.SALE.PRICE")

ggplot(data.frame(Predicted = predictions, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Predicted Values (Queens)",
       x = "Predicted LOG.SALE.PRICE",
       y = "Residuals")


#Applying lin.mod6
predictions <- predict(lin.mod6, newdata = Queens.data)
residuals <- Queens.data$LOG.SALE.PRICE - predictions


ggplot(Queens.data, aes(x = predictions, y = LOG.SALE.PRICE)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "blue") +
  labs(title = "Predicted vs Actual Sale Price (Queens)",
       x = "Predicted LOG.SALE.PRICE",
       y = "Actual LOG.SALE.PRICE")

ggplot(data.frame(Predicted = predictions, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Predicted Values (Queens)",
       x = "Predicted LOG.SALE.PRICE",
       y = "Residuals")


### APPLYING THE SUPERVISED LEARNING MODELS ###

# First applying best performing knn model 2
model2InputQueens <- scale(Queens.data[, c("LOG.SALE.PRICE","LOG.GROSS.SQUARE.FEET", "LOG.LAND.SQUARE.FEET")])
labelManhattan <- Manhattan.data$NEIGHBORHOOD
knnModel2Queens <- knn(train = model2Input, test = model2InputQueens, cl = labelManhattan, k = 38)

# Evaluate
confusionMatrixQueens <- table(Predicted = knnModel2Queens, Actual = Queens.data$NEIGHBORHOOD)
correct <- sum(diag(confusionMatrixQueens))
total <- nrow(Queens.data)
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("KNN Model 2 Accuracy on Queens:", round(accuracy, 3)))


# Now applying Random Forest
Queens.data$NEIGHBORHOOD <- as.factor(Queens.data$NEIGHBORHOOD)
predRFQueens <- predict(rf.model2, newdata = Queens.data)

# Evaluate
confusionMatrixRFQueens <- table(Predicted = predRFQueens, Actual = Queens.data$NEIGHBORHOOD)
correctRF <- sum(diag(confusionMatrixRFQueens))
total <- nrow(Queens.data)
accuracyRF <- correctRF / total
print(paste("Amount Correct:", correctRF))
print(paste("RF Model 2 Accuracy on Queens:", round(accuracyRF, 3)))

