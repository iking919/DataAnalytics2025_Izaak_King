library(ggplot2)
library(class)
library(dplyr)
library(lubridate)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(GGally)


### DATASET READING ###

# Set Column Names
colnames <- c(
  "age",
  "workclass",
  "fnlwgt",
  "education",
  "education_num",
  "marital_status",
  "occupation",
  "relationship",
  "race",
  "sex",
  "capital_gain",
  "capital_loss",
  "hours_per_week",
  "native_country",
  "income"
)

CensusIncome.data <- read.csv(
  "C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_King/assignment6/census+income/adult.data",
  header = FALSE,
  col.names = colnames
)

CensusIncome.test.data <- read.csv(
  "C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_King/assignment6/census+income/adult.test",
  header = FALSE,
  col.names = colnames
)

### EXPLORATORY DATA ANALYSIS ###

# Summaries
continuous_vars <- c("age", "education_num", "hours_per_week")
categorical_vars <- c("sex", "race", "workclass", "income")

summary(CensusIncome.data[, continuous_vars])


# Data Cleaning

# Remove samples with missing data
CensusIncome.data <- CensusIncome.data[complete.cases(CensusIncome.data[, continuous_vars]), ]
CensusIncome.data <- CensusIncome.data[complete.cases(CensusIncome.data[, categorical_vars]), ]
CensusIncome.data <- CensusIncome.data[complete.cases(CensusIncome.data$income), ]
CensusIncome.data$income <- gsub("\\.", "", CensusIncome.data$income)
CensusIncome.data$income <- as.factor(trimws(CensusIncome.data$income))


CensusIncome.test.data <- CensusIncome.test.data[complete.cases(CensusIncome.test.data[, continuous_vars]), ]
CensusIncome.test.data <- CensusIncome.test.data[complete.cases(CensusIncome.test.data[, categorical_vars]), ]
CensusIncome.test.data <- CensusIncome.test.data[complete.cases(CensusIncome.test.data$income), ]
CensusIncome.test.data$income <- gsub("\\.", "", CensusIncome.test.data$income)
CensusIncome.test.data$income <- as.factor(trimws(CensusIncome.test.data$income))

numeric_vars <- c("age", "education_num", "hours_per_week")

# Convert to numeric in both train and test sets
CensusIncome.data[numeric_vars] <- lapply(CensusIncome.data[numeric_vars], function(x) as.numeric(as.character(x)))
CensusIncome.test.data[numeric_vars] <- lapply(CensusIncome.test.data[numeric_vars], function(x) as.numeric(as.character(x)))

for(col in categorical_vars) {
  CensusIncome.data[[col]] <- as.numeric(as.factor(CensusIncome.data[[col]]))
  CensusIncome.test.data[[col]] <- as.numeric(as.factor(CensusIncome.test.data[[col]]))
}
CensusIncome.data$income <- factor(trimws(CensusIncome.data$income))
CensusIncome.test.data$income <- factor(trimws(CensusIncome.test.data$income))
levels(CensusIncome.data$income) <- c("<=50K", ">50K")
levels(CensusIncome.test.data$income) <- c("<=50K", ">50K")


# Filtering out continuous outliers using IQR
for (var in continuous_vars) {
  
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(CensusIncome.data[[var]], 0.25)
  Q3 <- quantile(CensusIncome.data[[var]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  # Filter the dataset
  CensusIncome.data <- CensusIncome.data[CensusIncome.data[[var]] >= lower & 
                                           CensusIncome.data[[var]] <= upper, ]
}

# Creating histograms and box plots for quantitative/continuous data

continuous_vars <- c("age", "education_num", "hours_per_week")

# Age
hist(CensusIncome.data$age,
     main="Histogram of Age",
     xlab="Age",
     col="skyblue",
     breaks=20)

# Education_num
hist(CensusIncome.data$education_num,
     main="Histogram of Education_num",
     xlab="Education_num",
     col="skyblue",
     breaks=16)

# Hours_per_week
hist(CensusIncome.data$hours_per_week,
     main="Histogram of Hours per Week",
     xlab="Hours per Week",
     col="skyblue",
     breaks=20)


# Box Plots
ggplot(CensusIncome.data, aes(x = income, y = age, fill = income)) +
  geom_boxplot() +
  ggtitle("Age vs Income Category")

ggplot(CensusIncome.data, aes(x = income, y = education_num, fill = income)) +
  geom_boxplot() +
  ggtitle("Education Level vs Income Category")

ggplot(CensusIncome.data, aes(x = income, y = hours_per_week, fill = income)) +
  geom_boxplot() +
  ggtitle("Hours Worked vs Income Category")


# Density Plots
ggplot(CensusIncome.data, aes(x = age, fill = income)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density of Age by Income")

ggplot(CensusIncome.data, aes(x = education_num, fill = income)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density of Education_num by Income")

ggplot(CensusIncome.data, aes(x = hours_per_week, fill = income)) +
  geom_density(alpha = 0.4) +
  ggtitle("Density of Hours per Week by Income")


# Creating bar plots for distributions of categorical variables

ggplot(CensusIncome.data, aes(x = sex)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Sex") +
  ylab("Count") +
  xlab("Sex")

ggplot(CensusIncome.data, aes(x = race)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Race") +
  ylab("Count") +
  xlab("Race")

ggplot(CensusIncome.data, aes(x = workclass)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Workclass") +
  ylab("Count") +
  xlab("Workclass")

ggplot(CensusIncome.data, aes(x = income)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Workclass") +
  ylab("Count") +
  xlab("Income")

# Creating plots for the proportions given categorical variables

ggplot(CensusIncome.data, aes(x = sex, fill = income)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Income Proportions by Sex") +
  ylab("Percentage") +
  xlab("Sex")

ggplot(CensusIncome.data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Income Proportions by Race") +
  ylab("Percentage") +
  xlab("Race")

ggplot(CensusIncome.data, aes(x = workclass, fill = income)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Income Proportions by Workclass") +
  ylab("Percentage") +
  xlab("Workclass")



### MODEL DEVELOPMENT AND VALIDATION

# Random Forest Model
rf.model <- randomForest(
  income ~ age + education_num + hours_per_week + sex + race + workclass,
  data = CensusIncome.data,
  classwt = c("<=50K" = 1, ">50K" = 2)  # weight minority class higher
)


# predict class labels
rf.predicted <- predict(rf.model, CensusIncome.test.data)

# confusion matrix/contingency table
cm <- table(
  predicted = rf.predicted,
  actual = CensusIncome.test.data$income
)
cm

# Accuracy, Precision, Recall, F1
accuracy <- sum(diag(cm)) / sum(cm)

positive_class <- ">50K"

TP <- cm[positive_class, positive_class]
FP <- sum(cm[positive_class, ]) - TP
FN <- sum(cm[, positive_class]) - TP

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)

metrics <- data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1 = f1
)

metrics

# Decision tree model 
tree.model <- rpart(income ~ age + education_num + hours_per_week + sex + race + workclass,
                    CensusIncome.data, method = "class")

tree.model

# plotting the decision tree model using rpart.plot() function 
rpart.plot(tree.model)

tree.model.predicted <-  predict(tree.model, CensusIncome.test.data, type = "class")

# confusion matrix/contingency table
cm <- table(
  predicted = tree.model.predicted,
  actual = CensusIncome.test.data$income
)
cm

# Accuracy, Precision, Recall, F1
accuracy <- sum(diag(cm)) / sum(cm)

positive_class <- ">50K"

TP <- cm[positive_class, positive_class]
FP <- sum(cm[positive_class, ]) - TP
FN <- sum(cm[, positive_class]) - TP

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)

metrics <- data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1 = f1
)

metrics


# kNN model

inputs <- c("age", "education_num", "hours_per_week","sex", "race","workclass")
knn_predicted <- knn(train = CensusIncome.data[, inputs],
                     test = CensusIncome.test.data[, inputs],
                     cl = CensusIncome.data$income,
                     k = 2)

# confusion matrix/contingency table
cm <- table(
  predicted = knn_predicted,
  actual = CensusIncome.test.data$income
)
cm

# Accuracy, Precision, Recall, F1
accuracy <- sum(diag(cm)) / sum(cm)

positive_class <- 2

TP <- cm[positive_class, positive_class]
FP <- sum(cm[positive_class, ]) - TP
FN <- sum(cm[, positive_class]) - TP

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)

metrics <- data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1 = f1
)

metrics

