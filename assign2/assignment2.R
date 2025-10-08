library(readr)
library(EnvStats)
library(nortest)
library(ggplot2)
library(class)

# set working directory (relative path)
setwd("C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_king/assign2")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp_v2.csv")

#Extract HLT data for each region
epi.data.GlobalWest <- epi.data[epi.data$region == "Global West", ]
epi.data.EasternEurope <- epi.data[epi.data$region == "Eastern Europe", ]

HLT.GlobalWest <- epi.data.GlobalWest$HLT.new
HLT.EasternEurope <- epi.data.EasternEurope$HLT.new

# get two boxplots for each set of data
boxplot(HLT.GlobalWest, HLT.EasternEurope, names = c("HLT for Global West", "HLT for Eastern Europe"))

# construct each histogram with density curve
x <- seq(60., 90., 5)
hist(HLT.GlobalWest, x, prob=TRUE, main = "Histogram of HLT in the Global West")
d1 <- dnorm(x,mean=mean(HLT.GlobalWest), sd=sd(HLT.GlobalWest),log=FALSE)
lines(x,d1)

y <- seq(35., 65., 5)
hist(HLT.EasternEurope, y, prob=TRUE, main = "Histogram of HLT in Eastern Europe")
d2 <- dnorm(y,mean=mean(HLT.EasternEurope), sd=sd(HLT.EasternEurope),log=FALSE)
lines(y,d2)

# construct qq plot of our datasets
qqplot(HLT.GlobalWest, HLT.EasternEurope,
       xlab = "HLT of Global West",
       ylab = "HLT of Eastern Europe",
       main = "Q-Q Plot of HLT in Global West vs Eastern Europe")

#First linear models using HLT as response of population, and gdp
lin.mod0 <- lm(HLT.new~population,epi.data)
summary(lin.mod0)

lin.mod1 <- lm(HLT.new~gdp,epi.data)
summary(lin.mod1)


# remove the outliers in our population data using boxplot Formula 
# one line method found in https://www.geeksforgeeks.org/r-language/outlier-analysis-in-r/ (step 3)

filteredPop.data <- epi.data[!epi.data$population %in% boxplot.stats(epi.data$population)$out, ]
lin.mod2 <- lm(HLT.new~population, filteredPop.data)
summary(lin.mod2)

filteredGDP.data <- epi.data[!epi.data$gdp %in% boxplot.stats(epi.data$gdp)$out, ]
lin.mod3 <- lm(HLT.new~gdp, filteredGDP.data)
summary(lin.mod3)


# now filter data using log

epi.data$log_Population <- log10(epi.data$population)
lin.mod4 <- lm(HLT.new~log_Population, epi.data)
summary(lin.mod4)

epi.data$log_GDP <- log10(epi.data$gdp)
lin.mod5 <- lm(HLT.new~log_GDP, epi.data)
summary(lin.mod5)


#Plot our two best models, lin.mod3 and lin.mod4.

ggplot(epi.data, aes(x = log_Population, y = HLT.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

ggplot(filteredGDP.data, aes(x = gdp, y = HLT.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# now make models for Eastern Europe

# first using log population 

epi.data.EasternEurope$log_Population <- log10(epi.data.EasternEurope$population)
lin.mod6 <- lm(HLT.new~log_Population, epi.data.EasternEurope)
summary(lin.mod6)
ggplot(epi.data.EasternEurope, aes(x = log_Population, y = HLT.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# now using GDP with no outliers

filteredGDP.data.EasternEurope <- epi.data.EasternEurope[!epi.data.EasternEurope$gdp %in% boxplot.stats(epi.data.EasternEurope$gdp)$out, ]
lin.mod7 <- lm(HLT.new~gdp, filteredGDP.data.EasternEurope)
summary(lin.mod7)
ggplot(filteredGDP.data.EasternEurope, aes(x = gdp, y = HLT.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod7, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


# now doing knn-plots

modelInputs <- scale(epi.data[, c("BDH.new", "AIR.new", "PCC.new")])
label <- epi.data$region

knnModel0 <- knn(train = modelInputs, test = modelInputs, cl = label, k = 3)

confusionMatrix0 <- table(Predicted = knnModel0, Actual = label)
print(confusionMatrix0)

# we can sum the diagonal to get correctly predicted regions

correct <- sum(diag(confusionMatrix0)) 

# we can sum all points total for entire amount of regions

total <- length(epi.data$region)

accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))

# repeat knn model with k = 5

knnModel1 <- knn(train = modelInputs, test = modelInputs, cl = label, k = 5)
confusionMatrix1 <- table(Predicted = knnModel1, Actual = label)
print(confusionMatrix1)
correct <- sum(diag(confusionMatrix1))
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))

# repeat knn model with k = 7

knnModel2 <- knn(train = modelInputs, test = modelInputs, cl = label, k = 7)
confusionMatrix2 <- table(Predicted = knnModel2, Actual = label)
print(confusionMatrix2)
correct <- sum(diag(confusionMatrix2))
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# now doing 3 new variables

modelInputs1 <- scale(epi.data[, c("AGR.new", "WRS.new", "H2O.new")])
knnModel3 <- knn(train = modelInputs1, test = modelInputs1, cl = label, k = 3)
confusionMatrix3 <- table(Predicted = knnModel3, Actual = label)
print(confusionMatrix3)
correct <- sum(diag(confusionMatrix3)) 
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# repeat knn model with k = 5

knnModel4 <- knn(train = modelInputs1, test = modelInputs1, cl = label, k = 5)
confusionMatrix4 <- table(Predicted = knnModel4, Actual = label)
print(confusionMatrix4)
correct <- sum(diag(confusionMatrix4))
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))

# repeat knn model with k = 7

knnModel5 <- knn(train = modelInputs1, test = modelInputs1, cl = label, k = 7)
confusionMatrix5 <- table(Predicted = knnModel5, Actual = label)
print(confusionMatrix5)
correct <- sum(diag(confusionMatrix5))
accuracy <- correct / total
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))



