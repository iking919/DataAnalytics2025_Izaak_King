####### Data Analytics Fall 2025 Lab 1 ######

library(ggplot2)

### set working directory
setwd("C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_king/lab2")

### read in data
nyHouse.data <- read.csv("NY-House-Dataset.csv")

Price <- nyHouse.data$PRICE



## summary stats
summary(Price)

fivenum(Price,na.rm=TRUE)



## created linear model of Price ~ PropertySqFt

lin.mod0 <- lm(PRICE~PROPERTYSQFT,nyHouse.data)
summary(lin.mod0)

## created linear model of Price ~ Bath + BedS

lin.mod1 <- lm(PRICE ~ BATH + BEDS,nyHouse.data)
summary(lin.mod1)

## created linear model of Price ~ PropertySqFt + Bath

lin.mod2 <- lm(PRICE ~ PROPERTYSQFT + BATH ,nyHouse.data)
summary(lin.mod2)

## created linear model of Price ~ PropertySqFt + Bath + Beds

lin.mod3 <- lm(PRICE ~ PROPERTYSQFT + BATH + BEDS,nyHouse.data)
summary(lin.mod3)



# remove the outliers in our Price data using boxplot Formula 
# one line method found in https://www.geeksforgeeks.org/r-language/outlier-analysis-in-r/ (step 3)

filtered.data <- nyHouse.data[!nyHouse.data$PRICE %in% boxplot.stats(nyHouse.data$PRICE)$out, ]

## created linear model of filtered Price ~ PropertySqFt

lin.mod4 <- lm(PRICE~PROPERTYSQFT,filtered.data)
summary(lin.mod4)

## created linear model of filterd Price ~ Bath + BedS

lin.mod5 <- lm(PRICE ~ BATH + BEDS,filtered.data)
summary(lin.mod5)

## created linear model of filtered price ~ PropertySqFt + Bath

lin.mod6 <- lm(PRICE ~ PROPERTYSQFT + BATH ,filtered.data)
summary(lin.mod6)

#####This is one we choose to plot####

ggplot(filtered.data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



## created linear model of filterd Price ~ PropertySqFt + Bath + Beds

lin.mod7 <- lm(PRICE ~ PROPERTYSQFT + BATH + BEDS,filtered.data)
summary(lin.mod7)


# remove the outliers in our all our used data, not just price, using the same boxplot Formula 

filtered.data <- filtered.data[!filtered.data$PROPERTYSQFT %in% boxplot.stats(filtered.data$PROPERTYSQFT)$out, ]

filtered.data <- filtered.data[!filtered.data$BATH %in% boxplot.stats(filtered.data$BATH)$out, ]

filtered.data <- filtered.data[!filtered.data$BEDS %in% boxplot.stats(filtered.data$BEDS)$out, ]

## created linear model of filtered Price ~ filted PropertySqFt

lin.mod8 <- lm(PRICE~PROPERTYSQFT,filtered.data)
summary(lin.mod8)

## created linear model of filterd Price ~ filted Bath + BedS

lin.mod9 <- lm(PRICE ~ BATH + BEDS,filtered.data)
summary(lin.mod9)

## created linear model of filtered price ~ filtered PropertySqFt + Bath

lin.mod10 <- lm(PRICE ~ PROPERTYSQFT + BATH ,filtered.data)
summary(lin.mod10)

## created linear model of filterd Price ~ PropertySqFt + Bath + Beds

lin.mod11 <- lm(PRICE ~ PROPERTYSQFT + BATH + BEDS,filtered.data)
summary(lin.mod11)



# Filter price data by log, rather than outliers

nyHouse.data$log_PRICE <- log10(nyHouse.data$PRICE)

## created linear model of Price ~ PropertySqFt

lin.mod12 <- lm(log_PRICE~PROPERTYSQFT,nyHouse.data)
summary(lin.mod12)

## created linear model of Price ~ Bath + BedS

lin.mod13 <- lm(log_PRICE ~ BATH + BEDS,nyHouse.data)
summary(lin.mod12)

## created linear model of Price ~ PropertySqFt + Bath

lin.mod14 <- lm(log_PRICE ~ PROPERTYSQFT + BATH ,nyHouse.data)
summary(lin.mod14)

## created linear model of Price ~ PropertySqFt + Bath + Beds

lin.mod15 <- lm(log_PRICE ~ PROPERTYSQFT + BATH + BEDS,nyHouse.data)
summary(lin.mod15)

#####This is one we choose to plot####

ggplot(filtered.data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod15, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



# Filter price by log, with outliers already removed from data

filtered.data$log_PRICE <- log10(filtered.data$PRICE)

## created linear model of filtered and logged Price ~  filtered PropertySqFt

lin.mod16 <- lm(log_PRICE~PROPERTYSQFT,filtered.data)

summary(lin.mod16)

## created linear model of filtered and logged Price ~ filtered Bath + Beds

lin.mod17 <- lm(log_PRICE~BATH + BEDS,filtered.data)

summary(lin.mod17)

## created linear model of filtered and logged Price ~ PropertySqFt + Bath

lin.mod18 <- lm(log_PRICE~PROPERTYSQFT + BATH,filtered.data)

summary(lin.mod18)

#####This is one we choose to plot####

ggplot(filtered.data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod18, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

## created linear model of filtered and logged Price ~ PropertySqFt + Bath + Beds

lin.mod19 <- lm(log_PRICE~PROPERTYSQFT + BATH + BEDS,filtered.data)

summary(lin.mod19)



