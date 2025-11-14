library(ggplot2)
library(class)
library(dplyr)
library(lubridate)
library(randomForest)

### DATASET CLEANING ###
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
