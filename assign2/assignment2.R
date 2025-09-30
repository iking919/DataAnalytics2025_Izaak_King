library(readr)
library(EnvStats)
library(nortest)

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

lin.mod0 <- lm(HLT.new~gdp,epi.data)
summary(lin.mod0)


# remove the outliers in our population data using boxplot Formula 
# one line method found in https://www.geeksforgeeks.org/r-language/outlier-analysis-in-r/ (step 3)

filtered.data <- epi.data[!epi.data$population %in% boxplot.stats(epi.data$population)$out, ]

