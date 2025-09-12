library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_king/lab1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

#Initialize ECO and BDH variables
ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new

#Get summaries for both variables
summary(ECO)
summary(BDH)

# boxplot of our data
boxplot(ECO, BDH, names = c("ECO", "BDH"))


#Histogram of our ECO data
x <- seq(20., 90., 5)
hist(ECO, x, prob=TRUE)

#Add our overlayed normal distribution
d1 <- dnorm(x,mean=mean(ECO), sd=sd(ECO),log=FALSE)
lines(x,d1)


#Now repeated process to create BDH data histogram
y <- seq(5., 90., 5)
hist(BDH, y, prob=TRUE)
d2 <- dnorm(y,mean=mean(BDH), sd=sd(BDH),log=FALSE)
lines(y,d2)




# plot ecdfs for both ECO and BDH
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 



# print QQ plots for both ECO and BDH with theoretical normal distribuion
qqnorm(ECO); qqline(ECO)
qqnorm(BDH); qqline(BDH)

# print quantile-quantile plot for ECO against BDH
qqplot(ECO, BDH, xlab = "Q-Q plot for ECO vs BDH")



## Statistical Tests

#Normality tests
shapiro.test(ECO)
shapiro.test(BDH)

ad.test(ECO)
ad.test(BDH)

#Statistical tests for the variables having the same distribution
ks.test(ECO,BDH)

wilcox.test(ECO,BDH)