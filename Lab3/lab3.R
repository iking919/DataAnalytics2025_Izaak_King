library(ggplot2)
library(class)
library(GGally)
library(cluster)
library(factoextra)


####################################
##### Abalone Data Preparation #####
####################################

# read dataset
abalone.data <- read.csv("C:/Users/kingi/Documents/github/DataAnalytics2025_Izaak_king/lab3/abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"



# Building kNN models

# Start with sqrt of sample size for k

kVal <- 65

# The first model will use length, diameter, height as inputs

modelInputs <- scale(abalone.data[, c("length", "diameter", "height")])
label <- abalone.data$age.group
knnModel0 <- knn(train = modelInputs, test = modelInputs, cl = label, k = kVal)
confusionMatrix0 <- table(Predicted = knnModel0, Actual = label)

# We can sum the diagonal to get correctly predicted regions

correct <- sum(diag(confusionMatrix0)) 

# We can sum all points total for entire amount of regions

total <- length(abalone.data$age.group)

accuracy <- correct / total

# We print the contingency table as well as accuracy

print(confusionMatrix0)

print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# We will now build and compare the second model, using 
# whole_weight, shucked_wieght, viscera_wieght, shell_weight

model1Inputs <- scale(abalone.data[, c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")])
knnModel1 <- knn(train = model1Inputs, test = model1Inputs, cl = label, k = kVal)
confusionMatrix1 <- table(Predicted = knnModel1, Actual = label)
correct <- sum(diag(confusionMatrix1)) 
accuracy <- correct / total
print(confusionMatrix1)
print(paste("Amount Correct:", correct))
print(paste("Accuracy:", round(accuracy, 3)))


# Model 1 performs better so we try it with multiple k values

k.list <- c(45, 50, 55, 60, 65, 70, 75, 80, 85)

TopAccuracy <- 0
TopkVal <- 0

for (kval in k.list) {
  knnModel <- knn(train = model1Inputs, test = model1Inputs, cl = label, k = kVal)
  confusionMatrix <- table(Predicted = knnModel, Actual = label)
  correct <- sum(diag(confusionMatrix)) 
  accuracy <- correct / total
  print(paste("k-value:", kval, "Accuracy:", accuracy))
  
  if (accuracy > TopAccuracy){
    TopAccuracy <- accuracy
    TopkVal <- kval
  }
  
}
print(paste("Best performing k-value:", TopkVal))



# Now we will train k-means model using the best inputs, from model1.
# We will do this for our range of k once again
# For loop derived from class example


wcss.list <- c()
si.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(model1Inputs, centers = k)
  wcss <- abalone.km$tot.withinss
  wcss.list <- c(wcss.list,wcss)
  si <- silhouette(abalone.km$cluster, dist(model1Inputs))
  avg.si <- mean(si[, 3])  
  si.list <- c(si.list,avg.si)
  print(paste("k =", k, "WCSS =", wcss, "Avg SI =", avg.si))
  
  
}

# Find "best" k based on wcss, si
best.k.wcss <- k.list[which.min(wcss.list)]
best.k.si  <- k.list[which.max(si.list)]

print(paste("Best k by wcss:", best.k.wcss))
print(paste("Best k by silhouette:", best.k.si))

# Plot silhouette with best by wcss

km.wcss <- kmeans(model1Inputs, centers = best.k.wcss)
si.wcss <- silhouette(km.wcss$cluster, dist(model1Inputs))
fviz_silhouette(si.wcss)

# Plot silhouette with best by silhouette width

km.si <- kmeans(model1Inputs, centers = best.k.si)
si.si <- silhouette(km.si$cluster, dist(model1Inputs))
fviz_silhouette(si.si)




# Now we will train CLARA (PAM) model using the best inputs, from model1.
# We will do this for our range of k once again
# For loop derived from class example


## run tests with multiple k values and plot sum of dissimilarities (sum of distances)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  abalone.clara <- clara(model1Inputs, k)
  
  # Calculate sum of dissimilarities manually
  sumdiss <- sum(abalone.clara$clustering %>% 
                   sapply(function(cl) {
                     sum(dist(model1Inputs[abalone.clara$clustering == cl, ]) / 2)
                   }))
  sumdiss.list <- c(sumdiss.list, sumdiss)
  
  # Silhouette
  si <- silhouette(abalone.clara$cluster, dist(model1Inputs))
  avg.si <- mean(si[, 3])
  si.list <- c(si.list, avg.si)
  
  print(paste("k =", k, "Sum Diss =", sumdiss, "Avg SI =", avg.si))
}

# Find best k
best.k.sumdiss <- k.list[which.min(sumdiss.list)]
best.k.si <- k.list[which.max(si.list)]

print(paste("Best k by sum of dissimilarities:", best.k.sumdiss))
print(paste("Best k by silhouette:", best.k.si))

# Plot silhouette for best by sum diss
clara.sumdiss <- clara(model1Inputs, best.k.sumdiss)
si.sumdiss <- silhouette(clara.sumdiss$cluster, dist(model1Inputs))
fviz_silhouette(si.sumdiss)

# Plot silhouette for best by silhouette width
clara.si <- clara(model1Inputs, best.k.si)
si.si <- silhouette(clara.si$cluster, dist(model1Inputs))
fviz_silhouette(si.si)
