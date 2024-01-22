# BUA751 - MLB Project (Group 1): Energy Efficiency

# Load packages
library(tidyverse)
library(ggplot2)
library(GGally)
library(psych)
library(car)
library(neuralnet)
library(e1071)
library(caret)
library(class)
library(gmodels)

ggpairs(EnergyEfficiency[,c(1:10)])
pairs.panels(EnergyEfficiency[1:10], digits = 3, pch = 21, lm=TRUE, ellipses = FALSE)

# y1 - heating load
ggpairs(EnergyEfficiency[,c(1:9)])
# or
pairs.panels(EnergyEfficiency[1:9], digits = 3, pch = 21, lm=TRUE, ellipses = FALSE) # Y1 - Heating load

# y2 - Cooling load
coolingload <- EnergyEfficiency
coolingload <- coolingload[, -c(9)]
ggpairs(coolingload[,c(1:9)])
# or
pairs.panels(coolingload[1:9], digits = 3, pch = 21, lm=TRUE, ellipses = FALSE) # Y2 - Cooling load

# Cut plots
pairs.panels(EnergyEfficiency[1:5], digits = 3, pch = 21, lm=TRUE, ellipses = FALSE)
pairs.panels(EnergyEfficiency[6:10], digits = 3, pch = 21, lm=TRUE, ellipses = FALSE)

# Histograms
hist(EnergyEfficiency$X1)
hist(EnergyEfficiency$X2)
hist(EnergyEfficiency$X3)
hist(EnergyEfficiency$X4)
hist(EnergyEfficiency$X5)
hist(EnergyEfficiency$X6)
hist(EnergyEfficiency$X6_East)
hist(EnergyEfficiency$X6_South)
hist(EnergyEfficiency$X6_West)
hist(EnergyEfficiency$X7)
hist(EnergyEfficiency$X8)
hist(EnergyEfficiency$X8_North)
hist(EnergyEfficiency$X8_East)
hist(EnergyEfficiency$X8_South)
hist(EnergyEfficiency$X8_West)
hist(EnergyEfficiency$Y1)
hist(EnergyEfficiency$Y2)

# Stats
describe(EnergyEfficiency)
summary(EnergyEfficiency)

# Alternatively, you can use individual functions for specific statistics
mean_values <- sapply(EnergyEfficiency, mean)
median_values <- sapply(EnergyEfficiency, median)
sd_values <- sapply(EnergyEfficiency, sd)
min_values <- sapply(EnergyEfficiency, min)
max_values <- sapply(EnergyEfficiency, max)

# Combine the results into a data frame
descriptive_stats <- data.frame(
  Mean = mean_values,
  Median = median_values,
  Standard_Deviation = sd_values,
  Min = min_values,
  Max = max_values
)

# Print the descriptive statistics
print(descriptive_stats)

# Correlation
cor(EnergyEfficiency)

# Linear Models
lmOut1 <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = EnergyEfficiency)
summary(lmOut1)

lmOut2 <- lm(Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = EnergyEfficiency)
summary(lmOut2)

# Non N/A variables only (without X4)
lmOut1a <- lm(Y1 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = EnergyEfficiency)
summary(lmOut1a)

lmOut2a <- lm(Y2 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = EnergyEfficiency)
summary(lmOut2a)

# VIF Analysis

alias(lmOut1)

# Using vif function for Variance inflation factor analysis
viftable1 <- vif(lmOut1a)
viftable1

viftable2 <- vif(lmOut2a)
viftable2

# Sort the table in desceasing oder
sorttable1 <- sort(viftable1,decreasing=TRUE)
sorttable1

sorttable2 <- sort(viftable2,decreasing=TRUE)
sorttable2

# Removing variables after VIF Analysis
EnergyEfficiency_df <- EnergyEfficiency[,c("X1", "X2", "X3", "X4", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

# Quantile
quantile(EnergyEfficiency_df$Y1)
quantile(EnergyEfficiency_df$Y2)

# Assiging variables (A,B,C,D)
#converting values of Y1
categories <- c("A","B","C","D")
ranges <- c(-Inf,12.99,19.00,31.68,Inf)
EnergyEfficiency_df$Y1 <- cut(EnergyEfficiency_df$Y1, breaks = ranges, labels = categories, include.lowest = TRUE)

#Converting values of Y2
categories <- c("A","B","C","D")
ranges <- c(-Inf,15.61,22.09,33.14,Inf)
EnergyEfficiency_df$Y2 <- cut(EnergyEfficiency_df$Y2, breaks = ranges, labels = categories, include.lowest = TRUE)


# Assiging variables (A,B) to 1,(C,D) to -1
# Function to assign numeric values based on predefined categories
assign_numeric_values <- function(category) {
  ifelse(category %in% c("A", "B"), 1, ifelse(category %in% c("C", "D"), -1, NA))
}

# Assign numeric values to Y1 and Y2 based on predefined categories
EnergyEfficiency_df$Y1_new <- assign_numeric_values(EnergyEfficiency_df$Y1)
EnergyEfficiency_df$Y2_new <- assign_numeric_values(EnergyEfficiency_df$Y2)


# Perceptrons
# Perceptron for Project - MLB
EnergyEfficiency_df <- EnergyEfficiency[, c("X1", "X2", "X3", "X4", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

# Assigning variables (A, B, C, D)
# converting values of Y1
categories <- c("A","B","C","D")
ranges <- c(-Inf, 12.99, 19.00, 31.68, Inf)
EnergyEfficiency_df$Y1 <- cut(EnergyEfficiency_df$Y1, breaks = ranges, labels = categories, include.lowest = TRUE)

# Converting values of Y2
categories <- c("A","B","C","D")
ranges <- c(-Inf, 15.61, 22.09, 33.14, Inf)
EnergyEfficiency_df$Y2 <- cut(EnergyEfficiency_df$Y2, breaks = ranges, labels = categories, include.lowest = TRUE)

# Assigning variables (A, B) to 1, (C, D) to -1
# Function to assign numeric values based on predefined categories
assign_numeric_values <- function(category) {
  ifelse(category %in% c("A", "B"), 1, ifelse(category %in% c("C", "D"), -1, NA))
}

# Assign numeric values to Y1 and Y2 based on predefined categories
EnergyEfficiency_df$Y1 <- assign_numeric_values(EnergyEfficiency_df$Y1)
EnergyEfficiency_df$Y2 <- assign_numeric_values(EnergyEfficiency_df$Y2)

# Create training and testing data
percep_index <- sample(nrow(EnergyEfficiency_df), 0.7 * nrow(EnergyEfficiency_df), replace = FALSE)
percep_train <- EnergyEfficiency_df[percep_index, ]
percep_test <- EnergyEfficiency_df[-percep_index, ]

# Creating the Perceptron Learning Algorithm
perceptron <- function(X, y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10)  # Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for (j in 1:numEpochs) {
    predictedResult <- numeric(length = 100)  # Initalize predictedResult vector
    numIncorrect = 0  # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for (i in 1:length(y)) {
      xi = as.numeric(unlist(X[i, ]))  # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi)  # Predict the point
      
      # If predicted point is incorrect - change weight
      if (predictedResult[i] != y[i]) {
        numIncorrect = numIncorrect + 1  # Add one to # of missclassified points
        w <- w + as.numeric(y[i]) * xi  # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
  
  # Return the learned weights
  return(w)
}

# Set seed for reproducibility
set.seed(7)

# Seperating input and output variables
X <- percep_train[, c("X1", "X2", "X3", "X4", 
                      "X5", "X7", "X6_North", 
                      "X6_East", "X6_South", 
                      "X8_Uniform","X8_North",
                      "X8_East","X8_South", 
                      "X8_zero")]
Y1 <- percep_train$Y1
Y2 <- percep_train$Y2

# Run the perceptron algorithm for Y1 and Y2 for 5 times
for (i in 1:5) {
  cat("\nTraining Perceptron - Iteration: ", i)
  
  # Train perceptron for Y1
  weights_Y1 <- perceptron(X, Y1, 8)
  
  # Train perceptron for Y2
  weights_Y2 <- perceptron(X, Y2, 8)
  
  # Checking accuracy for Y1
  percep_test$predict1 <- rowSums(percep_test[, c("X1", "X2", "X3", "X4", "X5", "X7", 
                                                  "X6_North", "X6_East", "X6_South", 
                                                  "X8_Uniform","X8_North","X8_East","X8_South", 
                                                  "X8_zero", "Y1","Y2")] * weights_Y1[1:8])
  perceptronpredicttable_Y1 <- table(percep_test$Y1 == 1, percep_test$predict1 > 0) + 
                                      table(percep_test$Y1 == -1, percep_test$predict1 < 0)
  accuracy_Y1 <- sum(diag(perceptronpredicttable_Y1)) / sum(perceptronpredicttable_Y1)
  cat("\nAccuracy for Y1 - Iteration ", i, ": ", accuracy_Y1)
  
  # Checking accuracy for Y2
  percep_test$predict2 <- rowSums(percep_test[, c("X1", "X2", "X3", "X4", "X5", "X7", 
                                                  "X6_North", "X6_East", "X6_South", 
                                                  "X8_Uniform","X8_North","X8_East","X8_South", 
                                                  "X8_zero", "Y1","Y2")] * weights_Y2[1:8])
  perceptronpredicttable_Y2 <- table(percep_test$Y2 == 1, percep_test$predict2 > 0) + 
                                      table(percep_test$Y2 == -1, percep_test$predict2 < 0)
  accuracy_Y2 <- sum(diag(perceptronpredicttable_Y2)) / sum(perceptronpredicttable_Y2)
  cat("\nAccuracy for Y2 - Iteration ", i, ": ", accuracy_Y2)
}


# Run the perceptron algorithm for Y1 and Y2 for 5 times
for (i in 1:5) {
  cat("\nTraining Perceptron - Iteration: ", i)
  
  # Train perceptron for Y1
  weights_Y1 <- perceptron(X, Y1, 8)
  
  # Train perceptron for Y2
  weights_Y2 <- perceptron(X, Y2, 8)
  
  # Checking accuracy for Y1
  percep_test$predict1 <- rowSums(percep_test[, c("X1", "X2", "X3", "X4", "X5", "X7", 
                                                  "X6_North", "X6_East", "X6_South", 
                                                  "X8_Uniform","X8_North","X8_East","X8_South", 
                                                  "X8_zero")] * weights_Y1[1:8])
  perceptronpredicttable_Y1 <- table(percep_test$Y1 == 1, percep_test$predict1 > 0) + 
    table(percep_test$Y1 == -1, percep_test$predict1 < 0)
  accuracy_Y1 <- sum(diag(perceptronpredicttable_Y1)) / sum(perceptronpredicttable_Y1)
  cat("\nAccuracy for Y1 - Iteration ", i, ": ", accuracy_Y1)
  
  # Checking accuracy for Y2
  percep_test$predict2 <- rowSums(percep_test[, c("X1", "X2", "X3", "X4", "X5", "X7", 
                                                  "X6_North", "X6_East", "X6_South", 
                                                  "X8_Uniform","X8_North","X8_East","X8_South", 
                                                  "X8_zero")] * weights_Y2[1:8])
  perceptronpredicttable_Y2 <- table(percep_test$Y2 == 1, percep_test$predict2 > 0) + 
    table(percep_test$Y2 == -1, percep_test$predict2 < 0)
  accuracy_Y2 <- sum(diag(perceptronpredicttable_Y2)) / sum(perceptronpredicttable_Y2)
  cat("\nAccuracy for Y2 - Iteration ", i, ": ", accuracy_Y2)
}



# SVM - Spport Vector Machines
svm_EnergyEfficiency_df <- EnergyEfficiency[, c("X1", "X2", "X3", "X4", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

# Assigning variables (A, B, C, D)
# converting values of Y1
categories <- c("A","B","C","D")
ranges <- c(-Inf, 12.99, 19.00, 31.68, Inf)
svm_EnergyEfficiency_df$Y1 <- cut(svm_EnergyEfficiency_df$Y1, breaks = ranges, labels = categories, include.lowest = TRUE)

# Converting values of Y2
categories <- c("A","B","C","D")
ranges <- c(-Inf, 15.61, 22.09, 33.14, Inf)
svm_EnergyEfficiency_df$Y2 <- cut(svm_EnergyEfficiency_df$Y2, breaks = ranges, labels = categories, include.lowest = TRUE)


svm_df <- svm_EnergyEfficiency_df
#svm_df <- svm_df[, -c(17, 18)]

# Convert the  variables into factor
svm_df$Y1 <- as.factor(svm_df$Y1)
svm_df$Y2 <- as.factor(svm_df$Y2)

# Create training and testing data
svm_index <- sample(nrow(svm_df), 0.7*nrow(svm_df), replace = FALSE)
svm_train <- svm_df[svm_index, ]
svm_test <- svm_df[-svm_index, ]

# Creating SVM Model
svm_model1 <- svm(Y1 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = svm_train)
svm_model2 <- svm(Y2 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero, data = svm_train)

# Printing the model and it's summary
print(svm_model1)
print(svm_model2)

summary(svm_model1)
summary(svm_model2)

# Working viz of svm
svm_Energy_df <- svm_EnergyEfficiency_df[, c("X1", "X2", "X3", "X4", "X5", "X7", "Y1","Y2")]
index_svm <- sample(nrow(svm_Energy_df), 0.7*nrow(svm_Energy_df), replace = FALSE)
train_svm <- svm_Energy_df[index_svm, ]
test_svm <- svm_Energy_df[-index_svm, ]
svm_modela <- svm(Y1 ~ X1 + X2 + X3 + X5 + X7, data = train_svm)
svm_modelb <- svm(Y2 ~ X1 + X2 + X3 + X5 + X7, data = train_svm)

plot(svm_modela, train_svm, X1~X5, slice=list(X1=2,X5=3))
plot(svm_modelb, svm_train, X1~X2, slice=list(X6=2,X8=3))

# Plot for svm_modela
plot(svm_modela, train_svm, X1 ~ X3, grid = 50, slice = list(X2 = median(train_svm$X2), X5 = median(train_svm$X5), X7 = median(train_svm$X7)))

# Plot for svm_modelb
plot(svm_modelb, train_svm, X1 ~ X3, grid = 50, slice = list(X2 = median(train_svm$X2), X5 = median(train_svm$X5), X7 = median(train_svm$X7)))


#Predict the model
energypredict1 <- predict(svm_model1,svm_test)
predicttable1 <- table(energypredict1, svm_test$Y1)
predicttable1

energypredict2 <- predict(svm_model2,svm_test)
predicttable2 <- table(energypredict1, svm_test$Y2)
predicttable2

#Calculate the accuracy
Accuracy1 <- sum(diag(predicttable1))/sum(predicttable1)
Accuracy2 <- sum(diag(predicttable2))/sum(predicttable2)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted1 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy1 * 100)
accuracy_formatted2 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy2 * 100)

# Displaying the accuracy of svm model1
print(accuracy_formatted1)
print(accuracy_formatted2)

# Confusion Matrix
confusionMatrix(energypredict1, svm_test$Y1)
confusionMatrix(energypredict2, svm_test$Y2)


# Neural Network (One hidden Layer with loop from 1 to 5 nodes)
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
denormalize <- function(y,x){return(y*(max(x)-min(x))+min(x))}

# Creating dataset to be used for neural net
neural_df <- EnergyEfficiency[,c("X1", "X2", "X3", "X4", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

neural_df_norm <- as.data.frame(lapply(neural_df, normalize))

set.seed(7)
#Create training and testing data
neural_index <- sample(nrow(neural_df_norm), 0.7*nrow(neural_df_norm), replace = FALSE)
neural_train <- neural_df_norm[neural_index, ]
neural_test <- neural_df_norm[-neural_index, ]

trainingnorm <- as.data.frame(lapply(neural_train, normalize))

testingnorm <- as.data.frame(lapply(neural_test,normalize))

temptest <- subset(neural_test, select = c("X1", "X2", "X3", "X4", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2"))

energynet <- neuralnet(Y1 + Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero,
                       neural_train, hidden=5, lifesign="minimal", linear.output=TRUE,
                       threshold=0.1,stepmax=1e7)
plot(energynet)

energynet.results <- compute(energynet, temptest)

energynetdenorm <- denormalize(energynet.results$net.result,neural_test) 

actual_Y1 <- neural_test$Y1
actual_Y2 <- neural_test$Y2

resultstable <- data.frame(actual_Y1,actual_Y2, energynetdenorm)
resultstable

cor(resultstable$X1,resultstable$actual_Y1 )
#.534

cor(resultstable$X2,resultstable$actual_Y2)
#.492

mean(c(cor(resultstable$X1,resultstable$actual_Y1),cor(resultstable$X2,resultstable$actual_Y2)))

cur_max_list <- list()
for (layer_one in 1:5){
  energynet <- neuralnet(Y1 + Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + 
                         X8_North + X8_East + X8_South + X8_zero,
                         neural_train, hidden=layer_one, lifesign="minimal", linear.output=TRUE,
                         threshold=0.1,stepmax=1e7)
  energynet_results <- compute(energynet, temptest)
  energynetdenorm <- denormalize(energynet_results$net.result,neural_test)
  resulttable <- data.frame(actual_Y1,actual_Y2, energynetdenorm)
  cor(resulttable$X1,resultstable$actual_Y1)
  cor(resulttable$X2,resultstable$actual_Y2)
  average_cor <- mean(c(cor(resulttable$X1,resultstable$actual_Y1),cor(resulttable$X2,resultstable$actual_Y2)))
  print(average_cor)
  cur_max_list[paste(layer_one)] <- average_cor
}
cur_max_list[which.max(sapply(cur_max_list,max))]

energynet <- neuralnet(Y1 + Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + X8_North + X8_East + X8_South + X8_zero,
                       neural_train, hidden=5, lifesign="minimal", linear.output=TRUE,
                       threshold=0.1,stepmax=1e7)
plot(energynet)
# Extra
# 2 layer neural network
set.seed(7)
cur_max_list <- list()
for (layer_one in 1:5){
  for (layer_two in 1:5){
    energynet <- neuralnet(Y1 + Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + 
                           X8_North + X8_East + X8_South + X8_zero,
                           neural_train, hidden=c(layer_one,layer_two), lifesign="minimal", linear.output=TRUE,
                           threshold=0.1,stepmax=1e7)
    energynet_results <- compute(energynet, temptest)
    energynetdenorm <- denormalize(energynet_results$net.result,neural_test)
    resulttable <- data.frame(actual_Y1,actual_Y2, energynetdenorm)
    cor(resulttable$X1,resultstable$actual_Y1)
    cor(resulttable$X2,resultstable$actual_Y2)
    average_cor2 <- mean(c(cor(resulttable$X1,resultstable$actual_Y1),cor(resulttable$X2,resultstable$actual_Y2)))
    print(average_cor2)
    key <- paste(layer_one, layer_two, sep='-')
    cur_max_list[key] <- average_cor2
  }}
cur_max_list[which.max(sapply(cur_max_list,max))]


energynet <- neuralnet(Y1 + Y2 ~ X1 + X2 + X3 + X4 + X5 + X7 + X6_North + X6_East + X6_South + X8_Uniform + 
                         X8_North + X8_East + X8_South + X8_zero,
                       neural_train, hidden=c(5,4), lifesign="minimal", linear.output=TRUE,
                       threshold=0.1,stepmax=1e7)

plot(energynet)

# K-nearest 
knn_EnergyEfficiency_df <- EnergyEfficiency[, c("X1", "X2", "X3", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

# Assigning variables (A, B, C, D)
# converting values of Y1
categories <- c("A","B","C","D")
ranges <- c(-Inf, 12.99, 19.00, 31.68, Inf)
knn_EnergyEfficiency_df$Y1 <- cut(knn_EnergyEfficiency_df$Y1, breaks = ranges, labels = categories, include.lowest = TRUE)

# Converting values of Y2
categories <- c("A","B","C","D")
ranges <- c(-Inf, 15.61, 22.09, 33.14, Inf)
knn_EnergyEfficiency_df$Y2 <- cut(knn_EnergyEfficiency_df$Y2, breaks = ranges, labels = categories, include.lowest = TRUE)

knn_df <- knn_EnergyEfficiency_df
#knn_df <- knn_df[, -c(17, 18)]

# Z-score standardization
knn_df[,1:8] <- scale(knn_df[,1:8])

# Create training and testing data
knn_index <- sample(nrow(knn_df), 0.7*nrow(knn_df))
knn_train <- knn_df[knn_index, 1:4]
knn_test <- knn_df[-knn_index, 1:4]

# Create vectors for Y1 and Y2 labels
labels_Y1_train <- knn_df[knn_index,"Y1",drop=TRUE]
labels_Y1_test <- knn_df[-knn_index,"Y1",drop=TRUE]

labels_Y2_train <- knn_df[knn_index,"Y2",drop=TRUE]
labels_Y2_test <- knn_df[-knn_index,"Y2",drop=TRUE]

# Run K-nearest for Y1 and Y2
knn1 <- knn(train = knn_train, test = knn_test, cl = labels_Y1_train, k=21)
knn2 <- knn(train = knn_train, test = knn_test, cl = labels_Y2_train, k=21)

# Evaluate the model's performances
table1 <- CrossTable(x=labels_Y1_test, y=knn1, prop.chisq = FALSE)
table2 <- CrossTable(x=labels_Y2_test, y=knn2, prop.chisq = FALSE)

# Accuracy for models
sum(diag(table1$prop.tbl))
sum(diag(table2$prop.tbl))

# Confusion Matrix
confusionMatrix(knn1, labels_Y1_test)
confusionMatrix(knn2, labels_Y2_test)


# Naive Bayes
nb_EnergyEfficiency_df <- EnergyEfficiency[, c("X1", "X2", "X3", "X5", "X7", "X6_North", "X6_East", "X6_South", "X8_Uniform","X8_North","X8_East","X8_South", "X8_zero", "Y1","Y2")]

# Assigning variables (A, B, C, D)
# converting values of Y1
categories <- c("A","B","C","D")
ranges <- c(-Inf, 12.99, 19.00, 31.68, Inf)
nb_EnergyEfficiency_df$Y1 <- cut(nb_EnergyEfficiency_df$Y1, breaks = ranges, labels = categories, include.lowest = TRUE)

# Converting values of Y2
categories <- c("A","B","C","D")
ranges <- c(-Inf, 15.61, 22.09, 33.14, Inf)
nb_EnergyEfficiency_df$Y2 <- cut(nb_EnergyEfficiency_df$Y2, breaks = ranges, labels = categories, include.lowest = TRUE)


naivebayes_df <- nb_EnergyEfficiency_df
# naivebayes_df <- naivebayes_df[, -c(17, 18)]

# Create training and testing dataset
naivebayes_df_index <- sample(nrow(naivebayes_df), 0.7*nrow(naivebayes_df))
naivebayes_df_train <- naivebayes_df[naivebayes_df_index, ]
naivebayes_df_test <- naivebayes_df[-naivebayes_df_index, ]

# Run the Naive Bayes Classifier for Y1 and Y2
naivebayes_model1 <- naiveBayes(Y1 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + 
                                  X8_Uniform + X8_North + X8_East + X8_South + X8_zero, 
                                    data = naivebayes_df_train, laplace=1)
naivebayes_model1
naivebayes_model2 <- naiveBayes(Y2 ~ X1 + X2 + X3 + X5 + X7 + X6_North + X6_East + X6_South + 
                                X8_Uniform + X8_North + X8_East + X8_South + X8_zero,
                                              data = naivebayes_df_train, laplace=1)
naivebayes_model2

# Evaluating the  model's performance and accuracy
naivebayes_predict1 <- predict(naivebayes_model1, naivebayes_df_test, type="class")
naivebayes_predict1_table <- table(naivebayes_df_test$Y1, naivebayes_predict1)
naivebayes_predict1_table
sum(diag(naivebayes_predict1_table)) / sum(naivebayes_predict1_table)

naivebayes_predict2 <- predict(naivebayes_model2, naivebayes_df_test, type="class")
naivebayes_predict2_table <- table(naivebayes_df_test$Y2, naivebayes_predict2)
naivebayes_predict2_table
sum(diag(naivebayes_predict2_table)) / sum(naivebayes_predict2_table)
