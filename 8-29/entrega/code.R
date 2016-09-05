#Required packages
library(e1071)
library(discretization)
library(infotheo)

setwd("/Users/nicolaslopezj/Code/r/clase\ 29\ ago/entrega")

data <- read.csv("data.csv", header = TRUE, sep=";")
data <- data[, -1] # Remove id row
data$Taxable.Income <- gsub('K', '000', data$Taxable.Income) # Convert k to 
data$Taxable.Income <- as.numeric(as.character(data$Taxable.Income)) # convert to number

test <- read.csv("test.csv", header = TRUE, sep=";")
test$Taxable.Income <- gsub('K', '000', test$Taxable.Income) # Convert k to 
test$Taxable.Income <- as.numeric(as.character(test$Taxable.Income)) # convert to number

algo <- naiveBayes(Evade~.,data=data)

results <- predict(algo, test, type="raw")
realExamples <- predict(algo, data[, -4], type="raw")
