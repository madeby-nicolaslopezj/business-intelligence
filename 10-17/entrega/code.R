# Load libraries
library(flexclust)
library(mclust)
library(dplyr)

# Import data
data(Zoo, package = "seriation")

# Show a summary of the data
summary(Zoo)

# Distance without class attribtue
distance <- dist(Zoo[,-17], method = "euclidean")

######
# 1: Hierarchical clustering
######

bestMethod <- 'None'
bestResult <- 0
for (method in c("single", "ward.D", "average", "complete", "centroid", "median", "mcquitty")){
  fit <- hclust(distance, method=method)
  groups <- cutree(fit, k=7)
  result <- randIndex(groups, Zoo[,17], correct=FALSE)
  print(paste(method, " result:", result))
  if (bestResult < result) {
    bestResult <- result
    bestMethod <- method
  }
}
print(paste("Best method is", bestMethod, "with", bestResult))

######
# 2: GMM with EM algorithm
######

for (modelName in c("EII", "VII", "EEI", "EVI", "VEI", "VVI")){
  model <- Mclust(Zoo, modelNames=modelName)
  print(summary(model,data=Zoo))
}
# Best is VEI with -1411.076


#####
# 3: Compare Ward.D with VEI
#####
fit.ward <- hclust(distance, method='ward.D')
groups.ward <- cutree(fit.ward, k=7)
result.ward <- randIndex(groups.ward, Zoo[,17], correct=FALSE)

fit.vei <- Mclust(Zoo, modelNames='VEI')
groups.vei <- fit.vei$classification
result.vei <- randIndex(groups.vei, Zoo[,17], correct=FALSE)

if (result.ward > result.vei) {
  print('Ward is better')
} else {
  print('VEI is better')
}

#####
# 4: Redefine classes
####

plot(fit.ward)
plot(fit.ward, label=Zoo$class)
rect.hclust(fit.ward, k=7, border="red") 

