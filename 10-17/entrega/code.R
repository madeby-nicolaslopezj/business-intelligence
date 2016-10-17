library(flexclust)

# Import data
data(Zoo, package = "seriation")
# Show a summary of the data
summary(Zoo)

# Distance without class
distance <- dist(Zoo[,-17], method = "euclidean")

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

