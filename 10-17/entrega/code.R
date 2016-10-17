# Import data
data(Zoo, package = "seriation")
# Show a summary of the data
summary(Zoo)

# Distance without class
distance <- dist(Zoo[,-17], method = "euclidean")


fit.single <- hclust(distance, method="single")

plot(fit.single) # display dendogram
plot(fit.single, label=Zoo$class) # Add label

# cut the tree for 7 clusters
groups.single <- cutree(fit.single, k=7)

# Dendrogram with the three clusters inside a red box
rect.hclust(fit.single, k=7, border="red") 

