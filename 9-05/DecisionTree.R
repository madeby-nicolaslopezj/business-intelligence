###################################################
### Classification using decision trees (iris data set)
###################################################
#Generate training and testing set
sub <- sample(nrow(iris), floor(nrow(iris) * 0.7))
train.set<-iris[sub, ] #70 % for training
test.set<-iris[-sub, ] #30 % for testing

library(rpart)

rpart.tree <- rpart(Species ~ ., data=train.set)
plot(rpart.tree,margin=0.2)
text (rpart.tree, use.n = T, pretty = TRUE)
title("Training Set's Classification Tree")

#prediction using test set
predictions <- predict(rpart.tree, test.set, type="class")

#Compare with real labels through the confusion matrix
table(test.set$Species, predictions)

#Prune
prune.rpart.tree <- prune(rpart.tree, cp=0.01)
plot(prune.rpart.tree,margin=0.2)
text (prune.rpart.tree, use.n = T, pretty = TRUE)

#control parameters (minimum number for split)
rpart.tree <- rpart(Species ~ ., data=train.set,control = rpart.control(minsplit=5))
plot(rpart.tree,margin=0.2)
text (rpart.tree, use.n = T, pretty = TRUE)
###################################################
### prediccion usando datos de la encuesta Adult
###################################################
#Train:
Adult<-read.csv(file.choose(),header = FALSE,sep=";")
summary(Adult)
m <- rpart(V15~.,data=Adult)
plot(m,margin=0.2)
text(m, use.n=T,pretty=TRUE)

post(m, file = "/Users/gonzaloruz/Desktop/tree2.ps",
     title = "Classification Tree for Adult dataset")

# Test:
AdultTest<-read.csv(file.choose(),header = FALSE,sep=";")
p<-predict(m, AdultTest,type="class")
table(AdultTest[,15],p)

#prune
prune.m <- prune(m, cp=0.1)
plot(prune.m,margin=0.2)
text(prune.m, use.n=T,pretty=TRUE)

post(prune.m, file = "/Users/gonzaloruz/Desktop/tree2p.ps",
     title = "Classification Tree for Adult dataset")

p<-predict(prune.m, AdultTest,type="class")
table(AdultTest[,15],p)
