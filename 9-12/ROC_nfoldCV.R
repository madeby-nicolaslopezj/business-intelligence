library(ROCR)
library(e1071)
library(cvTools) #crossvalidation

setwd("/Users/nicolaslopezj/Code/r/bi")
bupa<-read.table("8-29/cmc.data",header = FALSE,sep=",")
sub <- sample(nrow(bupa), floor(nrow(bupa) * 0.7))
train_bupa<-bupa[sub, ] #train set 70 %
test_bupa<-bupa[-sub, ] #test set 30%

a=naiveBayes(V7~.,data=train_bupa)
output=predict(a,test_bupa[,-7],type="raw")
pred1=max.col(output)
CM=table(pred1,test_bupa[,7]) #confusion matrix
CM

#Accuracy
(CM[1,1]+CM[2,2])/(CM[1,1]+CM[1,2]+CM[2,1]+CM[2,2])


# ROCR
pred <- prediction(output[,2],test_bupa[,7])
perf <- performance(pred, measure = "acc") 
plot(perf, col=rainbow(10))

y=max(data.frame(perf@y.values))
y

indice=which(data.frame(perf@y.values)== max(data.frame(perf@y.values)) )

cutpoint=perf@x.values[[1]][[indice[1]]]

cutpoint

#using the cutpoint (threshold)
pred2=ifelse(output[,2]>=cutpoint,2,1)
CM=table(test_bupa[,7],pred2)
CM

#accuracy
(CM[1,1]+CM[2,2])/(CM[1,1]+CM[1,2]+CM[2,1]+CM[2,2])


#ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))
plot(perf, col=rainbow(10),print.cutoffs.at=cutpoint)

#AUC: area under the curve
perf <- performance(pred, measure = "auc")
auc<-perf@y.values[[1]]
auc


#ten fold cross validation

nf=10
folds <- cvFolds(nrow(bupa), K=nf, type="random")

for(i in 1:nf) {
  train <- bupa[folds$subsets[folds$which != i], ]
  test <- bupa[folds$subsets[folds$which == i], ]
  #training a naive Bayes
  bayes=naiveBayes(V7~.,data=train)
 
  #prediction using test set
  predclass <- predict(bayes, newdata=test[,-7], type='raw')
  pred1=max.col(predclass)
  
  print(table(test[,7],pred1))
  
  if (i==1){
  predictions<-list(predclass[,2])
  labels<-list(test[,7])}
  else {
    predictions[[i]] <-predclass[,2]
    labels[[i]]<-test[,7]
  }
  
}

pred <- prediction(predictions,labels)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))
plot(perf, col=rainbow(10),avg="threshold")
plot(perf, col=rainbow(10),avg="threshold",spread.estimate="stddev")
