#Required packages
library(e1071)
library(discretization)
library(infotheo)

# Open file that contains the Bupa dataset
bupa<-read.table(file.choose(),header = FALSE,sep=",")

#Generate training and testing set
sub <- sample(nrow(bupa), floor(nrow(bupa) * 0.7))
trainbupa<-bupa[sub, ] #70 % for training
testbupa<-bupa[-sub, ] #30 % for testing

#Naive Bayes classification without data discretization 
a=naiveBayes(V7~.,data=trainbupa)
pred=predict(a,testbupa[,-7],type="raw")
pred1=max.col(pred)
table(testbupa[,7],pred1)

#Naive Bayes classification with data discretization (entropy)
dbupa=mdlp(bupa)$Disc.data
trainbupa<-dbupa[sub, ]
testbupa<-dbupa[-sub, ]
b=naiveBayes(V7~.,data=trainbupa)
pred=predict(b,testbupa[,-7],type="raw")
pred1=max.col(pred)
table(testbupa[,7],pred1)


#Naive Bayes classification with data discretization (equal width)
dbupa=discretize(bupa, disc="equalwidth")
trainbupa<-dbupa[sub, ]
testbupa<-dbupa[-sub, ]
c=naiveBayes(V7~.,data=trainbupa)
pred=predict(c,testbupa[,-7],type="raw")
pred1=max.col(pred)
table(testbupa[,7],pred1)

#Naive Bayes classification with data discretization (equal frequency)
dbupa=discretize( bupa, disc="equalfreq")
trainbupa<-dbupa[sub, ]
testbupa<-dbupa[-sub, ]
d=naiveBayes(V7~.,data=trainbupa)
pred=predict(d,testbupa[,-7],type="raw")
pred1=max.col(pred)
table(testbupa[,7],pred1)

#Naive Bayes classification with data discretization (ChiMerge)
dbupa=chiM(bupa)$Disc.data
trainbupa<-dbupa[sub, ]
testbupa<-dbupa[-sub, ]
e=naiveBayes(V7~.,data=trainbupa)
pred=predict(e,testbupa[,-7],type="raw")
pred1=max.col(pred)
table(testbupa[,7],pred1)



#Train Adult
Adult<-read.csv(file.choose(),header = FALSE,sep=";")
m <- naiveBayes(V15~.,data=Adult)


# Test Adult
AdultTest<-read.csv(file.choose(),header = FALSE,sep=";")
p<-predict(m, AdultTest[,-15],type="raw")
p2<-max.col(p)
table(AdultTest[,15],p2)