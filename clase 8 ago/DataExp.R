#Data Exploration

#Reviewing data dimensions
dim(iris)

#Name of variables or columns
names(iris)

#Structure
str(iris)

#Attributes
attributes(iris)

#Get the first 5 rows
iris[1:5,]

#Get Sepal.Length of the first 10 rows
iris[1:10, "Sepal.Length"]

#The same as before
iris[1:10,1]

#The same as before
iris$Sepal.Length[1:10]

#Descriptive statistics for each variable
summary(iris)

#Frequency
table(iris$Species)

#Pie Chart
pie(table(iris$Species))

#Average of Sepal.Length
mean(iris$Sepal.Length)

#Variance of Sepal.Length
var(iris$Sepal.Length)

#Standard Deviation of Sepal.Length
sd(iris$Sepal.Length)

#Covariance of two variables
cov(iris$Sepal.Length, iris$Petal.Length)

#Correlation of two variables
cor(iris$Sepal.Length, iris$Petal.Length)

#Correlation matrix
cor(iris[,-5])

#Distribution of subsets
aggregate(Sepal.Length ~ Species, summary, data=iris)

#Box plots
boxplot(Sepal.Length~Species, data=iris)

bp<-boxplot(Sepal.Length~Species, data=iris)

#Histograms
hist(iris$Sepal.Length)

#Density
plot(density(iris$Sepal.Length))

# Scatter plot           
plot(iris$Sepal.Length, iris$Sepal.Width)

#Another alternative
with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))

#When there are many points, some of them may overlap.
#We can use jitter () to add a little noise to the data.
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))

#Plot in pairs
plot(iris)

#the same as before
pairs(iris)

#3D Scatter plot
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)
