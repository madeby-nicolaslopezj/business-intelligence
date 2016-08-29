#input from excel file
library("XLConnect")
data<-readWorksheetFromFile(file.choose(), sheet=1)

plot(data)
cor(data)
a <- dist(data, method='euclidean') # 6
b <- dist(data, method='manhattan') # 6 tambien
data[6,]

dataSub40 <- data[data[, "Edad"]<40, ]

plot(dataSub40)
cor(dataSub40)
