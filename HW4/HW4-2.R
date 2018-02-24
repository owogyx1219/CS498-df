library(factoextra)
rawData <- read.table("employment_data2.txt", TRUE, "\t")

k1 <- kmeans(rawData[, 2:10], centers = 5, nstart = 15)
k2 <- kmeans(rawData[, 2:10], centers = 10, nstart = 15)
k3 <- kmeans(rawData[, 2:10], centers = 15, nstart = 15)
k4 <- kmeans(rawData[, 2:10], centers = 20, nstart = 15)

print(k1)
print(k2)
print(k3)
print(k4)

fviz_nbclust(rawData[, 2:10], kmeans, method = "wss", k.max = 25)
