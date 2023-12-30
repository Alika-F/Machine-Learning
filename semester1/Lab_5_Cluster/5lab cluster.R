#Задание 1

library(cluster)
data(pluton)

iterations <- c(3, 5, 10,50, 100)

for (i in iterations) {
  cl <- kmeans(pluton, iter.max = i, centers = 3)
  totss <- cl$totss
  betweenss <- cl$betweenss
  accuracy <- round(betweenss / totss,4)
  plot(pluton,
       col = cl$cluster,
       main = paste("Kmeans ", i, "итераций", 'точность:',accuracy))
}

#Задание 2
library(cluster)

col = c(rep("#FF0000", 100), rep("#00FF00", 100), rep("#0000FF", 100))
data2 <- data.frame(x = c(rnorm(100, 6, 0.5), rnorm(100, 15, 3.5), rnorm(100, 17, 3.5)), y = c(rnorm(100, -13, 7), rnorm(100, -7, 1), rnorm(100, -2, 1)))
data2 <- cbind(data2, col)

plot(data2$x, data2$y, xlab="x", ylab="y", col = data2$col, pch = 19)

model = clara(data2[, 1:2], k = 3, metric = "euclidean", stand = FALSE)
colors <- c("#FF0000", "#00FF00", "#0000FF")
plot(data2[, 1:2], col = colors[model$clustering], xlab = "x", ylab = "y", main = "Евклидова метрика - без стандартизации", pch = 19)

model = clara(data2[, 1:2], k = 3, metric = "manhattan", stand = FALSE)
plot(data2[, 1:2], col = colors[model$clustering], xlab = "x", ylab = "y", main = "Манхэттенская метрика - без стандартизации", pch = 19)

model = clara(data2[, 1:2], k = 3, metric = "euclidean", stand = TRUE)
plot(data2[, 1:2], col = colors[model$clustering], xlab = "x", ylab = "y", main = "Евклидова метрика - со стандартизацией", pch = 19)

model = clara(data2[, 1:2], k = 3, metric = "manhattan", stand = TRUE)
plot(data2[, 1:2], col = colors[model$clustering], xlab = "x", ylab = "y", main = "Манхэттенская метрика - со стандартизацией", pch = 19)

#Задание 3
library(cluster)
data3<-votes.repub
plot(agnes(votes.repub))

#Задание 4
library(cluster)
data4<-animals
plot(agnes(animals))

#Задание 5
library(cluster)
data5<-read.table("seeds_dataset.txt", sep = "", header=FALSE)
data5 <- na.omit(data5)
label <- data5[, 8]
features <- data5[,-8]

model <- clara(features, 3,metric = "manhattan", stand = FALSE)
accuracy <- round(mean(model$cluster == label),4)
plot(features, col = model$cluster, main = paste("кластеризация (Clara), точность:",accuracy))

plot(agnes(data5))


