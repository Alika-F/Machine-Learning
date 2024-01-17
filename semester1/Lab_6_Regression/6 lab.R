#задание 1
data1 <- read.table( "reglab1.txt", sep = "\t", header = TRUE)

# Построение моделей
model1 <- lm(z ~ ., data = data1)
model2 <- lm(x ~ ., data = data1)
model3 <- lm(y ~ ., data = data1)

# Анализ результатов
summary(model1)
summary(model2)
summary(model3)

#задание 2
data2 <- read.table("reglab2.txt", header = TRUE)
min_RSS <- Inf
d <- ncol(data2) - 1

for (k in 1:d) {
  combination <- combn(data2[, -1], k)
  for (i in 1:nrow(combination)) {
    new_data <- data.frame(y = data2[, 1], combination[i, ])
    f <- lm(y ~ ., new_data)
    RSS <- sum((residuals(f))^2)
    if (RSS < min_RSS) {
      min_RSS <- RSS
      optimal_model <- f
    }
  }
}
cat("Минимальная остаточная сумма квадратов (RSS):", min_RSS, "\n")
cat("Summary модели:\n")
print(summary(optimal_model))
cat("Коэффициенты модели:\n")
print(optimal_model$coefficients)
plot(optimal_model)

#задание 3
data3 <- read.table("cygage.txt", stringsAsFactors = TRUE, header = TRUE)
model <- lm(calAge ~ Depth, data = data3, weights = Weight)

summary(model)
summary<-summary(model)

# Вычисление MSE
mse <- mean(summary$residuals^2)

cat("Mean Squared Error (MSE):", mse, "\n")

plot(data3$Depth, data3$calAge, pch = 16, col = "blue", xlab = "Глубина", ylab = "Возраст")
abline(model, col = "red")

#задание 4
data(longley)

model_base <- lm(Employed ~ ., longley)
longley <- subset(longley, select = -c(Population))

set.seed(123)
indices <- sample(1:nrow(longley), nrow(longley) / 2)
train_data <- longley[indices, ]
test_data <- longley[-indices, ]

# Построение гребневой регрессии
lambda_values <- 10^(-3 + 0.2 * (0:25))
ridge_errors_train <- numeric(length(lambda_values))
ridge_errors_test <- numeric(length(lambda_values))

for (i in seq_along(lambda_values)) {
  lambda <- lambda_values[i]
  ridge_model <- lm.ridge(Employed ~ ., train_data, lambda = lambda)
  ridge_coef <- coef(ridge_model)
  
  # Предсказание на обучающей выборке
  train_pred <- cbind(1, as.matrix(train_data[, -ncol(train_data)])) %*% ridge_coef
  ridge_errors_train[i] <- mean((train_pred - train_data$Employed)^2)
  
  # Предсказание на тестовой выборке
  test_pred <- cbind(1, as.matrix(test_data[, -ncol(test_data)])) %*% ridge_coef
  ridge_errors_test[i] <- mean((test_pred - test_data$Employed)^2)
}

plot(log10(lambda_values), ridge_errors_train, type = "b",pch = 16, col = "purple", xlab = "log10(lambda)",
     ylab = "Error", main = "Гребневая регрессия: Ошибка на обучающей и тестовой выборке от lambda")
lines(log10(lambda_values), ridge_errors_test, type = "b", pch = 16, col = "green")
legend("topright", legend = c("Train Error", "Test Error"), col = c("purple", "green"), pch = 16)

#задание 5
data(EuStockMarkets)
EuStockMarkets
plot(EuStockMarkets[,1], type = "l", col="blue",main = names(EuStockMarkets)[i], xlab = "Time", ylab = "Stock Prices")
lines(EuStockMarkets[,2], type = "l", col="red")
lines(EuStockMarkets[,3], type = "l", col="green")
lines(EuStockMarkets[,4], type = "l", col="pink")
legend("topleft", legend = c("Germany DAX", "Switzerland SMI", "France CAC", "UK FTSE"), col = c("blue", "red", "green", "pink"), lty = 1)

par(mfrow = c(2, 2))
models <- list()

for (i in 1:4) {
  model <- lm(EuStockMarkets[, i] ~ time(EuStockMarkets))
  models[[i]] <- model
  summary(model)

  plot(time(EuStockMarkets), EuStockMarkets[, i], type = "l", col = c("blue", "red", "green", "pink")[i], main = names(EuStockMarkets)[i], xlab = "Time", ylab = "Stock Prices")
  abline(model, col = "gray")
}

par(mfrow = c(1, 1))

# Линейная регрессии для всех моделей
overall_model <- lm(rowMeans(EuStockMarkets) ~ time(EuStockMarkets))
plot(time(EuStockMarkets), rowMeans(EuStockMarkets), type = "l", col = "purple", xlab = "Time", ylab = "Stock Prices")
abline(overall_model, col = "gray")

#задание 6
library(datasets)
data(JohnsonJohnson)

qtr1 <- JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)]
qtr2 <- JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)]
qtr3 <- JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)]
qtr4 <- JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)]

JohnsonJohnson.qtr1 <- data.frame(Year = year, Qtr = qtr1) 
JohnsonJohnson.qtr2 <- data.frame(Year = year, Qtr = qtr2) 
JohnsonJohnson.qtr3 <- data.frame(Year = year, Qtr = qtr3) 
JohnsonJohnson.qtr4 <- data.frame(Year = year, Qtr = qtr4)
JohnsonJohnson.year <- data.frame(Year = year, Qtr1 = qtr1, Qtr2 = qtr2, Qtr3 = qtr3, Qtr4 = qtr4)

plot(JohnsonJohnson.qtr1, type = "l", col="blue", xlab="Год", ylab="Прибыль")
lines(JohnsonJohnson.qtr2, col="green")
lines(JohnsonJohnson.qtr3, col="red")
lines(JohnsonJohnson.qtr4, col="purple")


plot_with_regression <- function(data, col, qtr_name) {
  model <- lm(Qtr ~ Year, data)
  plot(data$Year, data$Qtr, type = "l", col = col, xlab = "Год", ylab = "Прибыль", main = qtr_name)
  abline(model, col = "gray")
  print(summary(model))
  
  # Предсказание для 2016 года
  prediction <- predict(model, data.frame(Year = 2016))
  cat(qtr_name, "прогноз на 2016 год:", prediction, "\n\n")
}

par(mfrow = c(2, 2))

plot_with_regression(JohnsonJohnson.qtr1, "blue", "1 квартал")
plot_with_regression(JohnsonJohnson.qtr2, "green", "2 квартал")
plot_with_regression(JohnsonJohnson.qtr3, "red", "3 квартал")
plot_with_regression(JohnsonJohnson.qtr4, "purple", "4 квартал")

par(mfrow = c(1, 1))

model <- lm(Qtr1 + Qtr2 + Qtr3 + Qtr4  ~ Year, JohnsonJohnson.year )
summary(model)
prediction <- predict(model ,data.frame(Year = 2016))/4
prediction


#задание 7
data(sunspot.year)
model <- lm(sun ~ ., data.frame(time = time(sunspot.year), sun = sunspot.year))
summary(model)
plot(sunspot.year, main = "Sunspot year", col = "purple",lwd = 2)
abline(model, col = "yellow", lwd = 2)

#задание 8
data <- read.csv("UKgas.csv", stringsAsFactors = TRUE)
head(data)

qtr1 <- data$UKgas[seq(from = 1, to = nrow(data), by = 4)]
qtr2 <- data$UKgas[seq(from = 2, to = nrow(data), by = 4)]
qtr3 <- data$UKgas[seq(from = 3, to = nrow(data), by = 4)]
qtr4 <- data$UKgas[seq(from = 4, to = nrow(data), by = 4)]

model1 <- lm(qtr1 ~ data$time[seq(from = 1, to = nrow(data), by = 4)], data)
model1
model2 <- lm(qtr2 ~ data$time[seq(from = 2, to = nrow(data), by = 4)], data)
model2
model3 <- lm(qtr3 ~ data$time[seq(from = 3, to = nrow(data), by = 4)], data)
model3
model4 <- lm(qtr4 ~ data$time[seq(from = 4, to = nrow(data), by = 4)], data)
model4
model5 <- lm((qtr1 + qtr2 + qtr3 + qtr4) ~ data$time[seq(from = 1, to = nrow(data), by = 4)], data)
model5

predict1 = coef(model1)[1]+coef(model1)[2]*2016
predict2 = coef(model2)[1]+coef(model2)[2]*2016
predict3 = coef(model3)[1]+coef(model3)[2]*2016
predict4 = coef(model4)[1]+coef(model4)[2]*2016
predict5 = (coef(model5)[1]+coef(model5)[2]*2016)/4

cat("Прогноз для 2016 года для 1 квартала: ", predict1, "\n")
cat("Прогноз для 2016 года для 2 квартала: ", predict2, "\n")
cat("Прогноз для 2016 года для 3 квартала: ", predict3, "\n")
cat("Прогноз для 2016 года для 4 квартала: ", predict4, "\n")
cat("Прогноз для 2016 года в среднем по году: ", predict5, "\n")

#задание 9
data(cars)
model <- lm(dist ~ ., data = cars)
plot(cars, main = "Cars", pch = 16, col = "purple", lwd = 2)
abline(model, col = "green", lwd = 2)
predict.lm(model, data.frame(speed = 40))
