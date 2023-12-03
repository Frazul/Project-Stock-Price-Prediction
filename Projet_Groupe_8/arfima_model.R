library(forecast)
library(xts)
library(Hmisc)
library(lmtest)

#Téléchargement des données 
CAC40 <- read.csv("~/ENSAI/3A/Séries temorelles avancées/Projet/^FCHI.csv")
DataClose <- data.frame(matrix(c(CAC40$Close,CAC40$Date),ncol = 2))
colnames(DataClose) = c("Close", "Date")
View(DataClose)

## Formatage
DataClose$Close <- ifelse(DataClose$Close == "null", NA, DataClose$Close)
DataClose$Close <- as.numeric(DataClose$Close)
DataClose$Date <- as.Date(DataClose$Date, format = "%Y-%m-%d")

##Valeurs manquantes
print(sum(is.na(DataClose$Close)))
print(sum(is.na(DataClose$Date)))
DataClose <- na.omit(DataClose)

##Valeurs du CAC40 et rendements
ts_close <- DataClose$Close
train_size <- ceiling(0.80*length(ts_close))
train_close <- ts_close[1:train_size]
test_close <- ts_close[(train_size+1):length(ts_close)]

returns_close <- diff(ts_close)/Lag(ts_close, shift = 1)[-1]
train_returns <- returns_close[1:train_size]
test_returns <- returns_close[(train_size+1):length(returns_close)]

##Modélisation ARFIMA des valeurs du CAC

arfima_model <- arfima(train_close, estim = "mle")
summary(arfima_model)
arfima_model$d
arfima_model$ma
arfima_model$ar

### Validation du modèle 
res_arfima <- residuals(arfima_model)
plot(res_arfima)
Box.test(res_arfima)
acf(res_arfima) #Les résidus sont corrélés, on abandonne donc ce modèle 


### Modélisation ARFIMA des rendements 

arfima_model_r <- arfima(train_returns, estim = "mle")
summary(arfima_model_r)
arfima_model_r$d
arfima_model_r$ma
arfima_model_r$ar
arfima_model_r$fitted

plot(train_returns, col = 'blue', type = 'l', main = 'CAC40', ylab = "returns")
lines(arfima_model_r$fitted, col = "red")

### Validation du modèle 
res_arfima_r <- residuals(arfima_model_r)
plot(res_arfima_r, main= "Residuals of CAC40 returns", ylab = "residuals")
Box.test(res_arfima_r)
acf(res_arfima_r)

rmse_train <- sqrt(mean((train_returns - arfima_model_r$fitted)^2))

# Prédictions sur l'echantillon de test
predictions_r <- forecast(arfima_model_r, h = length(test_returns), level = FALSE)

predictions_test_r <- predictions_r$mean
plot(test_returns,col = 'blue', type = 'l', main = 'CAC40', ylab = "returns")
lines(x= 1:length(predictions_test_r), y=predictions_test_r, col = "red")

#Erreur sur l'échantillon de test
rmse <- sqrt(mean((test_returns - predictions_test_r)^2))

