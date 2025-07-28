# funkcja do przeprowadzania rozszerzonego testu Dickey'a - Fullera
test_stationarity_adf <- function(df, colname) {
  test <- adf.test(df[[colname]])
  return (data.frame(statystyka = round(test$statistic,2),
                     pvalue = round(test$p.value,2)))
  
}

test_stationarity_adf(arima_data, "num_crimes")


# funkcja do przeprowadzania testu KPSS
test_stationarity_kpss <- function(df, colname) {
  test <- kpss.test(df[[colname]]) 
  
  stat <- round(test$statistic, 3)
  
  return(data.frame(
    statystyka = stat,
    wart_krytyczna = 0.463
  ))
}
test_stationarity_kpss(arima_data, "num_crimes")


stationary <- arima_data %>%
  mutate(diff_crimes = c(NA, diff(num_crimes))) %>%
  filter(!is.na(diff_crimes))

test_stationarity_adf(stationary, "diff_crimes")

test_stationarity_kpss(stationary, "diff_crimes")

ggtsdisplay((stationary[["diff_crimes"]]),
            main = "Monthly Differenced Time Series")

# parametry dla modelu ARIMA(p, d, q) - bez sezonowości 
without_seasonality <- expand.grid(p=c(2, 4, 6), d = 1, q=2, P =0,D = 0, Q =0, period=0)

# parametry dla modelu SARIMA(p, d, q)(P, D, Q) - z komponentem sezonowym
with_seasonality <- expand.grid(p=c(2,4), d=1, q=2, P=c(0,1), D=c(0,1), Q=c(0, 1), period=c(6, 12))

# ramka danych z testowanymi kombinacjami parametrów
params <- rbind(with_seasonality, without_seasonality)

# podział na dane treningowe i testowe
train_data <- arima_data[year(arima_data$Month) %in% 2016:2018,][["num_crimes"]]
test_data <- arima_data[year(arima_data$Month) %in% 2019:2020,][["num_crimes"]]

# horyzont predykcji - roczny (czyli predykcja dla roku 2019 - nie 2020 przez wzgląd na pandemię)
h <- 12

results <- do.call(rbind, apply(params, 1, function(param_set){
  p <- param_set["p"]
  q <- param_set["q"]
  d <- param_set["d"]
  P <- param_set["P"]
  Q <- param_set["Q"]
  D <- param_set["D"]
  period = param_set["period"]
  
  model <- Arima(train_data, order= c(p, d, q), seasonal = list(order= c(P, D, Q), period=period ),  )
  result <- data.frame(p=p, d=d, q=q, P = P, D = D, Q =Q, period = period,
                       AIC = model$aic,
                       train_RMSE = sqrt(mean((train_data - fitted(model))^2)),
                       test_RMSE = sqrt(mean((test_data[1:h] - forecast(model, h)$mean)^2)))
}
))

results <- results %>% arrange(test_RMSE)
results

model <- Arima(train_data, order= c(4, 1, 2), seasonal = list(order= c(0, 1, 1), period=12))
summary(model)

model_ar1 <- Arima(train_data, order=c(1, 0, 0))
summary(model_ar1)

checkresiduals(model)

