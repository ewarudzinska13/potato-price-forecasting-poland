# ==============================================================================
# PROGNOZOWANIE CEN ZIEMNIAKÓW W POLSCE (2009-2018)
# Autorzy: Ewa Rudzińska, Weronika Wyrwas
# Kurs: Prognozowanie i Symulacje
# Uniwersytet Warszawski, Wydział Nauk Ekonomicznych
# ==============================================================================

# Wczytanie bibliotek
library(forecast)
library(tseries)
library(Metrics)
library(RJDemetra)
library(zoo)

# ==============================================================================
# 1. WCZYTANIE I PRZYGOTOWANIE DANYCH
# ==============================================================================

# Wczytanie danych z GUS (Bank Danych Lokalnych)
# Przeciętne ceny detaliczne ziemniaków w zł/kg, styczeń 2009 - grudzień 2018

# UWAGA: Wczytaj swoje dane - zmień ścieżkę
# data <- read.csv("ceny_ziemniakow.csv")
# y <- ts(data$cena, start = c(2009, 1), frequency = 12)

# Dla przykładu używamy danych z projektu
y <- ts(prognozowanie_projekt_p$y, start = c(2009, 1), frequency = 12)
y <- na.omit(y)  # Usunięcie ewentualnych braków danych

cat("Dane: styczeń 2009 - grudzień 2018\n")
cat("Liczba obserwacji:", length(y), "\n")
cat("Podstawowe statystyki:\n")
print(summary(y))

# Wykres surowych danych
plot(y, main = "Ceny detaliczne ziemniaków w Polsce (2009-2018)", 
     ylab = "Cena (zł/kg)", xlab = "Rok", lwd = 2)

# ==============================================================================
# 2. DEKOMPOZYCJA SZEREGU METODĄ TRAMO-SEATS
# ==============================================================================

cat("\n=== DEKOMPOZYCJA TRAMO-SEATS ===\n")

# Odsezonowanie metodą Tramo-Seats
tramo_result <- tramoseats(y, tramoseats_spec(spec = "RSAfull"))

# Wydobycie komponentów
y_sa <- tramo_result[["final"]][["series"]][, 2]    # dane odsezonowane
y_trend <- tramo_result[["final"]][["series"]][, 3] # trend
y_seasonal <- tramo_result[["final"]][["series"]][, 4] # sezonowość
y_irregular <- tramo_result[["final"]][["series"]][, 5] # komponent nieregularny

# Wykres dekompozycji
par(mfrow = c(2, 2))
plot(y, main = "Dane oryginalne", ylab = "Cena (zł/kg)")
plot(y_sa, main = "Dane odsezonowane", ylab = "Cena (zł/kg)")
plot(y_seasonal, main = "Komponent sezonowy", ylab = "Efekt sezonowy")
plot(y_irregular, main = "Komponent nieregularny", ylab = "Wahania")
par(mfrow = c(1, 1))

cat("Charakter sezonowości: multiplikatywny\n")

# ==============================================================================
# 3. PODZIAŁ NA PRÓBĘ ESTYMACYJNĄ I TESTOWĄ
# ==============================================================================

h <- 12  # horyzont prognozy (12 miesięcy)
n_train <- length(y_sa) - h
train <- window(y_sa, end = time(y_sa)[n_train])
test <- window(y_sa, start = time(y_sa)[n_train + 1])

cat("\nPróba in-sample:", length(train), "obserwacji\n")
cat("Próba out-of-sample:", length(test), "obserwacji (2018)\n")

# Ramka danych do przechowywania wyników
results <- data.frame(
  Method = character(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

# ==============================================================================
# 4. METODA NAIWNA
# ==============================================================================

cat("\n=== METODA NAIWNA ===\n")

# Prognoza naiwna: f_t = y_{t-1}
forecast_naive <- rep(tail(train, 1), h)

# Błędy prognozy
rmse_naive <- rmse(test, forecast_naive)
mae_naive <- mae(test, forecast_naive)
mape_naive <- mape(test, forecast_naive)

results <- rbind(results, data.frame(
  Method = "Naiwna",
  RMSE = rmse_naive,
  MAE = mae_naive,
  MAPE = mape_naive
))

cat("RMSE:", round(rmse_naive, 4), "\n")
cat("MAE:", round(mae_naive, 4), "\n")
cat("MAPE:", round(mape_naive * 100, 2), "%\n")

# ==============================================================================
# 5. ŚREDNIA RUCHOMA
# ==============================================================================

cat("\n=== ŚREDNIA RUCHOMA ===\n")

# Funkcja do obliczenia średniej ruchomej
ma_forecast <- function(data, h, k) {
  n <- length(data)
  forecasts <- numeric(h)
  for (i in 1:h) {
    window_end <- n + i - 1
    window_start <- max(1, window_end - k + 1)
    forecasts[i] <- mean(data[window_start:min(n, window_end)])
  }
  return(forecasts)
}

# Testowanie różnych okien: k = 2, 3, 6, 12
windows <- c(2, 3, 6, 12)
ma_results <- list()

for (k in windows) {
  forecast_ma <- ma_forecast(train, h, k)
  
  rmse_ma <- rmse(test, forecast_ma)
  mae_ma <- mae(test, forecast_ma)
  mape_ma <- mape(test, forecast_ma)
  
  ma_results[[paste0("MA", k)]] <- list(
    forecast = forecast_ma,
    rmse = rmse_ma,
    mae = mae_ma,
    mape = mape_ma
  )
  
  results <- rbind(results, data.frame(
    Method = paste0("MA(", k, ")"),
    RMSE = rmse_ma,
    MAE = mae_ma,
    MAPE = mape_ma
  ))
  
  cat("MA(", k, "): RMSE =", round(rmse_ma, 4), 
      ", MAE =", round(mae_ma, 4),
      ", MAPE =", round(mape_ma * 100, 2), "%\n")
}

# Optymalizacja okna (k = 1 do 60)
rmse_by_k <- numeric(60)
for (k in 1:60) {
  forecast_ma <- ma_forecast(train, h, k)
  rmse_by_k[k] <- rmse(test, forecast_ma)
}

k_opt <- which.min(rmse_by_k)
cat("\nOptymalne okno prognozy: k =", k_opt, "\n")
cat("RMSE dla optymalnego okna:", round(min(rmse_by_k), 4), "\n")

# Wykres RMSE vs. długość okna
plot(1:60, rmse_by_k, type = "l", lwd = 2,
     main = "RMSE w zależności od długości okna MA",
     xlab = "Długość okna (k)", ylab = "RMSE")
abline(v = k_opt, col = "red", lty = 2, lwd = 2)
points(k_opt, min(rmse_by_k), col = "red", pch = 19, cex = 1.5)

# ==============================================================================
# 6. PROSTE WYGŁADZANIE WYKŁADNICZE (SES)
# ==============================================================================

cat("\n=== PROSTE WYGŁADZANIE WYKŁADNICZE ===\n")

# Estymacja modelu SES
fit_ses <- ses(train, h = h)

# Prognoza
forecast_ses <- as.numeric(fit_ses$mean)

# Błędy
rmse_ses <- rmse(test, forecast_ses)
mae_ses <- mae(test, forecast_ses)
mape_ses <- mape(test, forecast_ses)

results <- rbind(results, data.frame(
  Method = "SES",
  RMSE = rmse_ses,
  MAE = mae_ses,
  MAPE = mape_ses
))

cat("Alpha:", round(fit_ses$model$par["alpha"], 4), "\n")
cat("RMSE:", round(rmse_ses, 4), "\n")
cat("MAE:", round(mae_ses, 4), "\n")
cat("MAPE:", round(mape_ses * 100, 2), "%\n")

# Wykres
plot(fit_ses, main = "Proste wygładzanie wykładnicze (SES)",
     ylab = "Cena (zł/kg)", xlab = "Rok")
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Dane", "Prognoza", "Rzeczywiste"),
       col = c("black", "blue", "red"), lty = 1, lwd = 2)

# ==============================================================================
# 7. MODEL HOLTA-WINTERSA
# ==============================================================================

cat("\n=== MODEL HOLTA-WINTERSA ===\n")

# Wersja addytywna
cat("\nWersja addytywna:\n")
fit_hw_add <- hw(train, seasonal = "additive", h = h)
forecast_hw_add <- as.numeric(fit_hw_add$mean)

rmse_hw_add <- rmse(test, forecast_hw_add)
mae_hw_add <- mae(test, forecast_hw_add)
mape_hw_add <- mape(test, forecast_hw_add)

cat("Alpha:", round(fit_hw_add$model$par["alpha"], 4), "\n")
cat("Beta:", round(fit_hw_add$model$par["beta"], 4), "\n")
cat("Gamma:", round(fit_hw_add$model$par["gamma"], 4), "\n")
cat("RMSE:", round(rmse_hw_add, 4), "\n")

results <- rbind(results, data.frame(
  Method = "Holt-Winters (addytywny)",
  RMSE = rmse_hw_add,
  MAE = mae_hw_add,
  MAPE = mape_hw_add
))

# Wersja multiplikatywna
cat("\nWersja multiplikatywna:\n")
fit_hw_mult <- hw(train, seasonal = "multiplicative", h = h)
forecast_hw_mult <- as.numeric(fit_hw_mult$mean)

rmse_hw_mult <- rmse(test, forecast_hw_mult)
mae_hw_mult <- mae(test, forecast_hw_mult)
mape_hw_mult <- mape(test, forecast_hw_mult)

cat("Alpha:", round(fit_hw_mult$model$par["alpha"], 4), "\n")
cat("Beta:", round(fit_hw_mult$model$par["beta"], 4), "\n")
cat("Gamma:", round(fit_hw_mult$model$par["gamma"], 4), "\n")
cat("RMSE:", round(rmse_hw_mult, 4), "\n")

results <- rbind(results, data.frame(
  Method = "Holt-Winters (multiplikatywny)",
  RMSE = rmse_hw_mult,
  MAE = mae_hw_mult,
  MAPE = mape_hw_mult
))

# Wykresy obu wersji
par(mfrow = c(1, 2))
plot(fit_hw_add, main = "Holt-Winters Addytywny", ylab = "Cena (zł/kg)")
lines(test, col = "red", lwd = 2)
plot(fit_hw_mult, main = "Holt-Winters Multiplikatywny", ylab = "Cena (zł/kg)")
lines(test, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# ==============================================================================
# 8. MODEL ETS (AUTOMATYCZNY DOBÓR)
# ==============================================================================

cat("\n=== MODEL ETS (AUTOMATYCZNY DOBÓR) ===\n")

fit_ets <- ets(train)
forecast_ets <- forecast(fit_ets, h = h)

rmse_ets <- rmse(test, forecast_ets$mean)
mae_ets <- mae(test, forecast_ets$mean)
mape_ets <- mape(test, forecast_ets$mean)

cat("Wybrany model:", fit_ets$method, "\n")
cat("Alpha:", round(fit_ets$par["alpha"], 4), "\n")
if ("gamma" %in% names(fit_ets$par)) {
  cat("Gamma:", round(fit_ets$par["gamma"], 4), "\n")
}
cat("RMSE:", round(rmse_ets, 4), "\n")

results <- rbind(results, data.frame(
  Method = paste0("ETS (", fit_ets$method, ")"),
  RMSE = rmse_ets,
  MAE = mae_ets,
  MAPE = mape_ets
))

# Wykres
plot(forecast_ets, main = "Model ETS - Prognoza", 
     ylab = "Cena (zł/kg)", xlab = "Rok")
lines(test, col = "red", lwd = 2)

# ==============================================================================
# 9. MODEL SARIMA
# ==============================================================================

cat("\n=== MODEL SARIMA ===\n")

# Wykresy ACF i PACF dla szeregu oryginalnego
par(mfrow = c(1, 2))
acf(train, main = "ACF - dane oryginalne", lag.max = 36)
pacf(train, main = "PACF - dane oryginalne", lag.max = 36)
par(mfrow = c(1, 1))

# Pierwsze różnice regularne
diff_regular <- diff(train)
par(mfrow = c(1, 2))
acf(diff_regular, main = "ACF - pierwsze różnice", lag.max = 36)
pacf(diff_regular, main = "PACF - pierwsze różnice", lag.max = 36)
par(mfrow = c(1, 1))

# Pierwsze różnice sezonowe (po różnicowaniu regularnym)
diff_seasonal <- diff(diff_regular, lag = 12)
par(mfrow = c(1, 2))
acf(diff_seasonal, main = "ACF - różnice sezonowe", lag.max = 36)
pacf(diff_seasonal, main = "PACF - różnice sezonowe", lag.max = 36)
par(mfrow = c(1, 1))

# Testy stacjonarności
cat("\nTest ADF dla różnic sezonowych:\n")
adf_test <- adf.test(diff_seasonal)
print(adf_test)

cat("\nTest KPSS dla różnic sezonowych:\n")
kpss_test <- kpss.test(diff_seasonal)
print(kpss_test)

# Test Ljunga-Boxa i Boxa-Pierce'a
cat("\nTest Ljunga-Boxa:\n")
print(Box.test(diff_seasonal, type = "Ljung-Box"))

cat("\nTest Boxa-Pierce'a:\n")
print(Box.test(diff_seasonal, type = "Box-Pierce"))

# Procedura od ogółu do szczegółu - testowane modele
cat("\n--- Procedura selekcji modelu SARIMA ---\n")

models <- list(
  list(order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12)),
  list(order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12)),
  list(order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12)),
  list(order = c(0, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
)

aic_values <- numeric(length(models))
bic_values <- numeric(length(models))

for (i in 1:length(models)) {
  fit <- Arima(train, order = models[[i]]$order, 
               seasonal = models[[i]]$seasonal)
  aic_values[i] <- fit$aic
  bic_values[i] <- fit$bic
  
  cat("SARIMA", paste0(models[[i]]$order, collapse = ","), 
      "x", paste0(models[[i]]$seasonal$order, collapse = ","), 
      ": AIC =", round(fit$aic, 2), ", BIC =", round(fit$bic, 2), "\n")
}

# Najlepszy model na podstawie AIC i BIC
best_idx <- which.min(aic_values)
best_model <- models[[best_idx]]

cat("\nNajlepszy model: SARIMA", paste0(best_model$order, collapse = ","),
    "x", paste0(best_model$seasonal$order, collapse = ","), "\n")

# Estymacja najlepszego modelu
fit_sarima <- Arima(train, order = best_model$order, 
                    seasonal = best_model$seasonal)

cat("\nParametry modelu:\n")
print(coef(fit_sarima))

# Diagnostyka reszt
cat("\nTest Ljunga-Boxa dla reszt:\n")
print(Box.test(fit_sarima$residuals, type = "Ljung-Box", lag = 24))

# Wykresy reszt
par(mfrow = c(1, 2))
acf(fit_sarima$residuals, main = "ACF reszt")
pacf(fit_sarima$residuals, main = "PACF reszt")
par(mfrow = c(1, 1))

# Prognoza
forecast_sarima <- forecast(fit_sarima, h = h)

rmse_sarima <- rmse(test, forecast_sarima$mean)
mae_sarima <- mae(test, forecast_sarima$mean)
mape_sarima <- mape(test, forecast_sarima$mean)

results <- rbind(results, data.frame(
  Method = paste0("SARIMA(", paste(best_model$order, collapse = ","), 
                  ")(", paste(best_model$seasonal$order, collapse = ","), ")12"),
  RMSE = rmse_sarima,
  MAE = mae_sarima,
  MAPE = mape_sarima
))

cat("RMSE:", round(rmse_sarima, 4), "\n")
cat("MAE:", round(mae_sarima, 4), "\n")
cat("MAPE:", round(mape_sarima * 100, 2), "%\n")

# Wykres prognozy
plot(forecast_sarima, main = "Model SARIMA - Prognoza",
     ylab = "Cena (zł/kg)", xlab = "Rok")
lines(test, col = "red", lwd = 2)

# ==============================================================================
# 10. PORÓWNANIE WYNIKÓW
# ==============================================================================

cat("\n=== PODSUMOWANIE WYNIKÓW ===\n")

# Sortowanie wyników według RMSE
results <- results[order(results$RMSE), ]
rownames(results) <- NULL

print(results)

cat("\nNajlepsza metoda:", results$Method[1], "\n")
cat("RMSE:", round(results$RMSE[1], 4), "\n")
cat("MAE:", round(results$MAE[1], 4), "\n")
cat("MAPE:", round(results$MAPE[1] * 100, 2), "%\n")

# Wykres porównawczy błędów
par(mar = c(10, 4, 4, 2))
barplot(results$RMSE, names.arg = results$Method, 
        main = "Porównanie błędów RMSE", 
        ylab = "RMSE", las = 2, col = "steelblue")
par(mar = c(5, 4, 4, 2))

# ==============================================================================
# 11. TEST DIEBOLDA-MARIANO
# ==============================================================================

cat("\n=== TEST DIEBOLDA-MARIANO ===\n")

# Przechowywanie prognoz wszystkich metod
all_forecasts <- list(
  Naive = forecast_naive,
  MA3 = ma_results$MA3$forecast,
  SES = forecast_ses,
  HW_add = forecast_hw_add,
  HW_mult = forecast_hw_mult,
  ETS = as.numeric(forecast_ets$mean),
  SARIMA = as.numeric(forecast_sarima$mean)
)

# Funkcja do testu DM
dm_test_result <- function(f1, f2, actual) {
  e1 <- actual - f1
  e2 <- actual - f2
  dm.test(e1, e2, alternative = "two.sided")
}

# Przeprowadzenie testów DM dla wybranych par
cat("\nTest MA(3) vs SES:\n")
print(dm_test_result(all_forecasts$MA3, all_forecasts$SES, test))

cat("\nTest MA(3) vs Holt-Winters (add):\n")
print(dm_test_result(all_forecasts$MA3, all_forecasts$HW_add, test))

cat("\nTest MA(3) vs SARIMA:\n")
print(dm_test_result(all_forecasts$MA3, all_forecasts$SARIMA, test))

cat("\nTest SES vs Holt-Winters (add):\n")
print(dm_test_result(all_forecasts$SES, all_forecasts$HW_add, test))

# ==============================================================================
# 12. WYKRES PORÓWNAWCZY PROGNOZ
# ==============================================================================

# Wykres wszystkich prognoz vs. rzeczywiste wartości
plot(test, type = "l", lwd = 3, col = "black",
     main = "Porównanie prognoz - dane rzeczywiste vs. modele",
     ylab = "Cena (zł/kg)", xlab = "Miesiąc (2018)",
     ylim = range(c(test, forecast_naive, ma_results$MA3$forecast)))

lines(ts(forecast_naive, start = start(test), frequency = 12), 
      col = "gray", lty = 2, lwd = 2)
lines(ts(ma_results$MA3$forecast, start = start(test), frequency = 12), 
      col = "red", lty = 1, lwd = 2)
lines(ts(forecast_ses, start = start(test), frequency = 12), 
      col = "blue", lty = 1, lwd = 2)

legend("topleft", 
       legend = c("Rzeczywiste", "Naiwna", "MA(3)", "SES"),
       col = c("black", "gray", "red", "blue"),
       lty = c(1, 2, 1, 1), lwd = 2)

