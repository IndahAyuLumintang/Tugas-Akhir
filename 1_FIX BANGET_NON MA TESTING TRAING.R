# Memuat library yang dibutuhkan
library(readxl)
library(ggplot2)

# Import data dari file Excel
cat("Mengimpor data...\n")
DATA_FUZZY <- read_excel("Tugas Akhir Lulu/ANALISIS+LAPORAN/DATA_FUZZY.xlsx", col_types = c("date", "numeric"))
data <- DATA_FUZZY

# Parameter margin d1 dan d2
d1 <- 7.57
d2 <- 4.83

# Menghitung nilai minimum dan maksimum untuk himpunan semesta
nilai_min <- min(data$`Hasil Produksi`, na.rm = TRUE)
nilai_max <- max(data$`Hasil Produksi`, na.rm = TRUE)

universe_min <- nilai_min - d1
universe_max <- nilai_max + d2

# Jumlah interval dan lebar interval
num_intervals <- 12
interval_width <- (universe_max - universe_min) / num_intervals

# Membuat batas dan nilai tengah interval
interval_bounds <- seq(universe_min, universe_max, by = interval_width)
interval_midpoints <- interval_bounds[-1] - (interval_width / 2)

# Membuat tabel interval
interval_table <- data.frame(
  Lower_Bound = interval_bounds[-length(interval_bounds)],
  Upper_Bound = interval_bounds[-1],
  Midpoint = interval_midpoints
)

# Memisahkan Data Training dan Testing
data$Tanggal <- as.Date(data$Tanggal)
data_training <- subset(data, Tanggal >= as.Date("2018-11-01") & Tanggal <= as.Date("2023-12-24"))
data_testing <- subset(data, Tanggal >= as.Date("2023-12-25") & Tanggal <= as.Date("2023-12-31"))

# Fungsi fuzzifikasi
fuzzifikasi <- function(value, bounds) {
  for (i in 1:(length(bounds) - 1)) {
    if (value >= bounds[i] && value < bounds[i + 1]) {
      return(i)  # Menentukan fuzzy yang sesuai
    }
  }
  return(NA)
}

# Fuzzifikasi Data Training dan Testing
data_training$fuzzy <- sapply(data_training$`Hasil Produksi`, fuzzifikasi, bounds = interval_bounds)
data_testing$fuzzy <- sapply(data_testing$`Hasil Produksi`, fuzzifikasi, bounds = interval_bounds)

# Membangun FLR (Fuzzy Logic Rule)
FLR <- data.frame(Past = data_training$fuzzy[-nrow(data_training)], Next = data_training$fuzzy[-1])
FLR <- FLR[complete.cases(FLR), ]

# Membentuk FLRG untuk Model Chen dan Lee
FLRG_chen <- split(FLR$Next, FLR$Past)
FLRG_chen <- lapply(FLRG_chen, unique)

FLRG_lee <- split(FLR$Next, FLR$Past)

# Menghitung rata-rata untuk setiap grup FLRG (Defuzzifikasi)
FLRG_chen_avg <- lapply(FLRG_chen, function(x) mean(interval_midpoints[x], na.rm = TRUE))
FLRG_lee_avg <- lapply(FLRG_lee, function(x) mean(interval_midpoints[x], na.rm = TRUE))

# Fungsi untuk ramalan berdasarkan FLRG dan defuzzifikasi
get_forecast_based_on_previous_day_FLRG <- function(previous_fuzzy, FLRG_avg) {
  if (is.null(FLRG_avg[[as.character(previous_fuzzy)]])) {
    return(NA)
  } else {
    return(FLRG_avg[[as.character(previous_fuzzy)]])  # Mengembalikan nilai tengah grup
  }
}

# Ramalan Berdasarkan FLRG untuk Data Testing dengan model yang berbeda
data_testing$forecast_chen_dynamic <- NA
data_testing$forecast_lee_dynamic <- NA

# Langkah pertama: Ambil nilai fuzzifikasi dari data testing pertama
initial_fuzzy_testing_chen <- data_testing$fuzzy[1]  # Fuzzifikasi pertama untuk model Chen
initial_fuzzy_testing_lee <- data_testing$fuzzy[1]   # Fuzzifikasi pertama untuk model Lee

# Ramalan pertama untuk model Chen dan Lee
data_testing$forecast_chen_dynamic[1] <- get_forecast_based_on_previous_day_FLRG(initial_fuzzy_testing_chen, FLRG_chen_avg)
data_testing$forecast_lee_dynamic[1] <- get_forecast_based_on_previous_day_FLRG(initial_fuzzy_testing_lee, FLRG_lee_avg)

# Iterasi untuk ramalan berikutnya
for (i in 2:nrow(data_testing)) {
  # Untuk model Chen, gunakan nilai ramalan sebelumnya (Chen)
  fuzzy_prev_chen <- fuzzifikasi(data_testing$forecast_chen_dynamic[i - 1], interval_bounds)
  data_testing$forecast_chen_dynamic[i] <- get_forecast_based_on_previous_day_FLRG(fuzzy_prev_chen, FLRG_chen_avg)
  
  # Untuk model Lee, gunakan nilai ramalan sebelumnya (Lee)
  fuzzy_prev_lee <- fuzzifikasi(data_testing$forecast_lee_dynamic[i - 1], interval_bounds)
  data_testing$forecast_lee_dynamic[i] <- get_forecast_based_on_previous_day_FLRG(fuzzy_prev_lee, FLRG_lee_avg)
}

# Tabel hasil ramalan untuk data testing
forecast_table_testing <- data.frame(
  Tanggal = data_testing$Tanggal,
  Aktual = data_testing$`Hasil Produksi`,
  Forecast_Chen = data_testing$forecast_chen_dynamic,
  Forecast_Lee = data_testing$forecast_lee_dynamic
)

# Menampilkan hasil ramalan testing
cat("\nHasil Ramalan Testing:\n")
print(forecast_table_testing)

# Evaluasi MAPE dan RMSE
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

mape_testing_chen_dynamic <- mape(data_testing$`Hasil Produksi`, data_testing$forecast_chen_dynamic)
mape_testing_lee_dynamic <- mape(data_testing$`Hasil Produksi`, data_testing$forecast_lee_dynamic)

rmse_testing_chen_dynamic <- rmse(data_testing$`Hasil Produksi`, data_testing$forecast_chen_dynamic)
rmse_testing_lee_dynamic <- rmse(data_testing$`Hasil Produksi`, data_testing$forecast_lee_dynamic)

mape_rmse_table <- data.frame(
  Model = c("Chen (Testing)", "Lee (Testing)"),
  MAPE = c(mape_testing_chen_dynamic, mape_testing_lee_dynamic),
  RMSE = c(rmse_testing_chen_dynamic, rmse_testing_lee_dynamic)
)

cat("\nMAPE dan RMSE Testing:\n")
print(mape_rmse_table)

# Visualisasi Data
ggplot(data_testing, aes(x = Tanggal)) +
  geom_line(aes(y = `Hasil Produksi`, color = "Aktual"), size = 1) +
  geom_line(aes(y = forecast_chen_dynamic, color = "Chen Dynamic"), size = 1, linetype = "dashed") +
  geom_line(aes(y = forecast_lee_dynamic, color = "Lee Dynamic"), size = 1, linetype = "dotted") +
  labs(title = "Perbandingan Hasil Produksi vs Ramalan Chen dan Lee",
       x = "Tanggal", y = "Hasil Produksi / Ramalan") +
  scale_color_manual(name = "Legenda", values = c("Aktual" = "black", "Chen Dynamic" = "blue", "Lee Dynamic" = "red")) +
  theme_minimal()

