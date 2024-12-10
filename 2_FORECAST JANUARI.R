# Memuat library yang dibutuhkan
library(readxl)
library(ggplot2)

# Import data dari file Excel
DATA_FUZZY <- read_excel("G:/My Drive/Tugas Akhir Lulu/ANALISIS+LAPORAN/DATA_FUZZY.xlsx")
data <- DATA_FUZZY
print(head(data))

# Parameter margin d1 dan d2
d1 <- 7.57
d2 <- 4.83

# Menghitung nilai minimum dan maksimum untuk himpunan semesta
cat("Menghitung nilai minimum dan maksimum...\n")
nilai_min <- min(data$`Hasil Produksi`, na.rm = TRUE)
nilai_max <- max(data$`Hasil Produksi`, na.rm = TRUE)

universe_min <- nilai_min - d1
universe_max <- nilai_max + d2

cat("Nilai Minimum:", nilai_min, "\n")
cat("Nilai Maksimum:", nilai_max, "\n")
cat("Universe Min:", universe_min, "\n")
cat("Universe Max:", universe_max, "\n")

# Jumlah interval dan lebar interval
num_intervals <- 12
interval_width <- (universe_max - universe_min) / num_intervals
cat("Lebar Interval:", interval_width, "\n")

# Membuat batas dan nilai tengah interval
interval_bounds <- seq(universe_min, universe_max, by = interval_width)
interval_midpoints <- interval_bounds[-1] - (interval_width / 2)

cat("Interval Bounds:\n")
print(interval_bounds)
cat("Interval Midpoints:\n")
print(interval_midpoints)

# Fungsi fuzzifikasi
fuzzifikasi <- function(value, bounds) {
  if (value < bounds[1] || value > bounds[length(bounds)]) {
    return(NA)
  }
  for (i in 1:(length(bounds) - 1)) {
    if (value >= bounds[i] && value < bounds[i + 1]) {
      return(i)
    }
  }
  if (value == bounds[length(bounds)]) {
    return(length(bounds) - 1)
  }
  return(NA)
}

# Fuzzifikasi Data
data$fuzzy <- sapply(data$`Hasil Produksi`, fuzzifikasi, bounds = interval_bounds)

cat("Fuzzifikasi Seluruh Data:\n")
print(data[, c("Tanggal", "Hasil Produksi", "fuzzy")])

# Membentuk FLR (Fuzzy Logic Rule)
FLR <- data.frame(Past = data$fuzzy[-nrow(data)], Next = data$fuzzy[-1])
FLR <- FLR[complete.cases(FLR), ]

cat("FLR (Fuzzy Logic Relationships):\n")
print(FLR)

### Model Chen ###
# Membentuk FLRG untuk Model Chen
FLRG_chen <- split(FLR$Next, FLR$Past)
FLRG_chen <- lapply(FLRG_chen, unique)

# Menghitung rata-rata untuk setiap grup FLRG Chen
FLRG_chen_avg <- lapply(FLRG_chen, function(x) mean(interval_midpoints[x], na.rm = TRUE))

cat("FLRG Model Chen:\n")
print(FLRG_chen)
cat("Rata-rata FLRG Model Chen:\n")
print(FLRG_chen_avg)

# Fungsi untuk Meramalkan Berdasarkan FLRG Chen
get_forecast_based_on_group_chen <- function(fuzzy_value, FLRG_chen_avg) {
  if (is.na(fuzzy_value) || !(fuzzy_value %in% names(FLRG_chen_avg))) {
    return(NA)
  }
  return(FLRG_chen_avg[[as.character(fuzzy_value)]])
}

### Model Lee ###
# Membentuk FLRG untuk Model Lee
FLRG_lee <- split(FLR$Next, FLR$Past)

# Menghitung rata-rata untuk setiap grup FLRG Lee
FLRG_lee_avg <- lapply(FLRG_lee, function(x) mean(interval_midpoints[x], na.rm = TRUE))

cat("FLRG Model Lee:\n")
print(FLRG_lee)
cat("Rata-rata FLRG Model Lee:\n")
print(FLRG_lee_avg)

# Fungsi untuk Meramalkan Berdasarkan FLRG Lee
get_forecast_based_on_group_lee <- function(fuzzy_value, FLRG_lee_avg) {
  if (is.na(fuzzy_value) || !(fuzzy_value %in% names(FLRG_lee_avg))) {
    return(NA)
  }
  return(FLRG_lee_avg[[as.character(fuzzy_value)]])
}

# Membuat Data untuk Ramalan 7 Hari ke Depan
forecast_dates <- seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-01-07"), by = "day")
forecast_data <- data.frame(Tanggal = forecast_dates, forecast_chen_dynamic = NA, forecast_lee_dynamic = NA)

# Menggunakan Nilai Fuzzy Terakhir untuk Memulai Ramalan
last_fuzzy <- tail(data$fuzzy, 1)
forecast_data$forecast_chen_dynamic[1] <- get_forecast_based_on_group_chen(last_fuzzy, FLRG_chen_avg)
forecast_data$forecast_lee_dynamic[1] <- get_forecast_based_on_group_lee(last_fuzzy, FLRG_lee_avg)

# Melakukan Ramalan Dinamis untuk 7 Hari (Chen dan Lee)
for (i in 2:nrow(forecast_data)) {
  # Model Chen
  prev_forecast_chen <- forecast_data$forecast_chen_dynamic[i - 1]
  prev_fuzzy_chen <- fuzzifikasi(prev_forecast_chen, interval_bounds)
  forecast_data$forecast_chen_dynamic[i] <- get_forecast_based_on_group_chen(prev_fuzzy_chen, FLRG_chen_avg)
  
  # Model Lee
  prev_forecast_lee <- forecast_data$forecast_lee_dynamic[i - 1]
  prev_fuzzy_lee <- fuzzifikasi(prev_forecast_lee, interval_bounds)
  forecast_data$forecast_lee_dynamic[i] <- get_forecast_based_on_group_lee(prev_fuzzy_lee, FLRG_lee_avg)
}

cat("Ramalan 7 Hari ke Depan (Model Chen dan Lee):\n")
print(forecast_data)

# Visualisasi Peramalan 7 Hari ke Depan
ggplot(forecast_data, aes(x = Tanggal)) +
  geom_line(aes(y = forecast_chen_dynamic, color = "Forecast Chen"), size = 1, linetype = "dashed") +
  geom_line(aes(y = forecast_lee_dynamic, color = "Forecast Lee"), size = 1, linetype = "dotted") +
  labs(title = "Ramalan Produksi untuk 7 Hari ke Depan (Model Chen & Lee)",
       x = "Tanggal", y = "Hasil Produksi",
       color = "Legends") +
  theme_minimal() +
  scale_color_manual(values = c("Forecast Chen" = "blue", "Forecast Lee" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

