library(tidyverse)
library(openxlsx)
library(stringr)
presensi_pkb_juli <- read.xlsx("data/agustus_presensiadmin.sulbar.xlsx", startRow = 5)

data_tes <- presensi_pkb_juli %>%
  #filter(NIP %in% c("197905252008012012", "196805011991031015", "199802192022211002", "199309132022212003")) %>%
  select(NIP, Nama, Tanggal, Jam.Masuk, Jam.Pulang) %>%
  unique() %>%
  # Menambahkan kolom "Masuk" dan "Pulang" berdasarkan logika yang diberikan
  mutate(
    # Konversi jam ke format waktu, menangani NA
    Jam.Masuk.Time = ifelse(is.na(Jam.Masuk), NA, as.POSIXct(Jam.Masuk, format = "%H:%M")),
    Jam.Pulang.Time = ifelse(is.na(Jam.Pulang), NA, as.POSIXct(Jam.Pulang, format = "%H:%M")),
    
    # Kolom Masuk
    Keterangan = case_when(
      Jam.Masuk.Time < as.POSIXct("06:00", format = "%H:%M") | 
        Jam.Masuk.Time > as.POSIXct("07:30", format = "%H:%M") ~ "TM",
      is.na(Jam.Masuk.Time) ~ "TM5",
      is.na(Jam.Pulang.Time) ~ "A1",
      is.na(Jam.Masuk.Time) & is.na(Jam.Pulang.Time) ~ "TK",
      TRUE ~ "HN"
    )
  ) %>%
  select(-c(Jam.Masuk.Time, Jam.Pulang.Time))
data_tes$NIP <- as.character(data_tes$NIP)
# Menggabungkan kolom Jam_Masuk dan Jam_Pulang menjadi satu
data_tes <- data_tes %>%
  unite("Jam.Masuk.Pulang", Jam.Masuk, Jam.Pulang, Keterangan, sep = " - ")

# Mengubah tanggal menjadi nama kolom dengan nilai dari kolom yang digabung
data_tes_wide <- data_tes %>%
  pivot_wider(names_from = Tanggal, values_from = Jam.Masuk.Pulang) %>%
  mutate_all(~ ifelse(is.na(.), "TK", .))

# Mengambil nama kolom tanggal dan mengurutkannya
tanggal_cols <- colnames(data_tes_wide)[-(1:2)]

# Konversi string tanggal ke objek Date
start_date <- "01-08-2024"
# Mengubah format objek Date menjadi string dengan format "dd-mm-yyyy"
start_date <- format(start_date, format = "%d-%m-%Y")

# Konversi string tanggal ke objek Date
end_date <- "31-08-2024"
# Mengubah format objek Date menjadi string dengan format "dd-mm-yyyy"
end_date <- format(end_date, format = "%d-%m-%Y")

generate_date_vector <- function(start_date_str, end_date_str) {
  # Konversi string tanggal ke objek Date dengan format "dd-mm-yyyy"
  start_date <- as.Date(start_date_str, format = "%d-%m-%Y")
  end_date <- as.Date(end_date_str, format = "%d-%m-%Y")
  
  # Buat vektor tanggal dari start_date hingga end_date
  date_vector <- seq(start_date, end_date, by = "day")
  
  # Format tanggal dalam format "dd-mm-yyyy"
  formatted_dates <- format(date_vector, format = "%d-%m-%Y")
  
  return(formatted_dates)
}

date_vector <- generate_date_vector(start_date, end_date)

tanggal_cols <- tanggal_cols[tanggal_cols %in% date_vector]

tanggal_cols_sorted <- sort(as.Date(tanggal_cols, format = "%d-%m-%Y"))

# Mengurutkan data frame berdasarkan kolom yang diurutkan
data_tes_wide <- data_tes_wide %>%
  select(NIP, Nama, all_of(format(tanggal_cols_sorted, "%d-%m-%Y")))

# Ubah nama kolom menjadi tipe Date untuk mengenali hari dalam minggu
tanggal_as_date <- as.Date(tanggal_cols, format = "%d-%m-%Y")
# Identifikasi hari Sabtu (6) dan Minggu (7)
days_of_week <- weekdays(tanggal_as_date)
weekend_cols <- tanggal_cols[days_of_week %in% c("Saturday", "Sunday")]

# Drop kolom-kolom yang jatuh pada hari Sabtu atau Minggu
data_tes_wide <- data_tes_wide[, !(names(data_tes_wide) %in% weekend_cols)]


# Mengambil nama kolom tanggal dan mengurutkannya (perbarui setelah keluarkan sabtu minggu)
tanggal_cols <- colnames(data_tes_wide)[-(1:2)]
tanggal_cols_sorted <- sort(as.Date(tanggal_cols, format = "%d-%m-%Y"))

### Menhitung telat
# Mengubah tanggal menjadi nama kolom dengan nilai dari kolom yang digabung
data_tes_wide_telat <- data_tes_wide

data_tes_wide_telat <- data_tes_wide_telat %>%
  gather("Tanggal", "Presensi", 3:ncol(data_tes_wide_telat)) %>%
  mutate(
    Waktu_Masuk = str_extract(Presensi, "^\\d{2}:\\d{2}") %>% 
      ifelse(. %in% c("NA", "TK") | is.na(.), "12:00", .),
    Menit_Lambat = ifelse(
      is.na(Waktu_Masuk), 
      270, 
      pmax(
        as.numeric(difftime(
          as.POSIXct(Waktu_Masuk, format = "%H:%M"), 
          as.POSIXct("07:30", format = "%H:%M"), 
          units = "mins"
        )), 
        0
      )
    )
  ) %>%
  select(NIP, Menit_Lambat) %>%
  group_by(NIP) %>%
  summarise(Menit_Lambat = sum(Menit_Lambat))

##

data_tes_wide <- data_tes_wide %>%
  mutate(
    Hari.Kerja = ncol(data_tes_wide) - 2
  )

# Menghitung jumlah "HN" untuk setiap baris dalam kolom-kolom tersebut
data_tes_wide$Hadir.Normal <- rowSums(sapply(data_tes_wide[, tanggal_cols], function(x) grepl("HN", x)))
data_tes_wide$Telat <- rowSums(sapply(data_tes_wide[, tanggal_cols], function(x) grepl("TM", x)))
data_tes_wide$Absen.Masuk <- rowSums(sapply(data_tes_wide[, tanggal_cols], function(x) x == "TM5"))
data_tes_wide$Absen.Pulang <- rowSums(sapply(data_tes_wide[, tanggal_cols], function(x) grepl("A1", x)))
data_tes_wide$Tanpa.Keterangan <- rowSums(sapply(data_tes_wide[, tanggal_cols], function(x) grepl("TK", x)))

data_tes_wide <- data_tes_wide %>%
  mutate(Masuk.Kerja = Telat + Absen.Masuk + Absen.Pulang + Hadir.Normal) %>%
  select(NIP, Nama, Hari.Kerja, Masuk.Kerja, Hadir.Normal, Tanpa.Keterangan, 
         Absen.Masuk, Absen.Pulang, Telat,
         all_of(format(tanggal_cols_sorted, "%d-%m-%Y")))

pkb_kec = read.xlsx("data/pkb_kec.xlsx")
data_tes_wide <- inner_join(pkb_kec, data_tes_wide, by = "NIP")
data_tes_wide <- inner_join(data_tes_wide_telat, data_tes_wide, by = "NIP")

data_tes_wide <- data_tes_wide %>%
  select(-c(3,6)) %>%
  select(Kabupaten:Nama.y, NIP, Hari.Kerja:Telat, Menit_Lambat, everything())

data_tes_wide = data_tes_wide %>%
  rename(Nama = Nama.y, `Hari Kerja` = Hari.Kerja, `Masuk Kerja` = Masuk.Kerja,
         `Hadir Normal` = Hadir.Normal, `Tanpa Keterangan` = Tanpa.Keterangan,
         `Absen Masuk` = Absen.Masuk, `Absen Pulang` = Absen.Pulang, `Menit Lambar` = Menit_Lambat)
