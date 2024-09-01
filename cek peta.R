setwd("/home/hi/Documents/projects/cek-presensi")
library(leaflet)
library(tidygeocoder)
batas_kec_sulbar <- readRDS("data/batas_kec_sulbar.rds")
batas_kec_sulbar <- batas_kec_sulbar %>%
  mutate(Kab_Kota = if_else(Kab_Kota == "MAMUJU UTARA", "PASANGKAYU", Kab_Kota))

presensi <- read.xlsx("data/agustus_presensi.xlsx")
presensi$lat1 <- as.numeric(presensi$lat1)
presensi$lat2 <- as.numeric(presensi$lat2)
presensi$lon1 <- as.numeric(presensi$lon1)
presensi$lon2 <- as.numeric(presensi$lon2)

presensi$presensi_cek= 1:nrow(presensi)

###
kantor_opd <- data.frame(
  Kab = c("PASANGKAYU", "MAMUJU TENGAH", "MAMUJU", 
          "MAJENE", "POLEWALI MANDAR", "MAMASA"),
  Nama = c("OPD KB Pasangkayu", "OPD KB MATENG", "OPD KB MAMUJU", 
           "OPD KB MAJENE", "OPD KB POLMAN", "OPD KB Mamasa"),
  Lat = as.numeric(c(-1.1746695, -2.0615864, -2.6780518, 
                     -3.5421326, -3.4174718, -2.9458679)),
  Long = as.numeric(c(119.3759606, 119.2865281, 118.8911821, 
                      118.9849558, 119.319912, 119.3700759))
)


# Fungsi untuk membuat leaflet map
create_leaflet_map <- function(presensi_data, index) {
  # Memeriksa apakah index valid
  if (index > nrow(presensi_data) || index < 1) {
    stop("Index out of bounds.")
  }
  
  # Menyaring data berdasarkan index
  selected_row <- presensi_data[index, ]
  
  titik_kantor_opd = kantor_opd %>%
    filter(Kab == selected_row$Kabupaten)
  
  batas_kec = batas_kec_sulbar %>%
    filter(Kecamatan == selected_row$Kecamatan)

  
  # Gunakan reverse_geocode untuk mengambil alamat
  hasil = tibble(
    latitude = c(selected_row$lat1, selected_row$lat2),
    longitude = c(selected_row$lon1, selected_row$lon2)
  ) %>%
    reverse_geocode(
      lat = latitude,
      long = longitude,
      method = "osm",
      address = address_found,
      full_results = TRUE
    )
  # Membuat leaflet map
  leaflet(batas_kec) %>%
    addTiles() %>%
    addPolygons() %>%
    addMarkers(lng = selected_row$lon1, lat = selected_row$lat1, 
               popup = paste(hasil$address_found[1], hasil$road[1], hasil$village[1], 
                             hasil$municipality[1], sep = " | "), 
               label = "Awal") %>%
    addMarkers(lng = selected_row$lon2, lat = selected_row$lat2, 
               popup = paste(hasil$address_found[2], hasil$road[1],
                             hasil$village[2], hasil$municipality[2], sep = " | "),
               label = "Akhir") %>%
    addCircles(lng = titik_kantor_opd$Long, lat = titik_kantor_opd$Lat, radius = 1000, color = "red", fillColor = "red", fillOpacity = 0.2) %>%
    setView(lng = (selected_row$lon1 + selected_row$lon2) / 2, 
            lat = (selected_row$lat1 + selected_row$lat2) / 2, zoom = 12)
}

# Contoh pemanggilan fungsi
create_leaflet_map(presensi, 1)

cari_alamat$address_found[1]

