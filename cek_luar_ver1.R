library(sf)
library(leaflet)
library(dplyr)
library(openxlsx)
library(units)

batas_kec_sulbar <- readRDS("data/batas_kec_sulbar.rds")
batas_kec_sulbar <- batas_kec_sulbar %>%
  mutate(Kab_Kota = if_else(Kab_Kota == "MAMUJU UTARA", "PASANGKAYU", Kab_Kota),
         Kecamatan = if_else(Kecamatan == "SIMBORO DAN KEPULAUAN", "SIMBORO", Kecamatan))
presensi <- readxl::read_excel("/home/hi/Documents/projects/Scraping Profil Desa/hasil/scrap_nov_presensi.xlsx")
presensi$lat1 <- as.numeric(presensi$lat1)
presensi$lat2 <- as.numeric(presensi$lat2)
presensi$lon1 <- as.numeric(presensi$lon1)
presensi$lon2 <- as.numeric(presensi$lon2)

presensi <- presensi %>%
  mutate(lat1 = if_else(lat1 > 10, 0, lat1),
         lat2 = if_else(lat2 > 10, 0, lat2)
         )

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

####
for (i in 1:nrow(presensi)) {
  print(i)
  batas_kec = batas_kec_sulbar %>%
    filter(Kab_Kota == presensi$Kabupaten[i],
           Kecamatan == presensi$Kecamatan[i])
  
  titik_awal <- st_sfc(st_point(c(presensi$lon1[i], presensi$lat1[i])), 
                       crs = st_crs(batas_kec))
  hasil_awal <- st_within(titik_awal, batas_kec)
  
  titik_akhir <- st_sfc(st_point(c(presensi$lon2[i], presensi$lat2[i])), 
                        crs = st_crs(batas_kec))
  hasil_akhir <- st_within(titik_akhir, batas_kec)
  
  ## Cek OPD KB
  kantor_opd_filter = kantor_opd %>%
    filter(Kab == presensi$Kabupaten[i])
  
  opd_long = kantor_opd_filter$Long
  opd_lat = kantor_opd_filter$Lat
  presensi_lon1 = presensi$lon1[i]
  presensi_lat1 = presensi$lat1[i]
  
  presensi_lon2 = presensi$lon2[i]
  presensi_lat2 = presensi$lat2[i]
  
  cek_radius <- function(lon_pusat, lat_pusat, lon_pemeriksa, lat_pemeriksa, radius_m = 1000) {
    # Membuat titik pusat dan titik yang akan diperiksa
    titik_pusat <- st_point(c(lon_pusat, lat_pusat)) # longitude, latitude
    titik_pemeriksa <- st_point(c(lon_pemeriksa, lat_pemeriksa)) # longitude, latitude
    
    # Mengubah menjadi objek sf dengan CRS WGS 84 (EPSG: 4326)
    sf_pusat <- st_sfc(titik_pusat, crs = 4326)
    sf_pemeriksa <- st_sfc(titik_pemeriksa, crs = 4326)
    
    # Mengubah ke CRS yang menggunakan meter sebagai satuan (misalnya UTM Zone 48S, EPSG: 32748)
    sf_pusat <- st_transform(sf_pusat, crs = 32748)
    sf_pemeriksa <- st_transform(sf_pemeriksa, crs = 32748)
    
    # Menghitung jarak antara dua titik dalam meter
    jarak <- st_distance(sf_pusat, sf_pemeriksa)
    
    # Konversi radius ke objek dengan satuan yang sama
    threshold <- set_units(radius_m, "m")
    
    # Menentukan apakah titik berada di luar radius yang ditentukan
    di_luar_radius <- jarak > threshold
    
    return(di_luar_radius)
  }
  
  awal_opd_kb <- cek_radius(presensi_lon1, presensi_lat1, opd_long, opd_lat)
  akhir_opd_kb <- cek_radius(presensi_lon2, presensi_lat2, opd_long, opd_lat)
  # Mengecek hasilnya
  if ((lengths(hasil_awal) > 0 && lengths(hasil_akhir) > 0) || 
      (lengths(hasil_awal) > 0 && awal_opd_kb == F) ||
      (lengths(hasil_awal) > 0 && akhir_opd_kb == F) ||
      (lengths(hasil_akhir) > 0 && awal_opd_kb == F ) ||
      (lengths(hasil_akhir) > 0 && akhir_opd_kb == F ) ||
      (awal_opd_kb == F && akhir_opd_kb == F)) {
    print("Titik berada di dalam poligon kecamatan.")
    presensi$presensi_cek[i] = "Presensi Sesuai"
  #} else if (lengths(hasil_awal) > 0 & lengths(hasil_akhir) <= 0) {
    #print("Titik akhir berada di luar poligon kecamatan.")
    #presensi$presensi_cek[i] = "Titik akhir berada di luar poligon kecamatan"
  #} else if (lengths(hasil_awal) <= 0 & lengths(hasil_akhir) > 0) {
   # print("Titik awal berada di luar poligon kecamatan.")
  #  presensi$presensi_cek[i] = "Titik awal berada di luar poligon kecamatan"
  } else {
    print("Titik berada di luar poligon kecamatan.")
    presensi$presensi_cek[i] = "Presensi Tidak Sesuai"
  }
  
}


write.xlsx(presensi, "hasil/cek_presensi_nov_full.xlsx")
