library(sf)
library(leaflet)
library(dplyr)
library(openxlsx)

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
  Nama = c("OPD KB Pasangkayu", "OPD KB MATENG", "OPD KB MAMUJU", 
           "OPD KB MAJENE", "OPD KB POLMAN", "OPD KB Mamasa"),
  Lat = as.numeric(c(-1.1746695, -2.0615864, -2.6780518, 
                     -3.5421326, -3.4174718, -2.9458679)),
  Long = as.numeric(c(119.3759606, 119.2865281, 118.8911821, 
                      118.9849558, 119.319912, 119.3700759))
)

####
for (i in 1:nrow(presensi)) {
  
  batas_kec = batas_kec_sulbar %>%
    filter(Kab_Kota == presensi$Kabupaten[i],
           Kecamatan == presensi$Kecamatan[i])
  
  titik_awal <- st_sfc(st_point(c(presensi$lon1[i], presensi$lat1[i])), 
                       crs = st_crs(batas_kec))
  hasil_awal <- st_within(titik_awal, batas_kec)
  
  titik_akhir <- st_sfc(st_point(c(presensi$lon2[i], presensi$lat2[i])), 
                        crs = st_crs(batas_kec))
  hasil_akhir <- st_within(titik_akhir, batas_kec)
  
  # Mengecek hasilnya
  if (lengths(hasil_awal) > 0 & lengths(hasil_akhir) > 0) {
    print("Titik berada di dalam poligon kecamatan.")
    presensi$presensi_cek[i] = "Titik berada di dalam poligon kecamatan"
  } else if (lengths(hasil_awal) > 0 & lengths(hasil_akhir) <= 0) {
    print("Titik akhir berada di luar poligon kecamatan.")
    presensi$presensi_cek[i] = "Titik akhir berada di luar poligon kecamatan"
  } else if (lengths(hasil_awal) <= 0 & lengths(hasil_akhir) > 0) {
    print("Titik awal berada di luar poligon kecamatan.")
    presensi$presensi_cek[i] = "Titik awal berada di luar poligon kecamatan"
  } else {
    print("Titik berada di luar poligon kecamatan.")
    presensi$presensi_cek[i] = "Titik berada di luar poligon kecamatan"
  }
  
}


write.xlsx(presensi, "cek_presensi_agustus.xlsx")
#########
presensi$cek[1] = 2 

for (i in 1:4697) {
  
  batas_kec = batas_kec_sulbar %>%
    filter(Kab_Kota == presensi$Kabupaten[i],
           Kecamatan == presensi$Kecamatan[i])
  
  titik <- st_sfc(st_point(c(presensi$lon1[i], presensi$lat1[i])), 
                  crs = st_crs(batas_kec))
  
  hasil <- st_within(titik, batas_kec)
  
  # Mengecek hasilnya
  if (lengths(hasil) > 0) {
    print("Titik berada di dalam poligon kecamatan.")
    presensi$cek[i] = "Titik berada di dalam poligon kecamatan"
  } else {
    print("Titik berada di luar poligon kecamatan.")
    presensi$cek[i] = "Titik berada di luar poligon kecamatan"
  }
  
}

plot(batas_kec_sulbar$geometry)
plot(batas_kec_mapilli$geometry)

titik <- st_sfc(st_point(c(118.8907, -2.677908)), crs = st_crs(batas_kec_sulbar))

hasil <- st_within(titik, batas_kec_sulbar)

# Mengecek hasilnya
if (lengths(hasil) > 0) {
  print("Titik berada di dalam poligon kecamatan.")
} else {
  print("Titik berada di luar poligon kecamatan.")
}

/html/body/div[3]/div[3]/div/div/div/div/div[2]/div/form/div/div[2]/div/div[1]/table/tbody/tr[1]/td[5]/a