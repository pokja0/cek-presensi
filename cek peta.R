library(leaflet)

batas_kec <- batas_kec_sulbar %>%
  filter(Provinsi == "SULAWESI BARAT", 
         Kab_Kota == "PASANGKAYU",
         Kecamatan == "DAPURANG")

leaflet(batas_kec) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=119.3356, lat=-1.744539, popup="The birthplace of R") %>%
  addPolygons()
