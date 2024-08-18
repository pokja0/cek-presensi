library(leaflet)

batas_kec <- batas_kec_sulbar %>%
  filter(Provinsi == "SULAWESI BARAT", 
         Kab_Kota == "MAMUJU",
         Kecamatan == "MAMUJU")

leaflet(batas_kec_sulbar) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=118.8914, lat=-2.669750, popup="The birthplace of R") %>%
  addPolygons()
