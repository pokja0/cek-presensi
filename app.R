library(shiny)
library(bslib)
library(lubridate)  # Paket untuk fungsi days_in_month
library(openxlsx)
library(DT)
library(dplyr)
library(tidyr)
library(formattable)
library(waiter)
library(leaflet)
library(sf)
library(bsicons)
library(shinymanager)
library(stringr)
# Define UI
data_map <- readxl::read_excel("hasil/cek_presensi_agustus_full.xlsx")

# define some basic credentials (on data.frame)
credentials <- data.frame(
  user = c("shiny", "admin.sulbar"), # mandatory
  password = c("azerty", "666803"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2024-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# Change language
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "Selamat Datang",
  "Username:" = "Masukkan Username:",
  "Password:" = "Masukkan password:",
  "Logout" = "Keluar",
  "Login" = "Masuk"
)

autoWaiter()
ui <- page_navbar(
  title = "Presensi PKB",
  nav_panel(
    title = "Rekap Mandiri", 
    layout_sidebar(
      sidebar = sidebar(
        width = "20%",
        fillable = T,
        fileInput("file1", "Upload Data"),
        selectInput("month", "Pilih Bulan", 
                    choices = month.name, 
                    selected = "August"),
        dateRangeInput("daterange", "Pilih Rentang Tanggal:",
                       start = NULL,
                       end = NULL,
                       min = NULL,
                       max = NULL,
                       format = "dd-mm-yyyy",
                       separator = " - "),
        actionButton(
          inputId = "cari",
          label = "Cari",
          style = "jelly", 
          color = "primary", size = "sm"
        ),
        uiOutput("download_data")
      ),
      card(
        DT::dataTableOutput("table")
      )
    )
  ),
  nav_panel(
    title = "Titik Presensi", 
      navset_card_pill(
        nav_panel(
          "Rincian",
          DT::dataTableOutput("table_map")
        ),
        nav_panel(
          "Peta",
          layout_sidebar(
            sidebar = sidebar(
              width = "20%",
              fillable = T,
              selectInput("pilih_kecamatan", "Pilih Kecamatan", choices = unique(data_map$Kecamatan)),
              selectInput("pilih_pkb", "Pilih PKB/PLKB", choices = NULL),
              selectInput("month_map", "Pilih Bulan", 
                          choices = month.name[8], 
                          selected = "August"),
              selectInput("pilih_tanggal", "Pilih Tanggal", choices = NULL),
            ), #sidebar
            uiOutput("vb_ket_presensi"),
            leafletOutput("leaflet_map"),
            fluidRow(verbatimTextOutput("map_marker_click"))
          ) #layout sidebar
        )
      )
  )
)

ui <- secure_app(
  ui,
  tags_top = 
    tags$div(
      tags$img(
        src = "https://bkkbnsulbar.id/wp-content/uploads/2022/12/cropped-logobkkbnsulbar.png", width = 100
      )
    )
)

# Define server logic
server <- function(input, output, session) {
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(check_credentials = check_credentials(credentials), keep_token = TRUE)
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Render UI for date range input based on selected month
  observe({
    req(input$month)
    
    # Tentukan bulan dalam format numerik
    month_num <- match(input$month, month.name)
    
    # Set start and end dates based on selected month
    start_date <- as.Date(sprintf("2024-%02d-01", month_num))
    end_date <- as.Date(sprintf("2024-%02d-%02d", month_num, 
                                days_in_month(start_date)))
    
    # Update dateRangeInput dengan tanggal baru
    updateDateRangeInput(session, "daterange", start = start_date, end = end_date)
  })
  
  output$teks_dates <- renderPrint({
    print(input$daterange[1])
  })
  
  data_presensi <- eventReactive(input$cari, {
    req(input$file1)
    presensi_pkb_juli <- read.xlsx(input$file1$datapath, startRow = 5)
    
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
    start_date <- input$daterange[1]
    # Mengubah format objek Date menjadi string dengan format "dd-mm-yyyy"
    start_date <- format(start_date, format = "%d-%m-%Y")
    
    # Konversi string tanggal ke objek Date
    end_date <- input$daterange[2]
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
             `Absen Masuk` = Absen.Masuk, `Absen Pulang` = Absen.Pulang, `Menit Lambat` = Menit_Lambat)
    
    
  })
  
  # Render table untuk data yang diunggah
  output$table <- DT::renderDataTable({
    withProgress(message = 'Kudu Sabar', value = 0, {
      data_presensi = data_presensi()
      
      incProgress(1/2, detail = paste("Import Data"))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
      
      bg = function(start, end, color, ...) {
        paste("linear-gradient(90deg,transparent ",percent(start),",",
              color, percent(start), ",", color, percent(end),
              ", transparent", percent(end),")")
      } 
      
      color_bar2 =  function (color = "lightgray", fun = "proportion", ...) 
      {
        fun <- match.fun(fun)
        formatter("span", style = function(x) style(display = "inline-block",
                                                    `unicode-bidi` = "plaintext", 
                                                    "background" = bg(1-fun(as.numeric(x), ...), 1, color), "width"="100%" ))
      }
      
      data_presensi = as.datatable(formattable(data_presensi,
                               list(
                                 `Masuk Kerja` = color_tile("#d9544d", "lightgreen"),
                                 `Hadir Normal` = color_tile("#d9544d", "lightgreen"),
                                 `Tanpa Keterangan` = color_tile("lightgreen", "#d9544d"),
                                 `Absen Masuk` = color_tile("lightgreen", "#d9544d"),
                                 `Absen Pulang` = color_tile("lightgreen", "#d9544d"),
                                 Telat = color_tile("lightgreen", "#d9544d"),
                                 `Menit Lambat` = color_bar2("#d9544d")
                               ))
      )
      incProgress(2/2, detail = paste("Import Data"))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    })
    data_presensi
  })
  
  output$download_data <- renderUI({
    req(input$cari)
    downloadButton("downloadData", "Download")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data_presensi(), file)
    }
  )
  
  observeEvent(input$cari, {
    req(input$file1)
    selected_month <- input$month
    
    # Ambil bulan dari kolom tanggal di dataframe
    data_cek <- read.xlsx(input$file1$datapath, startRow = 5)
    
    tanggal <- as.Date(data_cek$Tanggal[1], format = "%d-%m-%Y")
    
    # Mengambil nama bulan
    df_month <- format(tanggal, "%B")
    
    # Jika bulan yang dipilih berbeda dengan bulan di dataframe, tampilkan modal dialog
    if (selected_month != df_month) {
      showModal(modalDialog(
        title = "Peringatan",
        paste0("Bulan yang Anda pilih: '", input$month, "' dan bulan pada data '", df_month,
          "' berbeda"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  #Map
  
  data_map <- readxl::read_excel("hasil/cek_presensi_agustus_full.xlsx")
  batas_kec_sulbar <- readRDS("data/batas_kec_sulbar.rds")
  batas_kec_sulbar <- batas_kec_sulbar %>%
    mutate(Kab_Kota = if_else(Kab_Kota == "MAMUJU UTARA", "PASANGKAYU", Kab_Kota),
           Kecamatan = if_else(Kecamatan == "SIMBORO DAN KEPULAUAN", "SIMBORO", Kecamatan))
  
  observeEvent(input$pilih_kecamatan, {
    # Menentukan pilihan baru untuk selectInput kedua berdasarkan kategori yang dipilih
    pkb_name = data_map %>%
      filter(Kecamatan == input$pilih_kecamatan)
    updateSelectInput(session, "pilih_pkb", choices = unique(pkb_name$Nama))
  })

  observeEvent(input$pilih_pkb, {
    # Menentukan pilihan baru untuk selectInput kedua berdasarkan kategori yang dipilih
    date_data = data_map %>%
      filter(Kecamatan == input$pilih_kecamatan,
             Nama == input$pilih_pkb) 
    
    updateSelectInput(session, "pilih_tanggal", choices = date_data$Tanggal)
  })
  
  # output$input_date_map <- renderUI({
  #   date_data = data_map %>%
  #     filter(Kecamatan == input$pilih_kecamatan,
  #            Nama == input$pilih_pkb) 
  #   
  #   date_data = date_data$Tanggal
  #   
  #   selectInput("pilih_tanggal", "Pilih Tanggal", choices = date_data)
  # })
  
  output$table_map <- DT::renderDataTable({
    data_map = data_map %>%
      select(-X1)
    
    DT::datatable(
      data_map, 
      filter = 'top',
      colnames = c("Kabupaten","Kecamatan","NIP","Nama","Tanggal",
                 "Latitude 1","Longitude 1", "Latitude 2", "Longitude 2","Keterangan")
    )
  })
  
  output$vb_ket_presensi <- renderUI({
    Kecamatan1 = input$pilih_kecamatan
    Nama1 = input$pilih_pkb
    Tanggal1 = input$pilih_tanggal
    
    vb_data = data_map %>%
      filter(Kecamatan == Kecamatan1,
             Nama == Nama1,
             Tanggal == Tanggal1) 
    
    if(vb_data$presensi_cek == "Presensi Sesuai"){
      icons_vb = "check-circle"
    } else{
      icons_vb = "exclamation-octagon"
    }
    value_box(
      title = "Keterangan:",
      value = vb_data$presensi_cek,
      showcase = bs_icon(icons_vb)
    )
    
  })

  output$leaflet_map <- renderLeaflet({
    data_map = data_map %>%
      filter(Kecamatan == input$pilih_kecamatan,
             Nama == input$pilih_pkb,
             Tanggal == input$pilih_tanggal)
    
    data_map$lat1 <- as.numeric(data_map$lat1)
    data_map$lat2 <- as.numeric(data_map$lat2)
    data_map$lon1 <- as.numeric(data_map$lon1)
    data_map$lon2 <- as.numeric(data_map$lon2)
    
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
      # Membuat leaflet map
      leaflet(batas_kec) %>%
        addTiles() %>%
        addPolygons(
          label = ~Kecamatan
        ) %>%
        addMarkers(lng = selected_row$lon1, lat = selected_row$lat1, popup = "Awal", label = "Awal") %>%
        addMarkers(lng = selected_row$lon2, lat = selected_row$lat2, popup = "Akhir", label = "Akhir") %>%
        addCircles(lng = titik_kantor_opd$Long, lat = titik_kantor_opd$Lat, radius = 1000, color = "red", fillColor = "red", fillOpacity = 0.2) %>%
        setView(lng = (selected_row$lon1 + selected_row$lon2) / 2, lat = (selected_row$lat1 + selected_row$lat2) / 2, zoom = 12)
    }
    
    # Contoh pemanggilan fungsi
    create_leaflet_map(data_map, 1)
  })
  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click  # typo was on this line
    print(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
