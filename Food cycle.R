# Memuat library
library(shiny)
library(leaflet)
library(dplyr)

# Dummy database akun & donasi
akun_db <- data.frame(
  email = character(),
  password = character(),
  nama = character(),
  alamat = character(),
  role = character(),
  stringsAsFactors = FALSE
)

donasi_db <- data.frame(
  tanggal_input = as.Date(character()),
  waktu_input = character(),
  nama_kontributor = character(),
  alamat = character(),
  lokasi = character(),
  jenis_makanan = character(),
  jumlah_porsi = numeric(),
  status = character(),
  stringsAsFactors = FALSE
)

penerima_db <- data.frame(
  nama_penerima = c("Panti Asuhan Yatim & Dhuafa Mizan Amanah Yogyakarta", "Panti Asuhan Anak Yatim Amanah", "Panti Jompo Pakem"),
  alamat = c("Jl. Melati Wetan No.8A", "Jl. Imogiri Tim. No.KM. 12", "JL. Kaliurang Km 17"),
  lokasi = c("-7.791697843281471, 110.386852724643", "-7.896325658392188, 110.3816462399871", "-7.661079928559681, 110.42446439580564"),
  kebutuhan_porsi = c(25, 30, 15),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', sans-serif;
        background-color: #ffffff; /* putih bersih */
        color: #2e7d32; /* hijau gelap untuk teks */
        margin: 0;
      }
      .centered-modal {
        position: fixed;
        top: 0; left: 0;
        width: 100vw; height: 100vh;
        display: flex; align-items: center; justify-content: center;
        background: rgba(46,125,50,0.1); /* transparansi hijau lembut */
        z-index: 1000;
      }
      .modal-content {
        background: #f9fff9; /* putih agak hijau muda */
        padding: 32px 40px;
        border-radius: 14px;
        box-shadow: 0 8px 32px rgba(46,125,50,0.2);
        min-width: 340px; min-height: 200px;
        border: 2px solid #a5d6a7; /* border hijau muda */
      }
      .main-fullscreen {
        min-height: 100vh;
        padding: 0;
        background: #e8f5e9; /* hijau sangat muda sebagai background */
      }
      .navbar-custom {
        background: #2e7d32; /* hijau gelap */
        color: #fff;
        padding: 18px 40px;
        font-size: 1.2em;
        display: flex;
        align-items: center;
        justify-content: space-between;
        box-shadow: 0 3px 8px rgba(46,125,50,0.4);
      }
      .navbar-custom .title {
        font-weight: bold;
        font-size: 1.3em;
        letter-spacing: 1px;
      }
      .navbar-custom .logout-btn {
        background: #a5d6a7; /* hijau muda */
        color: #2e7d32;
        border: none;
        padding: 8px 18px;
        border-radius: 6px;
        font-weight: 600;
        cursor: pointer;
        transition: background-color 0.3s ease;
      }
      .navbar-custom .logout-btn:hover {
        background: #81c784; /* hijau sedang saat hover */
      }
      .tabbable > .nav > li > a {
        color: #2e7d32;
        font-weight: 600;
      }
      .tabbable > .nav > li > a:hover {
        background-color: #a5d6a7;
        color: #1b5e20;
      }
      .tabbable > .nav > li[class=active] > a, 
      .tabbable > .nav > li[class=active] > a:hover {
        background-color: #4caf50;
        color: white;
        font-weight: 700;
      }
      .btn {
        background-color: #4caf50; /* hijau */
        color: white;
        border: none;
        transition: background-color 0.3s ease;
      }
      .btn:hover {
        background-color: #388e3c; /* hijau lebih gelap */
      }
      .form-control {
        margin-bottom: 14px;
        border: 1.5px solid #81c784; /* border hijau muda */
        border-radius: 6px;
        padding: 8px 12px;
      }
      input[type='text'], input[type='password'], select {
        background: #f0f9f0; /* putih dengan hint hijau */
        color: #2e7d32;
      }
      
    "))
  ),
  uiOutput("dynamic_ui")
)


server <- function(input, output, session) {
  user_logged <- reactiveVal(NULL)
  hasil_distribusi <- reactiveVal(NULL)
  
  output$userLogged <- reactive({ !is.null(user_logged()) })
  outputOptions(output, "userLogged", suspendWhenHidden = FALSE)
  
  output$auth_ui <- renderUI({
    req(input$role != "Pilih")
    radioButtons("has_account", "Apakah sudah memiliki akun?", choices = c("Ya", "Belum"))
  })
  
  observeEvent(input$has_account, {
    output$main_ui <- renderUI({
      if (input$has_account == "Ya") {
        tagList(
          textInput("login_email", "Email"),
          passwordInput("login_password", "Password"),
          actionButton("login_btn", "Login")
        )
      } else {
        if (input$role == "Kontributor" || input$role == "Admin") {
          tagList(
            textInput("reg_email", "Email"),
            passwordInput("reg_password", "Password"),
            textInput("reg_nama", ifelse(input$role == "Admin", "Nama Lembaga / Perorangan", "Nama perorangan / lembaga")),
            textInput("reg_alamat", "Alamat"),
            actionButton("register_btn", "Submit & Simpan")
          )
        } else {
          tagList(p("Silakan pilih role terlebih dahulu."))
        }
      }
    })
  })
  
  output$dynamic_ui <- renderUI({
    if (is.null(user_logged())) {
      # Tampilkan modal login/register seperti sebelumnya
      div(class = "centered-modal",
          div(class = "modal-content",
              div(style="text-align:center;margin-bottom:18px;",
                  tags$span("🍲", style = "font-size:48px;"),
                  h2("FOOD CYCLE"),
                  p("Solusi untuk Mengurangi Food Waste", style="color:#2e7d32;")
              ),
              selectInput("role", "Masuk sebagai:", choices = c("Kontributor", "Admin"), width="100%"),
              uiOutput("auth_ui"),
              uiOutput("main_ui")
          )
      )
    } else {
      # Tampilkan halaman utama dengan navbar dan welcome message di atas
      div(class = "main-fullscreen",
          div(class = "navbar-custom",
              span(class = "title", "FOOD CYCLE"),
              actionButton("logout_btn", "Logout", class = "logout-btn")
          ),
          # Tambahkan welcome message di sini
          div(style="text-align:center;margin-bottom:18px; padding-top: 20px;",
              tags$span("🍲", style = "font-size:48px;"),
              h2(paste("WELCOME TO FOOD CYCLE,", user_logged()$nama)),
              p("AYO KURANGI FOOD WASTE DAN MULAILAH BERDONASI MAKANAN", style="color:#2e7d32; font-weight: 600;")
          ),
          div(style="padding:32px 40px 40px 40px;",
              uiOutput("main_ui")
          )
      )
    }
  })
  
  observeEvent(input$register_btn, {
    existing_user <- subset(akun_db, email == input$reg_email & role == input$role)
    if (nrow(existing_user) > 0) {
      showModal(modalDialog("Email sudah terdaftar untuk role tersebut. Silakan login.", easyClose = TRUE))
    } else {
      akun_db <<- rbind(akun_db, data.frame(
        email = input$reg_email,
        password = input$reg_password,
        nama = input$reg_nama,
        alamat = input$reg_alamat,
        role = input$role,
        stringsAsFactors = FALSE
      ))
      showModal(modalDialog("Registrasi berhasil! Silakan login dengan data yang sudah didaftarkan.", easyClose = TRUE))
      updateRadioButtons(session, "has_account", selected = "Ya")
      output$main_ui <- renderUI({
        tagList(
          textInput("login_email", "Email", value = input$reg_email),
          passwordInput("login_password", "Password", value = input$reg_password),
          actionButton("login_btn", "Login")
        )
      })
    }
  })
  
  observeEvent(input$login_btn, {
    akun <- subset(akun_db, email == input$login_email & password == input$login_password & role == input$role)
    if (nrow(akun) == 1) {
      user_logged(as.list(akun[1, ]))
    } else {
      showModal(modalDialog("Login gagal. Email atau password salah."))
    }
  })
  
  food_index <- reactiveVal(1)
  
  observe({
    req(user_logged())
    if (user_logged()$role == "Kontributor") {
      output$main_ui <- renderUI({
        tabsetPanel(
          tabPanel("Input Donasi", 
                   dateInput("tanggal_input", "Tanggal Input", value = Sys.Date()),
                   textInput("waktu_input", "Waktu Input (misal 10:00 WIB)"),
                   textInput("alamat_input", "Alamat Lokasi Donasi"),
                   textInput("lokasi", "Link Google Maps"),
                   tags$div(id = "donasi_container"),
                   actionButton("add_makanan", "Tambah Jenis Makanan"),
                   actionButton("submit_donasi", "Kirim ke Admin")
          ),
          tabPanel("Lihat Donasi", h3("Riwayat Donasi Anda"), tableOutput("tabel_donasi"))
        )
      })
    } else if (user_logged()$role == "Admin") {
      output$main_ui <- renderUI({
        tabsetPanel(
          tabPanel("Kelola Donasi",
                   h3("Semua Donasi Masuk"),
                   uiOutput("admin_tabel_donasi"),
                   actionButton("prosesDistribusi", "Proses Distribusi"),
                   tableOutput("hasil_distribusi_table")
          ),
          tabPanel("Kelola Distribusi",
                   fluidRow(
                     column(6, h3("Data Lembaga Penerima"), tableOutput("tabel_penerima")),
                     column(6, h3("Peta Lokasi Lembaga"), leafletOutput("peta_lokasi"))
                   )
          )
        )
      })
    }
  })
  
  observeEvent(input$add_makanan, {
    id <- food_index()
    insertUI(
      selector = "#donasi_container",
      where = "beforeEnd",
      ui = tags$div(id = paste0("food_row_", id),
                    fluidRow(
                      column(6, textInput(paste0("jenis_makanan_", id), paste("Jenis Makanan", id))),
                      column(4, numericInput(paste0("jumlah_porsi_", id), paste("Jumlah Porsi", id), value = 1, min = 1)),
                      column(2, actionButton(paste0("remove_", id), "-", class = "btn btn-danger btn-sm"))
                    )
      )
    )
    observeEvent(input[[paste0("remove_", id)]], {
      removeUI(selector = paste0("#food_row_", id))
    }, once = TRUE)
    
    food_index(id + 1)
  })
  
  observeEvent(input$submit_donasi, {
    max_id <- food_index() - 1
    for (i in 1:max_id) {
      jenis_id <- paste0("jenis_makanan_", i)
      porsi_id <- paste0("jumlah_porsi_", i)
      if (!is.null(input[[jenis_id]]) && input[[jenis_id]] != "" &&
          !is.null(input[[porsi_id]]) && input[[porsi_id]] > 0) {
        donasi_db <<- rbind(donasi_db, data.frame(
          tanggal_input = input$tanggal_input,
          waktu_input = input$waktu_input,
          nama_kontributor = user_logged()$nama,
          alamat = input$alamat_input,
          lokasi = input$lokasi,
          jenis_makanan = input[[jenis_id]],
          jumlah_porsi = input[[porsi_id]],
          status = "On Progress",
          stringsAsFactors = FALSE
        ))
      }
    }
    showModal(modalDialog("Donasi berhasil dikirim ke Admin Food Bank!"))
    removeUI(selector = "#donasi_container > div", multiple = TRUE)
    food_index(1)
  })
  
  output$tabel_donasi <- renderTable({
    data <- subset(donasi_db, nama_kontributor == user_logged()$nama)[,
                                                                      c("tanggal_input", "waktu_input", "nama_kontributor", "alamat", "lokasi", "jenis_makanan", "jumlah_porsi", "status")]
    colnames(data) <- c("Tanggal Input", "Waktu Input", "Nama Kontributor", "Alamat", "Lokasi", "Jenis Makanan", "Jumlah Porsi", "Status")
    data
  })
  
  output$admin_tabel_donasi <- renderUI({
    if (nrow(donasi_db) == 0) return(HTML("<p>Belum ada donasi masuk.</p>"))
    donasi_rows <- lapply(1:nrow(donasi_db), function(i) {
      fluidRow(
        column(2, donasi_db$tanggal_input[i]),
        column(1, donasi_db$waktu_input[i]),
        column(2, donasi_db$nama_kontributor[i]),
        column(2, donasi_db$jenis_makanan[i]),
        column(1, donasi_db$jumlah_porsi[i]),
        column(2, textOutput(paste0("status_text_", i))),
        column(2, if (donasi_db$status[i] != "Finish") {
          actionButton(inputId = paste0("finish_", i), label = "Finish", class = "btn btn-success btn-sm")
        } else {
          HTML("<span>Selesai</span>")
        })
      )
    })
    
    tagList(
      fluidRow(
        column(2, strong("Tanggal")),
        column(1, strong("Waktu")),
        column(2, strong("Kontributor")),
        column(2, strong("Jenis Makanan")),
        column(1, strong("Porsi")),
        column(2, strong("Status")),
        column(2, strong("Aksi"))
      ),
      tags$hr(),
      donasi_rows
    )
  })
  
  observe({
    for (i in 1:nrow(donasi_db)) {
      local({
        idx <- i
        output[[paste0("status_text_", idx)]] <- renderText({ donasi_db$status[idx] })
        observeEvent(input[[paste0("finish_", idx)]], {
          donasi_db$status[idx] <<- "Finish"
          output[[paste0("status_text_", idx)]] <- renderText({ donasi_db$status[idx] })
          output$tabel_donasi <- renderTable({
            data <- subset(donasi_db, nama_kontributor == user_logged()$nama)[,
                                                                              c("tanggal_input", "waktu_input", "nama_kontributor", "alamat", "lokasi", "jenis_makanan", "jumlah_porsi", "status")]
            colnames(data) <- c("Tanggal Input", "Waktu Input", "Nama Kontributor", "Alamat", "Lokasi", "Jenis Makanan", "Jumlah Porsi", "Status")
            data
          })
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$prosesDistribusi, {
    if (nrow(donasi_db) == 0) {
      showModal(modalDialog("Belum ada data makanan untuk didistribusikan.", easyClose = TRUE))
    } else {
      hasil <- data.frame(
        Donatur = donasi_db$nama_kontributor,
        Lokasi_Makanan = donasi_db$lokasi,
        Jenis_Makanan = donasi_db$jenis_makanan,
        Porsi = donasi_db$jumlah_porsi,
        Penerima = rep(penerima_db$nama_penerima, length.out = nrow(donasi_db)),
        Lokasi_Penerima = rep(penerima_db$lokasi, length.out = nrow(donasi_db)),
        Porsi_Dibutuhkan = rep(penerima_db$kebutuhan_porsi, length.out = nrow(donasi_db)),
        Status = rep("Cocok dan Didistribusikan", nrow(donasi_db)),
        stringsAsFactors = FALSE
      )
      donasi_db$status <- "Terdistribusi"
      hasil_distribusi(hasil)
    }
  })
  
  output$hasil_distribusi_table <- renderTable({
    req(hasil_distribusi())
    hasil_distribusi()
  })
  
  output$tabel_penerima <- renderTable({
    data <- penerima_db
    colnames(data) <- c("Nama Lembaga", "Alamat", "Lokasi (Koordinat)", "Kebutuhan Porsi")
    data
  })
  
  output$peta_lokasi <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(
        lng = as.numeric(sapply(strsplit(penerima_db$lokasi, ","), function(x) x[2])),
        lat = as.numeric(sapply(strsplit(penerima_db$lokasi, ","), function(x) x[1])),
        label = penerima_db$nama_penerima
      )
  })
  
  observeEvent(input$logout_btn, {
    user_logged(NULL)
    updateSelectInput(session, "role", selected = "Pilih")
    output$main_ui <- renderUI({})
  })
}

shinyApp(ui = ui, server = server)