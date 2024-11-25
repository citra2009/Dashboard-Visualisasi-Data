library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(rlang)
library(plotly)
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(shinycssloaders)


setwd("D:/SEMESTER 5/Eksplorasi dan Visualisasi Data/EAS")
Data = read_excel("Data EAS_Citra_041.xlsx")
View(Data)


#PEMILIHAN SELECT INPUT
c1 = Data %>%
  select(-`Kabupaten/Kota`) %>%
  names()


######################################################################################################
ui = dashboardPage(
  dashboardHeader(title = "Dashboard Sosial dan Kependudukan Provinsi Jawa Timur Tahun 2023",
                  titleWidth = 700),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem(text = "Beranda", tabName = "Beranda", icon = icon("home")),
      menuItem(text = "Visualisasi Data", tabName = "Vis", icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'histplot'", selectInput(inputId = "var1", label = "Pilih Variabel untuk Distribusi", choices = c1)),
      conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'relation'", selectInput(inputId = "var2", label = "Pilih Variabel X untuk Korelasi", choices = c1, selected = "TPAK")),
      conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "Pilih Variabel Y untuk Korelasi", choices = c1, selected = "TPT")),
      conditionalPanel("input.sidebar == 'Vis' && input.t2 == 'trends'", radioButtons(inputId = "var4", label = "Pilih Indikator", choices = c1)),
      menuItem(text = "Profil", tabName = "Prof", icon = icon("address-card"))
    )
  ),
  dashboardBody(
    tabItems(
    
    #TAB BERANDA
      tabItem(tabName = "Beranda",
              tabBox(id = "t1", width = 12,
                     tabPanel(title = "Tentang", icon = icon("database"), fluidRow(
                       column(width = 7, imageOutput(outputId = "peta"),
                              tags$br(),
                              tags$a("Wilayah Administratif Provinsi Jawa Timur"), align = "center"),
                       column(width = 5, tags$br(),
                              tags$p(HTML("Jawa Timur adalah sebuah wilayah provinsi yang terletak di bagian timur Pulau Jawa, Indonesia yang terdiri atas 29 kabupaten dan 9 kota. Ibu kotanya adalah Kota Surabaya. Luas wilayahnya yakni 48.033 km², dengan jumlah penduduk sebanyak 41.149.974 jiwa dan kepadatan penduduk 857 jiwa/km². Jawa Timur dikenal sebagai pusat industri dan keuangan kawasan Tengah dan Timur Indonesia, yang memiliki signifikansi perekonomian cukup tinggi, yakni berkontribusi sekitar 15% terhadap Produk Domestik Bruto nasional.
                                     <br> <strong> Variabel yang digunakan:
                                     <br> IPM </strong>: Indeks Pembangunan Manusia
                                     <br> <strong> RLS </strong>: Rata-Rata Lama Sekolah
                                     <br> <strong> PPM </strong>: Persentase Penduduk Miskin
                                     <br> <strong> IKK (P1) </strong>: Indeks Kedalaman Kemiskinan
                                     <br> <strong> TPT </strong>: Tingkat Pengangguran Terbuka
                                     <br> <strong> TPAK </strong>: Tingkat Partisipasi Angkatan Kerja
                                     <br> <strong> AHH </strong>: Angka Harapan Hidup
                                     <br> <strong> PPK </strong>: Pengeluaran per Kapita (Ribu)")))
                     )),
                     
                     tabPanel(title = "Dataset", icon = icon("table"), dataTableOutput("dataT")),
                     tabPanel(title = "Summary", icon = icon("chart-pie"), verbatimTextOutput("Summary")),
              )
      ),
      
      #TAB VISUALISASI DATA
      tabItem(tabName = "Vis",
              tabBox(id = "t2", width = 12,
                     tabPanel(title = "Indikator Berdasarkan Kabupaten/Kota", value = "trends", 
                              fluidRow(tags$div(align = "center", box(tableOutput("top5"), title = textOutput("head1"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align = "center", box(tableOutput("low5"), title = textOutput("head2"), collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE))),
                              withSpinner(plotlyOutput("bar"))),
                     tabPanel(title = "Distribusi Data", value = "histplot", plotlyOutput("histplot")),
                     tabPanel(title = "Hubungan TPAK dan TPT", value = "Relation", 
                              radioButtons(inputId = "fit", label = "Pilih Tipe Bagan", choices = c("loess", "lm"), selected = "lm", inline = TRUE),
                              withSpinner(plotlyOutput("scatter"))),
                     tabPanel(title = "Matriks Korelasi", value = "distro", plotlyOutput("cor")),
              )
      ),
      
      #TAB PROFIL
      tabItem(tabName = "Prof",
              tabBox(id = "t1", width = 12,
                     fluidRow(
                       column(width = 6, imageOutput(outputId = "profil"),
                              tags$br(),
                              tags$a("Foto Profil Pencipta"), align = "center"),
                       column(width = 6, tags$br(),
                              tags$p(HTML("<br> <strong> FINAL PROJECT EKSPLORASI DAN VISUALISASI DATA </strong>
                                     <br> Dashboard ini diciptakan sebagai bentuk pemenuhan final project Semester 5 mata kuliah Eksplorasi dan Visualisasi Data pada perkuliahan yang sedang saya jalani sekarang, yakni di Departemen Statistika Bisnis, Fakultas Vokasi, ITS. 
                                     <br> Insight yang diberikan berupa visualisasi data pada indikator sosial dan kependudukan menurut kabupaten/kota di Provinsi Jawa Timur tahun 2023. Diharapkan dapat bermanfaat bagi pihak lain.
                                     <br> Terima Kasih.
                                     <br>
                                     <br> <strong> Nama </strong>: Citra Sindy Reynilda
                                     <br> <strong> NRP </strong>: 2043211041
                                     <br> <strong> Kelas </strong>: EVD - A")))
                     )
    )
  )
)
)
)
  

######################################################################################################
server = function(input, output, session){
  #BERANDA - TENTANG
  output[["peta"]] = renderImage({
    list(src = "peta.PNG", width = 600, height = 300)
  }, deleteFile = F)
  
  #BERANDA - DATASET
  output$dataT = renderDataTable(
    Data)
  
  #BERANDA - SUMMARY
  output$Summary = renderPrint(
    Data %>%
      summary())
  
  #VISUALISASI - DISTRIBUSI DATA
  output$histplot = renderPlotly({
    p1 = Data %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title = input$var1))
    
    p2 = Data %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis = list(showtickslabels = F))
    
    subplot(p2, p1, nrows = 2) %>%
      hide_legend() %>%
      layout(title = input$var1,
             yaxis = list(title = "Frekuensi"))
  })
  
  #VISUALISASI - SCATTERPLOT
  output$scatter = renderPlotly({
    p3 = Data %>%
      ggplot(aes(x=get(input$var2), y=get(input$var3))) +
      geom_point() +
      geom_smooth(method = get(input$fit)) +
      labs(title = paste("Hubungan antara", input$var2, "dan", input$var3),
           x = input$var2,
           y = input$var3) +
      theme(plot.title = element_textbox_simple(size = 10, halign = 0.5))
    ggplotly(p3)
  })
  
  #VISUALISASI - MATRIKS KORELASI
  output$cor = renderPlotly({
    DataCorr = Data %>%
      select(-`Kabupaten/Kota`)
    Korelasi = round(cor(DataCorr), 1)
    p.mat = cor_pmat(DataCorr)
    corr.plot = ggcorrplot(
      Korelasi,
      hc.order = TRUE,
      lab = TRUE,
      outline.color = "grey",
      p.mat = p.mat
    )
    ggplotly(corr.plot)
  })
  
  #VISUALISASI - BARCHART
  output$bar = renderPlotly({
    Data %>%
      plot_ly() %>%
      add_bars(x = ~`Kabupaten/Kota`, y = ~get(input$var4)) %>%
      layout(title = paste(input$var4, "Berdasarkan Kabupaten/Kota"),
             xaxis = list(title = "Kabupaten/Kota"),
             yaxis = list(title = paste(input$var4)))
  })
  
  #VISUALISASI - TOP 5
  output$head1 = renderText(
    paste("5 Kabupaten/Kota dengan", input$var4, "Tertinggi")
  )
  
  output$head2 = renderText(
    paste("5 Kabupaten/Kota dengan", input$var4, "Terendah")
  )
  
  output$top5 = renderTable({
    Data %>%
      select(`Kabupaten/Kota`, input$var4) %>%
      arrange(desc(get(input$var4))) %>%
      head(5)
  })
  
  output$low5 = renderTable({
    Data %>%
      select(`Kabupaten/Kota`, input$var4) %>%
      arrange(get(input$var4)) %>%
      head(5)
  })

  #PROFIL
  output[["profil"]] = renderImage({
    list(src = "Citra.PNG", width = 295, height = 394)
  }, deleteFile = F)
}

shinyApp(ui, server)