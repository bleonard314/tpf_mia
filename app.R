# options(repos = BiocManager::repositories()) # run this before uploading to shinyapps
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#
# BiocManager::install("EBImage")
library(EBImage)
library(tidyverse)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)
library(shinyalert)
library(magick)

list.files("modules") %>%
  purrr::map(~ source(paste0("modules/", .)))

chs <- c(
  "Area" = "s.area",
  "Perimeter" = "s.perimeter",
  "Radius (Mean)" = "s.radius.mean",
  "Radius (SD)" = "s.radius.sd",
  "Radius (Min)" = "s.radius.min",
  "Radius (Max)" = "s.radius.max"
)

###### HEADER #######
header <- dashboardHeader(
  title = h4("TPF MIA", circleButton("info", "", icon = icon("info-circle"), size = "s")),
  leftUi = tagList(
    useShinyjs(),
    bsTooltip("info", "<b>Tire Particle Filter - Microscopic Image Analyst:</b> Click for more info!", "right", options = list(container = "body")),
    dropdownButton(
      label = "Configuration",
      icon = icon("gears"),
      status = "primary",
      circle = FALSE,
      tooltip = tooltipOptions(placement = "right", title = "General settings. Note that writing uploaded files to image input directory is disabled if app is running on shinyapps.io."),
      materialSwitch("wupload", "Write Uploaded Files?", value = TRUE, status = "success", width = "100%", right = TRUE),
      materialSwitch("mode", "Color Preview?", value = TRUE, status = "primary", width = "100%", right = TRUE),
      materialSwitch("convert", "Convert to mm?", value = FALSE, status = "danger", width = "100%", right = TRUE),
      numericInput("scale", "Scale (px/mm):", value = 1322, min = 0),
      numericInput("resize", "Resize Image Width for Levels (px):", value = 256, min = 1),
      uiChangeThemeDropdown(dropDownLabel = "Theme:", defaultTheme = "blue_gradient")
    ),
    downloadButton("downloadData", "Download", style = "color: #fff; background-color: #27ae60; border-color: black; width: 150; padding: 5px 5px 5px 5px"),
    bsTooltip("downloadData", "Download data as CSV file.", "bottom", options = list(container = "body")),
    downloadButton("saveData", "Save", style = "color: #fff; background-color: #fa9fb5; border-color: black; width: 150; padding: 5px 5px 5px 5px"),
    bsTooltip("saveData", "Save input settings as RDS file.", "bottom", options = list(container = "body")),
    actionButton("resetData", "Reset", icon = icon("sliders"), style = "color: #fff; background-color: #fec44f; border-color: black; width: 150; padding: 5px 5px 5px 5px"),
    bsTooltip("resetData", "Reset all applicatoin settings (except for threshold).", "bottom", options = list(container = "body")),
    # actionButton("convertImg", "Convert"), # Placeholder if functionality is desired...
    # bsTooltip("convertImg", "", "bottom", options = list(container = "body"))
  )
) #header

###### SIDEBAR ######
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu1",
    menuItem(HTML("<b>Step #1:</b> Input"), tabName = "page1", icon = icon("image")),
    conditionalPanel(
      'input.menu1 == "page1"',
      selectizeInput("image", "Select Image:", choices = list.files("input", pattern = ".tif*$"), "test1.tif"),
      fileInput("upload", "Import File:", accept = c(".tif", ".tiff")),
    ),
    menuItem(HTML("<b>Step #2:</b>  Threshold"), tabName = "page2", icon = icon("eye-dropper")),
    conditionalPanel(
      'input.menu1 == "page2"',
      selectizeInput("gs", "Grayscale Method:", c("Minimum", "Maximum", "Average", "Luminosity"), selected = "Minimum"),
      bsTooltip("gs", "Function applied to RGB channels to determine grayscale values (intensities range from 0 to 1).", "right", options = list(container = "body")),
      noUiSliderInput("threshold", "Threshold:", min = 0, max = 1, value = 0.30),
      bsTooltip("threshold", "Convert grayscale to black & white (binary) based on threshold intensity.", "right", options = list(container = "body")),
      actionButton("auto", "Auto (Otsu's Method)"),
      bsTooltip("auto", "Nobuyuki Otsu, A threshold selection method from gray-level histograms. IEEE Trans. Sys., Man., Cyber. 9 (1): 62-66. doi:10.1109/TSMC.1979.4310076 (1979)", "right", options = list(container = "body")),
    ),
    menuItem(HTML("<b>Step #3:</b> Segment"), tabName = "page3", icon = icon("spider")),
    conditionalPanel(
      'input.menu1 == "page3"',
      awesomeCheckbox("hull", "Fill Holes?", value = FALSE, status = "info"),
      bsTooltip("hull", "Define all particle objects by their outer boundaries.", "top", options = list(container = "body")),
      awesomeCheckbox("opening", "Opening?", value = FALSE, status = "warning"),
      bsTooltip("opening", HTML("Remove objects with pixel areas smaller than the <b>Pixel Size</b> by contracting and expanding object boundaries based on the <b>Brush Shape</b>."), "top", options = list(container = "body")),
      noUiSliderInput("size", "Pixel Size:", min = 1, max = 15, value = 5, step = 2, format = wNumbFormat(decimals = 0), update_on = "end"),
      selectizeInput("shape", "Brush Shape:", choices = c("Box" = "box", "Disc" = "disc", "Diamond" = "diamond", "Gaussian" = "Gaussian", "Line" = "line"), selected = "disc"),
      awesomeCheckbox("watershed", "Watershed?", value = FALSE, status = "danger"),
      bsTooltip("watershed", "Split adjoining pixels by watershed algorithm using a binary depth map rather than negative space boundaries.", "top", options = list(container = "body")),
      sliderInput("tolerance", "Tolerance:", min = 0, max = 10, value = 2),
      numericInputIcon("extent", "Extent:", min = 1, max = 10, value = 1),
    ),
    menuItem(HTML("<b>Step #4:</b> Filter"), tabName = "page4", icon = icon("filter")),
    conditionalPanel(
      'input.menu1 == "page4"',
      selectizeInput("var", "Plot Variable:", choices = chs, selected = "s.area"),
      bsTooltip("var", "Object measurement variable to plot in histogram.", "right", options = list(container = "body")),
      awesomeCheckbox("filter", "Filter Pixels?", value = FALSE, status = "danger"),
      bsTooltip("filter", "Minimum and maximum range for object measurement variable.", "right", options = list(container = "body")),
      uiOutput("range"),
      noUiSliderInput("bins", "Bin Size:", min = 10, max = 500, value = 100, format = wNumbFormat(decimals = 0)),
      bsTooltip("bins", "Histogram bin width.", "right", options = list(container = "body")),
    )
  ),
  minified = FALSE
) #sidebar

###### BODY ######
body <- dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  uiChangeThemeOutput(),
  fluidRow(
    tabBox(
      title = "Full Image",
      tabPanel("Static", plotOutput("raster1", height = "400px")),
      tabPanel("Interactive", displayOutput("widget1", height = "400px"))
    ),
    tabBox(
      title = "Masked Image",
      tabPanel("Static", plotOutput("raster2", height = "400px")),
      tabPanel("Interactive", displayOutput("widget2", height = "400px"))
    )
  ),
  fluidRow(
    tabBox(
      title = "Levels",
      tabPanel("Grayscale", plotlyOutput("plot_gray", height = "250px")),
      bsTooltip("plot_gray", "Click to set threshold.", "right", options = list(container = "body")),
      tabPanel("RGB", plotlyOutput("plot_color", height = "250px"))
    ),
    tabBox(
      title = "Results",
      tabPanel("Histogram", plotlyOutput("plot2", height = "250px")),
      tabPanel("Data Table", DTOutput("table", height = "250px"))
    )
  )
)#body

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Only run this example in interactive R sessions
server <- function(input, output, session) {



  observe({
    loc <- if_else(session$clientData$url_hostname == "127.0.0.1", TRUE, FALSE)
    updateMaterialSwitch(session, inputId = "wupload", value = loc)
    if(!loc) {
      shinyjs::disable("wupload")
    }
  })

  serverChangeTheme("moduleChangeTheme")

  options(shiny.maxRequestSize=30*1024^2)

  observeEvent(input$resetData, {
    shinyjs::reset()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(input$image), "results.csv", sep = "_")
    },
    content = function(con) {
      write.csv(dat(), con, row.names = TRUE)
    }
  )

  output$saveData <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(input$image), "results.rds", sep = "_")
    },
    content = function(con) {
      write_rds(input, con)
    }
  )

  auto <- eventReactive(input$auto, {
    otsu(img_gray())
  })

  dat <- reactive({
    table() %>%
      mutate(
        File = tools::file_path_sans_ext(input$image),
        Scale = input$scale,
        Units = if_else(input$convert, "mm", "px"),
        Method = input$gs,
        Threshold = input$threshold,
        `Fill Holes` = input$hull,
        Opening = input$opening,
        `Opening Size` = input$size,
        `Opening Shape` = input$shape,
        Watershed = input$watershed,
        `Watershed Tolerance` = input$tolerance,
        `Watershed Extent` = input$extent,
        Filter = input$filter,
        `Filter Min` = input$range[1],
        `Filter Max` = input$range[2]
      )
  })

  table <- reactive({
    results_filtered() %>%
      rename(chs) %>%
      mutate(across(everything(), ~round(., 2)))
  })

  output$table <- renderDT(
    table(), options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE)
  )

  output$range <- renderUI({
    req(input$filter)
    ul <- results() %>% pull(s.area) %>% max()
    sliderInput("range", "Range (min/max):", value = c(0, ul), min = 0, max = ul)
  })

  observeEvent(input$info, {
    shinyalert(
      title = "Tire Particle Filter - Microscopic Image Analyst",
      text = div(
        HTML(paste0(
          "Image analysis platform for identifying and measuring tire particles captured by filtration.",
          br(),
          br(),
          "<b>Step #1:</b> Select a test image or uploade a valid image file for analysis.",
          br(),
          "<b>Step #2:</b> Create a binary image that separates tire particles from the background and other pieces.",
          br(),
          "<b>Step #3:</b> Segment thresholded image into individual particles specifying boundary conditions.",
          br(),
          "<b>Step #4:</b> Visualize results and apply filter by particle size.",
          br(),
          br(),
          "<i>v1.0 Created in May 2022 by Ben Leonard</i>"
        )), style="text-align: left"),
      type = "info",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      size = "m"
    )
  })

  fupload <- reactive({
    input$upload$datapath
  })

  observeEvent(input$upload, {
    f <- fupload()
    img <- tryCatch(
      {readImage(f)},
      error = function(e) {
        temp <- magick::image_read(f, defines = c("tiff:ignore-tags" = "37379"))
        magick::image_write(image = temp, path = f)
        img <- tryCatch(
          {readImage(f)},
          error = function(e) {
            shinyalert(
              title = paste0("<a href='", sample(
                c(
                  "https://www.youtube.com/watch?v=Lt6r-k9Bk6o&t=90s",
                  "https://www.youtube.com/watch?v=sT0g16_LQaQ&t=80s",
                  "https://www.youtube.com/watch?v=p4CAssOPJl4",
                  "https://www.youtube.com/watch?v=RVRBJg7w4UQ&t=7s",
                  "https://www.youtube.com/watch?v=IXXxciRUMzE&t=178s"
                ),
                1
              ), "' target = '_blank'>Damn!</a>"),
              text = "File could not be read. This may be due to tag 37379 which is embedded by Infinity Capture and cannot be read by ImageMagick (imported by EBImage). Try resaving the file using IrfanView or some other application and then re-upload. This action has already been attempted using the R magick library.",
              type = "error",
              closeOnEsc = TRUE,
              closeOnClickOutside = TRUE,
              html = TRUE
            )
            return()
          })
        return(img)
      })
    if (!is.null(img)) {
      if (input$wupload) {
        writeImage(img, file.path("input", input$upload$name))
        updateSelectizeInput(session, inputId = "image", selected = input$upload$name, choices = list.files("input", pattern = ".tif*$"))
      } else {
        updateSelectizeInput(session, inputId = "image", selected = input$upload$name, choices = c(list.files("input", pattern = ".tif*$"), input$upload$name))
      }
    }
  })

  observeEvent(input$opening, {
    if (input$opening) {
      shinyjs::show(id = "size")
      shinyjs::show(id = "shape")
    } else {
      shinyjs::hide(id = "size")
      shinyjs::hide(id = "shape")
    }
  })

  observeEvent(input$watershed, {
    if (input$watershed) {
      shinyjs::show(id = "tolerance")
      shinyjs::show(id = "extent")
    } else {
      shinyjs::hide(id = "tolerance")
      shinyjs::hide(id = "extent")
    }
  })


  observeEvent(input$auto, {
    updateNoUiSliderInput(inputId = "threshold", value = auto())
  })

  img_color <- eventReactive(input$image, {
    if(input$wupload | input$image %in% list.files("input", pattern = ".tif*$")) {
      f <- input$image
      readImage(file.path("input", f))
    } else {
      f <- fupload()
      readImage(f)
    }
  })

  threshold_click <- reactive({
    eventdata <- event_data("plotly_click", source = "B")
    eventdata$x
  })

  observe({
    updateNoUiSliderInput(inputId = "threshold", value = threshold_click())
  })

  output$plot_gray <- renderPlotly({

    img <- img_gray()
    img <- resize(img, w = input$resize)

    df <- img %>%
      as.vector() %>%
      as_tibble()

    p <- df %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "black", alpha = 0.2) +
      xlab("Value") +
      ylab("Density") +
      theme_bw()

    ggplotly(p, source = "B")

  })

  output$plot_color <- renderPlotly({

    channels <- c("red", "green", "blue")

    img <- img_color()
    img <- resize(img, w = input$resize)

    df <- 1:3 %>%
      map_df(~img[,,.x] %>%
               as.vector() %>%
               as_tibble() %>%
               mutate(channel = channels[.x])
      ) %>%
      mutate(channel = factor(channel, levels = channels))

    p <- df %>%
      ggplot(aes(x = value, fill = channel, group = channel)) +
      geom_density(alpha = 0.2) +
      scale_fill_manual(values = channels) +
      xlab("Value") +
      ylab("Density") +
      theme_bw()

    ggplotly(p)

  })

  img_gray <- reactive({
    img <- img_color()
    if(input$gs == "Minimum") {
      img <- img[,,min(1:3)]
    } else if(input$gs == "Maximum") {
      img <- img[,,max(1:3)]
    } else if(input$gs == "Average") {
      img <- img[,,mean(1:3)]
    } else if(input$gs == "Luminosity") {
      img <- img[,,weighted.mean(1:3, w = c(0.21, 0.72, 0.07))]
    }
    colorMode(img) <- Grayscale
    img
  })

  img_seg <- reactive({
    if(input$mode) {
      img1 <- img_color()
      col1 <- "#ff00ff"
    } else {
      img1 <- img_gray()
      col1 <- "#ffffff"
    }

    img2 <- img_bw()
    paintObjects(img2, img1, col = col1)
  })

  img_bw <- reactive({
    img <- img_gray()
    img <- img < input$threshold
    if(input$hull) {
      img <- fillHull(img)
    }
    if(input$opening) {
      img <- opening(img, makeBrush(input$size, shape = input$shape))
    }
    if(input$watershed) {
      watershed(distmap(img), input$tolerance, input$extent)
    } else {
      bwlabel(img)
    }
  })

  img_lab <- reactive({
    img <- img_bw()
    colorLabels(img)
  })

  results <- reactive({
    img <- img_bw()

    computeFeatures.shape(img) %>%
      as_tibble()
  })

  results_filtered <- reactive({
    results() %>%
      {if (input$filter)
        filter(., s.area >= input$range[1]) %>%
        filter(., s.area <= input$range[2])
        else . } %>%
      {if (input$convert)
        mutate(.,
          across(!s.area, ~./input$scale),
          s.area = (sqrt(s.area)/input$scale)^2)
        else . }
  })

  output$plot2 <- renderPlotly({

    df <- results_filtered()

    p <- df %>%
      ggplot(aes(x = get(input$var))) +
      geom_histogram(bins = input$bins) +
      xlab(paste(names(chs)[chs == input$var], if_else(input$convert, "(mm)", "(px)"))) +
      ylab("Frequency") +
      theme_bw()

    ggplotly(p)

  })

  output$widget1 <- renderDisplay({
    display(img_seg(), method = 'browser')
  })

  output$raster1 <- renderPlot({
    plot(img_seg(), all=TRUE)
  })

  output$widget2 <- renderDisplay({
    display(img_lab(), method = 'browser')
  })

  output$raster2 <- renderPlot({
    plot(img_lab(), all=TRUE)
  })

}

shinyApp(ui = ui, server = server)
