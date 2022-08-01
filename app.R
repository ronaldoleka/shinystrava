library(shiny)
library(shinydashboard)
library(shinybusy)
library(strava)

ui <- fluidPage(
  
  add_busy_bar(color = "black", height = "20px"),
  
  titlePanel("shinystrava"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput(inputId = "input_file", label = "Input Strava archive file (zip):", accept = ".zip"),
                 
                 downloadLink(outputId = "download_sample_dataset", "Download sample dataset (zip)"),
                 br(),
                 br(),
                 actionButton(inputId = "visualize_button", label = "Visualize data", icon = icon("palette")),
                 br(),
                 br(),
                 
                 uiOutput("download_UI")
    ),
    
    
    mainPanel(width = 9,
              
              tabBox(width = 6,
                     tabPanel("Facets",
                              plotOutput(outputId = "facets"))
              ),
              tabBox(width = 6,
                     tabPanel("Map",
                              plotOutput(outputId = "map"))
              ),
              tabBox(width = 6,
                     tabPanel("Ridges",
                              plotOutput(outputId = "ridges"))
              ),
              tabBox(width = 6,
                     tabPanel("Packed circles",
                              plotOutput(outputId = "packed_circles"))
              ),
              tabBox(width = 12,
                     tabPanel("Calendar",
                              plotOutput(outputId = "calendar"))
              )
    )
  )
)

server <- function(input, output) {
  
  # sample dataset
  
  output$download_sample_dataset <- downloadHandler(
    filename = "running.zip",
    content = function(con){
      file.copy("data/running.zip", con)
    },
    contentType = "application/zip"
  )
  
  # Read data
  
  input_data <- eventReactive(input$visualize_button, {
    
    unzip(input$input_file$datapath, exdir = "unzip", overwrite = FALSE)
    
    list_of_files <- list.files("unzip", recursive = TRUE)
    
    data_folder <- list_of_files[which(endsWith(list_of_files, ".gpx"))]
    
    data_folder <- strsplit(data_folder[1], "/")[[1]][1]
    
    data_folder <- paste0("unzip/", data_folder)
    
    input_data <- process_data(data_folder)
    
    input_data
  })
  
  
  
  # Plots
  
  facets_plot <- reactive({plot_facets(input_data())})
  map_plot <- reactive({plot_map(input_data())})
  ridges_plot <- reactive({plot_ridges(input_data())})
  packed_circles_plot <- reactive({plot_packed_circles(input_data())})
  calendar_plot <- reactive({plot_calendar(input_data())})
  
  
  output$facets <- renderPlot(facets_plot())
  
  output$map <- renderPlot(map_plot())
  
  output$ridges <- renderPlot(ridges_plot())
  
  output$packed_circles <- renderPlot(packed_circles_plot())
  
  output$calendar <- renderPlot(calendar_plot())
  
  
  plot_choices <- c("Facets", "Map", "Ridges", "Packed circles", "Calendar")
  
  
  
  observeEvent(input$visualize_button, {
    output$download_UI <- renderUI({
      actionButton(inputId = "download_button", label = "Download plots", icon = icon("download"))
    })
  })
  

  # Download the plots
  
  observeEvent(input$download_button, {

    showModal(modalDialog(
      title = "",
      checkboxGroupInput(inputId = "plots_to_download", label = "Choose plots to download", choices = plot_choices, selected = plot_choices),
      
      footer = tagList(
        modalButton("Close"),
        downloadButton(outputId = "download_button_2", label = "Download plots")
      )
    )
    )
  })
  
  
  output$download_button_2 <- downloadHandler(
    filename = "plots.zip",
    content = function(con) {
      
      save_directory <- "saved_plots"
      
      file_list <- c()
      
      if ("Facets" %in% input$plots_to_download) {
        ggsave(paste0(save_directory, "/facets.png"), facets_plot(), width = 20, height = 20, units = "cm")
        file_list <- c(file_list, paste0(save_directory, "/facets.png"))
      }
      if ("Map" %in% input$plots_to_download) {
        ggsave(paste0(save_directory, "/map.png"), map_plot(), width = 20, height = 15, units = "cm")
        file_list <- c(file_list, paste0(save_directory, "/map.png"))
      }
      if ("Ridges" %in% input$plots_to_download) {
        ggsave(paste0(save_directory, "/ridges.png"), ridges_plot(), width = 20, height = 20, units = "cm")
        file_list <- c(file_list, paste0(save_directory, "/ridges.png"))
      }
      if ("Packed circles" %in% input$plots_to_download) {
        ggsave(paste0(save_directory, "/packed_circles.png"), packed_circles_plot(), width = 20, height = 20, units = "cm")
        file_list <- c(file_list, paste0(save_directory, "/packed_circles.png"))
      }
      if ("Calendar" %in% input$plots_to_download) {
        ggsave(paste0(save_directory, "/calendar.png"), calendar_plot(), width = 40, height = 20, units = "cm")
        file_list <- c(file_list, paste0(save_directory, "/calendar.png"))
      }
      
      zip(zipfile = con, files = file_list)
      
    },
    contentType = "application/zip"
  )
  
  
  
}


# Run the application 
shinyApp(ui = ui,
         server = server,
         
         onStart = function() {
           onStop(function() {
             
             unlink("unzip", recursive = TRUE)
             unlink("saved_plots", recursive = TRUE)
             unlink("plots.zip")
             
           })
         })
