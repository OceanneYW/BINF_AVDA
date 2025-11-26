#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Cleanup environment
rm(list = ls())

tryCatch(
  {
    setwd(getSrcDirectory())
  },
  error = function(e) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
)

# Helper functions
source("dependencies.R")
source("parse.R")

# Plot functions
source("mds.R")
source("dendro.R")
source("cluster.R")
source("report.R")

# Define UI for application that draws a histogram 
ui <- fluidPage(
  setBackgroundImage(src = "/Data-Visualization_Blog-scaled.jpeg"),

  # Application title with style added
   div(
     style = "background-color: rgba(0, 0, 0, 0.5); padding: 20px; border: 1px solid #202020; border-radius: 10px; margin-top: 20px; margin-bottom: 20px; color: #ffffff; line-height: 1; font-size: 30px; font-weight: bold; text-align: center;",
     "Advanced Data Analysis and Visualization App"
   ),

  # Sidebar for options
  #File uploading window
  #MDS (Clusters, Driving force?)
  #Dendrogram
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", h3("Upload .csv file")),
      h4("MDS Options"),
      #Default algo: bray
      selectInput("mds_algo", "Scaling Algorithms",
                  list("Bray-Curtis" = "bray",
                  "Euclidean" = "euclidean")),
      #TODO: Drive force implementation
      checkboxInput("mds_drive_force", "Drive Force"),
      checkboxInput("mds_clusters", "Clusters"),
      conditionalPanel(condition = "input.mds_clusters",
        sliderInput("num_clusters", "Number of clusters:",
                    min = 1, max = 10,
                    value = 1, step = 1),
      ),
      h4("Dendrogram Options"),
      selectInput("dendro_algo", "Scaling Algorithms",
                  list("Bray-Curtis" = "bray",
                       "Euclidean" = "euclidean"))
    ),


    # Shows plots and results
    #Enable downloads for the plots
    mainPanel(
      div(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("rData")),
        tabPanel("MDS", plotlyOutput("plot_mds"), downloadButton('download_mds')),
        tabPanel("Dendrogram", plotlyOutput("plot_dendro"), downloadButton('download_dendro')),
        #TODO: sink() report generation
        tabPanel("Report", textOutput("logHistory"), downloadButton('download_report')),
      ),
      style = "background-color: #ffffff; padding: 5px;"
      )
    ),
  ),
)

server <- function(input, output) {
  set.seed(55)
  #Creates data object from input file
  rData <- reactive({
    validate(
      need(input$file1$datapath, "")
    )
    parse(input$file1$datapath)
  })
  
  #Creates an empty list in a variable for results logging
  vals <- reactiveValues(log=c())
  
  observeEvent(input$file1$datapath, {
    vals$log <- append(vals$log, paste("File Upload ", input$file1$datapath, "\n"))
  })
  
  #Eventlistener for MDS clustering changes
  clustersListener <- reactive({list(input$num_clusters, input$mds_clusters)})
  
  observeEvent(clustersListener(), {
    #Only log numbers of clusters when the toggle is on
    text <- if (input$mds_clusters) input$num_clusters else "Off"
    vals$log <- append(vals$log, paste("Clusters: " , text, "\n"))
  })
  
  rMdsData <- reactive({
    calcMds(rData(), input$mds_algo)
  })
  
  observeEvent(input$mds_algo, {
    vals$log <- append(vals$log, paste("MDS algorithm selected: ", input$mds_algo))
  })
  
  rPlotMds <- reactive({
    data <- rData()
    algo <- input$mds_algo
    nms <- rMdsData()
    plot <- ggplot()
    #If cluster toggles, plot pentagons on the mds plot
    if (input$mds_clusters) plot <- appendClustersToPlot(calcDendro(data), nms, plot, input$num_clusters)
    plot <- plotMds(plot, nms)
    return(plot)
  })
  
  rPlotMdsPrint <- reactive({
    data <- rData()
    nms <- rMdsData()
    plot <- ggplot()
    if (input$mds_clusters) plot <- appendClustersToPlot(calcDendro(data), nms, plot, input$num_clusters)
    plot <- plotMds(plot, nms, T)
    return(plot)
  })
  
  rPlotDendro <- reactive({
    data <- rData()
    plotDendro(calcDendro(data, input$dendro_algo))
  })
  
  output$rData <- renderDataTable({
      rData()
  },   options = list(scrollX = TRUE))
  
  output$plot_mds <- renderPlotly({
    plot <- rPlotMds()
    p <- ggplotly(plot)
    p
  })

  output$plot_dendro <- renderPlotly({
    plot <- rPlotDendro()
    ggplotly(plot)
  })
  
  
  output$download_mds <- downloadHandler(
    filename = "mds.pdf",
    content = function (file) {
      plot <- rPlotMdsPrint()
      ggsave(file, plot = plot, width=17, height=11, dpi=300, units="in")
    }
  )
  output$download_dendro <- downloadHandler(
    filename = "dendrogram.pdf",
    content = function (file) {
      plot <- rPlotDendro()
      ggsave(file, plot = plot, width=17, height=11, dpi=300, units="in")
    }
  )
  output$logHistory <- reactive({vals$log})
}


# Run the application
shinyApp(ui = ui, server = server)