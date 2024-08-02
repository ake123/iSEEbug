library(shiny)
library(mia)
library(miaViz)
library(biomformat)

mia_datasets <- data(package = "mia")
mia_datasets <- mia_datasets$results[ , "Item"]
data(list = mia_datasets, package = "mia")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SE Builder"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tabsetPanel(id = "format",
        
        tabPanel(title = "Dataset",
                 value = "dataset",
        
          selectInput(inputId = "data",
                      label = "Dataset:",
                      choices = mia_datasets,
                      selected = mia_datasets[1])      
        
        ),
        
        tabPanel(title = "R Object",
                 value = "rda",

          fileInput(inputId = "file",
                    label = "RDA:",
                    accept = ".rda")
        
        ),
        
        tabPanel(title = "Raw Data",
                 value = "raw",
                 
          fileInput(inputId = "assay",
                    label = "Assays:",
                    accept = ".csv",
                    multiple = TRUE),
                 
          fileInput(inputId = "coldata",
                    label = "colData:",
                    accept = ".csv"),
                 
          fileInput(inputId = "rowdata",
                    label = "rowData:",
                    accept = ".csv")     
        ),
      
        tabPanel(title = "Foreign",
                 value = "foreign",
                 
          fileInput(inputId = "biom",
                    label = "BIOM:",
                    accept = ".biom"),
          
          checkboxInput(inputId = "rm.tax.pref",
                        label = "Remove taxa prefixes"),
          
          checkboxInput(inputId = "rank.from.pref",
                        label = "Derive taxa from prefixes")
        
        )

      ),
        
      actionButton("build", "Build!", class = "btn-success"),
      actionButton("launch", "Launch!", class = "btn-success")
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      verbatimTextOutput(outputId = "object"),
      
      downloadButton(outputId = "download",
                     label = "Download")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  rObjects <- reactiveValues()
  
  observe({
    
      input$build

      if( input$format == "dataset" ){
        
          rObjects$tse <- isolate(get(input$data))

      }else if( input$format == "rda" ){
        
          isolate({
              req(input$file)
              load(file = input$file$datapath)
              rObjects$tse <- get(gsub(".rda", "", input$file$name))
          })
          
      }else if( input$format == "raw" ){
        
          isolate({
              req(input$assay, input$coldata, input$rowdata)
            
              assay_list <- lapply(input$assay$datapath,
                                   function(x) as.matrix(read.csv(x, row.names = 1)))
              coldata <- read.csv(input$coldata$datapath, row.names = 1)
              rowdata <- read.csv(input$rowdata$datapath, row.names = 1)
              
              names(assay_list) <- gsub(".csv", "", input$assay$name)
              
              rObjects$tse <- SummarizedExperiment(
                assays = assay_list,
                colData = coldata,
                rowData = rowdata
              )
          })
        
      }else if( input$format == "foreign" ){
        
          isolate({
              req(input$biom)
              biom_object <- read_biom(input$biom$datapath)
              
              rObjects$tse <- convertFromBIOM(
                biom_object,
                removeTaxaPrefixes = input$rm.tax.pref,
                rankFromPrefix = input$rank.from.pref
              )
          })
        
      }
    
  })
      
  output$object <- renderPrint({
    rObjects$tse
  })

  output$download <- downloadHandler(
    filename = function() paste0("se-", Sys.Date(), ".rds"),
    content = function(file) saveRDS(rObjects$tse, file)
  )

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
