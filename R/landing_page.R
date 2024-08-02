.landing_page <- function() {

  function (FUN, input, output, session) {
    # nocov start
    output$allPanels <- renderUI({
      tagList(
        
        fluidPage(
          
          titlePanel("SE Builder"),
          
          sidebarLayout(
            
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
      )
    })
    
    ## Disable navbar buttons that are not linked to any observer yet
    shinyjs::disable(iSEE:::.generalOrganizePanels) # organize panels
    shinyjs::disable(iSEE:::.generalLinkGraph) # link graph
    shinyjs::disable(iSEE:::.generalExportOutput) # export content
    shinyjs::disable(iSEE:::.generalCodeTracker) # tracked code
    shinyjs::disable(iSEE:::.generalPanelSettings) # panel settings
    shinyjs::disable(iSEE:::.generalVignetteOpen) # open vignette
    shinyjs::disable(iSEE:::.generalSessionInfo) # session info
    shinyjs::disable(iSEE:::.generalCitationInfo) # citation info
    
    pObjects <- .create_persistent_objects()
    rObjects <- reactiveValues(tse = 1L)
    
    .create_observers(input, session, pObjects, rObjects)
    .create_launch_observers(FUN, input, session, rObjects)
    
    .render_overview(output, pObjects, rObjects)
    .render_download(output, pObjects, rObjects)

    invisible(NULL)
    # nocov end
  }
}

.create_persistent_objects <- function() {
  pObjects <- new.env()
  # pObjects$datasets_visible <- datasets_table
  pObjects
}