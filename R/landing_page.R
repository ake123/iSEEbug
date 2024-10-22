#' Landing page
#' 
#' \code{.landing_page} creates the landing page of iSEEhub, where TreeSE objects
#' can be built and iSEE can be launched.
#'
#' @return The UI is defined by the function. A \code{NULL} value is invisibly
#'   returned.
#'
#' @name landing_page
#' @keywords internal

#' @rdname landing_page
#' @importFrom shiny actionButton fluidPage titlePanel sidebarLayout
#'   sidebarPanel mainPanel tabPanel tabsetPanel renderUI selectInput
#'   fileInput checkboxInput verbatimTextOutput downloadButton
#' @importFrom shinyjs disable
#' @importFrom utils data
.landing_page <- function(FUN, input, output, session) {

    mia_datasets <- data(package = "mia")
    mia_datasets <- mia_datasets$results[-2 , "Item"]
    data(list = mia_datasets, package = "mia")
  
    # nocov start
    output$allPanels <- renderUI({

        fluidPage(
          
            fluidRow(column(4, wellPanel(
              
              titlePanel("Import"),
            
                    tabsetPanel(id = "format",
                          
                        tabPanel(title = "Dataset", value = "dataset",
                        
                            selectInput(inputId = "data", label = "Dataset:",
                                choices = mia_datasets,
                                selected = mia_datasets[1])      
                                   
                        ),
                          
                        tabPanel(title = "R Object", value = "rda",
                                   
                            fileInput(inputId = "file", label = "RDA:",
                                accept = ".rda")
                                   
                        ),
                          
                        tabPanel(title = "Raw Data", value = "raw",
                                   
                            fileInput(inputId = "assay", label = "Assays:",
                                accept = ".csv", multiple = TRUE),
                                   
                            fileInput(inputId = "coldata", label = "colData:",
                                accept = ".csv"),
                                   
                            fileInput(inputId = "rowdata", label = "rowData:",
                                accept = ".csv")     
                        
                         ),
                          
                         tabPanel(title = "Foreign", value = "foreign",
                                  
                             radioButtons(inputId = "ftype",
                                 label = "Type:", choices = list("biom", "QZA",
                                 "MetaPhlAn")),
                             
                             fileInput(inputId = "main.file",
                                 label = "Main file:", accept = c(".biom",
                                 ".QZA", ".txt")),
                             
                             conditionalPanel(
                                 condition = "input.ftype == 'biom'",
                               
                                 checkboxInput(inputId = "rm.tax.pref",
                                     label = "Remove taxa prefixes"),
                               
                                 checkboxInput(inputId = "rank.from.pref",
                                     label = "Derive taxa from prefixes")
                             ),
                             
                             conditionalPanel(
                                 condition = "input.ftype == 'MetaPhlAn'",
                               
                                 fileInput(inputId = "col.data", label = "colData:",
                                     accept = ".tsv"),
                                 
                                 fileInput(inputId = "tree.file", label = "Tree:",
                                     accept = ".tree")
                             )
                                   
                          )
                          
                      ),
              
                  actionButton("import", "Import", class = "btn-success",
                      style = iSEE:::.actionbutton_biocstyle)
                
                )),
            
            column(4, wellPanel(
              
              titlePanel("Manipulate"),
              
              tabsetPanel(id = "manipulate",
                          
                  tabPanel(title = "Subset", value = "subset"),
                  tabPanel(title = "Aggregate", value = "aggregate"),
                  tabPanel(title = "Transform", value = "transform",
                  
                      selectInput(inputId = "assay.type", label = "Assay:",
                          choices = NULL),
              
                      selectInput(inputId = "trans.method", label = "Method:",
                          choices = c("relabundance", "clr", "standardize")),
              
                      checkboxInput(inputId = "pseudocount",
                          label = "Pseudocount:"),
              
                      textInput(inputId = "assay.name", label = "Name:"),
                      
                      radioButtons(inputId = "margin", label = "Margin:",
                          choices = c("samples", "features"), inline = TRUE)
                  
              )),
              
              actionButton("apply", "Apply", class = "btn-success",
                  style = iSEE:::.actionbutton_biocstyle)
            
            )),
            
            column(4, wellPanel(
              
              titlePanel("Estimate"),
              
              actionButton("estimate", "Estimate", class = "btn-success",
                  style = iSEE:::.actionbutton_biocstyle)
              
            ))),
  
            fluidRow(
              
              column(4, wellPanel(
                
                titlePanel("Visualise"),

                actionButton("launch", "Launch iSEE", class = "btn-success",
                    style = iSEE:::.actionbutton_biocstyle)
                
              )),
              
              column(8, wellPanel(
              
              titlePanel("Output"),
              
              verbatimTextOutput(outputId = "object"),
              
              downloadButton(outputId = "download", label = "Download")
              
            )))
          )
    })
    
    ## Disable navbar buttons that are not linked to any observer yet
    disable(iSEE:::.generalOrganizePanels) # organize panels
    disable(iSEE:::.generalLinkGraph) # link graph
    disable(iSEE:::.generalExportOutput) # export content
    disable(iSEE:::.generalCodeTracker) # tracked code
    disable(iSEE:::.generalPanelSettings) # panel settings
    disable(iSEE:::.generalVignetteOpen) # open vignette
    disable(iSEE:::.generalSessionInfo) # session info
    disable(iSEE:::.generalCitationInfo) # citation info
    
    rObjects <- reactiveValues(tse = 1L)
    
    .create_observers(input, session, rObjects)
    .create_launch_observers(FUN, input, session, rObjects)
    
    .render_overview(output, rObjects)
    .render_download(output, rObjects)

    invisible(NULL)
    # nocov end
}