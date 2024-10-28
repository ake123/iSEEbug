#' Landing page
#' 
#' \code{.landing_page} creates the landing page of miaDash, where TreeSE objects
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
    mia_datasets <- mia_datasets$results[-c(2, 5), "Item"]
    data(list = mia_datasets, package = "mia")
  
    # nocov start
    output$allPanels <- renderUI({

        fluidPage(
          
            fluidRow(column(4, wellPanel(id = "import.panel",
              
              titlePanel("Import"),
            
                    tabsetPanel(id = "format",
                          
                        tabPanel(title = "Dataset", value = "dataset",
                        
                            selectInput(inputId = "data", label = "Dataset:",
                                choices = mia_datasets,
                                selected = mia_datasets[1])      
                                   
                        ),
                          
                        tabPanel(title = "R Object", value = "rds",
                                   
                            fileInput(inputId = "file", label = "RDS:",
                                accept = ".rds")
                                   
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
              
                  actionButton("import", "Upload", class = "btn-success",
                      style = .actionbutton_biocstyle)
                
                )),
            
            column(4, wellPanel(id = "manipulate.panel",
              
              titlePanel("Manipulate"),
              
              tabsetPanel(id = "manipulate",
                          
                  tabPanel(title = "Subset", value = "subset",
                      
                      radioButtons(inputId = "subkeep", label = "Keep:",
                          choices = c("prevalent", "rare"), inline = TRUE),
                      
                      selectInput(inputId = "subassay", label = "Assay:",
                          choices = NULL),
                      
                      sliderInput(inputId = "prevalence", value = 0,
                          label = "Prevalence threshold:", step = 0.01,
                          min = 0, max = 1),
                      
                      numericInput(inputId = "detection", value = 0,
                          label = "Detection threshold:", min = 0, step = 1)
                           
                  ),
                  
                  tabPanel(title = "Agglomerate", value = "agglomerate",
                  
                      selectInput(inputId = "taxrank", label = "Taxonomic rank:",
                          choices = NULL)         
                        
                  ),
                  
                  tabPanel(title = "Transform", value = "transform",
                  
                      selectInput(inputId = "assay.type", label = "Assay:",
                          choices = NULL),
              
                      selectInput(inputId = "trans.method", label = "Method:",
                          choices = c("relabundance", "clr", "standardize")),
              
                      checkboxInput(inputId = "pseudocount",
                          label = "Pseudocount"),
              
                      textInput(inputId = "assay.name", label = "Name:"),
                      
                      radioButtons(inputId = "margin", label = "Margin:",
                          choices = c("samples", "features"), inline = TRUE)
                  
              )),
              
              actionButton("apply", "Apply", class = "btn-success",
                  style = .actionbutton_biocstyle)
            
            )),
            
            column(4, wellPanel(id = "estimate.panel",
              
              titlePanel("Estimate"),
              
              tabsetPanel(id = "estimate",
                          
                  tabPanel(title = "Alpha", value = "alpha",
                           
                      selectInput(inputId = "alpha.assay", label = "Assay:",
                          choices = NULL),
                      
                      selectInput(inputId = "alpha.index", label = "Metric:",
                          choices = c("coverage", "shannon", "faith"),
                          multiple = TRUE),
                      
                      textInput(inputId = "alpha.name", label = "Name:")
                           
                  ),
                  
                  tabPanel(title = "Beta", value = "beta",
                           
                      radioButtons(inputId = "bmethod", label = "Method:",
                          choices = c("MDS", "NMDS", "PCA", "RDA"), inline = TRUE),
                         
                           
                      selectInput(inputId = "beta.assay", label = "Assay:",
                          choices = NULL),
                      
                      conditionalPanel(
                          condition = "input.bmethod != 'PCA'",
                        
                          selectInput(inputId = "beta.index", label = "Metric:",
                              choices = c("euclidean", "bray", "jaccard", "unifrac")),
                      ),
                      
                      conditionalPanel(
                          condition = "input.bmethod == 'RDA'",
                        
                          textInput(inputId = "rda.formula", label = "Formula:",
                              placeholder = "data ~ var1 + var2 * var3"),
                      ),
                      
                      numericInput(inputId = "ncomponents", value = 5,
                          label = "Number of components:", min = 1, step = 1),
                      
                      textInput(inputId = "beta.name", label = "Name:")
                                  
                  )
                  
              ),
              
              actionButton("compute", "Compute", class = "btn-success",
                  style = .actionbutton_biocstyle)
              
            ))),
  
            fluidRow(
              
              column(4, wellPanel(id = "visualise.panel",
                
                titlePanel("Visualise"),
                
                selectInput(inputId = "panels", label = "Panels:",
                    choices = c(default_panels, other_panels),
                    multiple = TRUE, selected = c(default_panels)),
                
                actionButton("launch", "Launch iSEE", class = "btn-success",
                    style = .actionbutton_biocstyle)
                
              )),
              
              column(8, wellPanel(id = "output.panel",
              
              titlePanel("Output"),
              
              verbatimTextOutput(outputId = "object"),
              
              downloadButton(outputId = "download", label = "Download",
                  style = .actionbutton_biocstyle)
              
            )))
          )
    })
    
    ## Disable navbar buttons that are not linked to any observer yet
    disable("iSEE_INTERNAL_organize_panels")  # organize panels
    disable("iSEE_INTERNAL_link_graph")       # link graph
    disable("iSEE_INTERNAL_export_content")   # export content
    disable("iSEE_INTERNAL_tracked_code")     # tracked code
    disable("iSEE_INTERNAL_panel_settings")   # panel settings
    disable("iSEE_INTERNAL_open_vignette")    # open vignette
    disable("iSEE_INTERNAL_session_info")     # session info
    disable("iSEE_INTERNAL_citation_info")    # citation info
    
    rObjects <- reactiveValues(tse = NULL)
    
    .create_import_observers(input, rObjects)
    .create_manipulate_observers(input, rObjects)
    .create_estimate_observers(input, rObjects)
    .update_observers(input, session, rObjects)

    .create_launch_observers(FUN, input, session, rObjects)
    
    .render_overview(output, rObjects)
    .render_download(output, rObjects)

    invisible(NULL)
    # nocov end
}