#' Observers
#' 
#' \code{.create_observers} and \code{.create_launch_observers} define the
#' server to import and build TreeSE objects and track the state of the Build
#' and Launch buttons.
#'
#' @param input The Shiny input object from the server function.
#' @param pObjects An environment containing global parameters generated in the
#'   landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @name create_observers
#' @keywords internal

#' @rdname create_observers
#' @importFrom shiny isolate observeEvent req
#' @importFrom biomformat read_biom
#' @importFrom mia convertFromBIOM
#' @importFrom SummarizedExperiment SummarizedExperiment
.create_observers <- function(input, session, rObjects) {
  
    observeEvent(input$import, {
      
        if( input$format == "dataset" ){
      
            rObjects$tse <- isolate(base::get(input$data))
      
        }else if( input$format == "rda" ){
      
            isolate({
                req(input$file)
                load(file = input$file$datapath)
                rObjects$tse <- base::get(gsub(".rda", "", input$file$name))
            })
      
        }else if( input$format == "raw" ){
      
            isolate({
                req(input$assay)
        
                assay_list <- lapply(input$assay$datapath,
                    function(x) as.matrix(read.csv(x, row.names = 1)))
                
                if( !is.null(input$coldata) ){
                    coldata <- read.csv(input$coldata$datapath, row.names = 1)
                } else {
                    coldata <- DataFrame(row.names = colnames(assay_list[[1]]))
                }
      
                
                if( !is.null(input$rowdata) ){
                    rowdata <- read.csv(input$rowdata$datapath, row.names = 1)
                } else {
                    rowdata <- NULL
                }
                
                names(assay_list) <- gsub(".csv", "", input$assay$name)
        
                rObjects$tse <- SummarizedExperiment(assays = assay_list,
                    colData = coldata, rowData = rowdata)
            })
      
        }else if( input$format == "foreign" ){
          
            isolate({
                req(input$main.file)
      
                if( input$ftype == "biom" ){
              
                    biom_object <- read_biom(input$main.file$datapath)
                
                    rObjects$tse <- convertFromBIOM(biom_object,
                        removeTaxaPrefixes = input$rm.tax.pref,
                        rankFromPrefix = input$rank.from.pref)
              
                } else if( input$ftype == "MetaPhlAn" ){
                  
                    if( !is.null(input$col.data) ){
                        coldata <- input$col.data$datapath
                    } else {
                        coldata <- NULL
                    }
                  
                    if( !is.null(input$tree.file) ){
                        treefile <- input$tree.file$datapath
                    } else {
                        treefile <- NULL
                    }
              
                    rObjects$tse <- importMetaPhlAn(input$main.file$datapath,
                        col.data = coldata, tree.file = treefile) 
                 
                }
        
            })
            
        }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
    observeEvent(input$apply, {
      
        if( input$manipulate == "subset" ){
          
            isolate({
            
              # subsetByPrevalent
              # subsetByRare
              
            })
          
        }
      
        else if( input$manipulate == "aggregate" ){
          
            isolate({
                
                rObjects$tse <- agglomerateByRank(rObjects$tse,
                    rank = input$taxrank)
              
            })
          
        } else if( input$manipulate == "transform" ){

            isolate({
                req(input$assay.type)
              
                if( mia:::.is_non_empty_string(input$assay.name) ){
                    name <- input$assay.name
                } else {
                    name <- input$trans.method
                }

                rObjects$tse <- transformAssay(rObjects$tse, name = name,
                    assay.type = input$assay.type, method = input$trans.method,
                    pseudocount = input$pseudocount, MARGIN = input$margin)
            
            })
          
        }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    observe({
      
      if( isS4(rObjects$tse) ){
        
          updateSelectInput(session, inputId = "assay.type",
              choices = assayNames(rObjects$tse))
        
          updateSelectInput(session, inputId = "taxrank",
              choices = taxonomyRanks(rObjects$tse))
        
      }
    
    })
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom shiny observeEvent
.create_launch_observers <- function(FUN, input, session, rObjects) {
  
    observeEvent(input$launch, {
    
        .launch_isee(FUN, session, rObjects)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
    invisible(NULL)
}