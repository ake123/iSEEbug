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
  
    observeEvent(input$build, {

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
        
                rObjects$tse <- SummarizedExperiment(assays = assay_list,
                    colData = coldata, rowData = rowdata)
            })
      
        }else if( input$format == "foreign" ){
      
            isolate({
                req(input$biom)
              
                biom_object <- read_biom(input$biom$datapath)
                
                rObjects$tse <- convertFromBIOM(biom_object,
                    removeTaxaPrefixes = input$rm.tax.pref,
                    rankFromPrefix = input$rank.from.pref)
            })
      
        }

    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
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