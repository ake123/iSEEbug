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
#' @importFrom mia convertFromBIOM importMetaPhlAn
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
.create_import_observers <- function(input, rObjects) {
  
    observeEvent(input$import, {
      
        if( input$format == "dataset" ){
      
            rObjects$tse <- isolate(get(input$data))
      
        }else if( input$format == "rds" ){
      
            isolate({
                req(input$file)
                load(file = input$file$datapath)
                rObjects$tse <- readRDS(gsub(".rds", "", input$file$name))
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
        
                rObjects$tse <- TreeSummarizedExperiment(assays = assay_list,
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
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom shiny isolate observeEvent req
#' @importFrom mia subsetByPrevalent subsetByRare agglomerateByRank
#'   transformAssay
.create_manipulate_observers <- function(input, rObjects) {
  
    observeEvent(input$apply, {
      
        if( input$manipulate == "subset" ){
          
            isolate({
                req(input$subassay)
              
                if( input$subkeep == "prevalent" ){
                    subset_fun <- subsetByPrevalent
                } else if( input$subkeep == "rare" ){
                  subset_fun <- subsetByRare
                }
            
                rObjects$tse <- subset_fun(rObjects$tse,
                    assay.type = input$subassay, prevalence = input$prevalence,
                    detection = input$detection)
              
            })
          
        }
      
        else if( input$manipulate == "agglomerate" ){
          
            isolate({
                
                rObjects$tse <- agglomerateByRank(rObjects$tse,
                    rank = input$taxrank)
              
            })
          
        } else if( input$manipulate == "transform" ){

            if( input$trans.method == "clr" && !input$pseudocount &&
                any(assay(rObjects$tse, input$assay.type) <= 0)){
              
                .print_message(
                    "'clr' cannot be used with non-positive data:",
                    "please turn on pseudocount."
                )
              
                return()
            }
          
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
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom shiny isolate observeEvent req
#' @importFrom mia addAlpha runNMDS runRDA getDissimilarity
#' @importFrom scater runMDS runPCA
#' @importFrom vegan vegdist
.create_estimate_observers <- function(input, rObjects) {
  
    observeEvent(input$compute, {
        
        if( input$estimate == "alpha" ){
          
            if( is.null(input$alpha.index) ){
                .print_message("Please select one or more metrics.")
                return()
            }
        
            isolate({
                req(input$alpha.assay)
          
                if( mia:::.is_non_empty_string(input$alpha.name) ){
                    name <- input$alpha.name
                } else {
                    name <- input$alpha.index
                }
          
                rObjects$tse <- addAlpha(rObjects$tse, name = name,
                    assay.type = input$alpha.assay, index = input$alpha.index)
          
            })
        
        } else if( input$estimate == "beta" ){
          
            isolate({
                req(input$beta.assay)
              
                if( mia:::.is_non_empty_string(input$beta.name) ){
                    name <- input$beta.name
                } else {
                    name <- input$bmethod
                }
              
                beta_args <- list(x = rObjects$tse, assay.type = input$assay.type,
                    ncomponents = input$ncomponents, name = name)
              
                if( input$beta.index == "unifrac" ){
                  
                    beta_args <- c(beta_args, FUN = getDissimilarity,
                        tree = rowTree(rObjects$tse), ntop = nrow(rObjects$tse),
                        method = input$beta.index)
                    
                } else if( input$bmethod %in% c("MDS", "NMDS") ){
                  
                    beta_args <- c(beta_args, FUN = vegdist,
                        method = input$beta.index)
                    
                } else if( input$bmethod == "RDA" ){
                  
                    if( input$rda.formula == "" ){
                        .print_message("Please enter a formula.")
                        return()
                    }
                  
                    if( !.check_formula(input$rda.formula, rObjects$tse) ){
                        .print_message("Please make sure all elements in the",
                           "formula match variables of the column data.")
                        return()
                    }
                  
                    beta_args <- c(beta_args,
                        formula = as.formula(input$rda.formula))
                  
                }
              
                beta_fun <- eval(parse(text = paste0("run", input$bmethod)))
                rObjects$tse <- do.call(beta_fun, beta_args)
              
            })
        
        }
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom shiny updateSelectInput updateNumericInput observe
#' @importFrom SummarizedExperiment assayNames
#' @importFrom mia taxonomyRanks
#' @importFrom rintrojs introjs
.update_observers <- function(input, session, rObjects){
  
    observe({
      
      if( isS4(rObjects$tse) ){
        
          updateSelectInput(session, inputId = "subassay",
              choices = assayNames(rObjects$tse))
        
          updateSelectInput(session, inputId = "taxrank",
              choices = taxonomyRanks(rObjects$tse))
          
          updateSelectInput(session, inputId = "assay.type",
              choices = assayNames(rObjects$tse))
          
          updateSelectInput(session, inputId = "alpha.assay",
              choices = assayNames(rObjects$tse))
          
          updateSelectInput(session, inputId = "beta.assay",
              choices = assayNames(rObjects$tse))
          
          updateNumericInput(session, inputId = "ncomponents",
              max = nrow(rObjects$tse) - 1)
        
      }
    
    })
    
    observeEvent(input[[iSEE:::.generalTourSteps]], {
        introjs(session, options=list(steps=.landing_page_tour))
    }, ignoreInit=TRUE)
    
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom shiny observeEvent
.create_launch_observers <- function(FUN, input, session, rObjects) {
  
    observeEvent(input$launch, {
    
        .launch_isee(FUN, input$panels, session, rObjects)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
    invisible(NULL)
}