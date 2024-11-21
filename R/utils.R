#' Utilities
#' 
#' Helper functions and constants to support the app functionality.
#'
#' @name utils
#' @keywords internal

#' @rdname utils
.import_datasets <- function(selection) {
  
    mia_datasets <- data(package = "mia")
    mia_datasets <- mia_datasets$results[selection, "Item"]
    data(list = mia_datasets, package = "mia")
    
    return(mia_datasets)
}

#' @rdname utils
#' @importFrom shiny showNotification
.update_tse <- function(fun, fun_args) {
  
    messages <- c()
  
    withCallingHandlers({
      
        tse <- do.call(fun, fun_args)
      
    }, message = function(m) {
      
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
    
    })
  
    lapply(messages, showNotification)
  
    return(tse)
}

#' @rdname utils
#' @importFrom shiny showModal modalDialog
.print_message <- function(..., title = "Invalid input:"){

    showModal(modalDialog(
        title = title, ...,
        easyClose = TRUE, footer = NULL
    ))
  
}

#' @rdname utils
#' @importFrom SummarizedExperiment colData
.check_formula <- function(form, se){
  
    form <- gsub("data ~\\s*", "", form)
    vars <- unlist(strsplit(form, "\\s*[\\+|\\*]\\s*"))
  
    cond <- all(vars %in% names(colData(se)))
    return(cond)
}

#' @rdname utils
#' @importFrom S4Vectors isEmpty
#' @importFrom methods is
.check_panel <- function(se, panel_list, panel_class, panel_fun, wtext) {
  
  no_keep <- unlist(lapply(panel_list, function(x) is(x, panel_class)))
  
  if( any(no_keep) && (is.null(panel_fun(se)) || isEmpty(panel_fun(se))) ){
    panel_list <- panel_list[!no_keep]
    warning("no valid ", as.character(substitute(panel_fun)),
            " fields for ", panel_class, call. = FALSE)
  }
  
  return(panel_list)
}

#' @rdname utils
default_panels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
                    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
                    "ComplexHeatmapPlot")

#' @rdname utils
other_panels <- c("LoadingPlot", "ColumnTreePlot", "RDAPlot", "ColumnDataPlot",
                  "RowDataPlot")

#' @rdname utils
.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"
