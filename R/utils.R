default_panels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
                    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
                    "ComplexHeatmapPlot")

other_panels <- c("LoadingPlot", "ColumnTreePlot", "RDAPlot", "ColumnDataPlot",
                  "RowDataPlot")

.print_message <- function(...){

    showModal(modalDialog(
        title = "Invalid input:", paste(...),
        easyClose = TRUE, footer = NULL
    ))
  
}

.check_formula <- function(form, se){
  
    form <- gsub("data ~\\s*", "", form)
    vars <- unlist(strsplit(form, "\\s*[\\+|\\*]\\s*"))
  
    cond <- all(vars %in% names(colData(se)))
    return(cond)
}
