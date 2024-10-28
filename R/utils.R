default_panels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
                    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
                    "ComplexHeatmapPlot")

other_panels <- c("LoadingPlot", "ColumnTreePlot", "RDAPlot", "ColumnDataPlot",
                  "RowDataPlot")

.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"

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
