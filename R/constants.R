#' Constants
#'
#' Constant values used throughout miaDash panels and extensions.
#' 
#' @section Panel layout:
#' \describe{
#' \item{\code{.miaDashDefaultPanels}}{List of panel names in the default layout of miaDash.}
#' \item{\code{.miaDashOtherPanels}}{List of panel names not in the default layout of miaDash.}
#' }
#'
#' @author Giulio Benedetti
#' 
#' @name constants
#' @aliases .miaDashDefaultPanels
#' .miaDashOtherPanels
NULL

#' @rdname constants
.miaDashDefaultPanels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
    "ComplexHeatmapPlot")

#' @rdname constants
.miaDashOtherPanels <- c("RDAPlot", "ScreePlot", "LoadingPlot",
    "ColumnTreePlot", "RowGraphPlot", "ColumnGraphPlot", "ColumnDataPlot",
    "RowDataPlot")