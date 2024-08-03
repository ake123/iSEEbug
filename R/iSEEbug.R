#' iSEEbug
#'
#' iSEEbug is a web app that provides an interface to build and explore
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' (TreeSE) objects by means of \link[iSEE:iSEE]{iSEE}.
#'
#' @return An \code{\link[iSEE:iSEE]{iSEE}} app with a custom landing page to
#'   build TreeSE objects and explore \link[mia:mia-datasets]{mia datasets}.
#'
#' @examples
#' app <- iSEEbug()
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' 
#' @seealso \link[iSEE:iSEE]{iSEE} \link[mia:mia]{mia}
#'   \link[miaViz:miaViz]{miaViz}
#'
#' @name iSEEbug

#' @export
#' @rdname iSEEbug
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
iSEEbug <- function() {

    iSEE(
        landingPage = .landing_page,
        appTitle = sprintf("iSEEbug - v%s", packageVersion("iSEEbug"))
    )

}

#' @importFrom methods is
#' @importFrom shinyjs enable
#' @importFrom iSEEtree RowTreePlot AbundancePlot AbundanceDensityPlot
.launch_isee <- function(FUN, session, rObjects) {

    se <- rObjects$tse
  
    if( is(se, "TreeSummarizedExperiment") ){
    
    initial <- c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
                 AbundancePlot(), AbundanceDensityPlot(),
                 ReducedDimensionPlot(), ComplexHeatmapPlot())
    
    initial <- iSEEtree:::.check_panel(se, initial, "RowDataTable", rowData)
    initial <- iSEEtree:::.check_panel(se, initial, "ColumnDataTable", colData)
    initial <- iSEEtree:::.check_panel(se, initial, "RowTreePlot", rowLinks)
    initial <- iSEEtree:::.check_panel(se, initial, "AbundancePlot", taxonomyRanks)
    initial <- iSEEtree:::.check_panel(se, initial, "ReducedDimensionPlot", reducedDims)
    
    } else {
        initial <- NULL
    }
  
    FUN(SE = se, INIT = initial)#, EXTRA = initial)
  
    enable(iSEE:::.generalOrganizePanels) # organize panels
    enable(iSEE:::.generalLinkGraph) # link graph
    enable(iSEE:::.generalExportOutput) # export content
    enable(iSEE:::.generalCodeTracker) # tracked code
    enable(iSEE:::.generalPanelSettings) # panel settings
    enable(iSEE:::.generalVignetteOpen) # open vignette
    enable(iSEE:::.generalSessionInfo) # session info
    enable(iSEE:::.generalCitationInfo) # citation info
  
    invisible(NULL)
}