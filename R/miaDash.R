#' miaDash
#'
#' miaDash is a web app that provides an interface to build and explore
#' \code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
#' (TreeSE) objects by means of \link[iSEE:iSEE]{iSEE}.
#'
#' @return An \code{\link[iSEE:iSEE]{iSEE}} app with a custom landing page to
#'   build TreeSE objects and explore \link[mia:mia-datasets]{mia datasets}.
#'
#' @examples
#' app <- miaDash()
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#' 
#' @seealso \link[iSEE:iSEE]{iSEE} \link[mia:mia]{mia}
#'   \link[miaViz:miaViz]{miaViz}
#'
#' @name miaDash

#' @export
#' @rdname miaDash
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
miaDash <- function() {

    iSEE(
        landingPage = .landing_page,
        appTitle = sprintf("miaDash - v%s", packageVersion("miaDash"))
    )

}

#' @importFrom methods is
#' @importFrom shinyjs enable
#' @importFrom iSEE RowDataTable ColumnDataTable ReducedDimensionPlot
#'   ComplexHeatmapPlot
#' @importFrom iSEEtree RowTreePlot AbundancePlot RDAPlot AbundanceDensityPlot
#' @importFrom TreeSummarizedExperiment rowLinks
#' @importFrom mia taxonomyRanks
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom SingleCellExperiment reducedDims
.launch_isee <- function(FUN, initial, session, rObjects) {

    tse <- rObjects$tse
  
    initial <- lapply(initial, function(x) eval(parse(text = paste0(x, "()"))))
    
    initial <- iSEEtree:::.check_panel(tse, initial, "RowDataTable", rowData)
    initial <- iSEEtree:::.check_panel(tse, initial, "ColumnDataTable", colData)
    initial <- iSEEtree:::.check_panel(tse, initial, "RowTreePlot", rowLinks)
    initial <- iSEEtree:::.check_panel(tse, initial, "AbundancePlot", taxonomyRanks)
    initial <- iSEEtree:::.check_panel(tse, initial, "ReducedDimensionPlot", reducedDims)
  
    FUN(SE = tse, INIT = initial)#, EXTRA = initial)
  
    enable(iSEE:::.generalOrganizePanels) # organize panels
    enable(iSEE:::.generalLinkGraph)      # link graph
    enable(iSEE:::.generalExportOutput)   # export content
    enable(iSEE:::.generalCodeTracker)    # tracked code
    enable(iSEE:::.generalPanelSettings)  # panel settings
    enable(iSEE:::.generalVignetteOpen)   # open vignette
    enable(iSEE:::.generalSessionInfo)    # session info
    enable(iSEE:::.generalCitationInfo)   # citation info
  
    invisible(NULL)
}