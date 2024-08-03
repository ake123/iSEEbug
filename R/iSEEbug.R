iSEEbug <- function() {

  iSEE(
    landingPage = .landing_page,
    appTitle = sprintf("iSEEtree - v%s", packageVersion("iSEEtree"))
  )

}

#' @importFrom iSEEtree RowTreePlot AbundancePlot AbundanceDensityPlot
.launch_isee <- function(FUN, session, rObjects) {

  se <- rObjects$tse
  
  if( is(se, "TreeSummarizedExperiment") ){
    
    initial <- c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
                 AbundancePlot(), AbundanceDensityPlot(), ReducedDimensionPlot(),
                 ComplexHeatmapPlot())
    
    initial <- iSEEtree:::.check_panel(se, initial, "RowDataTable", rowData)
    initial <- iSEEtree:::.check_panel(se, initial, "ColumnDataTable", colData)
    initial <- iSEEtree:::.check_panel(se, initial, "RowTreePlot", rowLinks)
    initial <- iSEEtree:::.check_panel(se, initial, "AbundancePlot", taxonomyRanks)
    initial <- iSEEtree:::.check_panel(se, initial, "ReducedDimensionPlot", reducedDims)
    
  } else {
    initial <- NULL
  }
  
  FUN(SE = se, INIT = initial)#, EXTRA = initial)
  
  shinyjs::enable(iSEE:::.generalOrganizePanels) # organize panels
  shinyjs::enable(iSEE:::.generalLinkGraph) # link graph
  shinyjs::enable(iSEE:::.generalExportOutput) # export content
  shinyjs::enable(iSEE:::.generalCodeTracker) # tracked code
  shinyjs::enable(iSEE:::.generalPanelSettings) # panel settings
  shinyjs::enable(iSEE:::.generalVignetteOpen) # open vignette
  shinyjs::enable(iSEE:::.generalSessionInfo) # session info
  shinyjs::enable(iSEE:::.generalCitationInfo) # citation info
  
  invisible(NULL)
}
