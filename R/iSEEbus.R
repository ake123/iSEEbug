iSEEbus <- function() {
  
  iSEE(
    landingPage = .landing_page(),
    appTitle = sprintf("iSEEtree - v%s", packageVersion("iSEEtree"))
  )

}

.launch_isee <- function(FUN, session, rObjects) {

  tse <- rObjects$tse
  
  initial <- c(RowDataTable(), ColumnDataTable(), RowTreePlot(),
               AbundancePlot(), AbundanceDensityPlot(), ReducedDimensionPlot(),
               ComplexHeatmapPlot())
  
  initial <- .check_panel(tse, initial, "RowDataTable", rowData)
  initial <- .check_panel(tse, initial, "ColumnDataTable", colData)
  initial <- .check_panel(tse, initial, "RowTreePlot", rowLinks)
  initial <- .check_panel(tse, initial, "AbundancePlot", taxonomyRanks)
  initial <- .check_panel(tse, initial, "ReducedDimensionPlot", reducedDims)
  
  FUN(SE = tse, INIT = initial)#, EXTRA = initial)
  
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

.check_panel <- function(se, panel_list, panel_class, panel_fun, wtext) {
  
  no_keep <- unlist(lapply(panel_list, function(x) is(x, panel_class)))
  
  if( any(no_keep) && (is.null(panel_fun(se)) || isEmpty(panel_fun(se))) ){
    panel_list <- panel_list[!no_keep]
    warning("no valid ", as.character(substitute(panel_fun)),
            " fields for ", panel_class, call. = FALSE)
  }
  
  return(panel_list)
}
