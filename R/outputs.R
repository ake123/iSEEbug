.render_overview <- function(output, rObjects) {
  
  output$object <- renderPrint({
    rObjects$tse
  })

  invisible(NULL)
}


.render_download <- function(output, rObjects) {
  
  output$download <- downloadHandler(
    filename = function() paste0("se-", Sys.Date(), ".rds"),
    content = function(file) saveRDS(rObjects$tse, file)
  )
  
  invisible(NULL)
}