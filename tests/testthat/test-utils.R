test_that("utils", {
  
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020
    
    expect_equal(.import_datasets(c(1, 3)), c("GlobalPatterns", "Tengeler2020"))
    
    expect_no_error(
        tse <- .update_tse(transformAssay, list(x = tse, assay.type = "counts",
            method = "relabundance"))
    )
  
    expect_true(.check_formula("data ~ patient_status + cohort", tse))
    expect_false(.check_formula("data ~ wrong_var + sample_name", tse))
    
    panels <- c(RowDataTable(), ReducedDimensionPlot())
    
    expect_warning(
        expect_length(
            .check_panel(tse, panels, "ReducedDimensionPlot", reducedDims), 1
        )
    )

})
