test_that("utils", {
  
    data("Tengeler2020", package = "mia")
    tse <- Tengeler2020

    idx <- c(1, 3)
    expect_equal(.import_datasets(idx),
        data(package = "mia")$results[idx, "Item"])
    
    expect_no_error(
        tse <- .update_tse(tse, transformAssay,
            list(x = tse, assay.type = "counts", method = "relabundance"))
    )
    
    item <- NULL
    expect_identical(.set_optarg(item, alternative = "alternative"),
       "alternative")
    
    item <- "100"
    expect_identical(.set_optarg(item, loader = as.numeric), 100)
  
    expect_true(.check_formula("data ~ patient_status + cohort", tse))
    expect_false(.check_formula("data ~ wrong_var + sample_name", tse))
    
    panels <- c(RowDataTable(), ReducedDimensionPlot())
    
    expect_warning(
        expect_length(
            .check_panel(tse, panels, "ReducedDimensionPlot", reducedDims), 1
        )
    )

})
