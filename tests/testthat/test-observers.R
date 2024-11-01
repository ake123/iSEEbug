test_that(".create_observers works", {
  
  input <- new.env()
  rObjects <- new.env()
  FUN <- function(SE, INITIAL) invisible(NULL)
  
  import_out <- .create_import_observers(input, rObjects)
  expect_null(import_out)
  
  manipulate_out <- .create_manipulate_observers(input, rObjects)
  expect_null(manipulate_out)
  
  estimate_out <- .create_estimate_observers(input, rObjects)
  expect_null(estimate_out)
  
  update_out <- .update_observers(input, session = NULL, rObjects)
  expect_null(update_out)
  
  launch_out <- .create_launch_observers(FUN, input, session = NULL, rObjects)
  expect_null(launch_out)
  
})
