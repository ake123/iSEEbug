timestamp()
message("* Started")

library(iSEEbug)

app <- iSEEbug()
shiny::runApp(app, port = 1234, host = "0.0.0.0")

message("* Completed")
timestamp()
