timestamp()
message("* Started")

library(miaDash)

app <- miaDash()
shiny::runApp(app, port = 1234, host = "0.0.0.0")

message("* Completed")
timestamp()
