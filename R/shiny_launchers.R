#' Launch an instance of the Allen Brain Atlas (ABA) shiny app
#'
launch_aba_shiny <- function() {
  aba_shiny <- system.file("shiny-examples", "aba", package = "xstitch")
  shiny::runApp(aba_shiny)
}
