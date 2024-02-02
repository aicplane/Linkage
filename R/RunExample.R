#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
Linkage <- function() {
  appDir <- system.file(paste0("apps"), package = "Linkage")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir)
}
