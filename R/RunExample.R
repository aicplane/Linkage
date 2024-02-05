#' Launch Shiny App
#'
#' @export
#'
Linkage <- function() {
  appDir <- system.file(paste0("apps"), package = "Linkage")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir)
}
