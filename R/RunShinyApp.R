#' Launch Linkage Shiny App
#'
#' @return Shiny App
#' @export

Linkage.UIO <- function() {
  appDir <- system.file(paste0("apps"), package = "linkage")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir)
}
