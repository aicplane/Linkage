#' @importFrom utils packageDescription
.onAttach = function(libname, pkgname) {
  version = packageDescription(pkgname, fields = "Version")

  msg = paste0("====================================================
", pkgname, " version ", version, "
ShinyApp page: https://xulabgdpu.org.cn/linkage/
Github page: https://github.com/aicplane/Linkage/
Documentation: https://aicplane.github.io/Linkage/

This message can be suppressed by:
  suppressPackageStartupMessages(library(Linkage))
====================================================
")
  packageStartupMessage(msg)
}
