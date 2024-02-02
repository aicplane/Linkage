server <- function(input, output, session) {
  source("../../R/uioutput.R",local = TRUE,encoding = "UTF-8")
  source("../../R/updateValue.R",local = TRUE,encoding = "UTF-8")
  source("../../R/uploadData.R",local = TRUE,encoding = "UTF-8")
  source("../../R/RegulatoryPeaks.R",local = TRUE,encoding = "UTF-8")
  source("../../R/PeakVisualization.R",local = TRUE,encoding = "UTF-8")
  source("../../R/PeakAnnotation.R",local = TRUE,encoding = "UTF-8")
  source("../../R/MotifAnalysis.R",local = TRUE,encoding = "UTF-8")
  source("../../R/RegulatoryNetworks.R",local = TRUE,encoding = "UTF-8")
  source("../../R/GO.KEGG.Enrichment.R",local = TRUE,encoding = "UTF-8")
  source("../../R/Download.R",local = TRUE,encoding = "UTF-8")
}
