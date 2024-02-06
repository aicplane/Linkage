#' Annotation regulatory peak
#'
#' @param peakfile ATAC-Seq expression matrix or bed file.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#' @param ... ...
#'
#' @return A Annottation object
#' @export
#'
#' @examples
#' extdatadir <- system.file(paste0("extdata"), package = "Linkage")
#' peakfile <- read.csv(paste0(extdatadir, "/ENSG00000000419.csv"), header = T)
#' peakAnno <- Peak_Annottation(peakfile, Species = "Homo")
Peak_Annottation <- function(peakfile, Species = "Homo", ...) {
  gr <-
    GenomicRanges::makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
  if (Species == "Homo") {
    txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
    annoDb <- "org.Hs.eg.db"
  }
  if (Species == "Mus") {
    txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene::TxDb.Mmusculus.UCSC.mm10.knownGene
    annoDb <- "org.Mm.eg.db"
  }

  peakAnno <- ChIPseeker::annotatePeak(
    gr,
    tssRegion = c(-3000, 3000),
    TxDb = txdb,
    annoDb = annoDb
  )
  return(peakAnno)
}




#' Annotation regulatory peak upset plot
#'
#' @param object A Annottation object.
#' @param vennpie logical value, whether to include vennpie.The default is TRUE
#' @param ... ...
#'
#' @return A upset plot
#' @export
#'
#' @examples
#' extdatadir <- system.file(paste0("extdata"), package = "Linkage")
#' peakfile <- read.csv(paste0(extdatadir, "/ENSG00000000419.csv"), header = T)
#' peakAnno <- Peak_Annottation(peakfile, Species = "Homo")
#' vennpie(peakAnno)
vennpie <- function(object, vennpie = TRUE, ...) {
  p <- ChIPseeker::upsetplot(object, vennpie = vennpie)
  return(p)
}
