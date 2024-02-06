#' Regulatory peak motif enrichment analysis
#'
#' @param peakfile ATAC-Seq expression matrix or bed file
#' @param Species Select the species, Homo or Mus.
#'
#' @return Motif result
#' @export
#'
#' @examples
#' dir <- system.file("extdata","ENSG00000000419.csv", package = "Linkage")
#' peakfile <- read.csv(dir, header = T)
#' motif_analysis(peakfile, "Homo")
motif_analysis <- function(peakfile, Species) {
  colnames(peakfile) <- c("chrom", "chromStart", "chromEnd")
  peaks <- peakfile
  peaks <- GenomicRanges::GRanges(
    seqnames = c(peaks$chrom),
    ranges = IRanges::IRanges(start = peaks$chromStart, end = peaks$chromEnd)
  )

  if (Species == "Homo") {
    PFMatrixList.dir <- system.file("extdata","PFMatrixList.rds", package = "Linkage")
    pwm_library_dt.dir <- system.file("extdata","pwm_library_dt.rds", package = "Linkage")
    PFMatrixList <- readRDS(PFMatrixList.dir)
    pwm_library_dt <- readRDS(pwm_library_dt.dir)
    genome <- "hg38"
  }
  if (Species == "Mus") {
    PFMatrixList.dir <- system.file("extdata","Mus.PFMatrixList.rds", package = "Linkage")
    pwm_library_dt.dir <- system.file("extdata","Mus.pwm_library_dt.rds", package = "Linkage")
    PFMatrixList <- readRDS(PFMatrixList.dir)
    pwm_library_dt <- readRDS(pwm_library_dt.dir)
    genome <- "mm10"
  }

  # Get motif positions within peaks for example motifs in peaks
  motif_ix <- motifmatchr::matchMotifs(PFMatrixList, peaks,
    genome = genome,
    out = "positions"
  ) %>% data.frame()

  motif <- pwm_library_dt[motif_ix$group, ]
  motif <- cbind(motif, motif_ix)
  motif <- motif[, c(-3, -4)]
  return(motif)
}

#' Transcription factor Seqlogo plot
#'
#' @param motif_ID Motif ID
#'
#' @return seqLogo plot
#' @export
#'
#' @examples
#' dir <- system.file("extdata","ENSG00000000419.csv", package = "Linkage")
#' peakfile <- read.csv(dir, header = T)
#' motif_analysis(peakfile, "Homo")
#' seqLogo_plot("MA0618.1")
seqLogo_plot <- function(motif_ID) {
  sqlite.dir <- system.file("extdata","JASPAR2022.sqlite", package = "Linkage")
  m <- TFBSTools::getMatrixByID(sqlite.dir, motif_ID)
  return(TFBSTools::seqLogo(TFBSTools::toICM(m)))
}
