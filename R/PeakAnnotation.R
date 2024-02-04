Peak_Annottation <- function(peakfile,Species = "Homo",...){
  gr <-
    GenomicRanges::makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
  if (Species == "Homo") {
    txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
    annoDb <- "org.Hs.eg.db"
  }
  if (Species == "Mus"){
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

# peakAnno <- Peak_Annottation(peakfile,Species = "Homo")

vennpie <- function(object,...){
  ChIPseeker::upsetplot(object, vennpie = TRUE)
  return(p)
}

# p <- vennpie(peakAnno, vennpie = TRUE)


