Install.biocViews <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  if (!requireNamespace("TFBSTools", quietly = TRUE))
    BiocManager::install("TFBSTools")
  
  if (!requireNamespace("motifmatchr", quietly = TRUE))
    BiocManager::install("motifmatchr")
  
  if (!requireNamespace("Gviz", quietly = TRUE))
    BiocManager::install("Gviz")
  
  if (!requireNamespace("ChIPseeker", quietly = TRUE))
    BiocManager::install("ChIPseeker")
  
  if (!requireNamespace("TxDb.Hsapiens.UCSC.hg38.knownGene", quietly = TRUE))
    BiocManager::install("TxDb.Hsapiens.UCSC.hg38.knownGene")
  
  if (!requireNamespace("TxDb.Mmusculus.UCSC.mm10.knownGene", quietly = TRUE))
    BiocManager::install("TxDb.Mmusculus.UCSC.mm10.knownGene")
  
  if (!requireNamespace("BSgenome.Hsapiens.UCSC.hg38", quietly = TRUE))
    BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
  
  if (!requireNamespace("BSgenome.Mmusculus.UCSC.mm10", quietly = TRUE))
    BiocManager::install("BSgenome.Mmusculus.UCSC.mm10")
  
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
    BiocManager::install("org.Hs.eg.db")
  
  if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))
    BiocManager::install("org.Mm.eg.db")
  
}
