Install.biocViews <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("TFBSTools")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("motifmatchr")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("Gviz")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("ChIPseeker")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("TxDb.Hsapiens.UCSC.hg38.knownGene")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("TxDb.Mmusculus.UCSC.mm10.knownGene")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("BSgenome.Mmusculus.UCSC.mm10")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("org.Hs.eg.db")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("org.Mm.eg.db")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
  
  if (!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
}