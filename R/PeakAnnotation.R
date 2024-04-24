#' Annotation regulatory peak
#'
#' @param LinkageObject An Linkage Object after RegulatoryPeak.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#'
#' @return An LinkageObject after the annottation.
#'
#' @export
#' @importFrom TxDb.Hsapiens.UCSC.hg38.knownGene TxDb.Hsapiens.UCSC.hg38.knownGene
#' @importFrom TxDb.Mmusculus.UCSC.mm10.knownGene TxDb.Mmusculus.UCSC.mm10.knownGene
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom ChIPseeker annotatePeak
#'
#' @examples
#' library(linkage)
#' data("SmallLinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = SmallLinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' peakAnno <- PeakAnnottation(LinkageObject, Species = "Homo")
PeakAnnottation <- function(LinkageObject, Species = "Homo") {
  ann <- list()
  detail <- list()
  for (i in 1:length(LinkageObject@geneid)) {
    peakfile <- LinkageObject@cor.peak[[i]]
    gr <-
      makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
    if (Species == "Homo") {
      txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
      annoDb <- "org.Hs.eg.db"
    }
    if (Species == "Mus") {
      txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene
      annoDb <- "org.Mm.eg.db"
    }

    peakAnno <- annotatePeak(
      gr,
      tssRegion = c(-3000, 3000),
      TxDb = txdb,
      annoDb = annoDb,
      verbose = F
    )
    ann[[i]] <- peakAnno@anno
    detail[[i]] <- peakAnno@detailGenomicAnnotation
  }
  names(ann) <- LinkageObject@geneid
  names(detail) <- LinkageObject@geneid
  p <-
    new(
      "LinkageObject",
      RNA.mtrix = LinkageObject@RNA.mtrix,
      ATAC.matrix = LinkageObject@ATAC.matrix,
      active.gene = LinkageObject@active.gene,
      cor.peak = LinkageObject@cor.peak,
      cor.peak.annotation = ann,
      detailpeakannotation = detail,
      Motif = LinkageObject@Motif,
      Gene_TF = LinkageObject@Gene_TF,
      geneid = LinkageObject@geneid,
      Summary = LinkageObject@Summary
    )
  return(p)
}



#' AnnoUpsetPlot
#' @param LinkageObject An LinkageObject after the annottation.
#' @param order_by  How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.
#'
#' @return An upsetplot after the annottation.
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr distinct mutate
#' @importFrom tidyr unnest spread
#' @importFrom UpSetR upset
#'
#' @examples
#' library(linkage)
#' data("SmallLinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = SmallLinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' peakAnno <- PeakAnnottation(LinkageObject, Species = "Homo")
#' AnnoUpsetPlot(LinkageObject = peakAnno)
AnnoUpsetPlot <- function(LinkageObject, order_by = "freq") {

  y <- data.frame()
  for (i in 1:length(LinkageObject@geneid)) {
    y <- rbind(y,as.data.frame(LinkageObject@detailpeakannotation[[i]]))
  }

  nn <- names(y)
  y <- as.matrix(y)
  res <- lapply(1:nrow(y), function(i) nn[y[i,]])


  x <- data.frame()
  for (i in 1:length(LinkageObject@geneid)) {
    x <- rbind(x,as.data.frame(LinkageObject@cor.peak.annotation[[i]]))
  }
  x <- tibble(x)
  x$res <- res

  x <- x %>%
    distinct(annotation, .keep_all = TRUE) %>%
    unnest() %>%
    mutate(GenreMember = 1) %>%
    spread(res, GenreMember, fill = 0) %>%
    as.data.frame()

  upset(x,
        keep.order = F,
        order.by = "freq",
        decreasing = TRUE
      )
}


