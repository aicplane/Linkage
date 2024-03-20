#' Annotation regulatory peak
#'
#' @param LinkageObject An Linkage Object after regulatory_peak.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#' @param ... ...
#'
#' @import KernSmooth
#'
#' @return An LinkageObject after the annottation.
#' @export
#'
#' @examples
#' data("LinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' peakAnno <- Peak_Annottation(LinkageObject, Species = "Homo")
Peak_Annottation <- function(LinkageObject, Species = "Homo", ...) {
  ann <- list()
  detail <- list()
  for (i in 1:length(LinkageObject@geneid)) {
    peakfile <- LinkageObject@cor.peak[[i]]
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
    ann[[i]] <- peakAnno@anno
    detail[[i]] <- peakAnno@detailGenomicAnnotation
    # print(names(peakfile))
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
      geneid = LinkageObject@geneid,
      cor.peak.annotation = ann,
      detailpeakannotation = detail
    )
  return(p)
}



#' upsetplot.csAnno
#' @param LinkageObject An LinkageObject after the annottation.
#' @param order_by  How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.
#' @param vp
#'
#' @return An upsetplot after the annottation.
#' @export
#'
#' @examples
#' data("LinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' peakAnno <- Peak_Annottation(LinkageObject, Species = "Homo")
#' upsetplot.csAnno(LinkageObject = peakAnno)
upsetplot.csAnno <- function(LinkageObject, order_by = "freq", vp = list(x=.6, y=.7, width=.8, height=.8)) {

  y <- data.frame()
  for (i in 1:length(LinkageObject@geneid)) {
    y <- rbind(y,as.data.frame(LinkageObject@detailpeakannotation[[i]]))
  }

  # y <- LinkageObject@detailpeakannotation
  nn <- names(y)
  y <- as.matrix(y)
  # res <- tibble::tibble(anno = lapply(1:nrow(y), function(i) nn[y[i,]]))
  res <- lapply(1:nrow(y), function(i) nn[y[i,]])


  x <- data.frame()
  for (i in 1:length(LinkageObject@geneid)) {
    x <- rbind(x,as.data.frame(LinkageObject@cor.peak.annotation[[i]]))
  }
  x <- tibble::tibble(x)
  # x <- LinkageObject@cor.peak.annotation
  x$res <- res
  # x <- data.frame(x)

  x <- x %>%
    dplyr::distinct(annotation, .keep_all = TRUE) %>%
    tidyr::unnest() %>%
    dplyr::mutate(GenreMember = 1) %>%
    tidyr::spread(res, GenreMember, fill = 0) %>%
    as.data.frame()

  UpSetR::upset(x,
                # sets = c("distal_intergenic", "downstream", "Exon", "genic", "Intergenic","Intron","Promoter","threeUTR"),
                keep.order = F,order.by = "freq",decreasing = TRUE
  )
}


