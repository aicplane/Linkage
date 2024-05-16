#' Regulatory peak motif enrichment analysis
#'
#' @param peakfile ATAC-Seq expression matrix or bed file
#' @param Species Select the species, Homo or Mus.
#'
#' @return Motif result
#' @export
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom motifmatchr matchMotifs
#'
#' @examples
#' library(linkage)
#' library(LinkageData)
#' peakpath <- system.file("extdata","ENSG00000000419.rdata",package = "LinkageData")
#' load(peakpath)
#' motif <- MotifEnrichment(PeakFile, "Homo")
#' head(motif)
MotifEnrichment <- function(peakfile, Species) {
  colnames(peakfile) <- c("chrom", "chromStart", "chromEnd")
  peaks <- peakfile
  peaks <- GRanges(
    seqnames = c(peaks$chrom),
    ranges = IRanges(start = peaks$chromStart, end = peaks$chromEnd)
  )

  if (Species == "Homo") {
    data("PFMatrixList")
    data("pwm_library_dt")
    genome <- "hg38"
  }
  if (Species == "Mus") {
    data("Mus.PFMatrixList")
    data("Mus.pwm_library_dt")
    PFMatrixList <- Mus.PFMatrixList
    pwm_library_dt <- Mus.pwm_library_dt
    genome <- "mm10"
  }

  # Get motif positions within peaks for example motifs in peaks
  motif_ix <- matchMotifs(PFMatrixList, peaks,
    genome = genome,
    out = "positions"
  ) %>% data.frame()

  motif <- pwm_library_dt[motif_ix$group, ]
  motif <- cbind(motif, motif_ix)
  motif <- motif[, c(-3, -4)]
  return(motif)
}

#' Transcription factor Seqlogo plot.
#'
#' @param motif_ID Motif ID.
#'
#' @return seqLogo plot.
#' @export
#' @importFrom TFBSTools getMatrixByID seqLogo toICM
#'
#' @examples
#' library(linkage)
#' SeqLogoPlot("MA0618.1")
SeqLogoPlot <- function(motif_ID) {
  sqlite.dir <- system.file("extdata","JASPAR2022.sqlite", package = "linkage")
  m <- getMatrixByID(sqlite.dir, motif_ID)
  return(seqLogo(toICM(m)))
}


#' MotifAnalysis.
#'
#' @param LinkageObject An Linkage Object after RegulatoryPeak.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#' @param TF_cor_method For correlation calculation, pearson/spearman/kendall can be selected.
#'
#' @return An Linkage Object after BuildGRNs
#' @export
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom motifmatchr matchMotifs
#'
#' @examples
#' library(linkage)
#' library(LinkageData)
#' ATAC.seq <- BreastCancerATAC()
#' RNA.seq <- BreastCancerRNA()
#' LinkageObject <-
#'   CreateLinkageObject(
#'     ATAC_count = ATAC.seq,
#'     RNA_count = RNA.seq,
#'     Species = "Homo",
#'     id_type = "ensembl_gene_id"
#'   )
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = LinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' LinkageObject <-
#'   BuildGRNs (LinkageObject = LinkageObject,
#'                         Species = "Homo",
#'                         TF_cor_method = "pearson")
BuildGRNs <- function(LinkageObject,Species = "Homo",TF_cor_method){
  tf <- list()
  motif <- list()
  result_peak <- LinkageObject@cor.peak
  if (Species == "Homo") {
    data("PFMatrixList")
    data("pwm_library_dt")
    genome <- "hg38"
  }
  if (Species == "Mus") {
    data("Mus.PFMatrixList")
    data("Mus.pwm_library_dt")
    PFMatrixList <- Mus.PFMatrixList
    pwm_library_dt <- Mus.pwm_library_dt
    genome <- "mm10"
  }

  for (i in 1:length(result_peak)) {
    peaks <- result_peak[[i]]
    peaks <- GRanges(
      seqnames = c(peaks$chrom),
      ranges = IRanges(
        start = peaks$chromStart,
        end = peaks$chromEnd
      )
    )

    # Get motif positions within peaks for example motifs in peaks
    motif_ix <- matchMotifs(PFMatrixList,
                                         peaks,
                                         genome = genome,
                                         out = "positions") %>% data.frame()

    motif[[i]] <- pwm_library_dt[motif_ix$group, ]
    motif[[i]] <- cbind(motif[[i]], motif_ix)
    motif[[i]] <- unique(motif[[i]]$name)

    tf[[i]] <- LinkageObject@RNA.mtrix[LinkageObject@RNA.mtrix$external_gene_name %in% motif[[i]],]

    RNA_row <- LinkageObject@active.gene[i,]

    p <- c()
    r <- c()

    for (j in 1:nrow(tf[[i]])) {
      df <- rbind(tf[[i]][j, c(-1:-6)],
                  RNA_row[, c(-1:-6)])
      tdf <- t(df)
      ftdf <- data.frame(tdf)

      tryCatch({
        if (TF_cor_method == "pearson") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
        }
        if (TF_cor_method == "spearman") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$p.value
        }
        if (TF_cor_method == "kendall") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
        }
      },
      error = function(e) {
        p[j] <- NULL
      })
      tryCatch({
        if (TF_cor_method == "pearson") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
        }
        if (TF_cor_method == "spearman") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$estimate
        }
        if (TF_cor_method == "kendall") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
        }
      },
      error = function(e) {
        r[j] <- NULL
      })
    }

    tf[[i]]$p_value <- p
    tf[[i]]$FDR <- p.adjust(p, method = "BH")
    tf[[i]]$rho <- r
  }
  names(tf) <- LinkageObject@geneid
  LinkageObject <-
    new(
      "LinkageObject",
      RNA.mtrix = LinkageObject@RNA.mtrix,
      ATAC.matrix = LinkageObject@ATAC.matrix ,
      active.gene = LinkageObject@active.gene,
      cor.peak = LinkageObject@cor.peak,
      cor.peak.annotation = LinkageObject@cor.peak.annotation,
      detailpeakannotation = LinkageObject@detailpeakannotation,
      Motif = tf,
      Gene_TF = LinkageObject@Gene_TF,
      geneid = LinkageObject@geneid,
      Summary = LinkageObject@Summary
    )
  return(LinkageObject)
}


