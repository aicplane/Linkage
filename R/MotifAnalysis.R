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
#' peakfile <- read.csv(dir, header = TRUE)
#' motif <- SingleMotifAnalysis(peakfile, "Homo")
#' head(motif)
SingleMotifAnalysis <- function(peakfile, Species) {
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

#' Transcription factor Seqlogo plot.
#'
#' @param motif_ID Motif ID.
#'
#' @return seqLogo plot.
#' @export
#'
#' @examples
#' seqLogo_plot("MA0618.1")
seqLogo_plot <- function(motif_ID) {
  sqlite.dir <- system.file("extdata","JASPAR2022.sqlite", package = "Linkage")
  m <- TFBSTools::getMatrixByID(sqlite.dir, motif_ID)
  return(TFBSTools::seqLogo(TFBSTools::toICM(m)))
}


#' MotifAnalysis.
#'
#' @param LinkageObject An Linkage Object after regulatory_peak.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#' @param TF_cor_method For correlation calculation, pearson/spearman/kendall can be selected.
#'
#' @return An Linkage Object after MultipleMotifAnalysis
#' @export
#'
#' @examples
#' RNA.seq.dir <- system.file("extdata", "TCGA-BRCA-RNA.txt", package = "Linkage")
#' RNA.seq <- data.table::fread(RNA.seq.dir, header = TRUE)
#' Homo.list.files.dir <- system.file("extdata", "Homo.ATAC", package = "Linkage")
#' Homo.list.files <- list.files(Homo.list.files.dir)
#' Homo.list.files <- paste0(Homo.list.files.dir, "/", Homo.list.files)
#' Homo.df_list <- lapply(Homo.list.files, function(file) data.table::fread(file, header = TRUE))
#' ATAC.seq <- do.call(rbind, Homo.df_list)
#' LinkageObject <- CreateLinkageObject(ATAC_count = ATAC.seq, RNA_count = RNA.seq, Species = "Homo", id_type = "ensembl_gene_id")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13", "ARX", "HCCS")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' LinkageObject <- MultipleMotifAnalysis(LinkageObject = LinkageObject,Species = "Homo",TF_cor_method = "pearson")
MultipleMotifAnalysis <- function(LinkageObject,Species = "Homo",TF_cor_method){
  tf <- list()
  motif <- list()
  result_peak <- LinkageObject@cor.peak
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

  for (i in 1:length(result_peak)) {
    peaks <- result_peak[[i]]
    peaks <- GenomicRanges::GRanges(
      seqnames = c(peaks$chrom),
      ranges = IRanges::IRanges(
        start = peaks$chromStart,
        end = peaks$chromEnd
      )
    )

    # Get motif positions within peaks for example motifs in peaks
    motif_ix <- motifmatchr::matchMotifs(PFMatrixList,
                                         peaks,
                                         genome = genome,
                                         out = "positions") %>% data.frame()

    motif[[i]] <- pwm_library_dt[motif_ix$group, ]
    motif[[i]] <- cbind(motif[[i]], motif_ix)
    motif[[i]] <- unique(motif[[i]]$name)
    # print(motif[[i]])

    tf[[i]] <- LinkageObject@RNA.mtrix[LinkageObject@RNA.mtrix$external_gene_name %in% motif[[i]],]

    RNA_row <- LinkageObject@active.gene[i,]

    p <- c()
    r <- c()

    for (j in 1:nrow(tf[[i]])) {
      df <- rbind(tf[[i]][j, c(-1:-6)],
                  RNA_row[, c(-1:-6)]) # 构造新的数据框
      tdf <- t(df)
      ftdf <- data.frame(tdf) # 转置数据框

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
        message("error: ", conditionMessage(e))
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
        message("error: ", conditionMessage(e))
        r[j] <- NULL
      })
    }

    tf[[i]]$p_value <- p
    tf[[i]]$FDR <- p.adjust(p, method = "BH")
    tf[[i]]$rho <- r
  }
  names(tf) <- LinkageObject@geneid
  # print(tf)
  LinkageObject <-
    new(
      "LinkageObject",
      RNA.mtrix = LinkageObject@RNA.mtrix,
      ATAC.matrix = LinkageObject@ATAC.matrix ,
      active.gene = LinkageObject@active.gene,
      cor.peak = LinkageObject@cor.peak,
      Motif = tf,
      geneid = LinkageObject@geneid
    )
  return(LinkageObject)
}


