#' Regulate peak genomic location browsing
#'
#' @param peakfile  ATAC-Seq expression matrix or bed file.
#' @param select_peak The line index of ATAC-Seq expression matrix or bed file.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#'
#' @return A track plot
#' @export
#'
#' @examples
#' extdatadir <- system.file(paste0("extdata"), package = "Linkage")
#' peakfile <- read.csv(paste0(extdatadir, "/ENSG00000000419.csv"), header = T, check.names = F)
#' gene <- data.table::fread("inst/extdata/TCGA-BRCA-RNA.txt", header = T)
#' gene <- gene[gene$ensembl_gene_id == "ENSG00000000419", ]
#' p <- trackplot(peakfile = peakfile, 1, "Homo")
#' box_plot(peakfile, gene, select_peak = 1, F)
trackplot <- function(peakfile, select_peak, Species) {
  df <- peakfile[, c(-1:-3)]
  t_df <- t(df)
  dt_df <- data.frame(t_df)
  sort_df <- dt_df[order(dt_df[, select_peak]), , drop = FALSE]
  v1 <- sort_df[c(1:(nrow(sort_df) %/% 5)), , drop = FALSE]
  v2 <- sort_df[c(((nrow(sort_df) %/% 5) + 1):(2 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v3 <- sort_df[c((2 * (nrow(sort_df) %/% 5) + 1):(3 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v4 <- sort_df[c((3 * (nrow(sort_df) %/% 5) + 1):(4 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v5 <- sort_df[c((4 * (nrow(sort_df) %/% 5) + 1):nrow(sort_df)), , drop = FALSE]
  m1 <- colMeans(v1)
  m2 <- colMeans(v2)
  m3 <- colMeans(v3)
  m4 <- colMeans(v4)
  m5 <- colMeans(v5)
  data <- data.frame(group1 = m1, group2 = m2, group3 = m3, group4 = m4, group5 = m5)
  # print(data)
  gr <- GenomicRanges::makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
  GenomicRanges::values(gr) <- data
  # print(gr)
  ax <- Gviz::GenomeAxisTrack()

  tracks_list <- list()
  if (Species == "1") {
    gen <- "hg38"
  } else {
    gen <- "mm10"
  }

  for (i in 1:length(names(GenomicRanges::elementMetadata(gr)))) {
    track_name <- paste("track", i, sep = "")
    tracks_list[[i]] <- Gviz::DataTrack(
      gr[, names(GenomicRanges::elementMetadata(gr))[i]],
      genome = gen,
      name = names(GenomicRanges::elementMetadata(gr))[i],
      type = "histogram",
      ylim = c(-1, 10)
    )
  }

  # genome
  gen <- GenomeInfoDb::genome(tracks_list[[i]])
  # Chromosme name
  chr <- as.character(unique(GenomeInfoDb::seqnames(tracks_list[[i]])))
  # Ideogram track (take a long time)
  peak <- peakfile[select_peak, ]

  tryCatch(
    {
      itrack <- IdeogramTrack(genome = gen, chromosome = chr)
      # 突出显示某一区域
      ht <- HighlightTrack(tracks_list,
        start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      return(plotTracks(list(ht, ax, itrack), type = "histogram", col = NULL))
    },
    error = function(e) {
      # 突出显示某一区域
      ht <- Gviz::HighlightTrack(tracks_list,
        start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      return(Gviz::plotTracks(list(ht, ax), type = "histogram", col = NULL))
    }
  )
}



#' Regulate peak genomic location browsing boxplot
#'
#' @param peakfile ATAC-Seq expression matrix or bed file.
#' @param gene Genes regulated by peak.
#' @param select_peak The line index of ATAC-Seq expression matrix or bed file.
#' @param plotly logical value,Whether or not to use plotly.The default is TRUE.
#' @param ... ...
#'
#' @return A box plot.
#' @export
#'
#' @examples
#' extdatadir <- system.file(paste0("extdata"), package = "Linkage")
#' peakfile <- read.csv(paste0(extdatadir, "/ENSG00000000419.csv"), header = T, check.names = F)
#' gene <- data.table::fread(paste0(extdatadir, "/TCGA-BRCA-RNA.txt"), header = T)
#' gene <- gene[gene$ensembl_gene_id == "ENSG00000000419", ]
#' p <- trackplot(peakfile = peakfile, 1, "Homo")
#' box_plot(peakfile, gene, select_peak = 1, F)
box_plot <- function(peakfile, gene, select_peak, plotly) {
  df <- peakfile[, c(-1:-3)]
  t_df <- t(df)
  dt_df <- data.frame(t_df)
  sort_df <- dt_df[order(dt_df[, select_peak]), , drop = FALSE]
  v1 <- sort_df[c(1:(nrow(sort_df) %/% 5)), , drop = FALSE]
  v2 <- sort_df[c(((nrow(sort_df) %/% 5) + 1):(2 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v3 <- sort_df[c((2 * (nrow(sort_df) %/% 5) + 1):(3 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v4 <- sort_df[c((3 * (nrow(sort_df) %/% 5) + 1):(4 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v5 <- sort_df[c((4 * (nrow(sort_df) %/% 5) + 1):nrow(sort_df)), , drop = FALSE]
  m1 <- colMeans(v1)
  m2 <- colMeans(v2)
  m3 <- colMeans(v3)
  m4 <- colMeans(v4)
  m5 <- colMeans(v5)
  data <- data.frame(group1 = m1, group2 = m2, group3 = m3, group4 = m4, group5 = m5)

  group1 <- subset(gene, select = colnames(gene) %in% rownames(v1)) %>%
    t() %>%
    data.frame(group = "group1")
  group2 <- subset(gene, select = colnames(gene) %in% rownames(v2)) %>%
    t() %>%
    data.frame(group = "group2")
  group3 <- subset(gene, select = colnames(gene) %in% rownames(v3)) %>%
    t() %>%
    data.frame(group = "group3")
  group4 <- subset(gene, select = colnames(gene) %in% rownames(v4)) %>%
    t() %>%
    data.frame(group = "group4")
  group5 <- subset(gene, select = colnames(gene) %in% rownames(v5)) %>%
    t() %>%
    data.frame(group = "group5")
  gene_cluster_data <- rbind(group1, group2, group3, group4, group5)
  names(gene_cluster_data)[1] <- "gene"
  # b <- boxplot(boxwex=0.125,axes=FALSE,group5$., group4$., group3$., group2$., group1$., names = c("group5", "group4", "group3", "group2", "group1"),horizontal=TRUE)
  print(gene_cluster_data)
  sample <- rownames(gene_cluster_data)
  if (plotly == TRUE) {
    b <- ggplot2::ggplot(gene_cluster_data, ggplot2::aes(x = group, y = gene, fill = group, color = group)) +
      ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.2), width = 0.4, alpha = 0.4) +
      ggplot2::geom_jitter() +
      ggplot2::labs(
        x = "Group name",
        y = "RNA-seq",
        title = paste0(gene[, 1], "\n", peakfile[select_peak, ]$chrom, ":", peakfile[select_peak, ]$chromStart, "-", peakfile[select_peak, ]$chromEnd)
        # subtitle = paste0()
      ) +
      ggplot2::theme_set(ggpubr::theme_pubr())
    b <- plotly::style(b, text = paste0("sample:", rownames(gene_cluster_data), "\n", "gene:", gene_cluster_data$gene))
  }
  if (plotly == FALSE) {
    b <- ggplot2::ggplot(gene_cluster_data, ggplot2::aes(x = group, y = gene, fill = group, color = group)) +
      ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.2), width = 0.4, alpha = 0.4) +
      ggplot2::geom_jitter() +
      ggplot2::labs(
        x = "Group name",
        y = "RNA-seq",
        title = paste0(gene[, 1], "\n", peakfile[select_peak, ]$chrom, ":", peakfile[select_peak, ]$chromStart, "-", peakfile[select_peak, ]$chromEnd)
        # subtitle = paste0()
      ) +
      ggplot2::theme_set(ggpubr::theme_pubr())
  }
  return(b)
}
