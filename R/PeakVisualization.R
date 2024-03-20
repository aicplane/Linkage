#' Plot Trackplot.Users initially select a regulatory peak that obtained from the Regulatory Peaks Detection Module. Linkage then categorizes samples into five groups based on the quantitative chromatin accessibility of the specific regulatory peak, ranging from low to high for each individual sample. The coverage track of mapped ATAC-seq reads and the boxplot of the target gene for each group will be shown simultaneously.
#'
#' @param LinkageObject An Linkage Object after regulatory_peak.
#' @param Geneid The gene you want to query.
#' @param peakid The peak you want to query.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#'
#' @return Trackplot.
#' @export
#'
#' @examples
#' data("LinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' Trackplot(LinkageObject,Geneid = "TSPAN6",peakid = "chrX:100635908-100636408",Species = "Homo")
Trackplot <- function(LinkageObject,Geneid,peakid,Species = "Homo"){

  index <- which(LinkageObject@active.gene==Geneid, arr.ind = TRUE) # row col
  genefile <- LinkageObject@active.gene[index[1],]
  peakfile <- LinkageObject@cor.peak[[Geneid]]
  peakfile$peakRange <- paste0(peakfile[[1]],":",peakfile[[2]],"-",peakfile[[3]])
  select_peak <- which(peakfile==peakid, arr.ind = TRUE) # row col
  peakfile <- dplyr::select(peakfile,-p_value, -FDR,-rho,-peakRange)
  df <- peakfile[, c(-1:-3)]
  t_df <- t(df)
  dt_df <- data.frame(t_df)
  sort_df <- dt_df[order(dt_df[, select_peak[1]]), , drop = FALSE]
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
  if (Species == "Homo") {
    gen <- "hg38"
  } else {
    gen <- "mm10"
  }

  for (i in 1:length(names(GenomicRanges::elementMetadata(gr)))) {
    # track_name <- paste("track", i, sep = "")
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
  peak <- peakfile[select_peak[1], ]

  tryCatch(
    {
      itrack <- Gviz::IdeogramTrack(genome = gen, chromosome = chr)
      # 突出显示某一区域
      ht <- Gviz::HighlightTrack(tracks_list,
                                 start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      x <- ggplotify::as.ggplot(ggplotify::grid2grob(Gviz::plotTracks(list(ht, ax, itrack), type = "histogram", col = NULL)))
    },
    error = function(e) {
      # 突出显示某一区域
      ht <- Gviz::HighlightTrack(tracks_list,
                                 start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      x <- ggplotify::as.ggplot(ggplotify::grid2grob(Gviz::plotTracks(list(ht, ax), type = "histogram", col = NULL)))
    }
  )

  data <- data.frame(group1 = m1, group2 = m2, group3 = m3, group4 = m4, group5 = m5)

  group1 <- subset(genefile, select = colnames(genefile) %in% rownames(v1)) %>%
    t() %>%
    data.frame(group = "group1")
  group2 <- subset(genefile, select = colnames(genefile) %in% rownames(v2)) %>%
    t() %>%
    data.frame(group = "group2")
  group3 <- subset(genefile, select = colnames(genefile) %in% rownames(v3)) %>%
    t() %>%
    data.frame(group = "group3")
  group4 <- subset(genefile, select = colnames(genefile) %in% rownames(v4)) %>%
    t() %>%
    data.frame(group = "group4")
  group5 <- subset(genefile, select = colnames(genefile) %in% rownames(v5)) %>%
    t() %>%
    data.frame(group = "group5")
  gene_cluster_data <- rbind(group1, group2, group3, group4, group5)
  names(gene_cluster_data)[1] <- "gene"
  # b <- boxplot(boxwex=0.125,axes=FALSE,group5$., group4$., group3$., group2$., group1$., names = c("group5", "group4", "group3", "group2", "group1"),horizontal=TRUE)
  # print(gene_cluster_data)
  sample <- rownames(gene_cluster_data)
  gene_cluster_data$group <- factor(gene_cluster_data$group,c("group5","group4","group3","group2","group1"))
  y <- ggplot2::ggplot(gene_cluster_data, ggplot2::aes(x = gene, y = group)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.1), width = 0.2, alpha = 0.6,fill="grey",color="#666666") +
    # ggplot2::geom_jitter() +
    ggplot2::theme_set(ggpubr::theme_pubr())+ggplot2::theme_classic()+
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(colour = "#666666"),
      axis.line.y = ggplot2::element_blank(),
      # axis.text.x = element_blank(),
      axis.text.y = ggplot2::element_blank(),
      # axis.ticks.x = element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 10,colour = "#666666",hjust = 0.5)
    )+ggplot2::labs(title = Geneid);y
  y <- ggplotify::grid2grob(print(y))
  x <- x+ggplot2::labs(title = peakid)+ggplot2::theme(plot.title = ggplot2::element_text(size = 10,colour = "#666666",hjust = 0.5))
  y <- y
  p <- cowplot::plot_grid(x,y,ncol = 2,rel_widths = c(5,1))
  return(p)
}

