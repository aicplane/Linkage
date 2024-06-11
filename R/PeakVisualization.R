#' TrackPlot
#'
#' @param LinkageObject An Linkage Object after RegulatoryPeak.
#' @param Geneid The gene you want to query.
#' @param peakid The peak you want to query.
#' @param Species Select the species, Homo or Mus.The default is Homo.
#'
#' @return TrackPlot.
#' @export
#' @importFrom dplyr select
#' @importFrom GenomicRanges makeGRangesFromDataFrame values<- elementMetadata
#' @importFrom Gviz GenomeAxisTrack DataTrack IdeogramTrack HighlightTrack plotTracks
#' @importFrom GenomeInfoDb genome seqnames
#' @importFrom ggplotify grid2grob as.ggplot
#' @importFrom ggpubr theme_pubr
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(LinkageR)
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#'   LinkageObject <-
#' RegulatoryPeak(
#'   LinkageObject = SmallLinkageObject,
#'   gene_list = gene_list,
#'   genelist_idtype = "external_gene_name"
#' )
#' TrackPlot(
#'   LinkageObject,
#'   Geneid = "TSPAN6",
#'   peakid = "chrX:100635908-100636408",
#'   Species = "Homo"
#' )
TrackPlot <- function(LinkageObject,Geneid,peakid,Species = "Homo"){

  index <- which(LinkageObject@active.gene==Geneid, arr.ind = TRUE) # row col
  genefile <- LinkageObject@active.gene[index[1],]
  peakfile <- LinkageObject@cor.peak[[Geneid]]
  peakfile$peakRange <- paste0(peakfile[[1]],":",peakfile[[2]],"-",peakfile[[3]])
  select_peak <- which(peakfile==peakid, arr.ind = TRUE) # row col
  peakfile <- select(peakfile,-"p_value", -"FDR",-"rho",-"peakRange")
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
  gr <- makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
  values(gr) <- data
  # print(gr)
  ax <- GenomeAxisTrack()

  tracks_list <- list()
  if (Species == "Homo") {
    gen <- "hg38"
  } else {
    gen <- "mm10"
  }

  for (i in 1:length(names(elementMetadata(gr)))) {
    # track_name <- paste("track", i, sep = "")
    tracks_list[[i]] <- DataTrack(
      gr[, names(elementMetadata(gr))[i]],
      genome = gen,
      name = names(elementMetadata(gr))[i],
      type = "histogram",
      ylim = c(-1, 10)
    )
  }

  # genome
  gen <- genome(tracks_list[[i]])
  # Chromosme name
  chr <- as.character(unique(seqnames(tracks_list[[i]])))
  # Ideogram track (take a long time)
  peak <- peakfile[select_peak[1], ]

  tryCatch(
    {
      itrack <- IdeogramTrack(genome = gen, chromosome = chr)
      # 突出显示某一区域
      ht <- HighlightTrack(tracks_list,
                                 start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      x <<- as.ggplot(grid2grob(plotTracks(list(ht, ax, itrack), type = "histogram", col = NULL)))
    },
    error = function(e) {
      message(e)
      # 突出显示某一区域
      ht <- HighlightTrack(tracks_list,
                                 start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      x <<- as.ggplot(grid2grob(plotTracks(list(ht, ax), type = "histogram", col = NULL)))
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
  y <- ggplot(gene_cluster_data, aes(x = gene, y = group)) +
    geom_boxplot(position = position_dodge(width = 0.1), width = 0.2, alpha = 0.6,fill="grey",color="#666666") +
    # geom_jitter() +
    theme_set(theme_pubr())+theme_classic()+
    theme(
      axis.title.x = element_text(colour = "#666666"),
      axis.line.y = element_blank(),
      # axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      # axis.title.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 10,colour = "#666666",hjust = 0.5)
    )+labs(title = Geneid);y
  y <- grid2grob(print(y))
  x <- x+labs(title = peakid)+theme(plot.title = element_text(size = 10,colour = "#666666",hjust = 0.5))
  y <- y
  p <- plot_grid(x,y,ncol = 2,rel_widths = c(5,1))
  return(p)
}

