#' Finding the regulatory peak of genes.
#'
#' @param LinkageObject An Linkage Object.
#' @param gene_list Character value,The gene you want to query.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param range Expand the range of genes to find regulatory peaks.
#' @param cor_method For correlation calculation, pearson/spearman/kendall can be selected.
#' @param filter_col p_value,FDR,rho can be selected.
#' @param filter_value Cutoff value of filter_col.
#'
#' @return Regulate the peak list
#' @export
#'
#' @examples
#' library(LinkageR)
#' data("SmallLinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = SmallLinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
RegulatoryPeak <- function(LinkageObject, gene_list, genelist_idtype, range = 500000, cor_method = "spearman", filter_col = "FDR", filter_value = 0.05) {
  if (class(LinkageObject) != "LinkageObject") stop("RegulatoryPeak must a LinkageObject")
  n <- 0
  n_peak <- 0
  neg <- 0
  pos <- 0
  result_peak <- list()
  RNA.seq <- subset(LinkageObject@RNA.mtrix, subset = LinkageObject@RNA.mtrix[[genelist_idtype]] %in% gene_list)
  ATAC.seq <- LinkageObject@ATAC.matrix
  for (i in 1:nrow(RNA.seq)) {
    RNA_row <- RNA.seq[i, ]
    result_peak[[i]] <-
      ATAC.seq[RNA_row$chromosome_name == ATAC.seq$chrom &
        ATAC.seq$chromStart > RNA_row$start_position - range &
        ATAC.seq$chromEnd < RNA_row$end_position + range, ]
    p <- c()
    r <- c()
    for (j in 1:nrow(result_peak[[i]])) {
      df <- rbind(
        result_peak[[i]][j, c(-1:-3)],
        RNA_row[, c(-1:-6)]
      )
      tdf <- t(df)
      ftdf <- data.frame(tdf)

      tryCatch(
        {
          if (cor_method == "pearson") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
          }
          if (cor_method == "spearman") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman", exact = FALSE)$p.value
          }
          if (cor_method == "kendall") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
          }
        },
        error = function(e) {
          message("error: ", conditionMessage(e))
          p[j] <- NULL
        }
      )
      tryCatch(
        {
          if (cor_method == "pearson") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
          }
          if (cor_method == "spearman") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman", exact = FALSE)$estimate
          }
          if (cor_method == "kendall") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
          }
        },
        error = function(e) {
          message("error: ", conditionMessage(e))
          r[j] <- NULL
        }
      )
    }
    result_peak[[i]]$p_value <- p
    result_peak[[i]]$FDR <- p.adjust(p, method = "BH")
    result_peak[[i]]$rho <- r
    if (filter_col == "FDR") {
      result_peak[[i]] <- result_peak[[i]][result_peak[[i]]$FDR <= filter_value, ]
    }
    if (filter_col == "rho") {
      result_peak[[i]] <- result_peak[[i]][abs(result_peak[[i]]$rho) >= filter_value, ]
    }
    if (filter_col == "p_value") {
      result_peak[[i]] <- result_peak[[i]][result_peak[[i]]$p_value <= filter_value, ]
    }
    if (nrow(result_peak[[i]]) > 0) {
      pos_peak <- result_peak[[i]][result_peak[[i]]$rho > 0, ]
      neg_peak <- result_peak[[i]][result_peak[[i]]$rho < 0, ]
      n_peak <- n_peak + nrow(result_peak[[i]])
      pos <- pos + nrow(pos_peak)
      neg <- neg + nrow(neg_peak)
      n <- n + 1
    }
  }
  names(result_peak) <- gene_list
  p <-
    new(
      "LinkageObject",
      RNA.mtrix = LinkageObject@RNA.mtrix,
      ATAC.matrix = LinkageObject@ATAC.matrix,
      active.gene = RNA.seq,
      cor.peak = result_peak,
      cor.peak.annotation = LinkageObject@cor.peak.annotation,
      detailpeakannotation = LinkageObject@detailpeakannotation,
      Motif = LinkageObject@Motif,
      Gene_TF = LinkageObject@Gene_TF,
      geneid = gene_list,
      Summary = list(
        "positive_peak" = pos,
        "negetive_peak" = neg,
        "TF_num" = 0,
        "genelist_idtype" = NULL,
        "filter_col" = NULL
      )
    )
  return(p)
}

#' Correlation scatter plot.
#'
#' @param LinkageObject An Linkage Object.
#' @param gene A gene that want to be visualized.
#' @param color The color of the straight line.
#' @param fill The color of the fill.
#'
#' @return Scatterplot of correlation between genes and peaks.
#' @export
#' @importFrom dplyr select
#' @import ggplot2
#'
#' @examples
#' library(LinkageR)
#' data("SmallLinkageObject")
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = SmallLinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' CorrPlot(LinkageObject, gene = "TSPAN6")
CorrPlot <- function(LinkageObject, gene,color = "black",fill = "lightgray") {

  index <- which(LinkageObject@active.gene==gene, arr.ind = TRUE) # row col
  corr.gene <- LinkageObject@active.gene[index[1],]
  corr.peak <- LinkageObject@cor.peak[[gene]]
  plot_list <- list()
  for (i in 1:nrow(corr.peak)) {
    ATAC3 <- corr.peak[i,]
    ATAC4 <- select(ATAC3, -"p_value", -"FDR",-"rho")
    ATAC5 <- ATAC4[, c(-1:-3)]
    plot_data <- rbind(gene = corr.gene[, c(-1:-6)], peak = ATAC5)
    t_plot_data <- t(plot_data)
    ft_plot_data <- data.frame(t_plot_data)

    FDR <- signif(ATAC3$FDR,digits = 3)
    p_value <- signif(ATAC3$p_value,digits = 3)
    rho <- signif(ATAC3$rho,digits = 3)

    plot_list[[i]] <- ggplot(ft_plot_data, aes(x = gene, y = peak)) +
      geom_point() +
      geom_smooth(method = "lm", color = color, fill = fill) +
      labs(
        x = "RNA-seq", y = "ATAC-seq",
        title = (paste0(gene,
                       "\n",
                       ATAC4$chrom,
                       ":",
                       ATAC4$chromStart,
                       "-",
                       ATAC4$chromEnd,
                       "\nFDR = ",FDR," ","p_value = ",p_value," ","rho = ",rho
        ))) + theme_bw() + theme(title = element_text(size = 5))
  }

  p <- CombinePlots(plot_list)

  return(p)
}



#' Combine ggplot2-based plots into a single plot
#'
#' @param plots A list of gg objects
#' @param ncol Number of columns
#' @param legend Combine legends into a single legend
#' choose from 'right' or 'bottom'; pass 'none' to remove legends, or \code{NULL}
#' to leave legends as they are
#' @param ... Extra parameters passed to plot_grid
#'
#' @return A combined plot
#'
#' @importFrom cowplot plot_grid get_legend
#' @concept RegulatoryPeaks
CombinePlots <- function(plots, ncol = NULL, legend = NULL, ...) {
  plots.combined <- if (length(x = plots) > 1) {
    if (!is.null(x = legend)) {
      if (legend != 'none') {
        plot.legend <- get_legend(plot = plots[[1]] + theme(legend.position = legend))
      }
      plots <- lapply(
        X = plots,
        FUN = function(x) {
          return(x + NoLegend())
        }
      )
    }
    plots.combined <- plot_grid(
      plotlist = plots,
      ncol = ncol,
      align = 'hv',
      ...
    )
    if (!is.null(x = legend)) {
      plots.combined <- switch(
        EXPR = legend,
        'bottom' = plot_grid(
          plots.combined,
          plot.legend,
          ncol = 1,
          rel_heights = c(1, 0.2)
        ),
        'right' = plot_grid(
          plots.combined,
          plot.legend,
          rel_widths = c(3, 0.3)
        ),
        plots.combined
      )
    }
    plots.combined
  } else {
    plots[[1]]
  }
  return(plots.combined)
}

