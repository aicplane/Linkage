#' Finding the regulatory peak of genes
#'
#' @param RNA.seq Expression of a single or multiple genes
#' @param ATAC.seq ATAC-Seq Expression Matrix
#' @param range Expand the range of genes to find regulatory peaks
#' @param cor_method For correlation calculation, pearson/spearman/kendall can be selected
#'
#' @return Regulate the peak list
#' @export
#'
#' @examples
#' RNA.seq.dir <- system.file("extdata","TCGA-BRCA-RNA.txt", package = "Linkage")
#' RNA.seq <- data.table::fread(RNA.seq.dir, header = T)[c(1:10), ]
#' position.dir <- system.file("extdata","homo.gene_positions.plus.txt", package = "Linkage")
#' position <- data.table::fread(position.dir, header = T, sep = "\t")
#' RNA.seq <- merge(position, RNA.seq, by.x = "ensembl_gene_id", by.y = "ensembl_gene_id")
#' Homo.list.files.dir <- system.file("extdata","Homo.ATAC", package = "Linkage")
#' Homo.list.files <- list.files(Homo.list.files.dir)
#' Homo.list.files <- paste0(Homo.list.files.dir,"/", Homo.list.files)
#' Homo.df_list <- lapply(Homo.list.files, function(file) data.table::fread(file, header = T))
#' ATAC.seq <- do.call(rbind, Homo.df_list)
#' regulatory_peak(RNA.seq, ATAC.seq, 500000, "spearman")
regulatory_peak <- function(RNA.seq, ATAC.seq, range, cor_method) {
  result_peak <- list()
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
      ) # 构造新的数据框
      tdf <- t(df)
      ftdf <- data.frame(tdf) # 转置数据框

      tryCatch(
        {
          if (cor_method == "1") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
          }
          if (cor_method == "2") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman", exact = FALSE)$p.value
          }
          if (cor_method == "3") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          p[j] <- NULL
        }
      )
      tryCatch(
        {
          if (cor_method == "1") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
          }
          if (cor_method == "2") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman", exact = FALSE)$estimate
          }
          if (cor_method == "3") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          r[j] <- NULL
        }
      )
    }
    result_peak[[i]]$p_value <- p
    result_peak[[i]]$FDR <- p.adjust(p, method = "BH")
    result_peak[[i]]$rho <- r
  }
  return(result_peak)
}
