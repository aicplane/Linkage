regulatory_peak <- function(RNA.seq,ATAC.seq,range,cor_method){
  result_peak <- list()
  # colnames(RNA.seq)[2:4] <- c("chromosome_name","start_position","end_position")
  # colnames(ATAC.seq)[1:3] <- c("chrom","chromStart","chromEnd")
  for (i in 1:nrow(RNA.seq)) {
    RNA_row <- RNA.seq[i,]
    result_peak[[i]] <-
      ATAC.seq[RNA_row$chromosome_name == ATAC.seq$chrom &
                 ATAC.seq$chromStart > RNA_row$start_position - range &
                 ATAC.seq$chromEnd < RNA_row$end_position + range, ]
    p <- c()
    r <- c()
    for (j in 1:nrow(result_peak[[i]])) {
      df <- rbind(result_peak[[i]][j, c(-1:-3)],
                  RNA_row[, c(-1:-6)]) # 构造新的数据框
      tdf <- t(df)
      ftdf <- data.frame(tdf) # 转置数据框

      tryCatch({
        if (cor_method == "1") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
        }
        if (cor_method == "2") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$p.value
        }
        if (cor_method == "3") {
          p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
        }
      },
      error = function(e) {
        message("发生了错误: ", conditionMessage(e))
        p[j] <- NULL
      })
      tryCatch({
        if (cor_method == "1") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
        }
        if (cor_method == "2") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$estimate
        }
        if (cor_method == "3") {
          r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
        }
      },
      error = function(e) {
        message("发生了错误: ", conditionMessage(e))
        r[j] <- NULL
      })
    }
    result_peak[[i]]$p_value <- p
    result_peak[[i]]$FDR <- p.adjust(p, method = "BH")
    result_peak[[i]]$rho <- r
  }
  return(result_peak)
}
# RNA.seq <- data.table::fread("inst/extdata/TCGA-BRCA-RNA.txt",header = T)[c(1:10),]
# position <- data.table::fread("inst/extdata/homo.gene_positions.plus.txt",header = T,sep = "\t")
# RNA.seq <- merge(position,RNA.seq,by.x = "ensembl_gene_id",by.y = "ensembl_gene_id")
# ATAC.seq <- data.table::fread("inst/extdata/TCGA-BRCA-ATAC.txt",header = T)
# regulatory_peak(RNA.seq,ATAC.seq,500000,"spearman")

