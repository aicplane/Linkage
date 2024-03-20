library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(RColorBrewer)
library(rvest)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(DT)
library(dplyr)
library(Gviz)
library(ChIPseeker)
library(TxDb.Hsapiens.UCSC.hg38.knownGene)
library(TxDb.Mmusculus.UCSC.mm10.knownGene)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(BSgenome.Hsapiens.UCSC.hg38) # 加载人类基因组HG38版的索引信息
library(BSgenome.Mmusculus.UCSC.mm10)
library(TFBSTools)
#library(JASPAR2022)
library(motifmatchr)
library(Biostrings)
library(igraph)
library(tidyverse)
library(visNetwork)
library(stringr)
library(clusterProfiler)
library(enrichplot)
library(plotly)
library(wordcloud2)
library(fresh)

color_from_middle <- function (data, color1,color2)
{
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
}


Blues <- c("#F7FBFF", "#F1F7FD", "#ECF4FB", "#E7F1FA", "#E2EDF8", "#DDEAF6", "#D8E7F5", "#D3E4F3", "#CEE0F1", "#C9DDF0", "#C3DAEE", "#BBD6EB", "#B3D3E8",
      "#ABCFE5", "#A3CCE2", "#9AC7E0", "#8FC2DD", "#85BCDB", "#7AB6D9", "#70B0D7", "#66ABD4", "#5EA5D1", "#559FCD", "#4D99CA", "#4594C7", "#3D8DC3",
      "#3787C0", "#3080BC", "#2979B9", "#2272B5", "#1D6CB1", "#1865AC", "#125EA6", "#0D58A1", "#08519C", "#084B93" ,"#084489", "#083D7F", "#083675",
      "#08306B")

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "230px",
    dark_bg = "#2E3440",
    # dark_hover_bg = "#434C5E",
    dark_hover_color = "#FFF",
    dark_color = "#E6E7E8"
  ),
  adminlte_global(
    #content_bg = "#FFF",
    # box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

options(encoding = "UTF-8")
options(shiny.maxRequestSize = 100000 * 1024 ^ 2) #### 上传文件的大小
theme_set(ggpubr::theme_pubr() +
            theme(legend.position = "top")) #### 设置散点图主题样式

homo.gene.positions <- fread("../extdata/homo.gene_positions.plus.txt",header = T,sep = "\t")
mus.gene.positions <- fread("../extdata/mus.gene_positions.plus.txt",header = T,sep = "\t")
#gene_name <- fread("gene_name.csv", header = TRUE)

mouse.list.files <- list.files("../extdata/Mus.ATAC/")
mouse.list.files <- paste0("../extdata/Mus.ATAC/",mouse.list.files)
mouse.df_list <- lapply(mouse.list.files,function(file) fread(file,header = T))
mouse.ATAC_matrix <- do.call(rbind, mouse.df_list)

Homo.list.files <- list.files("../extdata/Homo.ATAC/")
Homo.list.files <- paste0("../extdata/Homo.ATAC/",Homo.list.files)
Homo.df_list <- lapply(Homo.list.files,function(file) fread(file,header = T))
ATAC_matrix <- do.call(rbind, Homo.df_list)

RNA_matrix <- fread("../extdata/TCGA-BRCA-RNA.txt", header = TRUE)
# ATAC_matrix <- fread("../extdata/TCGA-BRCA-ATAC.txt", header = TRUE)
mouse.RNA_matrix <- fread("../extdata/mouse.normalize.rna.txt",header = TRUE)
# mouse.ATAC_matrix <- fread("../extdata/mouse.normalize.peak.txt",header = TRUE)
# mouse.TF <- fread("homo-mmu.TF.txt",header = T,sep = '\t')
# RNA_data <- NULL
# ATAC_data <- NULL

cor_test <- function(ATAC2, gene, method, Filter_col, Filter_value) {
  p <- c()
  r <- c()
  for (i in 1:nrow(ATAC2)) {
    local({
      df <- rbind(
        gene[1, c(-1:-6)],
        ATAC2[i, c(-1:-3)]
      ) # 构造新的数据框,gene列为用户查询的基因样本信息;peak列为初步查找到的调控序列
      #print(df)
      tdf <- t(df)
      ftdf <- data.frame(tdf) # 转置数据框
      p[i] <<- tryCatch({
        if(method == "spearman"){
          p[i] <- cor.test(ftdf[, 1], ftdf[, 2], method = method,exact=FALSE)$p.value
        }else{
          p[i] <- cor.test(ftdf[, 1], ftdf[, 2], method = method)$p.value
        }
      },
      error = function(e) {
        # 突出显示某一区域
        message("发生了错误: ", conditionMessage(e))
        p[i] <- NULL
      })
      r[i] <<- tryCatch({
        if(method == "spearman"){
          r[i] <- cor.test(ftdf[, 1], ftdf[, 2], method = method,exact=FALSE)$estimate
        }else{
          r[i] <- cor.test(ftdf[, 1], ftdf[, 2], method = method)$estimate
        }
      },
      error = function(e) {
        # 突出显示某一区域
        message("发生了错误: ", conditionMessage(e))
        r[i] <- NULL
      })
    })
  }
  FDR <- p.adjust(p, method = "BH")
  ATAC2$p_value <- p
  ATAC2$FDR <- FDR
  ATAC2$rho <- r
  if (Filter_col == "FDR") {
    ATAC3 <<- ATAC2[ATAC2$FDR <= Filter_value, ]
  }
  if (Filter_col == "rho") {
    ATAC3 <<- ATAC2[abs(ATAC2$rho) >= Filter_value, ]
  }
  if (Filter_col == "p_value") {
    ATAC3 <<- ATAC2[ATAC2$p_value <= Filter_value, ]
  }
  return(ATAC3)
}

lzh_plot <- function(gene, click_ATAT, method, Filter_col, Filter_value) {
  # p <- c()
  # for (i in 1:nrow(ATAC2)) {
  #   local({
  #     df <- rbind(
  #       gene = transform_gene[1, c(-1:-4)],
  #       peak = ATAC2[i, c(-1:-3)]
  #     ) # 构造新的数据框,gene列为用户查询的基因样本信息;peak列为初步查找到的调控序列
  #     tdf <- t(df)
  #     ftdf <- data.frame(tdf) # 转置数据框
  #     cortt <- cor.test(ftdf[, 1], ftdf[, 2], method = method)
  #     p[i] <<- cortt$p.value # 计算p值
  #   })
  # }

  # FDR <- p.adjust(p, method = "BH")
  # ATAC2$p_value <- p
  # ATAC2$FDR <- FDR
  # if (Filter_col == "FDR") {
  #   ATAC3 <- ATAC2[ATAC2$FDR <= Filter_value, ]
  # }
  # if (Filter_col == "rho") {
  #   ATAC3 <- ATAC2[ATAC2$rho >= Filter_value, ]
  # }
  # if (Filter_col == "p_value") {
  #   ATAC3 <- ATAC2[ATAC2$p_value <= Filter_value, ]
  # }
  ATAC4 <- dplyr::select(ATAC3, -p_value, -FDR,-rho)
  ATAC5 <- ATAC4[, c(-1:-3)]
  ATAC6 <- ATAC5[click_ATAT, ]
  plot_data <- rbind(gene = gene[, c(-1:-6)], peak = ATAC6)
  t_plot_data <- t(plot_data)
  ft_plot_data <- data.frame(t_plot_data)

  FDR <- signif(ATAC3[click_ATAT, ]$FDR,digits = 3)
  p_value <- signif(ATAC3[click_ATAT, ]$p_value,digits = 3)
  rho <- signif(ATAC3[click_ATAT, ]$rho,digits = 3)

  sort_df <- ft_plot_data[order(ft_plot_data[, 2]), , drop = FALSE]
  v1 <- sort_df[c(1:(nrow(sort_df) %/% 5)), , drop = FALSE]
  v2 <- sort_df[c(((nrow(sort_df) %/% 5) + 1):(2 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v3 <- sort_df[c((2 * (nrow(sort_df) %/% 5) + 1):(3 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v4 <- sort_df[c((3 * (nrow(sort_df) %/% 5) + 1):(4 * (nrow(sort_df) %/% 5))), , drop = FALSE]
  v5 <- sort_df[c((4 * (nrow(sort_df) %/% 5) + 1):nrow(sort_df)), , drop = FALSE]

  group1 <- ft_plot_data[rownames(ft_plot_data) %in% rownames(v1),] %>% data.frame(group = "group1")

  group2 <- ft_plot_data[rownames(ft_plot_data) %in% rownames(v2),] %>% data.frame(group = "group2")

  group3 <- ft_plot_data[rownames(ft_plot_data) %in% rownames(v3),] %>% data.frame(group = "group3")

  group4 <- ft_plot_data[rownames(ft_plot_data) %in% rownames(v4),] %>% data.frame(group = "group4")

  group5 <- ft_plot_data[rownames(ft_plot_data) %in% rownames(v5),] %>% data.frame(group = "group5")

  gene_cluster_data <- rbind(group1, group2, group3, group4, group5)

  sample <- rownames(gene_cluster_data)

  fig <<- ggplot(gene_cluster_data, aes(x = gene, y = peak)) +
    geom_point() +
    geom_smooth(method = "lm", color = "black", fill = "lightgray") +
    labs(
      x = "RNA-seq", y = "ATAC-seq",
      title = paste0(gene[,1],
                     "\n",
                     ATAC4[click_ATAT,]$chrom,
                     ":",
                     ATAC4[click_ATAT,]$chromStart,
                     "-",
                     ATAC4[click_ATAT,]$chromEnd,
                     "\nFDR = ",FDR," ","p_value = ",p_value," ","rho = ",rho
      )) +
    theme(
        plot.title = element_text(size=10),legend.position = "top")

  # fig <- ggplotly(fig)

  fig <- style(fig , text = paste0("sample:",rownames(gene_cluster_data),"\n","gene:",gene_cluster_data$gene,"\n","peak:",gene_cluster_data$peak))
  return(fig)
}

trackplot <- function(peakfile, select_peak,Species) {
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
  #print(data)
  gr <- makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
  values(gr) <- data
  #print(gr)
  ax <- GenomeAxisTrack()

  tracks_list <- list()
  if(Species == "1"){
    gen <- "hg38"
  }else{
    gen <- "mm10"
  }

  for (i in 1:length(names(elementMetadata(gr)))) {
    track_name <- paste("track", i, sep = "")
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
      ht <- HighlightTrack(tracks_list,
                           start = peak$chromStart, width = as.numeric(peak$chromEnd - peak$chromStart), chromosome = substring(peak[, 1], 4)
      )
      return(plotTracks(list(ht, ax), type = "histogram", col = NULL))
    }
  )
}

box_plot <- function(peakfile, gene, select_peak) {
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
  b <<- ggplot(gene_cluster_data, aes(x = group, y = gene, fill = group,color = group)) +
    geom_boxplot(position=position_dodge(width =0.2),width=0.4,alpha = 0.4) +
    geom_jitter() +
    labs(
      x = "Group name",
      y = "RNA-seq",
      title = paste0(gene[, 1],"\n",peakfile[select_peak, ]$chrom, ":", peakfile[select_peak, ]$chromStart, "-", peakfile[select_peak, ]$chromEnd)
      # subtitle = paste0()
    )
  b <- style(b , text = paste0("sample:",rownames(gene_cluster_data),"\n","gene:",gene_cluster_data$gene))

  return(b)
}

motif_analysis <- function(peakfile, select_peak,Species) {


  # Make a set of peaks
  peaks <- peakfile[select_peak, ]
  peaks <- GRanges(
    seqnames = c(peaks$chrom),
    ranges = IRanges(start = peaks$chromStart, end = peaks$chromEnd)
  )

  if(Species == "1"){
    PFMatrixList <- readRDS("../extdata/PFMatrixList.rds")
    pwm_library_dt <- readRDS("../extdata/pwm_library_dt.rds")
    genome <- "hg38"
  }else{
    PFMatrixList <- readRDS("../extdata/Mus.PFMatrixList.rds")
    pwm_library_dt <- readRDS("../extdata/Mus.pwm_library_dt.rds")
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

seqLogo_plot <- function(motif, select_row) {
  select_motif <- motif[select_row, ]
  #m <- getMatrixByID(JASPAR2022, select_motif$ID)
  m <- TFBSTools::getMatrixByID('../extdata/JASPAR2022.sqlite', select_motif$ID)
  return(seqLogo(toICM(m)))
}

actionBttnParams <- list(
  size = "sm",
  color = "primary",
  style = "fill",
  block = TRUE
)
