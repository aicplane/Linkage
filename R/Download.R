# 下载 --------------------------------------------------------------------
output$tableDown <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1], ".", input$extTable)
  },
  content = function(file) {
    if (input$extTable == "csv") {
      write.csv(select_ATAC(),
                file,
                row.names = F,
                quote = F)
    } else {
      write.table(
        select_ATAC(),
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)
output$plotDown <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1],
           "_",
           input$ATAC_rows_selected,
           ".",
           input$extPlot)
  },
  content = function(file) {
    if (input$extPlot == "pdf") {
      pdf(file)
    } else if (input$extPlot == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(fig)
    dev.off()
  }
)
output$plotDown2 <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1],
           "_",
           input$ATAC2_rows_selected,
           "trackplot.",
           input$extPlot2)
  },
  content = function(file) {
    if (input$extPlot2 == "pdf") {
      pdf(file)
    } else if (input$extPlot2 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    peakfile <-
      select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
    select_peak <- req(input$ATAC2_rows_selected)
    df <- peakfile[, c(-1:-3)]
    t_df <- t(df)
    dt_df <- data.frame(t_df)
    sort_df <- dt_df[order(dt_df[, 1]), , drop = FALSE]
    v1 <- sort_df[c(1:(nrow(sort_df) %/% 5)), , drop = FALSE]
    v2 <-
      sort_df[c(((nrow(sort_df) %/% 5) + 1):(2 * (nrow(sort_df) %/% 5))), , drop = FALSE]
    v3 <-
      sort_df[c((2 * (nrow(sort_df) %/% 5) + 1):(3 * (nrow(sort_df) %/% 5))), , drop = FALSE]
    v4 <-
      sort_df[c((3 * (nrow(sort_df) %/% 5) + 1):(4 * (nrow(sort_df) %/% 5))), , drop = FALSE]
    v5 <-
      sort_df[c((4 * (nrow(sort_df) %/% 5) + 1):nrow(sort_df)), , drop = FALSE]
    m1 <- colMeans(v1)
    m2 <- colMeans(v2)
    m3 <- colMeans(v3)
    m4 <- colMeans(v4)
    m5 <- colMeans(v5)
    data <-
      data.frame(
        group1 = m1,
        group2 = m2,
        group3 = m3,
        group4 = m4,
        group5 = m5
      )
    gr <-
      makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
    values(gr) <- data
    
    ax <- GenomeAxisTrack()
    
    tracks_list <- list()
    for (i in 1:length(names(elementMetadata(gr)))) {
      track_name <- paste("track", i, sep = "")
      tracks_list[[i]] <- DataTrack(
        gr[, names(elementMetadata(gr))[i]],
        genome = "hg38",
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
    peak <- peakfile[select_peak,]
    tryCatch({
      itrack <- IdeogramTrack(genome = gen, chromosome = chr)
      # 突出显示某一区域
      ht <- HighlightTrack(
        tracks_list,
        start = peak$chromStart,
        width = as.numeric(peak$chromEnd - peak$chromStart),
        chromosome = substring(peak[, 1], 4)
      )
      plotTracks(list(ht, ax, itrack), type = "histogram", col = NULL)
    },
    error = function(e) {
      # 突出显示某一区域
      ht <- HighlightTrack(
        tracks_list,
        start = peak$chromStart,
        width = as.numeric(peak$chromEnd - peak$chromStart),
        chromosome = substring(peak[, 1], 4)
      )
      plotTracks(list(ht, ax), type = "histogram", col = NULL)
    })
    dev.off()
  }
)
output$plotDown3 <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1],
           "_",
           input$ATAC_rows_selected,
           "boxplot.",
           input$extPlot3)
  },
  content = function(file) {
    if (input$extPlot3 == "pdf") {
      pdf(file)
    } else if (input$extPlot3 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(b)
    dev.off()
  }
)
output$plotDown4 <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1],
           "_",
           input$ATAC_rows_selected,
           "upsetplot.",
           input$extPlot4)
  },
  content = function(file) {
    if (input$extPlot4 == "pdf") {
      pdf(file)
    } else if (input$extPlot4 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(ChIPseeker::upsetplot(peakAnno, vennpie = TRUE))
    dev.off()
  }
)
output$Download_Annotation <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1], "_PeakAnnotation.", input$extTable2)
  },
  content = function(file) {
    if (input$extTable2 == "csv") {
      write.csv(Annotation_table,
                row.names = F,
                quote = F,
                file)
    } else {
      write.table(
        Annotation_table,
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)
output$Download_Motif <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1], "_Motif.", input$extTable3)
  },
  content = function(file) {
    if (input$extTable3 == "csv") {
      write.csv(select_motif(),
                row.names = F,
                quote = F,
                file)
    } else {
      write.table(
        select_motif(),
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)
output$plotDown5 <- downloadHandler(
  filename = function() {
    paste0(output_gene()[, 1],
           "_",
           input$Motif_rows_selected,
           "seqLogo.",
           input$extPlot5)
  },
  content = function(file) {
    if (input$extPlot5 == "pdf") {
      pdf(file)
    } else if (input$extPlot5 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    motif <- select_motif()
    select_row <- req(input$Motif_rows_selected)
    select_motif <- motif[select_row,]
    m <-
      TFBSTools::getMatrixByID('JASPAR2022.sqlite', select_motif$ID)
    seqLogo(toICM(m))
    dev.off()
  }
)

output$plotDown6 <- downloadHandler(
  filename = function() {
    paste0("GO_dotplot.",
           input$extPlot6)
  },
  content = function(file) {
    if (input$extPlot6 == "pdf") {
      pdf(file)
    } else if (input$extPlot6 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(
      dotplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          str_wrap(x, width = 30)
      )
    )
    dev.off()
  }
)

output$plotDown7 <- downloadHandler(
  filename = function() {
    paste0("GO_barplot.",
           input$extPlot7)
  },
  content = function(file) {
    if (input$extPlot7 == "pdf") {
      pdf(file)
    } else if (input$extPlot7 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(
      barplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          str_wrap(x, width = 30)
      )
    )
    dev.off()
  }
)

output$plotDown8 <- downloadHandler(
  filename = function() {
    paste0("GO_upsetplot.",
           input$extPlot8)
  },
  content = function(file) {
    if (input$extPlot8 == "pdf") {
      pdf(file)
    } else if (input$extPlot8 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(upsetplot(go))
    dev.off()
  }
)

output$plotDown13 <- downloadHandler(
  filename = function() {
    paste0("GO_cnetplot.",
           input$extPlot13)
  },
  content = function(file) {
    if (input$extPlot13 == "pdf") {
      pdf(file)
    } else if (input$extPlot13 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(cnetplot(go))
    dev.off()
  }
)

output$plotDown10 <- downloadHandler(
  filename = function() {
    paste0("KEGG_dotplot.",
           input$extPlot10)
  },
  content = function(file) {
    if (input$extPlot10 == "pdf") {
      pdf(file)
    } else if (input$extPlot10 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(
      dotplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          str_wrap(x, width = 30)
      )
    )
    dev.off()
  }
)

output$plotDown11 <- downloadHandler(
  filename = function() {
    paste0("KEGG_barplot.",
           input$extPlot11)
  },
  content = function(file) {
    if (input$extPlot11 == "pdf") {
      pdf(file)
    } else if (input$extPlot11 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(
      barplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          str_wrap(x, width = 30)
      )
    )
    dev.off()
  }
)

output$plotDown12 <- downloadHandler(
  filename = function() {
    paste0("KEGG_upsetplot.",
           input$extPlot12)
  },
  content = function(file) {
    if (input$extPlot12 == "pdf") {
      pdf(file)
    } else if (input$extPlot12 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(upsetplot(kegg))
    dev.off()
  }
)

output$plotDown14 <- downloadHandler(
  filename = function() {
    paste0("KEGG_cnetplot.",
           input$extPlot14)
  },
  content = function(file) {
    if (input$extPlot14 == "pdf") {
      pdf(file)
    } else if (input$extPlot14 == "png") {
      png(file)
    } else {
      jpeg(file)
    }
    plot(cnetplot(kegg))
    dev.off()
  }
)
output$Download_GO <- downloadHandler(
  filename = function() {
    paste0("GO_result.", input$extTable5)
  },
  content = function(file) {
    if (input$extTable5 == "csv") {
      write.csv(go@result,
                row.names = F,
                quote = F,
                file)
    } else {
      write.table(
        go@result,
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)
output$Download_KEGG <- downloadHandler(
  filename = function() {
    paste0("KEGG_result.", input$extTable6)
  },
  content = function(file) {
    if (input$extTable6 == "csv") {
      write.csv(kegg@result,
                row.names = F,
                quote = F,
                file)
    } else {
      write.table(
        kegg@result,
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)

output$Download_vis <- downloadHandler(
  filename = function() {
    paste0("Network_table.", input$extTable4)
  },
  content = function(file) {
    if (input$extTable4 == "csv") {
      write.csv(BuildNetwork(),
                row.names = F,
                quote = F,
                file)
    } else {
      write.table(
        BuildNetwork(),
        file,
        sep = "\t",
        col.names = T,
        row.names = F,
        quote = F
      )
    }
  }
)