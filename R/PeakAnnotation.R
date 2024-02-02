# 模块三 ------------------------------------------------------------------
observeEvent(input$Annote, {
  if (nrow(select_ATAC()) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "This gene didn't find a peak with high correlation and couldn't be annotated!",
      type = "error"
    )
    return()
  }
  if (nrow(select_ATAC()) > 0) {
    progressSweetAlert(
      session = session,
      id = "simulationProgress1",
      title = "Commenting...",
      display_pct = TRUE,
      value = 20
    )
    
    peakfile <-
      select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
    # select_peak <- req(input$ATAC2_rows_selected)
    gr <-
      makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
    if (input$Species == "1") {
      txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
      annoDb <- "org.Hs.eg.db"
    } else{
      txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene
      annoDb <- "org.Mm.eg.db"
    }
    
    peakAnno <<- ChIPseeker::annotatePeak(
      gr,
      tssRegion = c(-3000, 3000),
      TxDb = txdb,
      annoDb = annoDb
    )
    Annotation_table <<- data.frame(peakAnno)
    
    updateProgressBar(
      session = session,
      id = "simulationProgress1",
      title = "Commenting...",
      value = 50
    )
    
    # peakfile <-
    #   select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
    # gr <-
    #   makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
    # txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
    # peakAnno <- ChIPseeker::annotatePeak(
    #   gr,
    #   tssRegion = c(-3000, 3000),
    #   TxDb = txdb,
    #   annoDb = "org.Hs.eg.db"
    # )
    p <- ChIPseeker::upsetplot(peakAnno, vennpie = TRUE)
    
    output$Peak_Annotation <-
      DT::renderDataTable(
        Annotation_table,
        selection = "none",
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 260,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F
        )
      )
    
    output$displot4 <- renderPlot({
      p
    })
    
    
    updateProgressBar(
      session = session,
      id = "simulationProgress1",
      title = "Commenting...",
      value = 100
    )
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "DONE",
      text = "The annotation is complete!",
      type = "success"
    )
  }
})
