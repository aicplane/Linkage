# 模块二 ------------------------------------------------------------------
# 输出peak选择数据库
observe({
  if (is.null(select_ATAC())) {
    output$ATAC2 <- DT::renderDataTable(NULL)
  } else{
    output$ATAC2 <-
      DT::renderDataTable({
        df <- select_ATAC()[, c(-4:-(ncol(select_ATAC()) - 3))]
        # Create 19 breaks and 20 rgb color values ranging from white to blue
        brks1 <-
          quantile(
            df[, c(6)] %>% select_if(is.numeric),
            probs = seq(.05, .95, .05),
            na.rm = TRUE
          )
        brks2 <-
          quantile(
            df[, c(5)] %>% select_if(is.numeric),
            probs = seq(.05, .95, .05),
            na.rm = TRUE
          )
        brks3 <-
          quantile(
            df[, c(4)] %>% select_if(is.numeric),
            probs = seq(.05, .95, .05),
            na.rm = TRUE
          )
        # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
        #   {paste0("rgb(", .,",", .,",220)")}
        DT::datatable(
          df,
          selection = "single",
          # extensions = c("Scroller", "RowReorder"),
          option = list(
            pageLength = 10,
            pageWidth = 10,
            # autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(
            #   targets = 2, width = "210px"
            # )),
            scrollX = TRUE
          )
        ) %>%
          formatStyle(names(df[, 6]), backgroundColor = styleInterval(brks1, head(Blues, n = length(brks1) + 1))) %>%
          formatStyle(names(df[, 5]), backgroundColor = styleInterval(brks2, Blues[(length(brks3) + 1):1])) %>%
          formatStyle(names(df[, 4]), backgroundColor = styleInterval(brks3, Blues[(length(brks3) + 1):1]))
      })
  }
})



# 轨道图
select_trackplot <- reactive({
  tryCatch({
    peakfile <-
      select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))] %>% data.frame()
    # print(peakfile)
    select_peak <- req(input$ATAC2_rows_selected)
    Species <- input$Species
    return(trackplot(peakfile, select_peak, Species))
  })
})

# 箱线图
select_boxplot <- reactive({
  gene <- output_gene()
  if (input$geneid_method == "ENSEMBL") {
    index <- "ensembl_gene_id"
  }
  if (input$geneid_method == "SYMBOL") {
    index <- "external_gene_name"
  }
  if (input$geneid_method == "ENTREZID") {
    index <- "entrezgene_id"
  }
  if (length(which(duplicated(gene[[index]]))) > 0) {
    gene <-
      gene[-which(duplicated(gene[[index]])),]
  }
  peakfile <-
    select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
  select_peak <- req(input$ATAC2_rows_selected)
  b <- box_plot(peakfile, gene, select_peak)
  return(b)
})

output$displot2 <- renderPlot({
  select_trackplot()
})

# 输出箱线图
output$displot3 <- renderPlotly({
  plotly::ggplotly(select_boxplot())
})
