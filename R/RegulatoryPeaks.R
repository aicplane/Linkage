# 模块一 --------------------------------------------------------

output_gene <- reactive({
  if (is.null(RNA_count()))
    return()
  RNA <- RNA_count()
  
  if (is.null(RNA) == F) {
    if (input$geneid_method == "ENSEMBL") {
      gene <- RNA[RNA$ensembl_gene_id == req(input$geneid),]
    }
    if (input$geneid_method == "SYMBOL") {
      gene <- RNA[RNA$external_gene_name == req(input$geneid),]
    }
    if (input$geneid_method == "ENTREZID") {
      gene <- RNA[RNA$entrezgene_id == req(input$geneid),]
    }
    
    output_gene <- gene
    # print(output_gene)
    return(output_gene)
  }
})

# peak筛选
select_ATAC <- reactive({
  if (is.null(ATAC_count()))
    return()
  tryCatch({
    if (input$bins > 1000000) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Search scope(±bp) must less 1,000,000!",
        type = "error"
      )
      return(NULL)
    }
    #scope <- as.numeric(req(input$bins))
    scope <- req(input$bins)
    if (is.null(ATAC_count())) {
      return(NULL)
    }
    else {
      gene <- output_gene()
      if (nrow(gene) == 0) {
        return()
      }
      df <- data.frame()
      Filter_value <- req(input$value)
      Filter_Method <- req(input$filter_method)
      
      ATAC <- ATAC_count()
      transform_gene <-
        transform(gene,
                  start_position = start_position - scope,
                  end_position = end_position + scope)
      ATAC1 <-
        ATAC[ATAC$chrom == transform_gene$chromosome_name,]
      
      ATAC2 <-
        ATAC1[ATAC1$chromStart > transform_gene$start_position &
                ATAC1$chromEnd < transform_gene$end_position,]
      # print(ATAC2)
      
      if (req(input$method) == "1") {
        return(cor_test(ATAC2,
                        gene,
                        "pearson",
                        Filter_Method,
                        Filter_value))
      }
      if (req(input$method) == "2") {
        return(cor_test(ATAC2,
                        gene,
                        "spearman",
                        Filter_Method,
                        Filter_value))
      }
      if (req(input$method) == "3") {
        return(cor_test(ATAC2,
                        gene,
                        "kendall",
                        Filter_Method,
                        Filter_value))
      }
    }
  },
  error = function(e) {
    return()
  })
})

# 相关性图像
select_plot <- reactive({
  tryCatch({
    if (input$geneid_method == "ENSEMBL") {
      index <- "ensembl_gene_id"
    }
    if (input$geneid_method == "SYMBOL") {
      index <- "external_gene_name"
    }
    if (input$geneid_method == "ENTREZID") {
      index <- "entrezgene_id"
    }
    gene <- output_gene()
    if (length(which(duplicated(gene[[index]]))) > 0) {
      gene <-
        gene[-which(duplicated(gene[[index]])),]
    }
    
    click_ATAT <- req(input$ATAC_rows_selected)
    if (input$method == "1") {
      return(lzh_plot(
        gene,
        click_ATAT,
        "pearson",
        Filter_Method,
        Filter_value
      ))
    }
    if (input$method == "2") {
      return(lzh_plot(
        gene,
        click_ATAT,
        "spearman",
        Filter_Method,
        Filter_value
      ))
    }
    if (input$method == "3") {
      return(lzh_plot(
        gene,
        click_ATAT,
        "kendall",
        Filter_Method,
        Filter_value
      ))
    }
  },
  error = function(e) {
    #message("有限值的观察量不够", e)
    
    return(NULL)
  })
})



# 输出筛选基因
output$RNA <- DT::renderDataTable(
  output_gene()[, c(1:6)],
  selection = "none",
  rownames = FALSE,
  options = list(
    dom = 't',
    pageLength = 20,
    autoWidth = F,
    searchHighlight = TRUE,
    # columnDefs = list(list(targets = 2, width = "210px")),
    scrollX = TRUE
  )
)

observe({
  if (is.null(select_ATAC())) {
    output$ATAC <- DT::renderDataTable(NULL)
  } else{
    output$ATAC <-
      DT::renderDataTable({
        df <- select_ATAC()[, c(-4:-(ncol(select_ATAC()) - 3))]
        # Create 19 breaks and 20 rgb color values ranging from white to blue
        brks1 <-
          quantile(df[, 6],
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        brks2 <-
          quantile(df[, 5],
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        brks3 <-
          quantile(df[, 4],
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
        #   {paste0("rgb(", .,",", .,",220)")}
        DT::datatable(
          df,
          selection = "single",
          extensions = c("Scroller", "RowReorder"),
          option = list(
            rowReorder = TRUE,
            deferRender = TRUE,
            scrollY = 295,
            scroller = TRUE,
            scrollX = TRUE,
            searchHighlight = TRUE,
            orderClasses = TRUE,
            autoWidth = F,
            fixedColumns = TRUE
          )
        ) %>%
          formatStyle('rho',background=color_from_middle(brks1,'red','lightblue')) %>%
          formatStyle(
            "FDR",
            background=color_from_middle(brks2,'red','lightblue'),
            backgroundSize = '90% 100%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center')%>%
          formatStyle(
            "p_value",
            background=color_from_middle(brks3,'red','lightblue')
          )
      })
    
  }
})

# 输出图像
output$displot <- renderPlotly({
  if (is.null(select_plot()))
    return()
  select_plot()
})
