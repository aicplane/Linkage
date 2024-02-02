# 模块四 --------------------------------------------------------------------
observe({
  if (is.null(select_ATAC())) {
    output$ATAC3 <- DT::renderDataTable(NULL)
  } else{
    output$ATAC3 <-
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
            autoWidth = F,
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



# motif分析
select_motif <- reactive({
  # isolate({
  # pwm_library <- TFBSTools::getMatrixSet(
  #   '~/project/03.linkage/JASPAR2022.sqlite',
  #   opts = list(
  #     collection = "CORE",
  #     species    = "Homo sapiens",
  #     matrixtype = "PWM"
  #   )
  # )
  #
  # # extract the motif names from the pwm library
  # pwm_library_list <- lapply(pwm_library, function(x) {
  #   data.frame(ID = ID(x), name = name(x))
  # })
  #
  # # combine the list into one data frame
  # pwm_library_dt <- dplyr::bind_rows(pwm_library_list)
  #
  # PFMatrixList <-
  #   TFBSTools::getMatrixByID('~/project/03.linkage/JASPAR2022.sqlite', ID = pwm_library_dt$ID)
  # })
  
  peakfile <-
    select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
  select_peak <- req(input$ATAC3_rows_selected)
  Species <- input$Species
  return(motif_analysis(peakfile, select_peak, Species))
  
  # tryCatch({
  # })
})

# seqlogo图
select_seqlogo <- reactive({
  motif <- select_motif()
  select_row <- req(input$Motif_rows_selected)
  return(seqLogo_plot(motif, select_row))
})

# # 输出motif分析结果数据框
# output$Motif <-
#   DT::renderDataTable(
#     select_motif(),
#     selection = "single",
#     extensions = c("Scroller", "RowReorder"),
#     option = list(
#       rowReorder = TRUE,
#       deferRender = TRUE,
#       scrollY = 290,
#       scroller = TRUE,
#       scrollX = F,
#       searchHighlight = TRUE,
#       orderClasses = TRUE,
#       autoWidth = F
#     )
#   )
output$Motif <-
  DT::renderDataTable({
    df <- select_motif()
    # Create 19 breaks and 20 rgb color values ranging from white to blue
    brks1 <-
      quantile(df$score,
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
        autoWidth = F
      )
    ) %>% formatStyle(
      names(df)[8],
      background = styleColorBar(range(brks1), 'lightblue'),
      backgroundSize = '90% 100%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
  })
# 输出seqlogo图
output$displot5 <- renderPlot({
  select_seqlogo()
})