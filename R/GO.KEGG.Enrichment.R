# 模块六 ------------------------------------------------------------------
observeEvent(input$submit7, {
  progressSweetAlert(
    session = session,
    id = "simulationProgress6",
    title = "GO enrichment analysis...",
    display_pct = TRUE,
    value = 20
  )
  
  gene_list <- input$gene_list_six
  gene_list <- unlist(strsplit(gene_list, "\n"))
  
  if (input$six_genelist_idtype == "1") {
    index <- "ensembl_gene_id"
  }
  if (input$six_genelist_idtype == "2") {
    index <- "external_gene_name"
  }
  if (input$six_genelist_idtype == "3") {
    index <- "entrezgene_id"
  }
  RNA <- RNA_count()
  
  if (input$Species == "1") {
    if (input$six_genelist_idtype != "3") {
      gene <- RNA[RNA[[index]] %in% gene_list, 3]
    }
    OrgDb <- "org.Hs.eg.db"
    organism <- 'hsa'
  } else{
    if (input$six_genelist_idtype != "3") {
      gene <- RNA[RNA[[index]] %in% gene_list, 3]
    }
    OrgDb <- "org.Mm.eg.db"
    organism <- 'mmu'
  }
  
  print(gene)
  
  go <<- enrichGO(
    gene = gene,
    OrgDb = OrgDb,
    ont = "ALL",
    pAdjustMethod = "BH",
    pvalueCutoff = input$six_p_value,
    qvalueCutoff = input$six_q_value,
    minGSSize = input$minGSSize,
    maxGSSize = input$maxGSSize,
    readable = T
  )
  
  updateProgressBar(
    session = session,
    id = "simulationProgress6",
    title = "KEGG enrichment analysis...",
    value = 50
  )
  
  kegg <<- enrichKEGG(
    gene = gene,
    organism = organism,
    pvalueCutoff = input$six_p_value,
    qvalueCutoff = input$six_q_value,
    minGSSize = input$minGSSize,
    maxGSSize = input$maxGSSize
  )
  
  #print(go@result)
  output$Enrichment.Table <- renderUI({
    shinydashboard::tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      width = 9,
      tabPanel(
        tagList(icon("table"),"GO table"),
        dataTableOutput("GO.result"),
        fluidRow(
          column(
            3,
            radioButtons(
              inputId = "extTable5",
              label = helpText("Table output format"),
              choices = c("CSV" = "csv", "TXT" = "txt"),
              inline = T
            )
          ),
          column(
            9,
            downloadButton("Download_GO", "Download"),
            shiny::tags$style(
              "#Download_GO {background-color: white; color: black;margin-top:4%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          )
        )
      ),
      
      tabPanel(
        tagList(icon("table"),"KEGG table"),
        dataTableOutput("KEGG.result"),
        fluidRow(
          column(
            3,
            radioButtons(
              inputId = "extTable6",
              label = helpText("Table output format"),
              choices = c("CSV" = "csv", "TXT" = "txt"),
              inline = T
            )
          ),
          column(
            9,
            downloadButton("Download_KEGG", "Download"),
            shiny::tags$style(
              "#Download_KEGG {background-color: white; color: black;margin-top:4%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          )
        )
      )
    )
  })
  
  output$Enrichment.Plot <- renderUI({
    shinydashboard::tabBox(
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset2",
      width = 12,
      height = "auto",
      tabPanel(tagList(icon(
        "chart-column"
      ), "GO dotplot"),
      fluidRow(
        column(3,
               fluidRow(column(
                 12,
                 radioButtons(
                   inputId = "extPlot6",
                   label = helpText("Plot output format"),
                   choices = c(
                     "PNG" = "png",
                     "PDF" = "pdf",
                     "JPEG" = "jpeg"
                   ),
                   inline = T
                 )
               )),
               fluidRow(
                 column(
                   12,
                   downloadButton("plotDown6", "Download"),
                   shiny::tags$style(
                     "#plotDown6 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                   )
                 )
               )),
        column(9,
               plotlyOutput("GO.dotplot"))
      )),
      tabPanel(tagList(icon("chart-line"), "GO barplot"),
               fluidRow(
                 column(3,
                        fluidRow(column(
                          12,
                          radioButtons(
                            inputId = "extPlot7",
                            label = helpText("Plot output format"),
                            choices = c(
                              "PNG" = "png",
                              "PDF" = "pdf",
                              "JPEG" = "jpeg"
                            ),
                            inline = T
                          )
                        )),
                        fluidRow(
                          column(
                            12,
                            downloadButton("plotDown7", "Download"),
                            shiny::tags$style(
                              "#plotDown7 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                            )
                          )
                        )),
                 column(9,
                        plotlyOutput("GO.barplot"))
               )),
      tabPanel(
        tagList(icon("chart-area"), "GO upsetplot"),
        fluidRow(column(
          3,
          fluidRow(column(
            12,
            radioButtons(
              inputId = "extPlot8",
              label = helpText("Plot output format"),
              choices = c(
                "PNG" = "png",
                "PDF" = "pdf",
                "JPEG" = "jpeg"
              ),
              inline = T
            )
          )),
          fluidRow(column(
            12,
            downloadButton("plotDown8", "Download"),
            shiny::tags$style(
              "#plotDown8 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          ))
        ),
        column(9,
               plotOutput(
                 "GO.upsetplot"
               )))
      ),
      tabPanel(
        tagList(icon("sitemap"), "GO cnetplot"),
        fluidRow(column(
          3,
          fluidRow(column(
            12,
            radioButtons(
              inputId = "extPlot13",
              label = helpText("Plot output format"),
              choices = c(
                "PNG" = "png",
                "PDF" = "pdf",
                "JPEG" = "jpeg"
              ),
              inline = T
            )
          )),
          fluidRow(column(
            12,
            downloadButton("plotDown13", "Download"),
            shiny::tags$style(
              "#plotDown13 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          ))
        ),
        column(9,
               plotOutput(
                 "GO.cnetplot"
               )))
      ),
      tabPanel(tagList(
        icon("object-group"), "GO wordcloud"
      ),
      fluidRow(
        column(12,
               wordcloud2::wordcloud2Output("GO.wordcloud"))
      )),
      tabPanel(
        tagList(icon("chart-column"), "KEGG dotplot"),
        fluidRow(
          column(3,
                 fluidRow(column(
                   12,
                   radioButtons(
                     inputId = "extPlot10",
                     label = helpText("Plot output format"),
                     choices = c(
                       "PNG" = "png",
                       "PDF" = "pdf",
                       "JPEG" = "jpeg"
                     ),
                     inline = T
                   )
                 )),
                 fluidRow(
                   column(
                     12,
                     downloadButton("plotDown10", "Download"),
                     shiny::tags$style(
                       "#plotDown10 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                     )
                   )
                 )),
          column(9,
                 plotlyOutput("KEGG.dotplot"))
        )
      ),
      tabPanel(
        tagList(icon("chart-line"), "KEGG barplot"),
        fluidRow(
          column(3,
                 fluidRow(column(
                   12,
                   radioButtons(
                     inputId = "extPlot11",
                     label = helpText("Plot output format"),
                     choices = c(
                       "PNG" = "png",
                       "PDF" = "pdf",
                       "JPEG" = "jpeg"
                     ),
                     inline = T
                   )
                 )),
                 fluidRow(
                   column(
                     12,
                     downloadButton("plotDown11", "Download"),
                     shiny::tags$style(
                       "#plotDown11 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                     )
                   )
                 )),
          column(9,
                 plotlyOutput("KEGG.barplot"))
        )
      ),
      tabPanel(
        tagList(icon("chart-area"), "KEGG upsetplot"),
        fluidRow(column(
          3,
          fluidRow(column(
            12,
            radioButtons(
              inputId = "extPlot12",
              label = helpText("Plot output format"),
              choices = c(
                "PNG" = "png",
                "PDF" = "pdf",
                "JPEG" = "jpeg"
              ),
              inline = T
            )
          )),
          fluidRow(column(
            12,
            downloadButton("plotDown12", "Download"),
            shiny::tags$style(
              "#plotDown12 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          ))
        ),
        column(9,
               plotOutput(
                 "KEGG.upsetplot"
               )))
      ),
      tabPanel(
        tagList(icon("sitemap"), "KEGG cnetplot"),
        fluidRow(column(
          3,
          fluidRow(column(
            12,
            radioButtons(
              inputId = "extPlot14",
              label = helpText("Plot output format"),
              choices = c(
                "PNG" = "png",
                "PDF" = "pdf",
                "JPEG" = "jpeg"
              ),
              inline = T
            )
          )),
          fluidRow(column(
            12,
            downloadButton("plotDown14", "Download"),
            shiny::tags$style(
              "#plotDown14 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
            )
          ))
        ),
        column(9,
               plotOutput(
                 "KEGG.cnetplot"
               )))
      ),
      tabPanel(tagList(
        icon("object-group"), "KEGG wordcloud"
      ),
      fluidRow(
        column(12,
               wordcloud2::wordcloud2Output("KEGG.wordcloud"))
      ))
      
    )
  })
  
  tryCatch({
    output$GO.result <- DT::renderDataTable(
      go@result,
      # server = FALSE,
      # selection = "none",
      # options = list(
      #   pageLength = 10,
      #   autoWidth = F,
      #   # columnDefs = list(list(
      #   #   targets = 2, width = "210px"
      #   # )),
      #   scrollX = TRUE
      # )
      extensions = c("Scroller", "RowReorder"),
      server = FALSE,
      selection = "none",
      option = list(
        rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        scrollX = TRUE,
        searchHighlight = TRUE,
        orderClasses = TRUE,
        autoWidth = F
      )
    )
    
    wcdf.GO <- read.table(text = go$GeneRatio, sep = "/")[1]
    wcdf.GO$word <- go[, 3]
    names(wcdf.GO)[1] <- "freq"
    wcdf.GO <- select(wcdf.GO, c(2, 1))
    
    
    output$GO.dotplot <- renderPlotly({
      plotly::ggplotly(
        dotplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 100)
        )
      )
    })
    
    output$GO.barplot <- renderPlotly({
      plotly::ggplotly(
        barplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 100)
        )
      )
    })
    
    output$GO.upsetplot <- renderPlot({
      upsetplot(go)
    })
    
    output$GO.cnetplot <- renderPlot({
      cnetplot(go)
    })
    
    output$GO.wordcloud <- wordcloud2::renderWordcloud2({
      wordcloud2(wcdf.GO, size = 1)
    })
  }, error = function(e) {
    output$GO.result <- DT::renderDataTable(return())
    
    
    
    output$GO.dotplot <- renderPlotly({
      return()
    })
    
    output$GO.barplot <- renderPlotly({
      return()
    })
    
    output$GO.upsetplot <- renderPlot({
      return()
    })
    
    output$GO.wordcloud <- wordcloud2::renderWordcloud2({
      return()
    })
    
    output$GO.cnetplot <- renderPlot({
      return()
    })
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "GO did not enrich to the result!",
      type = "error"
    )
  })
  
  tryCatch({
    wcdf.KEGG <- read.table(text = kegg$GeneRatio, sep = "/")[1]
    wcdf.KEGG$word <- kegg[, 2]
    names(wcdf.KEGG)[1] <- "freq"
    wcdf.KEGG <- select(wcdf.KEGG, c(2, 1))
    output$KEGG.result <- DT::renderDataTable(
      kegg@result,
      extensions = c("Scroller", "RowReorder"),
      server = FALSE,
      selection = "none",
      option = list(
        rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        scrollX = TRUE,
        searchHighlight = TRUE,
        orderClasses = TRUE,
        autoWidth = F
      )
    )
    output$KEGG.dotplot <- renderPlotly({
      plotly::ggplotly(
        dotplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 100)
        )
      )
    })
    
    output$KEGG.barplot <- renderPlotly({
      plotly::ggplotly(
        barplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 100)
        )
      )
    })
    
    output$KEGG.upsetplot <- renderPlot({
      upsetplot(kegg)
    })
    
    output$KEGG.cnetplot <- renderPlot({
      cnetplot(kegg)
    })
    
    output$KEGG.wordcloud <- wordcloud2::renderWordcloud2({
      wordcloud2(wcdf.KEGG, size = 1)
    })
    updateProgressBar(
      session = session,
      id = "simulationProgress6",
      title = "KEGG enrichment analysis...",
      value = 100
    )
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "DONE",
      text = "The annotation is complete!",
      type = "success"
    )
  }, error = function(e) {
    output$KEGG.result <- DT::renderDataTable(return())
    output$KEGG.dotplot <- renderPlotly({
      return()
    })
    
    output$KEGG.barplot <- renderPlotly({
      return()
    })
    
    output$KEGG.upsetplot <- renderPlot({
      return()
    })
    
    output$KEGG.cnetplot <- renderPlot({
      return()
    })
    
    output$KEGG.wordcloud <- wordcloud2::renderWordcloud2({
      return()
    })
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "KEGG did not enrich to the result!",
      type = "error"
    )
  })
  
  
  
  
  # dotplot(go)
  # barplot(go)
  # upsetplot(go)
})