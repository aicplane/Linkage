library(shiny)
ui <- dashboardPage(
  title = "Linkage",
  skin = "blue",
  header = dashboardHeader(# disable = TRUE,
    title = shiny::tags$a(img(
      src = "Linkage.png", height = 50
    )),
    titleWidth = "230"
    ),
  sidebar = shinydashboard::dashboardSidebar(fluidRow(column(
    12,
    sidebarMenu(
      id = "inTabset",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home", lib = "glyphicon")
      ),
      menuItem(
        "Regulatory Peaks Module",
        tabName = "one",
        icon = icon("th", lib = "glyphicon")
      ),
      menuItem(
        "Peak Visualization Module",
        tabName = "tow",
        icon = icon("glyphicon glyphicon-equalizer", lib = "glyphicon")
      ),
      menuItem(
        "Peak Annotation Module",
        tabName = "three",
        icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")
      ),
      menuItem(
        "Motif Analysis Module",
        tabName = "four",
        icon = icon("glyphicon glyphicon-list-alt", lib = "glyphicon")
      ),
      menuItem(
        # HTML("<div>&nbspRegulatory Networks Module</div>"),
        "Regulatory Networks Module",
        tabName = "five",
        icon = icon("glyphicon glyphicon-certificate", lib = "glyphicon")
      ),
      menuItem(
        "GO&KEGG Enrichment Module",
        tabName = "six",
        icon = icon("glyphicon glyphicon-object-align-bottom", lib = "glyphicon")
      ),
      menuItem(
        "Help",
        tabName = "help",
        icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon")
      ),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("glyphicon glyphicon-info-sign", lib = "glyphicon")
      )
    )
  )),

  shiny::tags$head(
    shiny::tags$style(
      HTML(
        "
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='home'] {
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='one'] {
            color: #808080; /* 灰色 */
            font-size: 13px
    }


    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='tow'] {
            color: #808080; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='three'] {
            color: #808080; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='four'] {
            color: #808080; /* 灰色 */
            font-size: 13px
      }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='five'] {
            color: #808080; /* 灰色 */
            font-size: 13px
    }

    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='six'] {
            color: #808080; /* 灰色 */
            font-size: 13px
    }

    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
            background-color: #434C5E;
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='Help'] {
            font-size: 13px
    }
    .skin-blue .main-sidebar .sidebar-menu > li > a[data-value='About'] {
            font-size: 13px
    }

    .sidebar {
        position: fixed;
        top: 50px;
        bottom: 0;
        width:230px;
      }

      /*固定标题*/
        .skin-blue .main-header .logo{
            position: fixed;
            height: 50px;
            width: 230px;
        }
        .skin-blue .navbar-static-top {
          position: fixed;
          width: 100%;
        }
"
      )
    )
  )),
  body = dashboardBody(
    useShinyjs(),
    use_theme(mytheme),
    shiny::tags$div(style = "height: 50px;"),
    # <-- use the theme
    tabItems(
      # First tab content
      tabItem(
        tabName = "home",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("flask"), "Instruction"),
            width = 8,
            height = 480,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(column(
              12,
              align = "center",
              shiny::tags$img(src = "fig1.png", style = "max-height:270px; width:auto;max-width:100%;")
            )),
            fluidRow(column(12, helpText(
              div(
                code("Linkage"),
                "is a user-friendly, interactive, open-source R-Shiny web application for",
                code("exploring and visualizing potential gene regulatory elements"),
                "based on ",
                strong("ATAC-seq"),
                " and",
                strong("RNA-seq"),
                " data. Users can upload customized data and obtain genome-wide gene regulatory
                  elements with simple clicks. All the gene regulatory elements are predicted from multi-omics
                  data, rather than being experimentally determined. Users can easily search, browse, and
                  download the peak-gene scatter plot for a given gene. We will continue to update Linkage
                  and add more modules for downstream analysis in the future.",
                style = "font-size:17.5px;font-style:calibri;color:black;",
                align = "justify"
              )
            )))
          ),
          #shinydashboard::box(title = "Data Select", width = 4, solidHeader = F, status = "primary", collapsible = TRUE,
          column(
            4,
            shinydashboard::box(
              title = tagList(icon("gears"), "Import Data"),
              width = NULL,
              height = 480,
              solidHeader = F,
              status = "primary",
              selectInput(
                inputId = "data",
                label = "Data type",
                choices = list("TCGA Breast Cancer cohort" = 1,
                               "GSE121589" = 2,
                               "Custom data" = 3),
                selected = 1
              ),
              shiny::tags$style(
                ".btn-two {background-color:  #E6E7E8; color: black;box-shadow: inset 0px 2px 5px #888888;}",
                ".btn-one.active {background-color: #017351; color: white;box-shadow: inset 0px 2px 5px #888888;}",
                ".btn-one {background-color:  #E6E7E8; color: black;box-shadow: inset 0px 2px 5px #888888;}",
                ".btn-two.active {background-color: #017351; color: white;box-shadow: inset 0px 2px 5px #888888;}"
              ),
              fluidRow(column(
                12,
                radioGroupButtons(
                  inputId = "Species",
                  label = "Species type",
                  choiceValues = c(#'Markdown',
                    1,
                    2),
                  choiceNames = c(#'Markdown',
                    "Homo sapiens",
                    "Mus musculus"),
                  justified = TRUE,
                  status = c("one", "two")
                )
              )),
              uiOutput("datahelptext"),
              uiOutput("Fileinput"),
              do.call(actionBttn, c(
                list(
                  inputId = "submit",
                  label = "Do analysis",
                  icon = icon("play")
                ),
                actionBttnParams
              ))
            )
          ),
          shiny::tags$style(
            "#submit {box-shadow: 0px 2px 5px #888888;}"
          ),
          shiny::tags$style(
            "#ATAC_data {background-color: white; color: black;margin-top: 10%;margin-left:70%}"
          ),
          shiny::tags$style(
            "#RNA_data {background-color: white; color: black;margin-top: 10%;margin-left:70%}"
          )
          #)
        ),
        fluidRow(
          shinydashboard::box(
            column = 12,
            title = tagList(icon("table"), "RNA-seq Data"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("eg_RNA")
            # %>% withSpinner(color = "#3c8cbc"),
          )
        ),
        fluidRow(
          shinydashboard::box(
            column = 12,
            title = tagList(icon("table"), "ATAC-seq Data"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("eg_ATAC")
            # %>% withSpinner(color = "#3c8cbc")
          )
        ),
      ),
      tabItem(
        tabName = "one",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("flask"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                code("Correlation module"),
                " allows a user to detect all potential regulatory DNA regions for a given gene.
            Given the input gene and targeting DNA regions,
            Linkage will perform canonical correlation analysis between each quantitative chromatin accessibility measure in the region and the
            quantitative expression level of the gene across all samples.
            Users can adjust the region of interest and customize the correlation algorithm",
                strong(" spearman / pearson / kendall"),
                "With a click on the submit button,
            the",
                code(
                  "user can view the scatter plot of RNA-seq and ATAC-seq correlation."
                ),
                "The rho and FDR for correlation analysis will also be shown on the scatter plot.",
                style = "font-size:17.5px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("gears"), "Filter"),
            width = 7,
            height = 230,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(
                4,
                selectInput(
                  inputId = "geneid_method",
                  label = "Search type",
                  choices = list("ENSEMBL",
                                 "SYMBOL",
                                 "ENTREZID"),
                  selected = "SYMBOL"
                )
              ),
              column(
                4,
                numericInput(
                  "bins",
                  label = "Search scope(±bp)",
                  value = 500000,
                  max = 1000000,
                  min = 0,
                  step = 1000
                )
              ),
              column(
                4,
                selectInput(
                  inputId = "filter_method",
                  label = "Fliter type",
                  choices = list("p_value",
                                 "FDR",
                                 "rho"),
                  selected = "FDR"
                ),
              )
            ),
            fluidRow(
              column(
                4,
                textInput("geneid",
                          label = "Gene input",
                          value = "ENSG00000000419")
              ),
              column(
                4,
                selectInput(
                  inputId = "method",
                  label = "Method",
                  choices = list(
                    "pearson" = 1,
                    "spearman" = 2,
                    "kendall" = 3
                  ),
                  selected = 2
                )
              ),
              column(
                4,
                numericInput(
                  "value",
                  label = "Threshold configuration",
                  value = "0.05",
                  max = 1,
                  min = 0,
                  step = 0.01
                )
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("table"), "Output Gene"),
            width = 5,
            height = 230,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("RNA") #%>% withSpinner(color = "#3c8cbc")
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("table"), "Output Peak"),
            width = 7,
            height = 530,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            DT::dataTableOutput("ATAC"), #%>% withSpinner(color = "#3c8cbc"),
            fluidRow(
              column(
                4,
                radioButtons(
                  inputId = "extTable",
                  label = helpText("Table output format"),
                  choices = c("CSV" = "csv", "TXT" = "txt"),
                  inline = T
                )
              ),
              column(4,
                     div(
                       downloadButton("tableDown", "Download"),
                       shiny::tags$style(
                         "#tableDown {background-color: white; color: black; margin-left: 0%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                       ),
                     )),
              column(
                4,
                uiOutput("next_1"),
                shiny::tags$style("#submit2 {margin-top: 10%;box-shadow: 0px 2px 5px #888888;}"),
              )
            )
          ),
          shinydashboard::box(
            title = tagList(icon("chart-column"), "Correlation Plot"),
            width = 5,
            height = 530,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            plotlyOutput(outputId = "displot") %>% withSpinner(color = "#3c8cbc"),
            fluidRow(
              column(
                6,
                radioButtons(
                  inputId = "extPlot",
                  label = helpText("Plot output format"),
                  choices = c(
                    "PNG" = "png",
                    "PDF" = "pdf",
                    "JPEG" = "jpeg"
                  ),
                  inline = T
                )
              ),
              column(
                6,
                downloadButton("plotDown", "Download"),
                shiny::tags$style(
                  "#plotDown {background-color: white; color: black;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "tow",
              fluidRow(
                shinydashboard::box(
                  title = tagList(icon("flask"), "Instruction"),
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  helpText(
                    div(
                      code("Regulatory Peaks Visualization Module"),
                      " allows users to visualize the ATAC-seq and
                RNA-seq signal distribution around a specific regulatory peak simultaneously.
                Given an input gene and the corresponding regulatory peak that getting from Regulatory
                Peaks Detection Module, users can view
                the trackplot of ATAC-seq count matrix and the shinydashboard::boxplot of RNA-seq count matrix.",
                      style = "font-size:17.5px;font-style:calibri;color:black;",
                      align = "justify"
                    )
                  )
                )
              ),
              fluidRow(
                shinydashboard::box(
                  column = 12,
                  align = "center",
                  title = tagList(icon("gears"), "Select Peak"),
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  DT::dataTableOutput("ATAC2") %>% withSpinner(color = "#3c8cbc"),
                  uiOutput("next_2"),
                  shiny::tags$style("#submit3 {margin-top: 10%;margin-left:170%;box-shadow: 0px 2px 5px #888888;}"),
                )
              ),
              fluidRow(
                shinydashboard::box(
                  title = tagList(icon("chart-column"), "Output Trackplot"),
                  width = 6,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  plotOutput(outputId = "displot2") %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        inputId = "extPlot2",
                        label = helpText("Plot output format"),
                        choices = c(
                          "PNG" = "png",
                          "PDF" = "pdf",
                          "JPEG" = "jpeg"
                        ),
                        inline = T
                      )
                    ),
                    column(
                      6,
                      downloadButton("plotDown2", "Download"),
                      shiny::tags$style(
                        "#plotDown2 {background-color: white; color: black;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                      )
                    )
                  )
                ),
                shinydashboard::box(
                  title = tagList(icon("chart-column"), "Output shinydashboard::boxplot"),
                  width = 6,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  plotlyOutput(outputId = "displot3") %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        inputId = "extPlot3",
                        label = helpText("Plot output format"),
                        choices = c(
                          "PNG" = "png",
                          "PDF" = "pdf",
                          "JPEG" = "jpeg"
                        ),
                        inline = T
                      )
                    ),
                    column(
                      6,
                      downloadButton("plotDown3", "Download"),
                      shiny::tags$style(
                        "#plotDown3 {background-color: white; color: black;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                      )
                    )
                  )
                )
              )),
      tabItem(tabName = "three",
              fluidRow(
                shinydashboard::box(
                  title = tagList(icon("flask"), "Instruction"),
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  helpText(
                    div(
                      code("Regulatory Peaks Annotation Module"),
                      " allows users to visualize the annotation of a given
                regulatory peak in terms of genomic location features, which includes whether a peak is in",
                      strong("the TSS, Exon, 5' UTR, 3' UTR, Intronic or Intergenic."),
                      style = "font-size:17.5px;font-style:calibri;color:black;",
                      align = "justify"
                    )
                  )
                )
              ),
              fluidRow(
                shinydashboard::box(
                  title = tagList(icon("table"), "Annotation Table"),
                  width = 7,
                  height = 530,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  fluidRow(column(4,
                                  do.call(
                                    actionBttn, c(
                                      list(
                                        inputId = "Annote",
                                        label = "Do Peak annotated",
                                        icon = icon("play")
                                      ),
                                      actionBttnParams
                                    )
                                  ))),
                  shiny::tags$style(
                    "#Annote {box-shadow: 0px 2px 5px #888888;}"
                  ),
                  DT::dataTableOutput("Peak_Annotation"),
                  # %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      4,
                      radioButtons(
                        inputId = "extTable2",
                        label = helpText("Table output format"),
                        choices = c("CSV" = "csv", "TXT" = "txt"),
                        inline = T
                      )
                    ),
                    column(
                      4,
                      downloadButton("Download_Annotation", "Download"),
                      shiny::tags$style(
                        "#Download_Annotation {background-color: white; color: black; margin-left:30%; margin-top:10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                      )
                    ),
                    column(
                      4,
                      uiOutput("next_3"),
                      shiny::tags$style("#submit4 {margin-top:10%;box-shadow: 0px 2px 5px #888888;}")
                    )
                  )
                ),
                shinydashboard::box(
                  title = tagList(icon("chart-column"), "Output Upsetplot"),
                  width = 5,
                  height = 530,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  plotOutput(outputId = "displot4"),
                  # %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        inputId = "extPlot4",
                        label = helpText("Plot output format"),
                        choices = c(
                          "PNG" = "png",
                          "PDF" = "pdf",
                          "JPEG" = "jpeg"
                        ),
                        inline = T
                      )
                    ),
                    column(
                      6,
                      downloadButton("plotDown4", "Download"),
                      shiny::tags$style(
                        "#plotDown4 {background-color: white; color: black;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                      )
                    )
                  )
                )
              )),
      tabItem(tabName = "four",
              fluidRow(
                shinydashboard::box(
                  title = tagList(icon("flask"), "Instruction"),
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  helpText(
                    div(
                      code("Motif Enrichment Analysis Module"),
                      " allows users to visualize the enriched transcription
                factor motifs of a given regulatory peak, which includes the motif enrichment table and
                the corresponding sequencelogo. Users can",
                      code(
                        "view the location and binding score
                information of each enriched transcription factor"
                      ),
                      "within the motif enrichment table.",
                      style = "font-size:17.5px;font-style:calibri;color:black;",
                      align = "justify"
                    )
                  )
                )
              ),
              fluidRow(
                shinydashboard::box(
                  column = 12,
                  title = tagList(icon("gears"), "Peak Select"),
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  DT::dataTableOutput("ATAC3") %>% withSpinner(color = "#3c8cbc"),
                  uiOutput("next_4"),
                  shiny::tags$style("#submit5 {margin-top: 10%;margin-left:170%;box-shadow: 0px 2px 5px #888888;}"),
                )
              ),
              fluidRow(
                shinydashboard::box(
                  column = 12,
                  title = tagList(icon("table"), "Motif Table"),
                  width = 7,
                  height = 530,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  DT::dataTableOutput("Motif") %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      4,
                      radioButtons(
                        inputId = "extTable3",
                        label = helpText("Table output format"),
                        choices = c("CSV" = "csv", "TXT" = "txt"),
                        inline = T
                      )
                    ),
                    column(4,
                           div(
                             downloadButton("Download_Motif", "Download"),
                             # align = "center",
                             shiny::tags$style(
                               "#Download_Motif {background-color: white; color: black;margin-left:0%; margin-top:10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                             )
                           )),
                    # column(
                    #   4,
                    #   uiOutput("next_4"),
                    #   shiny::tags$style("#submit5 {margin-top: 10%;}"),
                    # )
                  )
                ),
                shinydashboard::box(
                  title = tagList(icon("chart-column"), "Output Seqlogoplot"),
                  width = 5,
                  height = 530,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  plotOutput(outputId = "displot5") %>% withSpinner(color = "#3c8cbc"),
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        inputId = "extPlot5",
                        label = helpText("Plot output format"),
                        choices = c(
                          "PNG" = "png",
                          "PDF" = "pdf",
                          "JPEG" = "jpeg"
                        ),
                        inline = T
                      )
                    ),
                    column(
                      6,
                      downloadButton("plotDown5", "Download"),
                      shiny::tags$style(
                        "#plotDown5 {background-color: white; color: black;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                      )
                    )
                  )
                )
              )),
      tabItem(
        tabName = "five",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("flask"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                "Motif Enrichment Analysis Module allows users to visualize the enriched transcription
                factor motifs of a given regulatory peak, which includes the motif enrichment table and
                the corresponding sequencelogo. Users can view the location and binding score
                information of each enriched transcription factor within the motif enrichment table.",
                style = "font-size:17.5px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::tabBox(
            # title = tagList(icon("gears"), "Option"),
            width = 3,
            # solidHeader = F,
            # status = "primary",
            # collapsible = TRUE,
            tabPanel(
              tagList(icon("gears"), "Gene list input"),
            column(
              12,
              textAreaInput(
                "gene_list",
                "Gene list",
                value =
                  "ENSG00000001167
ENSG00000001460
ENSG00000000003
ENSG00000000005
ENSG00000000419
ENSG00000000457
ENSG00000000460
ENSG00000000938
ENSG00000000971
ENSG00000001036
ENSG00000001084
ENSG00000001461
ENSG00000001497
ENSG00000001561
ENSG00000001617
ENSG00000001626
ENSG00000001629
ENSG00000001630
ENSG00000001631
ENSG00000002016
ENSG00000002079
ENSG00000002330
ENSG00000002549
ENSG00000002586
ENSG00000002587
ENSG00000002726
ENSG00000002745
ENSG00000002746
ENSG00000002822
ENSG00000002834
ENSG00000002919
ENSG00000002933
ENSG00000003056
ENSG00000003096
ENSG00000003137
ENSG00000003147
ENSG00000003249
ENSG00000003393
ENSG00000003400
ENSG00000003402
ENSG00000003436
ENSG00000003509
ENSG00000003756
ENSG00000003987
ENSG00000003989
ENSG00000004059
ENSG00000004139
ENSG00000004142
ENSG00000004399
ENSG00000004455
ENSG00000004468
ENSG00000004478
ENSG00000004487
ENSG00000004534
ENSG00000004660
ENSG00000004700
ENSG00000004766
ENSG00000004776
ENSG00000004777
ENSG00000004779
ENSG00000004799
ENSG00000004809
ENSG00000004838
ENSG00000004846
ENSG00000004848
ENSG00000004864
ENSG00000004866
ENSG00000004897
ENSG00000004939
ENSG00000004948
ENSG00000004961
ENSG00000004975
ENSG00000005001
ENSG00000005007
ENSG00000005020
ENSG00000005022
ENSG00000005059
ENSG00000005073
ENSG00000005075
ENSG00000005100
ENSG00000005102
ENSG00000005108
ENSG00000005156
ENSG00000005175
ENSG00000005187
ENSG00000005189
ENSG00000005194
ENSG00000005206
ENSG00000005238
ENSG00000005243
ENSG00000005249
ENSG00000005302
ENSG00000005339
ENSG00000005379
ENSG00000005381
ENSG00000005421
ENSG00000005436
ENSG00000005448
ENSG00000005469
ENSG00000005471",
                rows = 10
              )
            ),
            shiny::tags$style("#gene_list {resize: none;}"),

            column(
              12,
              selectInput(
                inputId = "genelist_idtype",
                label = "Gene id type",
                choices = list(
                  "ENSEMBL" = 1,
                  "SYMBOL" = 2,
                  "ENTREZID" = 3
                ),
                selected = 2
              )
            ),

            column(
              12,
              selectInput(
                inputId = "TF_cor_method",
                label = "Method",
                choices = list(
                  "pearson" = 1,
                  "spearman" = 2,
                  "kendall" = 3
                ),
                selected = 2
              )
            ),
            column(
              12,
              selectInput(
                inputId = "TF_filter_method",
                label = "Fliter type",
                choices = list("p_value",
                               "FDR",
                               "rho"),
                selected = "FDR"
              )
            ),
            column(
              12,
              numericInput(
                "TF_cor_value",
                label = "Threshold configuration",
                value = "0.05",
                max = 1,
                min = 0,
                step = 0.01
              )
            ),

            # column(12,radioGroupButtons(
            #   inputId = 'format',
            #   label = 'Search type',
            #   choices = c(#'Markdown',
            #     'HTML',
            #     'Word'),
            #   justified = TRUE,
            #   status = "primary"
            # )),

            # column(12, textInput("dir", "Create User Folder", value = "lzh_1")),
            # uiOutput("buildaction"),
            # column(
            #   12,
            #   div(actionButton("BuildNetwork", "BuildNetwork", icon("refresh")), align = "center"),
            #   shiny::tags$style("#refresh {background-color: white; color: black}")
            # ),
            br(),

            fluidRow(column(12,
                            do.call(
                              actionBttn, c(
                                list(
                                  inputId = "submit6",
                                  label = "Build Network",
                                  icon = icon("play")
                                ),
                                actionBttnParams
                              )
                            )))
          ),
          shiny::tags$style(
            "#submit6 {box-shadow: 0px 2px 5px #888888;}"
          ),
          tabPanel(
            tagList(icon("cloud-arrow-up"), "Upload data"),
            fluidRow(
              column(
                12,
                fileInput("Network_data",
                          label = "Input data")
              ),
              column(
                12,
                selectInput(
                  inputId = "genelist_idtype2",
                  label = "Gene id type",
                  choices = list(
                    "ENSEMBL" = 1,
                    "SYMBOL" = 2,
                    "ENTREZID" = 3
                  ),
                  selected = 2
                )
              ),
              column(
                12,
                selectInput(
                  inputId = "TF_filter_method2",
                  label = "Fliter type",
                  choices = list("p_value",
                                 "FDR",
                                 "rho"),
                  selected = "FDR"
                )
              ),
              column(
                12,
                numericInput(
                  "TF_cor_value2",
                  label = "Threshold configuration",
                  value = "0.05",
                  max = 1,
                  min = 0,
                  step = 0.01
                )
              ),
              column(12,
                              do.call(
                                actionBttn, c(
                                  list(
                                    inputId = "Build",
                                    label = "Build Network",
                                    icon = icon("play")
                                  ),
                                  actionBttnParams
                                )
                              )),
              shiny::tags$style(
                "#Build {box-shadow: 0px 2px 5px #888888;}"
              )
            )

          )

        ),        uiOutput("Gene.TF.Table")),

        fluidRow(uiOutput("visNetwork"))
      ),
      tabItem(
        tabName = "six",
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("flask"), "Instruction"),
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            helpText(
              div(
                "Motif Enrichment Analysis Module allows users to visualize the enriched transcription
                factor motifs of a given regulatory peak, which includes the motif enrichment table and
                the corresponding sequencelogo. Users can view the location and binding score
                information of each enriched transcription factor within the motif enrichment table.",
                style = "font-size:17.5px;font-style:calibri;color:black;",
                align = "justify"
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = tagList(icon("gears"), "Option"),
            width = 3,
            # height = 350,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            column(
              12,
              textAreaInput(
                "gene_list_six",
                "Gene list",
                value =
                  "PGF
CTSB
EDN1
DKK1
FGF1
SPX
SERPINE2
IL15
IL2
PLAU
IGF1
IGFBP4
SPP1
SEMA3F
ANG
ANG
ANG
C3
BMP6
FGF2
PAPPA
IL18
ANGPT1
CCL20
VEGFC
TNF
IGFBP7
PLAT
CCL1
CCL1
IL6
CXCL12
VEGFA
CCL7
IL10
EREG
IL7
MIF
CXCL10
NRG1
CD55
CD55
IL13
ANG
ANG
CSF1
ANGPTL4
CSF2
GDF15
CCL8
WNT16
HGF
TIMP2
ESM1
MMP2
IGFBP6
MMP3
CCL4
MMP1
MMP1
IL1A
IGFBP2
IL1B
MMP12
CCL24
CXCL16
MMP13
IGFBP1
SERPINE1
IGFBP3
FGF7
CCL3L1
IGFBP5
INHA
BMP2
VGF
CCL3
MMP9",
                rows = 10
              )
            ),
            shiny::tags$style("#gene_list_six {resize: none;}"),
            column(
              12,
              selectInput(
                inputId = "six_genelist_idtype",
                label = "Gene id type",
                choices = list(
                  "ENSEMBL" = 1,
                  "SYMBOL" = 2,
                  "ENTREZID" = 3
                ),
                selected = 2
              )
            ),
            column(
              12,
              popify(
                numericInput(
                  "six_p_value",
                  label = "pvalueCutoff",
                  value = 0.05,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                title = "Arguments",
                content = "Cutoff value of pvalue.",
                placement = "top"
              )
            ),

            column(
              12,
              popify(
                numericInput(
                  "six_q_value",
                  label = "qvalueCutoff",
                  value = 0.2,
                  max = 1,
                  min = 0,
                  step = 0.1
                ),
                title = "Arguments",
                content = "Cutoff value of qvalue.",
                placement = "top"
              )
            ),

            column(
              12,
              popify(
                numericInput(
                  "minGSSize",
                  label = "minGSSize",
                  value = 10,
                  max = 500,
                  min = 0,
                  step = 1
                ),
                title = "Arguments",
                content = "minimal size of genes annotated by Ontology term for testing.",
                placement = "top"
              )
            ),

            column(
              12,
              popify(
                numericInput(
                  "maxGSSize",
                  label = "maxGSSize",
                  value = 500,
                  min = 500,
                  step = 1
                ),
                title = "Arguments",
                content = "maximal size of genes annotated for testing.",
                placement = "top"
              )
            ),

            br(),

            fluidRow(column(12,
                            do.call(
                              actionBttn, c(
                                list(
                                  inputId = "submit7",
                                  label = "Analysis",
                                  icon = icon("play")
                                ),
                                actionBttnParams
                              )
                            ))),
            shiny::tags$style(
              "#submit7 {box-shadow: 0px 2px 5px #888888;}"
            )
          ),
          uiOutput("Enrichment.Table")
        ),
        fluidRow(uiOutput("Enrichment.Plot"))
      ),
      tabItem(
        tabName = "help",
        fluidRow(
          shinydashboard::box(
            title = "Data Input",
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            # fluidRow(
            #   column(
            #     12,
            #     helpText(
            #       div(
            #         "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
            #       RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
            #       ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
            #         style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
            #       )
            #     )
            #   )
            # ),
            fluidRow(column(
              6,
              shiny::tags$img(src = "data_1.png", style = "max-width: 100%; max-height: 100%;")
            ),
            column(
              6,
              shiny::tags$img(src = "data_2.png", style = "max-width: 100%; max-height: 100%;")
            )),
            fluidRow(column(
              6,
              shiny::tags$img(src = "data_3.png", style = "max-width: 100%; max-height: 100%;")
            ),
            column(
              6,
              shiny::tags$img(src = "data_4.png", style = "max-width: 100%; max-height: 100%;")
            ))
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Model One",
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            # fluidRow(
            #   column(
            #     12,
            #     helpText(
            #       div(
            #         "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
            #       RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
            #       ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
            #         style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
            #       )
            #     )
            #   )
            # ),
            fluidRow(column(
              12,
              shiny::tags$img(src = "model_1.png", style = "max-width: 100%; max-height: 100%;")
            ))
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Model Two",
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            # fluidRow(
            #   column(
            #     12,
            #     helpText(
            #       div(
            #         "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
            #       RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
            #       ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
            #         style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
            #       )
            #     )
            #   )
            # ),
            fluidRow(column(
              12,
              shiny::tags$img(src = "model_2.png", style = "max-width: 100%; max-height: 100%;")
            ))
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Model Three",
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            # fluidRow(
            #   column(
            #     12,
            #     helpText(
            #       div(
            #         "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
            #       RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
            #       ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
            #         style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
            #       )
            #     )
            #   )
            # ),
            fluidRow(column(
              12,
              shiny::tags$img(src = "model_3.png", style = "max-width: 100%; max-height: 100%;")
            ))
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Model Four",
            width = 12,
            solidHeader = F,
            status = "primary",
            collapsible = TRUE,
            # fluidRow(
            #   column(
            #     12,
            #     helpText(
            #       div(
            #         "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
            #       RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
            #       ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
            #         style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
            #       )
            #     )
            #   )
            # ),
            fluidRow(column(
              12,
              shiny::tags$img(src = "model_4.png", style = "max-width: 100%; max-height: 100%;")
            ))
          )
        ),
        # fluidRow(
        #   shinydashboard::box(
        #     title = "Model Five", width = 12, solidHeader = F, status = "primary", collapsible = TRUE,
        #     fluidRow(
        #       column(
        #         12,
        #         helpText(
        #           div(
        #             "RNA-seq和ATAC-seq必须有三个样本以上才能进行分析；
        #           RNA-seq文件内容前四列信息必须依次为基因名称、染色体名称、基因所处染色体的位置，其中start和end可以颠倒；
        #           ATAC-seq文件内容前三列信息必须依次为染色体名称、start列、end列或染色体名称、end列、start列。",
        #             style = "font-size:17.5px;font-style:calibri;color:black;", align = "justify"
        #           )
        #         )
        #       )
        #     ),
        #     fluidRow(
        #       column(
        #         12,
        #         shiny::tags$img(src = "data.png", style = "max-width: 50%; max-height: 50%;")
        #       )
        #     )
        #   )
        # )
      ),
      tabItem(tabName = "about",
              fluidRow(
                shinydashboard::box(
                  title = "Contact Us",
                  width = 12,
                  solidHeader = F,
                  status = "primary",
                  collapsible = TRUE,
                  helpText(
                    div(
                      "e-mail:2470587020@qq.com;1353144607@qq.com",
                      style = "font-size:17.5px;font-style:calibri;color:black;",
                      align = "justify"
                    )
                  )
                )
              ))
    )
    # hr(),
    # shiny::tags$div(style = "bottom: 150px;"),
    # div(
    #   class = "footer",
    #   img(src = "shiny-logo1.png", style = "position:absolute;bottom:0px;height:70px;left:230px"),
    #   img(src = "yh.png", style = "position:absolute;bottom:0px;height:70px;right:0px"),
    #   HTML(paste0(  "</br><p style = ' font-size:1.0em; color: black; line-height: 10%;'>",
    #                             "<b>Created by</b>: XuLabGDPU | ",
    #                             "<b>Last update</b>: 26/01/2024",
    #                             "</p>",
    #                             "</br><p style = ' font-size:1.0em; color: black; line-height: 10%;'>",
    #                             "<b>Address</b>: No. 160, Outer Ring East Road, Panyu District, Guangzhou City, Guangdong Province, China | ",
    #                             "<b>Postcode</b>: 511400",
    #                             "</p>",
    #                             "</br><p style = 'text-align: 0px; font-size:1.0em; line-height: 10%;'> ",
    #                             "<a  href = 'https://github.com/lthevenard/dt_exercises'target='_blank'>Github</a> | ",
    #                             "<a  href = 'https://www.xulabgdpu.org.cn'target='_blank'>XuLabGDPU</a> | ",
    #                             "<a  href = 'https://www.xulabgdpu.org.cn/signacShiny'target='_blank'>ShinySignac.UiO</a> |",
    #                             "<a  href = 'https://www.gdpu.edu.cn/'target='_blank'>Guangdong Pharmaceutical University</a> |",
    #                             "<a href='https://beian.miit.gov.cn/' target='_blank'>黑ICP备2024016624</a>",
    #                             "</p>"))
    # )
  ),
  footer = dashboardFooter(
    div(
      # class = "footer",
      img(src = "shiny-logo1.png", style = "position:absolute;bottom:0px;height:60px;align = left"),
      img(src = "yh.png", style = "position:absolute;bottom:0px;height:60px;right:0px"),
      HTML(paste0(  "</br><p style = ' text-align: center;font-size:1.0em; color: black; line-height: 10%;'>",
                                "<b>Created by</b>: XuLabGDPU | ",
                                "<b>Last update</b>: 29/01/2024",
                                "</p>",
                                "</br><p style = 'text-align: center; font-size:1.0em; color: black; line-height: 10%;'>",
                                "<b>Address</b>: No. 160, Outer Ring East Road, Panyu District, Guangzhou City, Guangdong Province, China | ",
                                "<b>Postcode</b>: 511400",
                                "</p>",
                                "</br><p style = 'text-align: center; font-size:1.0em; line-height: 10%;'> ",
                                "<a  href = 'https://github.com/lthevenard/dt_exercises'target='_blank'>Github</a> | ",
                                "<a  href = 'https://www.xulabgdpu.org.cn'target='_blank'>XuLabGDPU</a> | ",
                                "<a  href = 'https://www.xulabgdpu.org.cn/signacShiny'target='_blank'>ShinySignac.UiO</a> |",
                                "<a  href = 'https://www.gdpu.edu.cn/'target='_blank'>Guangdong Pharmaceutical University</a> |",
                                "<a href='https://beian.miit.gov.cn/' target='_blank'>黑ICP备2024016624</a>",
                                "</p>"))
    )
  )
)
