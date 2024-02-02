# 动态修改默认值 -----------------------------------------------------------------
observe({
  if (is.null(input$data)) {
    return()
  }
  switch(
    input$data,
    "1" = updateSelectInput(session, "From",
                            choices = list("ENSEMBL")),
    "2" = updateSelectInput(session, "From",
                            choices = list("ENSEMBL")),
    "3" = updateSelectInput(session, "From",
                            choices = list("ENSEMBL",
                                           "SYMBOL",
                                           "ENTREZID"))
  )
})


observe({
  if (input$Species == "1") {
    if (is.null(input$geneid_method)) {
      return()
    }
    switch(
      input$geneid_method,
      "SYMBOL" = updateTextInput(session, "geneid",
                                 value = "DPM1"),
      "ENSEMBL" = updateTextInput(session, "geneid",
                                  value = "ENSG00000000419"),
      "ENTREZID" = updateTextInput(session, "geneid",
                                   value = "8813")
    )
  }
  if (input$Species == "2") {
    if (is.null(input$geneid_method)) {
      return()
    }
    switch(
      input$geneid_method,
      "SYMBOL" = updateTextInput(session, "geneid",
                                 value = "Acat3"),
      "ENSEMBL" = updateTextInput(session, "geneid",
                                  value = "ENSMUSG00000062480"),
      "ENTREZID" = updateTextInput(session, "geneid",
                                   value = "224530")
    )
  }
})

observe({
  if (is.null(input$filter_method)) {
    return()
  }
  switch(
    input$filter_method,
    "p_value" = updateTextInput(session, "value",
                                value = "0.01"),
    "FDR" = updateTextInput(session, "value",
                            value = "0.01"),
    "rho" = updateTextInput(session, "value",
                            value = "0.5")
  )
})

observe({
  if (is.null(input$fil_method)) {
    return()
  }
  switch(
    input$fil_method,
    "p_value" = updateTextInput(session, "network_peak_filter",
                                value = "0.01"),
    "FDR" = updateTextInput(session, "network_peak_filter",
                            value = "0.01"),
    "rho" = updateTextInput(session, "network_peak_filter",
                            value = "0.5")
  )
})

observe({
  if (input$Species == "1") {
    if (is.null(input$genelist_idtype)) {
      return()
    }
    switch(
      input$genelist_idtype,
      "1" = updateTextAreaInput(
        session,
        "gene_list",
        value = "ENSG00000000003
ENSG00000000005
ENSG00000000419
ENSG00000000457
ENSG00000000460
ENSG00000000938
ENSG00000000971
ENSG00000001036
ENSG00000001084
ENSG00000001461"
      ),
      "2" = updateTextAreaInput(
        session,
        "gene_list",
        value = "TSPAN6
TNMD
DPM1
SCYL3
C1orf112
FGR
CFH
FUCA2
GCLC
NFYA"
      ),
      "3" = updateTextAreaInput(
        session,
        "gene_list",
        value = "7105
64102
8813
57147
55732
2268
3075
2519
2729
4800"
      )
    )
  }
  if (input$Species == "2") {
    if (is.null(input$genelist_idtype)) {
      return()
    }
    switch(
      input$genelist_idtype,
      "1" = updateTextAreaInput(
        session,
        "gene_list",
        value = "ENSMUSG00000014603
ENSMUSG00000015665
ENSMUSG00000023906
ENSMUSG00000024224
ENSMUSG00000035930
ENSMUSG00000047518
ENSMUSG00000050343
ENSMUSG00000062480
ENSMUSG00000071234
ENSMUSG00000075286
ENSMUSG00000077780
ENSMUSG00000087097
ENSMUSG00000087393
ENSMUSG00000088478
ENSMUSG00000089168
ENSMUSG00000092981
ENSMUSG00000094806
ENSMUSG00000097529
ENSMUSG00000102106
"
      ),
      "2" = updateTextAreaInput(
        session,
        "gene_list",
        value = "Alx3
Awat1
Cldn6
Clpsl2
Chst4
Slfnl1
Or1ad6
Acat3
Syndig1l
Gm1968
Gm24161
4930586N03Rik
1700023C21Rik
Mir1946b
Gm24128
Mir5125
Cyp2d10
Rp31-ps19
2310043O21Rik
"
      ),
      "3" = updateTextAreaInput(
        session,
        "gene_list",
        value = "11694
245533
54419
328788
26887
194219
258912
224530
627191
328657
115486256
75887
73257
100316714
115488610
100628593
13101
100039275
69679
"
      )
    )
  }
})
observe({
  if (input$Species == "1") {
    if (is.null(input$To)) {
      return()
    }
    switch(
      input$To,
      "ENSEMBL" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "ENSG00000000003
ENSG00000000005
ENSG00000000419
ENSG00000000457
ENSG00000000460
ENSG00000000938
ENSG00000000971
ENSG00000001036
ENSG00000001084
ENSG00000001461"
      ),
      "SYMBOL" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "PGF
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
MMP9
"
      ),
      "ENTREZID" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "7105
64102
8813
57147
55732
2268
3075
2519
2729
4800"
      )
    )
  }
  if (input$Species == "2") {
    if (is.null(input$To)) {
      return()
    }
    switch(
      input$To,
      "ENSEMBL" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "ENSMUSG00000051951
ENSMUSG00000025900
ENSMUSG00000025902
ENSMUSG00000033845
ENSMUSG00000025903
ENSMUSG00000033813
ENSMUSG00000002459
ENSMUSG00000085623
ENSMUSG00000033793
ENSMUSG00000025905"
      ),
      "SYMBOL" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "Pgf
Ctsb
Edn1
Dkk1
Fgf1
Spx
Serpine2
Il15
Il2
Plau
Igf1
Igfbp4
Spp1
Sema3f
Ang
Ang2
Ang4
C3
Bmp6
Fgf2
Pappa
Il18
Angpt1
Ccl20
Vegfc
Tnf
Igfbp7
Plat
Ccl2
Ccl8
Il6
Cxcl12
Vegfa
Ccl7
Il10
Ereg
Il7
Mif
Cxcl10
Nrg1
Cd55b
Cd55
Il13
Ang5
Ang6
Csf1
Angptl4
Csf2
Gdf15
Ccl12
Wnt16
Hgf
Timp2
Esm1
Mmp2
Igfbp6
Mmp3
Ccl4
Mmp1a
Mmp1b
Il1a
Igfbp2
Il1b
Mmp12
Ccl24
Cxcl16
Mmp13
Igfbp1
Serpine1
Igfbp3
Fgf7
Ccl3
Igfbp5
Inha
Bmp2
Vgf
Ccl3
Mmp9
"
      ),
      "ENTREZID" = updateTextAreaInput(
        session,
        "gene_list_six",
        value = "497097
19888
20671
27395
18777
21399
58175
102631647
108664
18387"
      )
    )
  }
})


# 跳转页面 --------------------------------------------------------------------
# observeEvent(input$submit, {
#   updateTabItems(session, "inTabset", selected = "one")
# })

observeEvent(input$submit2, {
  updateTabItems(session, "inTabset", selected = "tow")
})

observeEvent(input$submit3, {
  updateTabItems(session, "inTabset", selected = "three")
})

observeEvent(input$submit4, {
  updateTabItems(session, "inTabset", selected = "four")
})

observeEvent(input$submit5, {
  updateTabItems(session, "inTabset", selected = "five")
})

observeEvent(input$submit8, {
  updateTabItems(session, "inTabset", selected = "six")
})
