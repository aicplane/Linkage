gene.list <- c("Pgf","Ctsb","Edn1","Dkk1","Fgf1","Spx","Serpine2","Il15","Il2","Plau","Igf1","Igfbp4","Spp1","Sema3f","Ang","Ang2","Ang4","C3","Bmp6","Fgf2","Pappa","Il18","Angpt1","Ccl20","Vegfc","Tnf","Igfbp7","Plat","Ccl2","Ccl8","Il6","Cxcl12","Vegfa","Ccl7","Il10","Ereg","Il7","Mif","Cxcl10","Nrg1","Cd55b","Cd55","Il13","Ang5","Ang6","Csf1","Angptl4","Csf2","Gdf15","Ccl12","Wnt16","Hgf","Timp2","Esm1","Mmp2","Igfbp6","Mmp3","Ccl4","Mmp1a","Mmp1b","Il1a","Igfbp2","Il1b","Mmp12","Ccl24","Cxcl16","Mmp13","Igfbp1","Serpine1","Igfbp3","Fgf7","Ccl3","Igfbp5","Inha","Bmp2","Vgf","Ccl3","Mmp9")
a <-
  GO.enrichment(
    gene.list,
    genelist_idtype = "external_gene_name",
    Species = "Mus",
    pvalueCutoff = 1,
    minGSSize = 0,
    qvalueCutoff = 1,
    maxGSSize = 1000
  )
