# Linkage

`Linkage` is a user-friendly, interactive, open-source R-Shiny web application for `exploring and visualizing potential gene regulatory elements` based on **ATAC-seq** and **RNA-seq** data. Users can upload customized data and obtain genome-wide gene regulatory elements with simple clicks. All the gene regulatory elements are predicted from multi-omics data, rather than being experimentally determined. Users can easily search, browse, and download the peak-gene scatter plot for a given gene. We will continue to update Linkage and add more modules for downstream analysis in the future.

```         
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("aicplane/Linkage")
```

```         
library(Linkage)
Install.biocViews()
Linkage()
```

The Shiny application is additionally hosted at <https://xulabgdpu.org.cn/linkage>
