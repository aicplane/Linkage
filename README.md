# Linkage: An R shiny web server for linking of ATAC-seq to gene expression data

::: {align="right"}
![](man/figures/Linkage.png){width="172"}
:::

<!-- badges: start -->

[![R-CMD-check](https://github.com/aicplane/Linkage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aicplane/Linkage/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

::: {align="justify"}
Linkage is a user-friendly, interactive, open-source R-Shiny web application for exploring and visualizing potential gene cis-regulatory elements (CREs) based on **ATAC-seq** and **RNA-seq** data. Users can upload customized data or re-analysis public datasets, then obtain genome-wide CREs with simple clicks. All the CREs are predicted from multi-omics sequencing data, rather than being experimentally determined. The main feature for Linkage is to identify potential CREs for the whole genome by performing canonical correlation analysis between each quantitative chromatin accessibility measure and the quantitative expression level across all samples. Additional modules are developed to allow users to perform more systematic and deeper analysis for the gene regulatory landscape.
:::

::: {align="justify"}
The Shiny application is additionally hosted at <https://xulabgdpu.org.cn/linkage>.
:::

![](man/figures/pinpeline.png)
