![](man/figures/shiny-logo1.png)

[![test-coverage](https://github.com/aicplane/Linkage/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/aicplane/Linkage/actions/workflows/test-coverage.yaml) [![pkgdown](https://github.com/aicplane/Linkage/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/aicplane/Linkage/actions/workflows/pkgdown.yaml) [![](https://img.shields.io/badge/shiny-linkage-green.svg)](https://xulabgdpu.org.cn/linkage/) [![](https://img.shields.io/badge/doi-10.1101/2024.04.24.590756-green.svg)](https://doi.org/10.1101/2024.04.24.590756) [![](https://img.shields.io/badge/devel%20version-1.2.0-green.svg)](https://github.com/linkage)

<img src="man/figures/imgfile.png" align="right" height="200" style="float:right; height:200px;"/>

Linkage is a user-friendly, interactive, open-source R-Shiny web application for exploring and visualizing potential gene cis-regulatory elements (CREs) based on ATAC-seq and RNA-seq data. Users can upload customized data or re-analysis public datasets, then obtain genome-wide CREs with simple clicks. All the CREs are predicted from multi-omics sequencing data, rather than being experimentally determined. The main feature for Linkage is to identify potential CREs for the whole genome by performing canonical correlation analysis between each quantitative chromatin accessibility measure and the quantitative expression level across all samples. Additional modules are developed to allow users to perform more systematic and deeper analysis for the gene regulatory landscape.

![](man/figures/pinpeline.png) The Shiny application is additionally hosted at <https://xulabgdpu.org.cn/linkage>.
