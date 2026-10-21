# featuremapshiny

`featuremapshiny` provides the interactive web interface for the FeatureMAP
workflow, covering metabolite annotation, network extraction, module
identification, and functional interpretation.

## Online application

The hosted application will be available at
[featuremap_shiny.jaspershenlab.com](https://featuremap_shiny.jaspershenlab.com).

## Installation

Install `BiocManager` and `remotes` first:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
```

Install the upstream packages from GitHub:

```r
remotes::install_github("tidymass/masstools")
remotes::install_github("tidymass/massdataset")
remotes::install_github("tidymass/metid")
remotes::install_github("tidymass/pcsf2featuremap")
```

Then install `featuremapshiny` and its remaining dependencies:

```r
remotes::install_github(
  "tidymass/featuremap_shiny",
  dependencies = TRUE,
  repos = BiocManager::repositories(),
  upgrade = "ask"
)
```

## Run locally

```r
library(featuremapshiny)
run_featuremap()
```

Authentication is disabled by default for local use. For a public deployment,
set `FEATUREMAP_AUTH_ENABLED=true`, configure `FEATUREMAP_AUTH_DB` to use a
persistent SQLite path, and provide the required SMTP settings for email
verification and password reset. Detailed server instructions are available in
[`deploy/README.md`](deploy/README.md).

## Tutorial

Input formats, complete workflows, result interpretation, and instructions for
both the FeatureMAP R package and Shiny application are provided in the
[FeatureMAP tutorial](https://tidymass.github.io/featuremap_tutorial/).

## Related project

The FeatureMAP R package is available from the
[FeatureMAP repository](https://github.com/tidymass/featuremap).
