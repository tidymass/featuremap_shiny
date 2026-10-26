# featuremapshiny

`featuremapshiny` provides the interactive web interface for the FeatureMAP
workflow, covering metabolite annotation, network extraction, module
identification, and functional interpretation.

## Online application

The hosted application will be available at
[featuremap.jaspershenlab.com](https://featuremap.jaspershenlab.com).

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
  repos = BiocManager::repositories(),
  upgrade = "ask"
)
```

## Run locally

```r
library(featuremapshiny)
run_featuremap()
```

Sign-in is disabled by default, so local installs and internal lab servers need
no account setup and do not install the account packages. Only a public
deployment needs it:

1. Install the optional account packages:
   `install.packages(c("DBI", "RSQLite", "sodium"))` (on Linux, `sodium` also
   needs the system library `libsodium-dev`).
2. Set `FEATUREMAP_PUBLIC_MODE=true`. This turns sign-in on and adds the
   restrictions an open server needs: visitors must enter their own LLM and
   NCBI API keys (keys in the server's environment are never used), and the
   "Local" LLM provider is hidden, since its base URL would let visitors make
   the server call arbitrary addresses. To require sign-in without those
   restrictions, set `FEATUREMAP_AUTH_ENABLED=true` instead.
3. Set `FEATUREMAP_PUBLIC_URL` to the address visitors open, and
   `FEATUREMAP_SMTP_PASSWORD` (plus the other SMTP settings if not using the
   defaults) for verification and password-reset emails.
4. Optionally set `FEATUREMAP_AUTH_DB` to a persistent SQLite path.

With sign-in switched on, the app checks all of this at launch and refuses to
start, listing what is missing, rather than failing at the first sign-up.
Detailed server instructions are available in
[`deploy/README.md`](deploy/README.md).

## Tutorial

Input formats, complete workflows, result interpretation, and instructions for
both the FeatureMAP R package and Shiny application are provided in the
[FeatureMAP tutorial](https://www.tidymass.org/featuremap_tutorial).

## Related project

The FeatureMAP R package is available from the
[FeatureMAP repository](https://github.com/tidymass/featuremap).
