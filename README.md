# featuremapshiny

Install:

```r
# install.packages("remotes")
remotes::install_github("tidymass/featuremap_shiny")
```

Run the app:

```r
library(featuremapshiny)
featuremapshiny::run_featuremap()
```

Authentication is disabled by default for local installs. For a public
deployment, set `FEATUREMAP_AUTH_ENABLED=true` and point `FEATUREMAP_AUTH_DB`
to a persistent SQLite path before starting the app. Email verification and
password reset use SMTP; see `deploy/README.md` for the required mail settings.
