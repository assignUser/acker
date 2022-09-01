
<!-- README.md is generated from README.Rmd. Please edit that file -->

# acker

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/assignUser/acker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/assignUser/acker/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Dieses Packet stellt eine Shiny App zur Verfügung die als didaktisch
reduziertes Interface für das
[iCrop2](https://github.com/assignUser/icrop2r) Wachstumsmodell dient
und erlaubt die simulierten Daten zur weiteren Nutzung zu exportieren.

## Installation

Wenn eine R Installation vorhanden ist, kann das R Packet von
[GitHub](https://github.com/) installiert werden:

``` r
# install.packages("devtools")
devtools::install_github("assignUser/acker")
# App starten
acker::run_app()
```

Alternative stellen wir auf ghcr.io ein Docker Image zur Verfügung, dass
alle Voraussetzungen enthält:

``` bash
# Ggf. ist ein login bei ghcr.io notwendig
docker run -p 80:80 ghcr.io/assignuser/acker:latest
# Anschließend die Web App unter 127.0.0.1:80 aufrufen
```

## Code of Conduct

Please note that the acker project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
