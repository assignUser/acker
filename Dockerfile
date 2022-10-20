FROM ghcr.io/rocker-org/geospatial:4.2.1-ubuntugis

RUN Rscript -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'

ADD . /app

RUN sudo apt update && sudo apt upgrade -y

RUN Rscript -e ' \
  pak::local_system_requirements(root = "/app", os = "ubuntu", os_release = "20.04", execute = TRUE); \
  pak::local_install(root = "/app", upgrade = FALSE, ask = FALSE) \
  '  

EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');acker::run_app()"
