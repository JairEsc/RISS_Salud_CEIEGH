FROM rocker/geospatial:4.5.0 AS builder
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install -y --no-install-recommends make pandoc libpq-dev libcurl4-openssl-dev libssl-dev libicu-dev libxml2-dev libx11-dev git cmake libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev zlib1g-dev libglpk-dev libjq-dev libprotobuf-dev protobuf-compiler libprotoc-dev libpng-dev libsodium-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.3")'
WORKDIR /src
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'

FROM rocker/shiny:4.5.0 AS runtime
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install -y --no-install-recommends libpq-dev libcurl4-openssl-dev libssl-dev libicu-dev libxml2-dev libx11-dev git cmake libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev zlib1g-dev libglpk-dev libjq-dev libprotobuf-dev protobuf-compiler libprotoc-dev libpng-dev libsodium-dev pandoc && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/
EXPOSE 8056
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=8056)'
