#!/bin/sh -l

apt-get update
apt-get install -y -qq r-cran-dplyr r-cran-rio r-cran-tidyr r-cran-devtools

Rscript process.R