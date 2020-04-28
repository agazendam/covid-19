FROM rocker/r-apt:trusty
# Any version more recent that trusty results in problems with dataframes vs. tibbles in R - see https://cran.r-project.org/web/packages/tibble/news/news.html and https://tibble.tidyverse.org/dev/articles/invariants.html

COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]