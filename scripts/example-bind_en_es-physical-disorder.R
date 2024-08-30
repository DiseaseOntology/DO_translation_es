# Example script for combining EN and ES files using physical disorder files)
# ASSUMPTIONS:
#   1. File order (due to file numbering/naming) will be the same for equivalent
#       English & Spanish files.
#   2. Row order of data will be the same for equivalent English & Spanish
#       files.

library(here)
library(purrr)
library(readr)
source(here::here("scripts/function-bind_en_es.R"))

pd_dir <- here::here("data/physical_disorder")
en_files <- list.files(pd_dir, pattern = ".*En.csv", full.names = TRUE)
es_files <- list.files(pd_dir, pattern = ".*TSG-Es.csv", full.names = TRUE)
en_es_files <- stringr::str_replace(en_files, "En", "En-Es")

en_es <- purrr::pmap(
    list(en_files, es_files, en_es_files),
    function(.en, .es, .out) bind_en_es(.en, .es, .out)
)
