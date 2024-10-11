# Example script for combining EN and ES files and generating a Google Sheet
# for review, including the automatic addition of GOOGLETRANSLATE formulas,
# using physical disorder files -- first submitted to TSG company
#
# ASSUMPTIONS:
#   1. File order (due to file numbering/naming) will be the same for equivalent
#       English & Spanish files.
#   2. Row order of data will be the same for equivalent English & Spanish
#       files. A basic check comparing class URI across languages is included.

box::use(
    here,
    purrr,
    readr,
    stringr,
    ../mod/bind[bind_en_es],
    ../mod/gs_review[create_gs_review]
)

pd_dir <- here$here("data/physical_disorder")
en_files <- list.files(pd_dir, pattern = ".*En.csv", full.names = TRUE)
es_files <- list.files(pd_dir, pattern = ".*TSG-Es.csv", full.names = TRUE)
en_es_files <- stringr$str_replace(en_files, "En", "En-Es")

en_es <- purrr$pmap(
    list(en_files, es_files, en_es_files),
    function(.en, .es, .out) bind_en_es(.en, .es, .out)
)

gs_out <- purrr$map(
    en_es_files,
    ~ create_gs_review(
        .x,
        gs = "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg",
        "disorder"
    )
)
