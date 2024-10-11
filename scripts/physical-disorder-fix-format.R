# CSV files from translation company for physical disorder branch (first sent)
#   have an unexpected format that doesn't load correctly in spreadsheet
#   applications... this is corrected by this script.
library(readr)
library(stringr)
library(here)
library(purrr)


#  custom function --------------------------------------------------------

# convert file from ISO-8859-1 to UTF-8, CRLF to LF line endings,
#   replace specialized quote characters, and remove unusual semicolons & commas
#   at line endings (probably there because of unusual quote characters)
reformat_file <- function(input, output) {
    readr::read_lines(input, locale = readr::locale(encoding = "ISO-8859-1")) |>
        stringr::str_replace_all(
            c("\\u0093" = '"', "\\u0094(\\.?)" = '\\1"', "\\r\\n" = "\n")
        ) |>
        stringr::str_remove("[;, ]*$") |>
        paste0(collapse = "\n") |>
        readr::read_csv() |>
        readr::write_csv(output)
}


# fix & save corrected data from translated files -------------------------

pd_dir <- here::here("data/physical_disorder")
es_files_orig <- list.files(
    pd_dir,
    pattern = ".*TSG-Es-original\\.csv",
    full.names = TRUE
)
es_files <- stringr::str_remove(es_files_orig, "-original")

purrr::walk2(es_files_orig, es_files, reformat_file)



# drop accidental inclusion of English in spanish file --------------------

es_files[1] |>
    readr::read_csv(col_types = "c") |>
    dplyr::filter(!stringr::str_detect(etiqueta, "syndrome")) |>
    readr::write_csv(es_files[1])
