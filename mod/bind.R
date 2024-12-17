# functions for combining equivalent English and Spanish files.

#' Bind Language data.frames
#'
#' Bind English and Spanish data.frames together, checking that classes match.
#'
#' @param en_file The English data file.
#' @param es_file The Spanish data file.
#' @param out_file The output file.
#'
#' @section Assumptions:
#' It is assumed that the order of the data in the English and Spanish (español)
#' files is the same. A simple check is performed to ensure classes from each
#' language file match up across a row and an error produced if there are any
#' that don't.
bind_en_es <- function(en_file, es_file, out_file) {
    box::use(dplyr, readr)

    en <- readr$read_csv(en_file, col_types = "c") |>
        rename_abbrev("en")
    es <- readr$read_csv(es_file, col_types = "c") |>
        rename_abbrev("es")
    out <- bind_cols(en, es) |>
        dplyr$mutate(class_match = class == clase)

    not_match <- sum(!out$class_match)
    if (not_match > 0) {
        stop(
            paste0("Some classes do not match across languages: ", not_match)
        )
    }

    readr$write_csv(out, out_file)
    out
}


#' Rename Abbreviated Columns
#'
#' Rename abbreviated columns with full name in specified language (en/es only).
#' @param .df A data.frame with abbreviated column names.
#' @param lang The language for the full name (default: "en").
#'
#' @section Notes:
#' Original English data extracted from the Human Disease Ontology for
#' translation by "The Spanish Group" (TSG) used abbreviations for
#' 'synonym' (syn) and 'definition' (def) as column names.
rename_abbrev <- function(.df, lang = "en") {
    box::use(dplyr)

    lang <- match.arg(lang, choices = c("en", "es"))

    if (lang == "en") {
        nm_recode <- c(synonym = "syn", definition = "def")
    }
    if (lang == "es") {
        nm_recode <- c(sinónimo = "syn", definición = "def")
    }

    dplyr$rename(.df, nm_recode[nm_recode %in% names(.df)])
}
