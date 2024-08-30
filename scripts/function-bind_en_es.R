# functions for combining equivalent English and Spanish files.

# rename abbreviated columns to full name in given language
rename_abbrev <- function(df, lang = "en") {
    lang <- match.arg(lang, choices = c("en", "es"))

    if (lang == "en") {
        nm_recode <- c(synonym = "syn", definition = "def")
    }
    if (lang == "es") {
        nm_recode <- c(sinónimo = "syn", definición = "def")
    }

    dplyr::rename(df, nm_recode[nm_recode %in% names(df)])
}

# bind language data.frames
# test: it is assumed that the order of English and español files are the
#   same; a simple check is performed to ensure classes from each language file
#   match up across a row and an error produced if there are any that don't
bind_en_es <- function(en_file, es_file, out_file) {
    en <- readr::read_csv(en_file, col_types = "c") |>
        rename_abbrev("en")
    es <- readr::read_csv(es_file, col_types = "c") |>
        rename_abbrev("es")
    out <- dplyr::bind_cols(en, es) |>
        dplyr::mutate(class_match = class == clase)

    not_match <- sum(!out$class_match)
    if (not_match > 0) {
        stop(
            paste0("Some classes do not match across languages: ", not_match)
        )
    }

    readr::write_csv(out, out_file)
    out
}
