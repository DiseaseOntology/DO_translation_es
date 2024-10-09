library(readr)
library(googlesheets4)


# supporting data objects -------------------------------------------------

col_en_es <- c(
    "label" = "etiqueta", "definition" = "definición", "synonym" = "sinónimo"
)


# functions ---------------------------------------------------------------

# form cell references using specified column input (letter) and row numbers,
#   as they will appear in google sheet
form_cell_ref <- function(col_letter) {
    paste0(col_letter, dplyr::row_number() + 1)
}

# Adds Google Sheets formulas to calculate translation in both directions
#   (one column each) using the GOOGLTRANSLATE function
#
# REQUIRED: Input must have 4 columns in a set order. The order is "class",
#   a column with a single type of English data directly from DO (label,
#   definition, or synonym), "clase", and a column with the Spanish translation
#   by The Spanish Group corresponding to the English data (etiqueta,
#   definición, or sinónimo).
add_gs_translate_cols <- function(df) {
    df |>
        dplyr::mutate(
            google_translate_to_en = googlesheets4::gs4_formula(
                glue::glue(
                    '=GOOGLETRANSLATE({cell_ref}, "es", "en")',
                    cell_ref = form_cell_ref("E")
                )
            ),
            .after = 2
        ) |>
        dplyr::mutate(
            google_translate_to_es = googlesheets4::gs4_formula(
                glue::glue(
                    '=GOOGLETRANSLATE({cell_ref}, "en", "es")',
                    cell_ref = form_cell_ref("C")
                )
            ),
            .after = dplyr::last_col()
        )
}


#' Create Initial GS Translation Review
#' @param gs URL to google sheet or another identifier recognizable by
#' googledrive::as_id()
#' @param ss_prefix (OPTIONAL) A prefix to append before "en_es" & the column
#' name when deriving a sheet name for the output.
#'
#' Example:
#' - prefix = "disorder"
#' - column in data.frame that matches `col_en_es` = "definition"
#' - sheet name would become "disorder_en_es_definition"
#'
#' @returns List with information to access the new GS review, including `gs`
#' and the names of the 3 sheets created.
#'
#' information.
create_gs_review <- function(en_es_file, gs, ss_prefix) {
    en_es <- readr::read_csv(en_es_file, col_types = "c")

    col_present <- col_en_es[names(col_en_es) %in% names(en_es)]

    df_split <- purrr::map2(
        names(col_present),
        col_present,
        ~ dplyr::select(en_es, dplyr::all_of(c("class", .x, "clase", .y))) |>
            dplyr::filter(!dplyr::if_all(c(.x, .y), is.na)) |>
            add_gs_translate_cols()
    )

    ss_nm <- paste(ss_prefix, "en_es", names(col_present), sep = "_")
    purrr::walk2(
        df_split,
        ss_nm,
        ~ googlesheets4::write_sheet(data = .x, ss = gs, sheet = .y)
    )

    list(gs = gs, ss = ss_nm)
}

