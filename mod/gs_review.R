#' Create Initial GS Translation Review
#'
#' Create a Google Sheets review file for comparing the translation of English
#' to Spanish (español) data. The review file will contain 3 sheets, one for
#' each type of data (label, definition, synonym) and will include columns for
#' Google Translate formulas in both directions.
#'
#' @param gs URL to google sheet or another identifier recognizable by
#' googledrive::as_id().
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
#' @export
create_gs_review <- function(en_es_file, gs, ss_prefix = NULL) {
    box::use(
        readr[read_csv],
        purrr[map2, walk2],
        dplyr[select, filter, all_of, if_all],
        googlesheets4[write_sheet],
        ./data[col_en_es]
    )

    en_es <- read_csv(en_es_file, col_types = "c")

    col_present <- col_en_es[names(col_en_es) %in% names(en_es)]

    df_split <- map2(
        names(col_present),
        col_present,
        ~ select(en_es, all_of(c("class", .x, "clase", .y))) |>
            filter(!if_all(c(.x, .y), is.na)) |>
            add_gs_translate_cols()
    )

    ss_nm <- paste(ss_prefix, "en_es", names(col_present), sep = "_")
    walk2(
        df_split,
        ss_nm,
        ~ write_sheet(data = .x, ss = gs, sheet = .y)
    )

    list(gs = gs, ss = ss_nm)
}


#' Read Initialized GS Translation Review
#'
#' Read the Google Sheets review file created by `create_gs_review()` (data
#' may have been updated since the initial creation).
#'
#' @inheritParams create_gs_review
#' @returns List of 3 `tibble`s with label, definition, and synonym translation
#' information.
#'
#' @export
read_gs_review <- function(gs, ss_prefix) {
    box::use(
        ./data[col_en_es],
        purrr[map, set_names],
        googlesheets4[with_gs4_quiet, read_sheet]
    )

    col_present <- names(col_en_es)
    ss_nm <- paste(ss_prefix, "en_es", col_present, sep = "_")

    map(ss_nm, ~ with_gs4_quiet(read_sheet(ss = gs, sheet = .x))) |>
        set_names(col_present)
}


### create_gs_review() helpers ###############################################

#' Add GS Translation Columns
#'
#' Adds columns with Google Sheets formulas to calculate translation in both
#' directions (one column each) using the GOOGLTRANSLATE function.
#'
#' @section REQUIRED:
#' Input must have 4 columns in a set order. The order is "class", a column with
#' a single type of English data directly from DO (label, definition, or
#' synonym), "clase", and a column with the Spanish translation by "The Spanish
#' Group" (TSG) corresponding to the English data (etiqueta, definición, or
#' sinónimo).
add_gs_translate_cols <- function(df) {
    box::use(
        dplyr[mutate, last_col],
        gs4 = googlesheets4,
        glue[glue]
    )

    df |>
        mutate(
            google_translate_to_en = gs4$gs4_formula(
                glue(
                    '=GOOGLETRANSLATE({cell_ref}, "es", "en")',
                    cell_ref = form_cell_ref("E")
                )
            ),
            .after = 2
        ) |>
        mutate(
            google_translate_to_es = gs4$gs4_formula(
               glue(
                    '=GOOGLETRANSLATE({cell_ref}, "en", "es")',
                    cell_ref = form_cell_ref("C")
                )
            ),
            .after = last_col()
        )
}

#' Form GS Cell References
#'
#' Form cell references using specified column input (letter) and row numbers,
#' as they will appear in google sheet.
#'
#' @param col_letter The column letter to use in the cell reference.
#' @param w_title Whether a title row will be present in the sheet (default:
#' `TRUE`). _Adjusts row numbers down 1._
form_cell_ref <- function(col_letter, w_title = TRUE) {
    box::use(dplyr[row_number])
    if (w_title) {
        paste0(col_letter, row_number() + 1)
    } else {
        paste0(col_letter, row_number())
    }
}


### read_gs_review() helpers #################################################

#' Automatically Identify & Read TSV/CSV files
#'
#' A light wrapper around [readr::read_delim()] that automatically identifies
#' the delimiter based on file extension (can include compression extensions).
#'
#' Note that this function is primarily intended for internal use; therefore,
#' messages about guessed column types are not generated.
#'
#' @param file The path to a csv/tsv file or connection.
#'
#' Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will be automatically
#' uncompressed. Files starting with `http://`, `https://`, `ftp://`, or
#' or `ftps://` will be automatically downloaded.
#'
#' @inheritParams readr::read_delim
#' @inheritDotParams readr::read_delim -delim -quoted_na
read_delim_auto <- function(file, ..., show_col_types = FALSE) {
    box::use(
        stringr[str_match],
        readr[read_delim]
    )
    ext <- str_match(file, "\\.([tc]sv)(\\.(gz|bz2|xz|zip))?")[,2]
    delim <- switch(ext, tsv = "\t", csv = ",")
    if (is.null(delim)) stop("`file` must have .tsv or .csv extension.")

    read_delim(
        file = file,
        delim = delim,
        show_col_types = show_col_types,
        ...
    )
}
