library(readr)
library(googlesheets4)


# supporting data objects -------------------------------------------------

col_en_es <- c(
    "label" = "etiqueta", "definition" = "definición", "synonym" = "sinónimo"
)



# functions ---------------------------------------------------------------

#' @param gs URL to google sheet or another identifier recognizable by
#' googledrive::as_id()
#' @param ss_prefix (OPTIONAL) A prefix to append before "en_es" & the column
#' name when deriving a sheet name for the output.
#'
#' Example:
#' - prefix = "disorder"
#' - column in data.frame that matches `col_en_es` = "definition"
#' - sheet name would become "disorder-definition"
create_gs_review <- function(en_es_file, gs, ss_prefix) {
    en_es <- readr::read_csv(en_es_file, col_types = "c")

    col_present <- col_en_es[names(col_en_es) %in% names(en_es)]

    df_split <- purrr::map2(
        names(col_present),
        col_present,
        ~ dplyr::select(
            en_es,
            dplyr::all_of(c("class", .x, "clase", .y))
        )
    )

    ss_nm <- paste(ss_prefix, "en_es", names(col_present), sep = "_")
    purrr::walk2(
        df_split,
        ss_nm,
        ~ googlesheets4::write_sheet(data = .x, ss = gs, sheet = .y)
    )
}

