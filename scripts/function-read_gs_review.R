#' Read Initialized GS Translation Review
#' @inheritParams create_gs_review
#' @returns List of 3 tibbles with label, definition, and synonym translation
#' information.
read_gs_review <- function(gs, ss_prefix) {
    col_present <- names(col_en_es)
    ss_nm <- paste(ss_prefix, "en_es", col_present, sep = "_")

    purrr::map(
        ss_nm,
        ~ googlesheets4::with_gs4_quiet(
            googlesheets4::read_sheet(ss = gs, sheet = .x)
        )
    ) |> purrr::set_names(col_present)
}
