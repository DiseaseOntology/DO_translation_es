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
    ext <- stringr::str_match(file, "\\.([tc]sv)(\\.(gz|bz2|xz|zip))?")[,2]
    delim <- switch(ext, tsv = "\t", csv = ",")
    if (is.null(delim)) rlang::abort("`file` must have .tsv or .csv extension.")

    readr::read_delim(
        file = file,
        delim = delim,
        show_col_types = show_col_types,
        ...
    )
}
