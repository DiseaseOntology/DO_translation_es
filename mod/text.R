#' Compare Regex-extracted Values from Paired Text
#'
#' Creates a string showing the regex-extracted values from two character
#' vectors, `x` and `y`, that are different or the same.
#'
#' @param x,y Character vectors from which to extract, using `regex`, and
#' compare values.
#' @param regex A regular expression to extract all matching values from `x` and
#' `y`.
#' @param as_string Whether the results should be returned as a single
#' string or as a list, as a boolean (default: `FALSE`).
#' @param delim A string to separate the values in the output string, if
#' `as_string = TRUE` (default: `" | "`).
#' @param di_delim A string to separate the different categories in the output
#' string, if `as_string = TRUE` (default: `"\n"`).
#'
#' @section PURPOSE:
#' The primary purpose for this function is in comparing untranslated elements
#' between text in two languages (e.g. gene symbols, chromosome coordinates).
#'
#' @returns A list, or a single string when `as_string = TRUE`.
#'
#' @examples
#' x <- "Eat an apple fritter, banana cream pie, or cherry tart."
#' y <- "An apple a day: still could slip on a banana peel or be cream pied in the face!!"
#'
#' # As a list
#' compare_regex_grp(x, y, regex = "app|pie|tart")
#'
#' # As a string
#' compare_regex_grp(x, y, regex = "app|pie|tart", as_string = TRUE, di_delim = ";")
#'
#' @md
#' @export
compare_regex_grp <- function(x, y, regex, as_string = FALSE,
                              delim = " | ", di_delim = "\n") {
  box::use(stringr, purrr)
  stopifnot("`x` & `y` must be the same length" = length(x) == length(y))

  # use input symbols, if any; otherwise default to `x` & `y` as names
  x_nm <- substitute(x)
  if (length(x_nm) != 1) x_nm <- "x"
  y_nm <- substitute(y)
  if (length(y_nm) != 1) y_nm <- "y"
  nms <- as.character(c(x_nm, y_nm, "both"))

  # Extract all matches from x and y using the provided regex
  x_match <- stringr::str_extract_all(x, regex)
  y_match <- stringr::str_extract_all(y, regex)

  out <- purrr::map2(
    x_match,
    y_match,
    function(.x, .y) {
      di_res <- diff_intersect(.x, .y)
      if (!as_string) return(di_res)
      di_nm_val <- purrr::map2(
        di_res,
        # add names here!!!
        nms,
        function(.a, .b) {
          .a <- stats::na.omit(.a)
          paste(.b, paste0(.a, collapse = delim), sep = ": ")
        }
      )
      paste0(di_nm_val, collapse = di_delim)
    }
  )
  if (as_string) out <- as.character(out)
  out
}

diff_intersect <- function(x, y) {
  list(
    x = setdiff(x, y),
    y = setdiff(y, x),
    both = intersect(x, y)
  )
}
