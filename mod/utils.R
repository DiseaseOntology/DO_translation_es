#' Generate Summary of Columns
#'
#' `digits` is passed to [round()]
#' @inheritParams round
col_summary <- function(.df, .cols, digits = NULL) {
    box::use(dplyr[reframe, across, mutate, arrange])

    val_nm <- c("min", "q25", "median", "mean", "q75", "max")
    if (is.null(digits)) {
        .fn <- function(.x) { as.numeric(summary(.x)) }
    } else {
        stopifnot(
            "`digits` must be a whole number" = is_whole_number(digits),
            "`digits` must be a scalar" = length(digits) == 1
        )
        .fn <- function(.x) { round(as.numeric(summary(.x)), digits = digits) }
    }

    out <- reframe(.df, across({{ .cols }}, .fn))

    if (nrow(out) == 7) val_nm <- append(val_nm, "NA")
    out <- mutate(out, stat = val_nm, .before = 1) |>
        # move median after quantiles
        arrange(
            factor(
                .data$stat,
                levels = c("min", "q25", "mean", "q75", "max", "median")
            )
        )
    out
}

#' Test for whole numbers
#' @param x A numeric vector.
#' @param tol Tolerance for comparison.
is_whole_number <- function(x, tol = .Machine$double.eps)  {
    stopifnot("`x` must be a number" = is.numeric(x))
    abs(x - round(x)) < tol
}
