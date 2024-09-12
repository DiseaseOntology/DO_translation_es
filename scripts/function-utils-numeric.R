#' Generate Summary of Columns
#'
#' `digits` is passed to [round()]
#' @inheritParams round
col_summary <- function(df, cols, digits = NULL) {
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

    out <- dplyr::reframe(
        df,
        dplyr::across({{ cols }}, .fn)
    )

    if (nrow(out) == 7) val_nm <- append(val_nm, "NA")
    out <- dplyr::mutate(out, stat = val_nm, .before = 1) |>
        # move median after quantiles
        dplyr::arrange(
            factor(
                .data$stat,
                levels = c("min", "q25", "mean", "q75", "max", "median")
            )
        )
    out
}

#' Test for whole numbers
is_whole_number <- function(x, tol = .Machine$double.eps)  {
    stopifnot("`x` must be a number" = is.numeric(x))
    abs(x - round(x)) < tol
}
