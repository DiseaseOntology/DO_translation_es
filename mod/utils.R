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


#' Generate All Possible Combinations
#'
#' Generate all possible combinations of input. See `Details` for a brief
#' description of how each function handles inputs. How they work is best
#' understood in the `Examples`.
#'
#' @param x,y A vector or list.
#'
#' @returns A list with all possible combinations of the elements as described
#' in `Details`.
#'
#' @section Details:
#' - `combn_all()` treats all first level inputs as entities but peels off
#' exactly one layer of inner lists _after_ combining them.
#' - `combn_xy()` executes a cross join of its inputs and then merges the
#' resulting elements together. Essentially, it does the same
#' crosses as `combn_all()` but does not include the original elements in the
#' output. The output is ordered such that every element in `y` is crossed with
#' the first element in `x`, then the second element in `x`, and so on.
#' `combn_xy()` is very much like [dplyr::cross_join()] followed by merging
#' across rows, except `combn_xy` _always_ treats inputs as lists.
#'
#' @examples
#' ### combn_all() examples ###
#'
#' # vectors & 1-element, 1-deep lists produce the same results
#' x <- 1:2
#' l <- as.list(1:2)
#' combn_all(x)    # list(1, 2, c(1, 2))
#' combn_all(l)    # list(1, 2, c(1, 2))
#'
#' # in deeper lists and 1-deep lists with longer vectors each inner element is
#' maintained and combined with c()
#' x <- list(
#'     list(1, 2), # element1
#'     list(3, 4)  # element2
#' )
#' combn_all(x)  # list(list(1, 2), list(3, 4), list(1, 2, 3, 4))
#'
#'
#' ### combn_xy() examples ###
#'
#' # Same as combn_all(x), except original inputs are not preserved
#' combn_xy(1, 2)
#' combn_xy(list(1), list(2))
#'
#' # Easiest to see with 2 lists of vectors
#' x <- list(a = 1:2, b = 3:4)
#' y <- list(c = 5:6, d = 7:8)
#' combn_xy(x, y)    # list(1, 2, c(1, 2))
#'
#' @md
#' @export
combn_all <- function(x) {
    box::use(purrr, utils)
    n <- length(x)
    if (n == 0) return(NULL) # prevents expanded lists with NULL elements
    index <- purrr$map(1:n, ~ utils$combn(n, .x, simplify = FALSE)) |>
        unlist(recursive = FALSE)
    purrr$map(index, ~ unlist(x[.x], recursive = FALSE))
}

#' @rdname combn_all
#' @export
combn_xy <- function(x, y) {
    box::use(purrr)

    purrr$map(
        x,
        ~ purrr$map(y, function(.y) c(.x, .y))
    ) |>
        unlist(recursive = FALSE)
}


#' Calculate Percent Match
#'
#' Calculates the percent match between two input vectors or pairwise
#' across two 1-deep lists. Percent match is calculated as the ratio of the
#' intersection of the two sets to the length of the longer set * 100.
#'
#' @param x,y A vector or 1-deep list of vectors to calculate percent match
#' between.
#' @inheritParams round
#'
#' @returns A numeric vector of percent match values.
#' @examples
#' pct_match(1:2, 2:3)                           # 50
#' pct_match(c("a", "b", "c"), c("c", "d"))      # 33.33
#' pct_match(list(1:2, 1), list(2:3, 1:5))       # c(50, 10)
#'
#' @md
#' @export
pct_match <- function(x, y, digits = 2) {
    if (class(x) != class(y)) stop("`x` and `y` must be the same class")
    if (is.list(x) && length(x) != length(y)) {
        stop("If lists, `x` and `y` must be the same length")
    }
    box::use(purrr)

    pct_fn <- function(.x, .y) {
        max_len <- max(c(length(.x), length(.y)))
        round(length(intersect(.x, .y)) / max_len * 100, digits = digits)
    }

    if (is.list(x)) {
        out <- purrr$map2(x, y, ~ purrr$map2_dbl(.x, .y, pct_fn))
    } else {
        out <- pct_fn(x, y)
    }

    out
}


### TESTS ####################################################################

if (is.null(box::name())) {
    box::use(testthat[...])

    # combn_all() tests ------------------------------------------------------
    test_that("combn_all() works for vectors", {
        expect_equal(combn_all(1:2), list(1, 2, c(1, 2)))
        expect_equal(
            combn_all(1:3),
            list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3), c(1, 2, 3))
        )
    })

    test_that("combn_all() works for simple lists", {
        expect_equal(combn_all(as.list(1:2)), list(1, 2, c(1, 2)))
        expect_equal(
            combn_all(as.list(1:3)),
            list(1, 2, 3, c(1, 2), c(1, 3), c(2, 3), c(1, 2, 3))
        )
    })

    test_that("combn_all() works for complex lists", {
        # elements are on individual lines (x & y) => output: list(x, y, c(x, y))
        .l <- list(
            list(list(1, 2), list(3, 4)), # x -> has 2 list elements, each with 2 list elements
            list(5, 6) # y -> has 2 list elements
        )
        .expect <- list(
            list(list(1, 2), list(3, 4)), # x
            list(5, 6), # y
            list(list(1, 2), list(3, 4), 5, 6) # c(x, y) -> element level maintained
        )
        expect_equal(combn_all(.l), .expect)
    })
}
