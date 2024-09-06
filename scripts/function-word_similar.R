
#' @param x, y A character vector.
#' @param ignore A logical vector indicating which values of x & y to ignore;
#' `FALSE` (default) means ignore nothing.
#' @param stopwords A character vector of stop words (words to be ignored).
#' @inheritDotParams tokenizers::tokenize_words
word_similar <- function(x, y, ignore = FALSE, stopwords = NULL, ...) {
    stopifnot("length of `y` must be the same as `x`" = length(y) == length(x))

    xtest <- x[!ignore]
    ytest <- y[!ignore]

    # retaining stop words
    xw <- tokenizers::tokenize_words(xtest, stopwords = stopwords, ...)
    yw <- tokenizers::tokenize_words(ytest, stopwords = stopwords, ...)
    all <- purrr::map2(xw, yw, union)

    pct_sim <- purrr::pmap_dbl(
        list(xw, yw, all),
        function(.x, .y, .a) round(sum(.x %in% .y) / length(.a) * 100, 2)
    ) |>
        purrr::set_names(xtest)

    pct_sim[x]
}
