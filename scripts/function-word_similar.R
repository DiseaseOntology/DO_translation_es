
#' @param x, y A character vector.
#' @param ignore A logical vector indicating which values of x & y to ignore;
#' `FALSE` (default) means ignore nothing.
#' @param stopwords A character vector of stop words (words to be ignored).
#' @param stem_word Whether to perform word stemming or not, as a boolean
#' (default: `FALSE`)
#' @inheritDotParams tokenizers::tokenize_words
word_similar <- function(x, y, ignore = FALSE, stopwords = NULL,
                         stem_words = FALSE, ...) {
    stopifnot("length of `y` must be the same as `x`" = length(y) == length(x))

    xtest <- x[!ignore]
    ytest <- y[!ignore]

    if (stem_words) {
        xw <- tokenizers::tokenize_word_stems(xtest, stopwords = stopwords, ...)
        yw <- tokenizers::tokenize_word_stems(ytest, stopwords = stopwords, ...)
    } else {
        xw <- tokenizers::tokenize_words(xtest, stopwords = stopwords, ...)
        yw <- tokenizers::tokenize_words(ytest, stopwords = stopwords, ...)
    }

    pct_sim <- purrr::map2_dbl(
        xw,
        yw,
        function(.x, .y, .a) {
            round(length(intersect(.x, .y)) / length(union(.x, .y)) * 100, 2)
        }
    ) |>
        purrr::set_names(xtest)

    unname(pct_sim[x])
}
