#' String Comparison
#'
#' Compare strings in 2 vectors and return the type of match (i.e. 'exact' or
#' one or more of 'numeral', 'case', 'space', 'punct', 'diacritic' with multiple
#' delimited).
#'
#' @param x,y A character vector.
#' @inheritParams str_mutate
#' @param delim The separator to use when listing transformations needed to
#' make the two strings identical (default: `"|"`).
#' @inheritParams stringr::str_equal
#' @param ... Arguments passed on to [stringr::str_equal()].
#'
#' @returns A character vector with the type of match.
#'
#' @md
#' @export
str_compare <- function(x, y, how = "all", delim = "|", locale = "en", ...) {
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))
    box::use(
        stringr, purrr, dplyr,
        ./mutate
    )

    # mutate strings
    xmut <- mutate$str_mutate_cum(x, how, locale = locale)
    ymut <- mutate$str_mutate_cum(y, how, locale = locale)

    # ensure comparison is done on the same transformed strings by using names
    comparison <- purrr$map(
        names(xmut),
        function(.nm) {
            .how <- stringr$str_replace_all(
                .nm,
                c("^x$" = "exact", "^x_" = "", "_" = "|")
            )
            lgl <- stringr$str_equal(
                xmut[[.nm]],
                ymut[[.nm]],
                locale = locale,
                ...
            )
            dplyr$if_else(lgl, .how, NA_character_)
        }
    )

    out <- dplyr$coalesce(!!!comparison)
    out
}


#' Describe String Similarity
#'
#' Compares strings in 2 vectors and returns the type of match (i.e. 'exact' or
#' one or more of 'case', 'space', 'punct', 'num_format' with multiple pipe
#' delimited).
#'
#' @param x,y A character vector.
#'
#' @export
str_similar <- function(x, y) {
    box::use(
        tibble[tibble],
        dplyr[mutate, case_when, across, if_else, ends_with],
        stringr[str_to_lower, str_remove_all],
    )
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))

    df <- tibble(
        x = x,
        y = y,
        comp = if_else(x == y, "exact", NA_character_)
    )

    # single conversion tests
    df <- df |>
        mutate(
            across(
                .cols = c(x, y),
                .fns = list(
                    lc = ~ str_to_lower(.x),
                    space = ~ str_remove_all(.x, "[[:space:]]+"),
                    punct = ~ str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = case_when(
                !is.na(comp) ~ comp,
                x_lc == y_lc ~ "case",
                x_space == y_space ~ "space",
                x_punct == y_punct ~ "punct",
                x_num == y_num ~ "num_format",
                .default = NA_character_
            )
        )

    # double conversion tests
    df <- df |>
        mutate(
            across(
                .cols = ends_with("lc"),
                .fns = list(
                    space = ~ str_remove_all(.x, "[[:space:]]+"),
                    punct = ~ str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            across(
                .cols = ends_with("space"),
                .fns = list(
                    punct = ~ str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            across(
                .cols = ends_with("punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = case_when(
                !is.na(comp) ~ comp,
                x_lc_space == y_lc_space ~ "case|space",
                x_lc_punct == y_lc_punct ~ "case|punct",
                x_lc_num == y_lc_num ~ "case|num_format",
                x_space_punct == y_space_punct ~ "space|punct",
                x_space_num == y_space_num ~ "space|num_format",
                x_punct_num == y_punct_num ~ "punct|num_format",
                .default = NA_character_
            )
        )

    # triple conversion tests
    df <- df |>
        mutate(
            across(
                .cols = ends_with("lc_space"),
                .fns = list(
                    punct = ~ str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            across(
                .cols = ends_with("lc_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            across(
                .cols = ends_with("space_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),

            comp = case_when(
                !is.na(comp) ~ comp,
                x_lc_space_punct == y_lc_space_punct ~ "case|space|punct",
                x_lc_space_num == y_lc_space_num ~ "case|space|num_format",
                x_lc_punct_num == y_lc_punct_num ~ "case|punct|num_format",
                x_space_punct_num == y_space_punct_num ~ "space|punct|num_format",
                .default = NA_character_
            )
        )

    # all conversion tests
    df <- df |>
        mutate(
            across(
                .cols = ends_with("lc_space_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = case_when(
                !is.na(comp) ~ comp,
                x_lc_space_punct_num == y_lc_space_punct_num ~ "case|space|punct|num_format",
                .default = NA_character_
            )
        )

    df$comp
}

#' Calculate Word Similarity
#'
#' Calculates the percent similarity between words in 2 vectors.
#'
#' @param x,y A character vector.
#' @param ignore A logical vector indicating which values of x & y to ignore;
#' `FALSE` (default) means ignore nothing.
#' @param stopwords A character vector of stop words (words to be ignored).
#' @param stem_word Whether to perform word stemming or not, as a boolean
#' (default: `FALSE`)
#' @inheritDotParams tokenizers::tokenize_words
#'
#' @export
word_similar <- function(x, y, ignore = FALSE, stopwords = NULL,
                         stem_words = FALSE, ...) {
    box::use(
        tokenizers[tokenize_words, tokenize_word_stems],
        purrr[map2_dbl, set_names]
    )
    stopifnot("length of `y` must be the same as `x`" = length(y) == length(x))

    xtest <- x[!ignore]
    ytest <- y[!ignore]

    if (stem_words) {
        xw <- tokenize_word_stems(xtest, stopwords = stopwords, ...)
        yw <- tokenize_word_stems(ytest, stopwords = stopwords, ...)
    } else {
        xw <- tokenize_words(xtest, stopwords = stopwords, ...)
        yw <- tokenize_words(ytest, stopwords = stopwords, ...)
    }

    pct_sim <- map2_dbl(
        xw,
        yw,
        function(.x, .y, .a) {
            round(length(intersect(.x, .y)) / length(union(.x, .y)) * 100, 2)
        }
    ) |>
        set_names(xtest)

    unname(pct_sim[x])
}


### str_similar() helpers ####################################################

#' Convert to Roman Numerals (Lowercase)
#' Converts numbers to lowercase roman numerals. _Intended to make comparing
#' arabic & roman numerals possible._ Converting roman numerals to arabic is
#' much harder and less precise, given the overlap of roman numerals with
#' letters.
#' @param x A character vector.
to_roman_lc <- function(x) {
    box::use(
        stringr[str_detect, str_extract_all, str_to_lower, str_replace_all],
        purrr[map, map2, map2_chr, set_names],
        utils[as.roman]
    )
    has_number <- str_detect(x, "[0-9]+")
    numbers <- str_extract_all(x, "[0-9]+")
    rn <- map(
        numbers,
        ~ str_to_lower(as.roman(.x))
    )
    replace_list <- map2(
        numbers,
        rn,
        function(num, rn) {
            if (length(num) == 0) {
                NA
            } else {
                pattern <- paste0("(^|[^0-9])", num, "([^0-9]|$)")
                set_names(rn, num)
            }
        }
    )

    map2_chr(
        x,
        replace_list,
        function(input, patt_repl) {
            if (all(is.na(patt_repl))) {
                input
            } else {
                str_replace_all(input, patt_repl)
            }
        }
    )
}


### TESTS ####################################################################
# Use RStudio "Run Tests" button to execute these tests interactively
if (is.null(box::name())) {
    box::use(testthat[...])

    test_that("str_compare() works for individual `how`", {
        x <- c("a", "i", "1", "I", "i ", "i.", "í")
        y <- c("b", "i", "I", "i", "i", "i", "i")
        expect_equal(
            str_compare(x, y, "numeral"),
            c(NA_character_, "exact", "numeral", rep(NA_character_, 4))
        )
        expect_equal(
            str_compare(x, y, "case"),
            c(NA_character_, "exact", NA_character_, "case", rep(NA_character_, 3))
        )
        expect_equal(
            str_compare(x, y, "space"),
            c(NA_character_, "exact", rep(NA_character_, 2), "space" , rep(NA_character_, 2))
        )
        expect_equal(
            str_compare(x, y, "punct"),
            c(NA_character_, "exact", rep(NA_character_, 3), "punct" , NA_character_)
        )
        expect_equal(
            str_compare(x, y, "diacritic"),
            c(NA_character_, "exact", rep(NA_character_, 4), "diacritic")
        )
    })

    test_that("str_compare() works for multiple `how`", {
        x <- c("a", "i", "1", "  I.", "Ñ", "i1I .î")
        y <- c("b", "i", "i", "I", "n", "iiii")
        expect_equal(
            str_compare(x, y, "all"),
            c(NA_character_, "exact", "numeral|case", "space|punct",
              "case|diacritic", "numeral|case|space|punct|diacritic")
        )
    })

    test_that("str_compare(..., locale = 'es') works", {
        x <- c("a", "i", "1", "  I.", "Ñ", "i1I .î")
        y <- c("b", "i", "i", "I", "n", "iiii")
        expect_equal(
            str_compare(x, y, "all", locale = "es"),
            c(NA_character_, "exact", "numeral|case", "space|punct",
              "case|diacritic", "numeral|case|space|punct|diacritic")
        )
    })
}
