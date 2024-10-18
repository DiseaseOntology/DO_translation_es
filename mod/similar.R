#' String Comparison
#'
#' Compare strings in 2 vectors and return the type of match.
#'
#' @param x,y A character vector.
#' @inheritParams str_mutate
#' @param delim The separator to use when listing transformations needed to
#' make the two strings identical (default: `"|"`).
#' @inheritParams stringr::str_equal
#' @param ... Arguments passed on to [stringr::str_equal()].
#'
#' @returns A character vector with the match type for each string pair. If
#' strings are identical the value will be 'exact'. See `Details` for what is
#' returned when matches require a transformation to be exact.
#'
#' @section Details:
#' * `str_compare()`: This function tests only for exact matches, after applying
#' string transformations. Those transformations include 'numeral', 'case',
#' 'space', 'punct', or 'diacritic'. All transformations required to make the
#' strings equal will be reported, separated by `delim`. If strings cannot be
#' made identical, the value returned will be `NA`.
#'
#' * `str_compare_all()`: Tests for matches using the string transformation
#' approach of `str_compare()`. These transformations are reported in the same
#' way but prefixed by "string:". Additionally, when string comparisons are
#' insufficient, `str_compare_all()` tests for word similarity based on
#' tokenization with possible modifications. Word similarity is reported as
#' "wordToken[percent_similarity]", with any word transformations applied listed
#' after a colon, separated by `delim`. If string transformations and word
#' transformations were both applied the resulting strings will be in the format:
#'
#' `"string:<str_transform1><delim><str_transform2>...;wordToken[percent_similarity]:<word_transform1><delim><word_transform2>..."`
#'
#' If strings share no word similarity after all transformations are attempted,
#' the value returned will be `NA`.
#'
#' @md
#' @export
str_compare <- function(x, y, how = "all", delim = "|", locale = "en", ...) {
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))
    box::use(
        stringr, purrr, dplyr,
        ./mutate
    )

    # mutate strings; x_nm & delim used later to set comparison output
    x_nm <- "x"
    xmut <- mutate$str_mutate_cum(
        x,
        how,
        x_nm = x_nm,
        names_sep = delim,
        locale = locale
    )
    ymut <- mutate$str_mutate_cum(
        y,
        how,
        x_nm = x_nm,
        names_sep = delim,
        locale = locale
    )

    # revise names to format transformations for comparison output
    mut_nm <- names(xmut)
    nm_pattern <- c(
        # 1. if no transformation -> 'exact'
        paste0("^", x_nm, "$"),
        # 2. drop x_nm & delim from start
        paste0("^", x_nm, stringr$str_escape(delim))
    )
    nm_replace <- c(
        "exact",
        ""
    )
    mut_out <- stringr$str_replace_all(
        mut_nm,
        purrr$set_names(nm_replace, nm_pattern)
    )

    # ensure comparison is done on the same transformed strings by using names
    comparison <- purrr$map2(
        mut_nm,
        mut_out,
        function(.nm, .mut) {
            lgl <- stringr$str_equal(
                xmut[[.nm]],
                ymut[[.nm]],
                locale = locale,
                ...
            )
            dplyr$if_else(lgl, .mut, NA_character_)
        }
    )

    out <- dplyr$coalesce(!!!comparison)
    out
}


#' @inheritParams str_homogenize_cum
#'
#' @rdname str_compare
#' @export
str_compare_all <- function(x, y, how = "all", delim = "|", locale = "en",
                            stopwords = NULL, ...) {
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))
    box::use(
        stringr, purrr, dplyr,
        ./mutate,
        ./utils
    )

    # apply transformations; x_nm & delim used later to set comparison output
    x_nm <- "x"
    xmod <- mutate$str_homogenize_cum(
        x,
        how,
        x_nm = x_nm,
        names_sep = delim,
        locale = locale,
        stopwords = stopwords
    )
    ymod <- mutate$str_homogenize_cum(
        y,
        how,
        x_nm = x_nm,
        names_sep = delim,
        locale = locale,
        stopwords = stopwords
    )

    # split string-only results from those with word transformations
    # --> different tests
    mod_nm <- names(xmod)
    split_fct <- ifelse(stringr$str_detect(mod_nm, "wordToken"), "word", "str")
    mod_split <- split(mod_nm, split_fct)

    # revise names to format transformations for comparison output
    delim_esc <- stringr$str_escape(delim)
    nm_pattern <- c(
        # 1. if no transformation -> 'exact'
        paste0("^", x_nm, "$"),
        # 2. drop x_nm & delim from start
        paste0("^", x_nm, delim_esc),
        # 3. add "string:" before string transformations
        paste0("((", paste0(mutate$str_mutate_opts, collapse = "|"), ")", delim_esc, ")+"),
        # 4. add percent similarity placeholder to "wordToken" (capture groups support next 2 changes)
        paste0("(", delim_esc, ")?wordToken(", delim_esc, ")?"),
        # 5. separate string and word tokenization with "; "
        paste0(delim_esc, delim_esc),
        # 6. use ":" after word tokenization if additional transformations were performed
        paste0(";", delim_esc)
    )
    nm_replace <- c(
        "exact",
        "",
        "string:\\0", # \\0 is the entire match
        "\\1\\1wordToken[%pct%]\\2\\2\\2",
        ";",
        ":"
        )
    mod_out <- stringr$str_replace_all(
        mod_nm,
        purrr$set_names(nm_replace, nm_pattern)
    ) |>
        split(split_fct)

    #### compare pairs using names --> avoids reliance on order ####
    # string comparison
    str_comparison <- purrr$map2(
        mod_split$str,
        mod_out$str,
        function(.nm, .mod) {
            .lgl <- stringr$str_equal(
                xmod[[.nm]],
                ymod[[.nm]],
                locale = locale,
                ...
            )
            dplyr$if_else(.lgl, .mod, NA_character_)
        }
    )

    # word comparison
    if (!is.null(mod_split$word)) {
        word_comparison <- utils$pct_sim(
            xmod[mod_split$word],
            ymod[mod_split$word]
        ) |>
            purrr$set_names(mod_out$word)
        word_matrix <- do.call(rbind, word_comparison)

        report_first_max <- function(x) {
            if (all(x == 0)) {
                return(NA)
            }
            m <- max(x)
            mod <- names(which(x == m)[1])
            # replace percent similarity placeholder with actual value
            stringr$str_replace(mod, stringr$coll("%pct"), m)
        }
        word_best <- apply(word_matrix, 2, report_first_max)
    }

    # combine string & word results; choose preferred transformation set
    if (is.null(mod_split$word)) {
        out <- dplyr$coalesce(!!!str_comparison)
    } else {
        full_comparison <- c(str_comparison, list(word_best))
        out <- dplyr$coalesce(!!!full_comparison)
    }

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
        tokenizers,
        purrr,
        ./utils
    )
    stopifnot("length of `y` must be the same as `x`" = length(y) == length(x))

    xtest <- x[!ignore]
    ytest <- y[!ignore]

    if (stem_words) {
        xw <- tokenizers$tokenize_word_stems(xtest, stopwords = stopwords, ...)
        yw <- tokenizers$tokenize_word_stems(ytest, stopwords = stopwords, ...)
    } else {
        xw <- tokenizers$tokenize_words(xtest, stopwords = stopwords, ...)
        yw <- tokenizers$tokenize_words(ytest, stopwords = stopwords, ...)
    }

    # may not calculate similarity as desired
    # example: x = 1:2, y = 2:3 --> 33.33; 50 seems more intuitive
    pct_sim <- utils$pct_sim(x, y) |>
        purrr$set_names(xtest)

    unname(pct_sim[x])
}


# str_similar() helpers ---------------------------------------------------

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

    # str_compare() tests ----------------------------------------------------
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

    # str_compare_all() tests ------------------------------------------------
    test_that("str_compare_all() works for string-only transformations", {
        x <- c("a", "i", "1", "I", "i ", "i.", "í")
        y <- c("b", "i", "I", "i", "i", "i", "i")
        expect_equal(
            str_compare_all(x, y, "numeral"),
            c(NA_character_, "exact", "string:numeral", rep(NA_character_, 4))
        )
        expect_equal(
            str_compare_all(x, y, "case"),
            c(NA_character_, "exact", NA_character_, "string:case", rep(NA_character_, 3))
        )
        expect_equal(
            str_compare_all(x, y, "space"),
            c(NA_character_, "exact", rep(NA_character_, 2), "string:space" , rep(NA_character_, 2))
        )
        expect_equal(
            str_compare_all(x, y, "punct"),
            c(NA_character_, "exact", rep(NA_character_, 3), "string:punct" , NA_character_)
        )
        expect_equal(
            str_compare_all(x, y, "diacritic"),
            c(NA_character_, "exact", rep(NA_character_, 4), "string:diacritic")
        )
        # `how` in reverse order to be sure it works still
        expect_equal(
            str_compare_all(x, y, c("diacritic", "punct", "space", "case", "numeral")),
            c(NA_character_, "exact", "string:numeral", "string:case",
              "string:space", "string:punct", "string:diacritic")
        )
    })

    test_that("str_compare_all() works for individual word transformations (exact)", {
        x <- c("never going", "1 word", "word order", "plus punct.",
               "need stemming", "and stopword", "or applying Total-here1")
        y <- c("to match", "1 word", "order word", "plus-punct",
               "needing stem", "or stopword", "and applying Total:here1")
        expect_equal(
            str_compare_all(
                c("never going", "1 word", "word order"),
                c("to match", "1 word", "order word"),
                "wordToken"
            ),
            c(NA_character_, "exact", "wordToken[100%]")
        )
        expect_equal(
            str_compare_all(
                c("never going", "1 word", "word order", "plus punct."),
                c("to match", "1 word", "order word", "plus-punct"),
                "wpunct"
            ),
            c(NA_character_, "exact", "wordToken[100%]", "wordToken[100%]:wpunct")
        )
        expect_equal(
            str_compare_all(
                c("never going", "1 word", "word order", "need stemming"),
                c("to match", "1 word", "order word", "needing stem"),
                "stemmed"
            ),
            c(NA_character_, "exact", "wordToken[100%]", "wordToken[100%]:stemmed")
        )
        expect_equal(
            str_compare_all(
                c("never going", "1 word", "and stopword"),
                c("to match", "1 word", "or stopword"),
                "stopwords"
            ),
            c(NA_character_, "exact", "wordToken[100%]", "wordToken[100%]:stopwords")
        )
        expect_equal(
            str_compare_all(
                x,
                y,
                c("wordToken", "case", "wpunct", "stemmed", "stopwords")
            ),
            c(NA_character_, "exact", "wordToken[100%]", "wordToken[100%]:wpunct",
              "wordToken[100%]:stemmed", "wordToken[100%]:stopwords",
              "wordToken[100%]:wpunct|stemmed|stopwords")
        )
    })
}
