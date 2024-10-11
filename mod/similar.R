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

#' String Comparison Functions
box::use(
    stringr[str_to_lower, str_remove_all],
    stringi[stri_trans_general]
)
str_compare_fn <- c(
    case = function(x) str_to_lower(x),
    space = function(x) str_remove_all(x, "[[:space:]]+"),
    punct = function(x) str_remove_all(x, "[[:punct:]]+"),
    num_format = function(x) to_roman_lc(x),
    diacritic = function(x) stri_trans_general(x, "Latin-ASCII")
)

#' String Comparison Function Mapper
#'
#' Applies the specified set of string comparison functions to a character
#' vector. It ignores previously applied functions, as identified by name in
#' `x_nm` (after splitting by `names_sep`) to support iterative execution and
#' avoid repetition.
#'
#' @inheritParams str_compare
#' @param x_nm The name of `x`, as a string.
#' @param names_sep The separator to use when creating new column names, and for
#' recognizing previously executed comparisons (to avoid unneeded repetition).
map_str_compare_fn <- function(x, x_nm, how, names_sep) {
    box::use(
        stringr[str_split, coll, str_to_lower, str_remove_all],
        stringi[stri_trans_general],
        purrr[map]
    )

    # ignore functions that have already been applied (name-based)
    how_ignore <- str_split(x_nm, coll(names_sep))[[1]]
    how <- how[!how %in% how_ignore]

    # select functions to apply
    fn_exec <- str_compare_fn[how]

    out <- map(fn_exec, function(.fn) .fn(x))
    names(out) <- paste0(x_nm, names_sep, names(fn_exec))
    out
}

#' String Comparison
#'
#' @param x,y A character vector.
#' @param how A character vector of comparisons to apply. One or more of 'case',
#' 'space', 'punct', 'num_format', 'diacritic', or 'all' (default; applies all
#' comparisons).
#'
#' @export
str_compare <- function(x, y, how = "all") {
    box::use(
        purrr[map2]
    )
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))

    how <- match.arg(
        how,
        choices = c("all", names(str_compare_fn)),
        several.ok = TRUE
    )
    if ("all" %in% how) how <- names(str_compare_fn)

    # establish name separator for coordinated use with map_str_compare_fn()
    #  --> REQUIRED for proper function application and column naming
    names_sep <- "_"

    # iteratively apply functions (to output created in previous iteration only)
    i <- 1
    out <- list(list(x = x, y = y))
    while (i <= length(how)) {
        nm <- names(out[[i]])
        out[[i + 1]] <<- map2(
            out[[i]],
            names(out[[i]]),
            ~ map_str_compare_fn(.x, .y, how, names_sep)
        )
        i <<- i + 1
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
