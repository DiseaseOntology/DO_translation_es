#' Mutate Strings
#'
#' Applies specified changes to a character vector.
#'
#' @param x A character vector.
#' @param how A character vector of transformations to apply to `x`. One of:
#' "case", "space", "punct", "numeral", "diacritic" or "all".
#' @inheritParams str_to_lower
#'
#' @returns A character vector with the specified transformations applied.
#'
#' @md
#' @export
str_mutate <- function(x, how, locale = "en") {
    # internal function to apply each transformation
    str_mutate_ <- function(x, how) {
        box::use(stringr, stringi)

        .fn <- switch(
            how,
            numeral = function(x) to_roman(x),
            case = function(x) stringr$str_to_lower(x, locale = locale),
            space = function(x) stringr$str_remove_all(x, "[[:space:]]+"),
            punct = function(x) stringr$str_remove_all(x, "[[:punct:]]+"),
            diacritic = function(x) stringi$stri_trans_general(x, "Latin-ASCII")
        )
        .fn(x)
    }

    how <- check_str_mutate_how(how)
    Reduce(str_mutate_, how, init = x)
}


#' Mutate Strings with All Possible Combinations
#'
#' Applies specified changes to a character vector generating all possible
#' combinations.
#'
#' @inheritParams str_mutate
#' @param x_nm The original name to use to identify the input `x`. If `NULL`
#' (default) no name will be used.
#' @param names_sep The separator to use when creating new column names, and for
#' recognizing previously executed comparisons to avoid unneeded repetition
#' (default: `"."`).
#'
#' @returns A named list of character vectors, including the original input `x`
#' named as `x_nm` (unless `x_nm = NULL`) and each of the possible combinations
#' of `how` named by the transformations applied and prefixed by `x_nm`,
#' if provided, using `names_sep` as the separator.
#'
#' @md
#' @export
str_mutate_cum <- function(x, how = "all", x_nm = "x", names_sep = "_",
                           locale = "en") {
    box::use(purrr, utils)
    how <- check_str_mutate_how(how)

    # get all possible combinations of how
    how_list <- purrr$map(
        1:length(how),
        ~ utils$combn(how, .x, simplify = FALSE)
    ) |>
        unlist(recursive = FALSE)

    out <- purrr$map(how_list, ~ str_mutate(x, .x, locale = locale))

    # add names
    how_nm <- purrr$map_chr(how_list, ~ paste0(c(x_nm, .x), collapse = names_sep))
    names(out) <- how_nm

    # add original input at start
    out <- c(list(x), out)
    if (!is.null(x_nm)) {
        names(out)[1] <- x_nm
    }

    out
}

#' Flexibly Tokenize Words
#'
#' Tokenize words in a character vector applying specified transformations.
#'
#' @param x A character vector.
#' @param how A character vector of options from `tokenize_words_opts`, or `"all"`
#' (default) to apply all of them.
#' @param locale The locale to use for stemming (default: `"en"`). Only locales
#' corresponding to the languages in [SnowballC::getStemLanguages()] are
#' supported.
#' @param stopwords A character vector of words to ignore, or `NULL` (default)
#' to use words from [stopwords::stopwords()].
#'
#' @return A list of the same length as `x` with words tokenized as specified
#' by `how`.
#'
#' @export
tokenize_words <- function(x, how = "all", locale = "en", stopwords = NULL) {
    # alias for stopwords to prevent name collision of box using stopwords$stopwords()
    box::use(sw = stopwords, tokenizers, SnowballC, purrr)

    how <- match.arg(
        how,
        choices = c("all", tokenize_words_opts),
        several.ok = TRUE
    )
    if ("all" %in% how) how <- tokenize_words_opts

    if ("case" %in% how) {
        lowercase <- TRUE
    } else {
        lowercase <- FALSE
    }

    if ("wpunct" %in% how) {
        strip_punct <- TRUE
    } else {
        strip_punct <- FALSE
    }

    if ("stopwords" %in% how) {
        if (is.null(stopwords)) stopwords <- sw$stopwords()
        if (!lowercase) stopwords <- c(stopwords, toupper(stopwords))
    } else {
        stopwords <- NULL
    }

    out <- tokenizers$tokenize_words(
        x,
        lowercase = lowercase,
        stopwords = stopwords,
        strip_punct = strip_punct,
        strip_numeric = FALSE
    )

    if (!"stemmed" %in% how) return(out)

    purrr$map(out, ~ SnowballC$wordStem(.x, language = locale))
}


#' Mutate & Tokenize to Homogenize Strings
#'
#' Applies specified changes to a character vector.
#'
#' @param x A character vector.
#' @param how A character vector of transformations to apply to `x`. Any of the
#' options for `how` in [str_mutate()], except "space", or [tokenize_words()]
#' can be used.
#'
#' Some transformation combinations are not allowed because they cannot be
#' applied in combination. Those are "punct" with "wpunct" (not possible) and
#' "numeral" with "stopwords" (1 -> I = stopword, and would get dropped).
#'
#' Also, "wpunct" is preferred over "punct" so "all" will use "wpunct" by
#' default. If "punct" is desired instead, include it explicitly (i.e.
#' `c("all", "punct")`).
#'
#' @inheritParams tokenize_words
#' @param quiet A logical indicating whether to suppress warnings about
#' dropping "numeral" when "stopwords" is used (default: `FALSE`).
#'
#' @returns A list the same length as `x` of character vectors with the
#' specified transformations applied.
#'
#' @md
#' @export
str_homogenize <- function(x, how = "all", locale = "en", stopwords = NULL,
                           quiet = FALSE) {
    how <- check_str_homogenize_how(how, quiet = quiet)

    if (length(how$str) == 0) {
        out <- x
    } else {
        out <- str_mutate(x, how$str, locale = locale)
    }

    if (length(how$word) == 0) return(as.list(out))

    tokenize_words(out, how$word, locale = locale, stopwords = stopwords)
}


#' Mutate & Tokenize to Homogenize Strings with All Possible Combinations
#'
#' Applies specified changes to a character vector generating all possible
#' combinations.
#'
#' @inheritParams str_homogenize
#' @param x_nm The original name to use to identify the input `x`. If `NULL`
#' (default) no name will be used.
#' @param names_sep The separator to use when creating new column names, and for
#' recognizing previously executed comparisons to avoid unneeded repetition
#' (default: `"."`).
#'
#' @section Note about `how`:
#' The rules for `how` from [str_homogenize()] apply here, but are applied
#' internally. Therefore, combinations that would result in [str_homogenize()]
#' errors are automatically and _silently_ excluded.
#'
#' Additionally, `how` inputs that would result in ONLY errors, will return an
#' error.
#'
#' @returns A named list of character vectors, including the original input `x`
#' named as `x_nm` (unless `x_nm = NULL`) and each of the possible combinations
#' of `how` named by the transformations applied and prefixed by `x_nm`,
#' if provided, using `names_sep` as the separator.
#'
#' @md
#' @export
str_homogenize_cum <- function(x, how = "all", x_nm = "x", names_sep = "_",
                               locale = "en", stopwords = NULL) {
    box::use(purrr, utils)
    how <- match.arg(
        how,
        choices = c("all", str_homogenize_cum_opts),
        several.ok = TRUE
    )

    how_error <- dplyr::case_when(
        all(how %in% c("numeral", "stopwords")) ~
            "'numeral' and 'stopwords' cannot be used alone",
        # just space and one other tokenize_word_opts = error
        ("space" %in% how && any(how %in% tokenize_words_opts) && length(how) == 2) ~
            "'space' cannot be used with only inputs to tokenize_words",
        all(how %in% c("punct", "wpunct")) ~
            "'punct' and 'wpunct' cannot be used alone",
        TRUE ~ NA_character_
    )
    if (!is.na(how_error)) stop(how_error)

    if ("all" %in% how) how <- str_homogenize_cum_opts

    # get all possible combinations of how
    how_list <- purrr$map(
        1:length(how),
        ~ utils$combn(how, .x, simplify = FALSE)
    ) |>
        unlist(recursive = FALSE)

    # drop any where only errors would be produced
    how_drop <- purrr$map_lgl(
        how_list,
            # "space" is included with tokenize_words_opts, take negative search
            # approach because "case" in both
        ~ "space" %in% .x && any(!.x %in% str_mutate_opts) ||
            # only combinations which cannot be together
            all(c("numeral", "stopwords") %in% .x) ||
            all(c("punct", "wpunct") %in% .x) ||
            # "wordToken" is redundant when used in combination with other tokenize_words_opts
            "wordToken" %in% .x && sum(!.x %in% str_mutate_opts) > 1
    )
    how_list <- how_list[!how_drop]

    out <- purrr$map(
        how_list,
        function(.h) {
            if (all(.h %in% str_mutate_opts)) {
                as.list(str_mutate(x, .h, locale = locale))
            } else if (all(.h %in% tokenize_words_opts)) {
                tokenize_words(x, .h, locale = locale, stopwords = stopwords)
            } else {
                str_homogenize(x, .h, locale = locale, stopwords = stopwords)
            }
        }
    )

    # add names
    how_nm <- purrr$map_chr(how_list, ~ paste0(c(x_nm, .x), collapse = names_sep))
    names(out) <- how_nm

    # add original input at start (but converted to a list, so the format is the
    # same
    out <- c(list(as.list(x)), out)
    if (!is.null(x_nm)) {
        names(out)[1] <- x_nm
    }

    out
}


# str_mutate() helpers -------------------------------------------------

#' str_mutate_opts must match function names in str_mutate
str_mutate_opts <- c("numeral", "case", "space", "punct", "diacritic")


#' Check `how` in str_mutate*() functions
#'
#' @inheritParams str_mutate
check_str_mutate_how <- function(how) {
    if (!all(how %in% c("all", str_mutate_opts)) || is.null(how)) {
        stop(
            paste0(
                "`how` must be one or more of: ", ,
                paste0(paste0("'", str_mutate_opts, "'"), collapse = ", "),
                ", or 'all'"
            )
        )
    }
    if ("all" %in% how) how <- str_mutate_opts
    # make order consistent with str_mutate_opts
    str_mutate_opts[str_mutate_opts %in% how]
}


#' Convert to Roman Numerals
#'
#' Converts numbers within strings to roman numerals.
#'
#' @section Note:
#' _Intended to make comparing arabic & roman numerals possible._ Converting
#' roman numerals to arabic is much harder and less precise, given the overlap
#' of roman numerals with letters.
#'
#' @param x A character vector.
to_roman <- function(x) {
    box::use(stringr, purrr, utils)
    numbers <- stringr$str_extract_all(x, "[0-9]+") |>
        # integer -> character round trip for proper sorting, which is needed
        # to ensure larger numbers are matched completely and not partially
        # as smaller ones
        purrr$map(~ as.character(sort(as.integer(.x), decreasing = TRUE)))

    rn <- purrr$map(numbers, ~ as.character(utils$as.roman(.x)))

    purrr$pmap_chr(
        list(x, numbers, rn),
        function(.x, .n, .rn) {
            if (length(.n) == 0) {
                .x
            } else {
                # setNames to handle multiple replacements in pairs
                stringr$str_replace_all(.x, purrr::set_names(.rn, .n))
            }
        }
    )
}


# tokenize_words() helpers ------------------------------------------------

# tokenize_words() options
tokenize_words_opts <- c("wordToken", "case", "wpunct", "stemmed", "stopwords")


# str_homogenize() helpers ------------------------------------------------

# str_homogenize() options; some must be variably excluded, only "space" not
# allowed, but included for informative error message
str_homogenize_opts <- c(
    str_mutate_opts[str_mutate_opts != "space"],
    tokenize_words_opts
)

# "space" allowed in str_homogenize_cum()
str_homogenize_cum_opts <- unique(c(str_mutate_opts, tokenize_words_opts))


#' Check `how` in str_mutate*() functions
#'
#' @inheritParams str_homogenize
check_str_homogenize_how <- function(how, quiet = FALSE) {
    # "space" str_compare_opts not supported because it's required for tokenizing
    if ("space" %in% how) {
        stop("`how` cannot include 'space' in str_homogenize. Did you mean to use `str_mutate*()` instead?")
    }

    if (!all(how %in% c("all", str_homogenize_opts)) || is.null(how)) {
        stop(
            paste0(
                "`how` must be one or more of: ", ,
                paste0(paste0("'", str_homogenize_opts, "'"), collapse = ", "),
                ", or 'all'"
            )
        )
    }

    # "punct" supported, but not in combination with "wpunct" (preferred),
    if (all(c("punct", "wpunct") %in% how)) {
        stop("`how` cannot include both 'punct' and 'wpunct' (preferred) together")
    }

    out <- list()
    if ("all" %in% how) {
        out$str <- str_mutate_opts[str_mutate_opts %in% str_homogenize_opts]
        out$word <- tokenize_words_opts[
            tokenize_words_opts %in% str_homogenize_opts &
                !tokenize_words_opts %in% out$str
        ]
    } else {
        # "space" & "punct" + "wpunct" excluded,
        # also need to avoid executing "case" (and possibly others) twice, all
        # duplicates will be executed in str_mutate() first
        out$str <- str_mutate_opts[str_mutate_opts %in% how]
        out$word <- tokenize_words_opts[tokenize_words_opts %in% how & !tokenize_words_opts %in% out$str]
    }

    # to use "punct" include it explicitly; if used with "all" will override
    # and exclude "wpunct"
    if ("punct" %in% how) {
        out$word <- out$word[out$word != "wpunct"]
    } else {
        out$str <- out$str[out$str != "punct"]
    }

    # UNACCEPTABLE OUTPUT: "numeral" converts 1 to I, which is removed by
    # "stopwords"
    # TEMPORARY FIX: exclude "numeral" if "stopwords"
    if ("numeral" %in% out$str && "stopwords" %in% out$word) {
        if (!quiet) {
            warning("Excluding 'numeral' from `how` due to 'stopwords' (e.g. 1 -> I = stopword)")
        }
        out$str <- out$str[out$str != "numeral"]
    }

    out
}



### TESTS ####################################################################

# Use RStudio "Run Tests" button to execute these tests interactively
if (is.null(box::name())) {
    box::use(testthat[...])

    # str_mutate() tests -----------------------------------------------------
    test_that("str_mutate() works for individual `how`", {
        x <- c("i", "1", "I", "i ", "i.", "í")
        expect_equal(
            str_mutate(x, "case"),
            c("i", "1", "i", "i ", "i.", "í")
        )
        expect_equal(
            str_mutate(x, "space"),
            c("i", "1", "I", "i", "i.", "í")
        )
        expect_equal(
            str_mutate(x, "punct"),
            c("i", "1", "I", "i ", "i", "í")
        )
        expect_equal(
            str_mutate(x, "numeral"),
            c("i", "I", "I", "i ", "i.", "í")
        )
        expect_equal(
            str_mutate(x, "diacritic"),
            c("i", "1", "I", "i ", "i.", "i")
        )
    })

    test_that("str_mutate() works for multiple `how`", {
        x <- c("i", "1", "I", "i ", "i.", "í", "i1I .î")
        expect_equal(
            str_mutate(x, c("case", "space")),
            c("i", "1", "i", "i", "i.", "í", "i1i.î")
        )
        expect_equal(
            str_mutate(x, c("case", "space", "punct", "numeral", "diacritic")),
            c("i", "i", "i", "i", "i", "i", "iiii")
        )
        expect_equal(
            str_mutate(x, "all"),
            c("i", "i", "i", "i", "i", "i", "iiii")
        )
    })

    test_that("str_mutate(..., locale = 'es') works", {
        x <- c("i", "1", "I", "i ", "i.", "í")
        expect_equal(
            str_mutate(x, "case", locale = "es"),
            c("i", "1", "i", "i ", "i.", "í")
        )
        expect_equal(
            str_mutate(x, "diacritic", locale = "es"),
            c("i", "1", "I", "i ", "i.", "i")
        )

        y <- c("i", "1", "I", "i ", "i.", "í", "i1I .î")
        expect_equal(
            str_mutate(y, "all", locale = "es"),
            c("i", "i", "i", "i", "i", "i", "iiii")
        )
    })

    # str_mutate_cum() tests -------------------------------------------------
    test_that("str_mutate_cum() works", {
        x <- c("i", "1", "I", "i ", "i.", "í", "i1I .î")

        expect_equal(
            str_mutate_cum(x, "case"),
            list(
                x = c("i", "1", "I", "i ", "i.", "í", "i1I .î"),
                x_case = c("i", "1", "i", "i ", "i.", "í", "i1i .î")
            )
        )
        expect_equal(
            str_mutate_cum(x, c("punct", "space")),
            list(
                x = c("i", "1", "I", "i ", "i.", "í", "i1I .î"),
                x_space = c("i", "1", "I", "i", "i.", "í", "i1I.î"),
                x_punct = c("i", "1", "I", "i ", "i", "í", "i1I î"),
                x_space_punct = c("i", "1", "I", "i", "i", "í", "i1Iî")
            )
        )
        expect_equal(
            str_mutate_cum(x, "all"),
            list(
                x = c("i", "1", "I", "i ", "i.", "í", "i1I .î"),
                x_numeral = c("i",  "I", "I", "i ", "i.", "í", "iII .î"),
                x_case = c("i", "1",  "i", "i ", "i.", "í", "i1i .î"),
                x_space = c("i", "1", "I",  "i", "i.", "í", "i1I.î"),
                x_punct = c("i", "1", "I", "i ",  "i", "í", "i1I î"),
                x_diacritic = c("i", "1", "I", "i ", "i.",  "i", "i1I .i"),
                x_numeral_case = c("i", "i", "i", "i ", "i.",  "í", "iii .î"),
                x_numeral_space = c("i", "I", "I", "i", "i.",  "í", "iII.î"),
                x_numeral_punct = c("i", "I", "I", "i ", "i",  "í", "iII î"),
                x_numeral_diacritic = c("i", "I", "I", "i ",  "i.", "i", "iII .i"),
                x_case_space = c("i", "1", "i", "i", "i.",  "í", "i1i.î"),
                x_case_punct = c("i", "1", "i", "i ", "i", "í",  "i1i î"),
                x_case_diacritic = c("i", "1", "i", "i ", "i.", "i",  "i1i .i"),
                x_space_punct = c("i", "1", "I", "i", "i", "í", "i1Iî" ),
                x_space_diacritic = c("i", "1", "I", "i", "i.", "i", "i1I.i" ),
                x_punct_diacritic = c("i", "1", "I", "i ", "i", "i", "i1I i" ),
                x_numeral_case_space = c("i", "i", "i", "i", "i.", "í", "iii.î" ),
                x_numeral_case_punct = c("i", "i", "i", "i ", "i", "í", "iii î" ),
                x_numeral_case_diacritic = c("i", "i", "i", "i ", "i.", "i",  "iii .i"),
                x_numeral_space_punct = c("i", "I", "I", "i", "i",  "í", "iIIî"),
                x_numeral_space_diacritic = c("i", "I", "I",  "i", "i.", "i", "iII.i"),
                x_numeral_punct_diacritic = c("i",  "I", "I", "i ", "i", "i", "iII i"),
                x_case_space_punct = c("i",  "1", "i", "i", "i", "í", "i1iî"),
                x_case_space_diacritic = c("i",  "1", "i", "i", "i.", "i", "i1i.i"),
                x_case_punct_diacritic = c("i",  "1", "i", "i ", "i", "i", "i1i i"),
                x_space_punct_diacritic = c("i",  "1", "I", "i", "i", "i", "i1Ii"),
                x_numeral_case_space_punct = c("i",  "i", "i", "i", "i", "í", "iiiî"),
                x_numeral_case_space_diacritic = c("i",  "i", "i", "i", "i.", "i", "iii.i"),
                x_numeral_case_punct_diacritic = c("i",  "i", "i", "i ", "i", "i", "iii i"),
                x_numeral_space_punct_diacritic = c("i",  "I", "I", "i", "i", "i", "iIIi"),
                x_case_space_punct_diacritic = c("i",  "1", "i", "i", "i", "i", "i1ii"),
                x_numeral_case_space_punct_diacritic = c("i",  "i", "i", "i", "i", "i", "iiii")
            )
        )
    })

    test_that("str_mutate_cum(..., locale = 'es') works", {
        x <- c("i", "1", "I", "i ", "i.", "í", "i1I .îñéó")

        expect_equal(
            str_mutate_cum(x, c("diacritic", "numeral"), locale = "es"),
            list(
                x = c("i", "1", "I", "i ", "i.", "í", "i1I .îñéó"),
                x_numeral = c("i",  "I", "I", "i ", "i.", "í", "iII .îñéó"),
                x_diacritic = c("i",  "1", "I", "i ", "i.", "i", "i1I .ineo"),
                x_numeral_diacritic = c("i",  "I", "I", "i ", "i.", "i", "iII .ineo")
            )
        )
    })

    # to_roman() tests -------------------------------------------------------
    test_that("to_roman() works", {
        x <- c("1", "4", "5", "10", "1and10", "110by50", "2by125and11")
        expect_equal(
            to_roman(x),
            c("I", "IV", "V", "X", "IandX", "CXbyL", "IIbyCXXVandXI")
        )
    })

    # tokenize_words() tests -------------------------------------------------
    test_that("tokenize_words() works", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            tokenize_words(x, how = "wordToken"),
            list(
                c("1", "word"),
                c("keep", "CASE"),
                c("plus", "punct", "."),
                c("and", "stopword"),
                c("need", "stemming"),
                c("or", "applying", "Total", "-", "1")
            )
        )
        expect_equal(
            tokenize_words(x, how = "case"),
            list(
                c("1", "word"),
                c("keep", "case"),
                c("plus", "punct", "."),
                c("and", "stopword"),
                c("need", "stemming"),
                c("or", "applying", "total", "-", "1")
            )
        )
        expect_equal(
            tokenize_words(x, how = "wpunct"),
            list(
                c("1", "word"),
                c("keep", "CASE"),
                c("plus", "punct"),
                c("and", "stopword"),
                c("need", "stemming"),
                c("or", "applying", "Total", "1")
            )
        )
        expect_equal(
            tokenize_words(x, how = "stopwords"),
            list(
                c("1", "word"),
                c("keep", "CASE"),
                c("plus", "punct", "."),
                c("stopword"),
                c("need", "stemming"),
                c("applying", "Total", "-", "1")
            )
        )
        expect_equal(
            tokenize_words(x, how = "stemmed"),
            list(
                c("1", "word"),
                c("keep", "CASE"),
                c("plus", "punct", "."),
                c("and", "stopword"),
                c("need", "stem"),
                c("or", "appli", "Total", "-", "1")
            )
        )
        expect_equal(
            tokenize_words(x, how = "all"),
            list(
                c("1", "word"),
                c("keep", "case"),
                c("plus", "punct"),
                c("stopword"),
                c("need", "stem"),
                c("appli", "total", "1")
            )
        )
    })

    # str_homogenize() tests -------------------------------------------------
    test_that("str_homogenize() works for string mutations alone", {
        x <- c("i", "1", "I", "i ", "i.", "í", "i1I .î")

        expect_equal(
            str_homogenize(x, how = "numeral"),
            as.list(str_mutate(x, how = "numeral"))
        )
        expect_equal(
            str_homogenize(x, how = "case"),
            as.list(str_mutate(x, how = "case"))
        )
        expect_equal(
            str_homogenize(x, how = "punct"),
            as.list(str_mutate(x, how = "punct"))
        )
        expect_equal(
            str_homogenize(x, how = "diacritic"),
            as.list(str_mutate(x, how = "diacritic"))
        )
        expect_equal(
            str_homogenize(x, how = "diacritic"),
            as.list(str_mutate(x, how = "diacritic"))
        )
        # not 'all' because 'space' excluded
        expect_equal(
            str_homogenize(x, how = c("numeral", "case", "punct", "diacritic")),
            as.list(str_mutate(x, how = c("numeral", "case", "punct", "diacritic")))
        )
    })

    test_that("str_homogenize() works for word tokenizations alone", {
        x <- c("1 word", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            str_homogenize(x, how = "wordToken"),
            tokenize_words(x, how = "wordToken")
        )
        expect_equal(
            str_homogenize(x, how = "wpunct"),
            tokenize_words(x, how = "wpunct")
        )
        expect_equal(
            str_homogenize(x, how = "stemmed"),
            tokenize_words(x, how = "stemmed")
        )
        expect_equal(
            str_homogenize(x, how = "stopwords"),
            tokenize_words(x, how = "stopwords")
        )
        expect_equal(
            str_homogenize(x, how = c("wordToken", "case", "wpunct", "stemmed", "stopwords")),
            tokenize_words(x, how = "all")
        )
    })

    test_that("str_homogenize() works for mixed string & word transformations", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            str_homogenize(x, how = c("numeral", "wordToken")),
            list(
                c("I", "word"),
                c("keep", "CASE"),
                c("plus", "punct", "."),
                c("and", "stopword"),
                c("need", "stemming"),
                c("or", "applying", "Total", "-", "I")
            )
        )
        # NOTE: demonstrates loss of number 1 when both "numeral" and "stopwords"
        #   are included in `how` --> currently prevented, fix?
        expect_equal(
            expect_warning(str_homogenize(x, how = "all")),
            list(
                c("1", "word"),
                c("keep", "case"),
                c("plus", "punct"),
                c("stopword"),
                c("need", "stem"),
                c("appli", "total", "1")
            )
        )
        expect_equal(
            str_homogenize(x, how = c("all", "punct"), quiet = TRUE),
            list(
                c("1", "word"),
                c("keep", "case"),
                c("plus", "punct"),
                c("stopword"),
                c("need", "stem"),
                c("appli", "total1")
            )
        )
    })

    test_that("str_homogenize() stopwords argument works", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        sw <- c("word", "case", "punct", "stopword", "stem", "Total")
        expect_equal(
            str_homogenize(x, how = c("punct", "stopwords"), stopwords = sw),
            list(
                c("1"),
                c("keep", "CASE"),
                c("plus"),
                c("and"),
                c("need", "stemming"),
                c("or", "applying", "Total1")
            )
        )
    })

    # str_homogenize_cum() tests ---------------------------------------------
    test_that("str_homogenize_cum() errors for disallowed inputs", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")

        expect_error(str_homogenize_cum(x, how = c("space", "stemmed")))
        expect_error(str_homogenize_cum(x, how = c("punct", "wpunct")))
        expect_error(str_homogenize_cum(x, how = c("numeral", "stopwords")))
    })

    test_that("str_homogenize_cum() works like str_mutate_cum()", {
        box::use(purrr)

        x <- c("i", "1", "I", "i ", "i.", "í", "i1I .î")

        expect_equal(
            str_homogenize_cum(x, how = str_mutate_opts),
            purrr$map(str_mutate_cum(x, how = "all"), ~ as.list(.x))
        )
    })

    test_that("str_homogenize_cum() works when `how` has only word inputs", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            str_homogenize_cum(x, how = c("wpunct", "stemmed")),
            list(
                x = as.list(x),
                x_wpunct = str_homogenize(x, how = "wpunct"),
                x_stemmed = str_homogenize(x, how = "stemmed"),
                x_wpunct_stemmed = str_homogenize(x, how = c("wpunct", "stemmed"))
            )
        )
    })

    test_that("str_homogenize_cum() works when `how` has both string & word inputs", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            str_homogenize_cum(x, how = c("numeral", "stemmed")),
            list(
                x = as.list(x),
                x_numeral = str_homogenize(x, how = "numeral"),
                x_stemmed = str_homogenize(x, how = "stemmed"),
                x_numeral_stemmed = str_homogenize(x, how = c("numeral", "stemmed"))
            )
        )
    })

    test_that("str_homogenize_cum() stopwords argument works", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        sw <- c("word", "keep", "plus", "and", "need", "or")
        expect_equal(
            str_homogenize_cum(x, how = c("numeral", "stemmed"), stopwords = sw),
            list(
                x = as.list(x),
                x_numeral = str_homogenize(x, how = "numeral", stopwords = sw),
                x_stemmed = str_homogenize(x, how = "stemmed", stopwords = sw),
                x_numeral_stemmed = str_homogenize(
                    x,
                    how = c("numeral", "stemmed"),
                    stopwords = sw
                )
            )
        )
    })
}
