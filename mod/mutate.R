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
    box::use(
        purrr,
        ./utils
    )
    how <- check_str_mutate_how(how)

    # get all possible combinations of how
    how_list <- utils$combn_all(how)

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
    box::use(sw = stopwords, tokenizers, SnowballC, purrr, stringr)

    how <- check_tokenize_words_how(how)
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

    out <- tokenizers$tokenize_words(
        x,
        lowercase = lowercase,
        stopwords = NULL,
        strip_punct = strip_punct,
        strip_numeric = FALSE
    )

    # stopwords case-insensitive without affecting output case
    if ("stopwords" %in% how) {
        if (is.null(stopwords)) {
            w_rm <- sw$stopwords()
        } else {
            w_rm <- stringr$str_to_lower(stopwords, locale = locale)
        }
        sw_index <- purrr$map(
            out,
            ~ stringr$str_to_lower(.x, locale = locale) %in% w_rm
        )
        out <- purrr$map2(out, sw_index, ~ .x[!.y])
    }

    if ("stemmed" %in% how) {
        out <- purrr$map(out, ~ SnowballC$wordStem(.x, language = locale))
    }

    out
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

    if (length(how$chr) == 0) {
        out <- x
    } else {
        out <- str_mutate(x, how$chr, locale = locale)
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
    box::use(
        purrr,
        ./utils
    )

    how_list <- create_how_list(how)

    result <- list()
    result$chr <- purrr$map(
        how_list$chr,
        function(.h) as.list(str_mutate(x, .h, locale = locale))
    )
    result$word <- purrr$map(
        how_list$word,
        function(.h) tokenize_words(x, .h, locale = locale, stopwords = stopwords)
    )
    result$homogenize <- purrr$map(
        how_list$homogenize,
        function(.h) str_homogenize(x, .h, locale = locale, stopwords = stopwords)
    )

    # collapse all results into a single list
    out <- unlist(result, recursive = FALSE)

    # add names
    how_nm <- purrr$map_chr(
        unlist(how_list, recursive = FALSE),
        ~ paste0(c(x_nm, .x), collapse = names_sep)
    )
    names(out) <- how_nm

    # add original input at start (but converted to a list, so the format is the
    # same as all other output)
    out <- c(list(as.list(x)), out)
    if (!is.null(x_nm)) names(out)[1] <- x_nm

    out
}


# str_mutate() helpers -------------------------------------------------

#' str_mutate_opts must match function names in str_mutate
#' @export
str_mutate_opts <- c("numeral", "case", "space", "punct", "diacritic")


#' Check `how` in str_mutate*() functions
#'
#' @inheritParams str_mutate
#' @param allow_mismatch A logical indicating whether to allow mismatches. If
#' `FALSE` (default), a mismatch will result in an error. If `TRUE`, mismatches
#' are dropped silently.
check_str_mutate_how <- function(how, allow_mismatch = FALSE) {
    if (!allow_mismatch && !all(how %in% c("all", str_mutate_opts)) || is.null(how)) {
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


# tokenize_words() helpers ---------------------------------------------------

# tokenize_words() options
#' @export
tokenize_words_opts <- c("wordToken", "case", "wpunct", "stemmed", "stopwords")


#' Check `how` in tokenize_words()
#'
#' @inheritParams str_mutate
#' @param allow_mismatch A logical indicating whether to allow mismatches. If
#' `FALSE` (default), a mismatch will result in an error. If `TRUE`, mismatches
#' are dropped silently.
check_tokenize_words_how <- function(how, allow_mismatch = FALSE) {
    if (!allow_mismatch && !all(how %in% c("all", tokenize_words_opts)) || is.null(how)) {
        stop(
            paste0(
                "`how` must be one or more of: ", ,
                paste0(paste0("'", tokenize_words_opts, "'"), collapse = ", "),
                ", or 'all'"
            )
        )
    }
    if ("all" %in% how) how <- tokenize_words_opts
    # make order consistent with str_mutate_opts
    tokenize_words_opts[tokenize_words_opts %in% how]
}


# str_homogenize_cum() helpers -----------------------------------------------

# "space" allowed in str_homogenize_cum()
#' @export
str_homogenize_cum_opts <- unique(c(str_mutate_opts, tokenize_words_opts))


#' Order str_homogenize_cum() output for comparison
#'
#' @inheritParams str_homogenize_cum
#'
#' @section Details:
#' Multiple comparisons across multiple character and/or word transformations
#' could result in what is considered equality. But not all equality is
#' considered of equal quality. This function orders the transformations by
#' quality to ensure the best transformation is used for comparison.
#'
#' The order of preference is:
#' 1. Character transformations with no word tokenization, order within
#' this set is equivalent to the order used in [str_mutate_cum()] with
#' preference _always_ for fewer transformations.
#' 2. Word tokenization, possibly with character transformations (_always_
#' preferring fewer character transformations within each group).
#'
#' Word tokenization groups are prioritized in the following order: simple
#' tokenization > with "wpunct" > with "stemmed" > with "stopwords". As with
#' character transformations, fewer transformations are preferred.
#'
#' @md
#' @export
create_how_list <- function(how) {
    box::use(
        purrr,
        ./utils
    )

    how <- match.arg(
        how,
        choices = c("all", str_homogenize_cum_opts),
        several.ok = TRUE
    )

    # error if any of the following are used
    how_error <- dplyr::case_when(
        all(c("numeral", "stopwords") %in% how) && length(unique(how)) == 2 ~
            "'numeral' and 'stopwords' cannot be used as the only `how` inputs",
        # just space and one other tokenize_word_opts = error
        "space" %in% how && any(how %in% tokenize_words_opts) && length(unique(how)) == 2 ~
            "'space' and a single word tokenization used as the only `how` inputs",
        all(c("punct", "wpunct") %in% how) && length(unique(how)) == 2 ~
            "'punct' and 'wpunct' cannot be used as the only `how` inputs",
        TRUE ~ NA_character_
    )
    if (!is.na(how_error)) stop(how_error)

    if ("all" %in% how) how <- str_homogenize_cum_opts

    how_split <- list(
        chr = check_str_mutate_how(how, allow_mismatch = TRUE),
        word = check_tokenize_words_how(how, allow_mismatch = TRUE)
    )

    # drop redundancies
    # - 'wordToken' to avoid redundant combinations with other word transformations
    # - 'case' if in character transformations
    if ("case" %in% how) {
        how_drop <- c("wordToken", "case")
    } else {
        how_drop <- "wordToken"
    }
    how_split$word <- how_split$word[!how_split$word %in% how_drop]

    # get all combinations for character & word alone; NULL if empty
    out <- purrr::map(
        how_split,
        ~ if (length(.x) > 0) {
            utils$combn_all(.x)
        } else {
            NULL
        }
    )

    # add 'wordToken' to all vectors in out$word list
    if (!is.null(out$word)) {
        out$word <- purrr::map(out$word, ~ c("wordToken", .x))
    }

    # add "wordToken" back if it was in how (dropped earlier)
    if ("wordToken" %in% how) out$word <- c(list("wordToken"), out$word)

    if (!is.null(out$chr) && !is.null(out$word)) {
        # create homogenized combination list in desired order
        hhow <- utils$combn_xy(out$word, out$chr)

        # drop any homogenized lists considered errors, probably only necessary
        # when how = "all" since errors are thrown if the user specifies them
        hdrop <- purrr$map_lgl(
            hhow,
            ~ "space" %in% .x && any(!.x %in% str_mutate_opts) ||
                all(c("numeral", "stopwords") %in% .x) ||
                all(c("punct", "wpunct") %in% .x)
        )
        hhow <- hhow[!hdrop]

        # ensure order matches str_homogenize_cum_opts to standardize naming
        out$homogenize <- purrr::map(
            hhow,
            ~ str_homogenize_cum_opts[str_homogenize_cum_opts %in% .x]
        )
    } else {
        out$homogenize <- NULL
    }

    out
}


# str_homogenize() helpers ---------------------------------------------------

# str_homogenize() options; some must be variably excluded, only "space" not
# allowed, but included for informative error message
#' @export
str_homogenize_opts <- str_homogenize_cum_opts[str_homogenize_cum_opts != "space"]


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
        out$chr <- str_mutate_opts[str_mutate_opts %in% str_homogenize_opts]
        out$word <- tokenize_words_opts[
            tokenize_words_opts %in% str_homogenize_opts &
                !tokenize_words_opts %in% out$chr
        ]
    } else {
        # "space" & "punct" + "wpunct" excluded,
        # also need to avoid executing "case" (and possibly others) twice, all
        # duplicates will be executed in str_mutate() first
        out$chr <- str_mutate_opts[str_mutate_opts %in% how]
        out$word <- tokenize_words_opts[tokenize_words_opts %in% how & !tokenize_words_opts %in% out$chr]
    }

    # to use "punct" include it explicitly; if used with "all" will override
    # and exclude "wpunct"
    if ("punct" %in% how) {
        out$word <- out$word[out$word != "wpunct"]
    } else {
        out$chr <- out$chr[out$chr != "punct"]
    }

    # UNACCEPTABLE OUTPUT: "numeral" converts 1 to I, which is removed by
    # "stopwords"
    # TEMPORARY FIX: exclude "numeral" if "stopwords"
    if ("numeral" %in% out$chr && "stopwords" %in% out$word) {
        if (!quiet) {
            warning("Excluding 'numeral' from `how` due to 'stopwords' (e.g. 1 -> I = stopword)")
        }
        out$chr <- out$chr[out$chr != "numeral"]
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

    test_that("tokenize_words() stopwords argument works & is case-insensitive", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-here1")
        sw <- c("case", "punct", "Stopword", "nEEd", "Total", "here")
        expect_equal(
            tokenize_words(x, how = c("wpunct", "stopwords"), stopwords = sw),
            list(
                c("1", "word"),
                c("keep"),
                c("plus"),
                c("and"),
                c("stemming"),
                c("or", "applying", "here1")
            )
        )
    })

    test_that("tokenize_words() handles punctuation as expected", {
        # always treated as word boundaries and removed
        expect_split_rm <- c(
            " ", " ", "-", "–", ",", ";", "?", "(", ")", "[", "]", "@", "/",
            "\\", "&", "%", "^", "+", "<", ">", "−", "≤", "≥"
        )
        expect_equal(
            tokenize_words(
                paste0("start", expect_split_rm, "end"),
                "wpunct"
            ),
            rep(list(c("start", "end")), length(expect_split_rm))
        )

        # never removed from words --> start, end, or between
        expect_retain <- "_"
        expect_equal(tokenize_words("_start", "wpunct"), list("_start"))
        expect_equal(tokenize_words("end_", "wpunct"), list("end_"))
        expect_equal(tokenize_words("start_end", "wpunct"), list("start_end"))

        # removed at start/end, not removed between words --> no spliting
        expect_between <- c(":", ".", "'", "’")
        expect_equal(
            tokenize_words(paste0(expect_between, "start"), "wpunct"),
            rep(list("start"), length(expect_between))
        )
        expect_equal(
            tokenize_words(paste0("end", expect_between), "wpunct"),
            rep(list("end"), length(expect_between))
        )
        expect_equal(
            tokenize_words(
                paste0("start", expect_between, "end"),
                "wpunct"
            ),
            list("start:end", "start.end", "start'end", "start’end")
        )

        # all punctuation removed when no words
        all_punct <- c(expect_split_rm, expect_retain)
        expect_equal(
            tokenize_words(
                paste0(all_punct, collapse = ""),
                "wpunct"
            ),
            list(character(0))
        )
    })

    # str_homogenize() tests -------------------------------------------------
    test_that("str_homogenize() works for character mutations alone", {
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

    test_that("str_homogenize() works for mixed character & word transformations", {
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

    # test shows difference between "punct" which removes punctuation before
    # word splitting (used here: "Total-here1" -> "Totalhere1", "Total" not
    # removed) and "wpunct" which replaces it after (tokenize_words()
    # "stopwords" test above)
    test_that("str_homogenize() stopwords argument works", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-here1")
        sw <- c("case", "punct", "Stopword", "nEEd", "Total", "here")
        expect_equal(
            str_homogenize(x, how = c("punct", "stopwords"), stopwords = sw),
            list(
                c("1", "word"),
                c("keep"),
                c("plus"),
                c("and"),
                c("stemming"),
                c("or", "applying", "Totalhere1")
            )
        )
    })

    # str_homogenize_cum() tests ---------------------------------------------
    test_that("str_homogenize_cum() errors only for disallowed inputs", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")

        expect_error(str_homogenize_cum(x, how = c("space", "stemmed")))
        expect_error(str_homogenize_cum(x, how = c("punct", "wpunct")))
        expect_error(str_homogenize_cum(x, how = c("numeral", "stopwords")))
        expect_no_error(str_homogenize_cum(x, how = "numeral"))
        expect_no_error(str_homogenize_cum(x, how = "stopwords"))
        expect_no_error(str_homogenize_cum(x, how = c("space", "numeral", "stopwords")))
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
            str_homogenize_cum(x, how = "wordToken"),
            list(
                x = as.list(x),
                x_wordToken = str_homogenize(x, how = "wordToken")
            )
        )
        expect_equal(
            str_homogenize_cum(x, how = c("wpunct", "stemmed")),
            list(
                x = as.list(x),
                x_wordToken_wpunct = str_homogenize(x, how = "wpunct"),
                x_wordToken_stemmed = str_homogenize(x, how = "stemmed"),
                x_wordToken_wpunct_stemmed = str_homogenize(x, how = c("wpunct", "stemmed"))
            )
        )
    })

    test_that("str_homogenize_cum() works when `how` has both character & word inputs", {
        x <- c("1 word", "keep CASE", "plus punct.", "and stopword", "need stemming",
               "or applying Total-1")
        expect_equal(
            str_homogenize_cum(x, how = c("numeral", "stemmed")),
            list(
                x = as.list(x),
                x_numeral = str_homogenize(x, how = "numeral"),
                x_wordToken_stemmed = str_homogenize(x, how = "stemmed"),
                x_numeral_wordToken_stemmed = str_homogenize(x, how = c("numeral", "stemmed"))
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
                x_wordToken_stemmed = str_homogenize(x, how = "stemmed", stopwords = sw),
                x_numeral_wordToken_stemmed = str_homogenize(
                    x,
                    how = c("numeral", "stemmed"),
                    stopwords = sw
                )
            )
        )
    })
}
