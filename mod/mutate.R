#' Mutate Strings
#'
#' Applies specified changes to a character vector.
#'
#' @param x A character vector.
#' create how documentation from str_mutate_opts
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

        .fn <- switch(how,
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


# str_mutate() helpers -------------------------------------------------

# opts must match function names in str_mutate()
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


### TESTS ####################################################################
# Use RStudio "Run Tests" button to execute these tests interactively
if (is.null(box::name())) {
    box::use(testthat[...])

    # str_mutate()
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

    # str_mutate_cum()
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

    # to_roman() tests
    test_that("to_roman() works", {
        x <- c("1", "4", "5", "10", "1and10", "110by50", "2by125and11")
        expect_equal(
            to_roman(x),
            c("I", "IV", "V", "X", "IandX", "CXbyL", "IIbyCXXVandXI")
        )
    })
}
