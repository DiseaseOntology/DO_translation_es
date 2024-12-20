#' Generate Summary of Columns
#'
#' `digits` is passed to [round()]
#' @inheritParams round
col_summary <- function(.df, .cols, digits = NULL) {
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

    box::use(dplyr)

    out <- dplyr$reframe(.df, dplyr$across({{ .cols }}, .fn))

    if (nrow(out) == 7) val_nm <- append(val_nm, "NA")
    out <- dplyr$mutate(out, stat = val_nm, .before = 1) |>
        # move median after quantiles
        dplyr$arrange(
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


#' Paste Non-missing Values
#'
#' A wrapper around [base::paste()] that pastes only values that exist with
#' results that depend on non-missing value dominance. If non-missing values are
#' dominant, they are preserved and missing values are dropped. If they are not,
#' missing values are treated as dominant and the output is `NA` without any
#' pasting. No `sep` or `collapse` delimiters are added to output because of
#' concatenation with missing or empty values.
#'
#' @inheritParams base::paste
#' @param dominant Whether to treat non-missing values as dominant over missing
#' values (default: `TRUE`), dropping `NA` and pasting, or not, returning `NA`
#' at every position with an `NA`. NOTE: `dominant` only applies with the `sep`
#' operator. Missing values are _NEVER_ dominant when applying `collapse` and
#' will _always_ be dropped.
#'
#' @section: Recycle Rules:
#' Unlike [base::paste()], `paste_present()` will only recycle length-1 inputs.
#' Inputs with different lengths that are not length-1 results in an error.
#'
#' Also, length-0 inputs are always ignored independent of the `dominant`
#' argument. There is no equivalent to `recycle0` in [base::paste()].
#'
#' @md
#' @export
paste_present <- function(..., sep = " ", collapse = NULL, dominant = TRUE) {
  dots <- list(...)
  dlen <- vapply(dots, length, 1L)
  stopifnot(
    "All inputs must be the same length or <= length 1" =
      length(unique(dlen)) == 1 || all(dlen[dlen != max(dlen)] <= 1)
  )

  if (dominant) {
    .fn <- function(x, .sep) paste0(x[!is.na(x)], collapse = .sep)
  } else {
    .fn <- function(x, .sep) {
      if (any(is.na(x))) {
        return(NA)
      } else {
        paste0(x, collapse = .sep)
      }
    }
  }
  out <- apply(cbind(...), 1, function(x) .fn(x, .sep = sep))
  out[out == ""] <- NA_character_

  if (!is.null(collapse)) {
    out <- paste0(out[!is.na(out)], collapse = collapse)
    out[out == ""] <- NA_character_
  }

  out
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
#' @param na How to handle NA values. Options are:
#'
#' * `"omit"` (default): To calculate the percent match ignoring `NA` values.
#'
#' * `"include"`: To calculate the percent match with `NA` values counting as
#' matches.
#'
#' * `"not_match"`: To treat `NA` values as non-matches but include them for
#' the sake of calculate the possible matches (i.e. `NA` values are only
#' included in the denominator and will reduce the percent match).
#'
#' @returns A numeric vector of percent match values.
#' @examples
#' pct_match(1:2, 2:3)                        # 50
#' pct_match(c("a", "b", "c"), c("c", "d"))   # 33.33
#' pct_match(list(1:2, 1), list(2:3, 1:5))    # c(50, 20)
#'
#' @section Note on Duplicates:
#' Duplicate values are included in calculating the total matches possible
#' but _NOT_ the total matches found. While this is okay when duplicates exist
#' in only one input (example: `pct_match(c(2L, 2L), 2:3) == 50`), it seems
#' unintuitive when duplicates are found in both, with a percent match that is
#' arbitrarily lower (example: `pct_match(c(1:2, 2), c(2:3, 2)) == 33.33
#' # expect 66.67).
#'
#' @md
#' @export
pct_match <- function(x, y, digits = 2, na = "omit") {
  na <- match.arg(na, c("omit", "include", "not_match"))
  if (class(x) != class(y)) stop("`x` and `y` must be the same class")
  if (is.list(x) && length(x) != length(y)) {
    stop("If lists, `x` and `y` must be the same length")
  }

  box::use(purrr)

  if (is.list(x)) {
    out <- purrr$map2_dbl(x, y, ~ pct_fn(.x, .y, digits, na))
  } else {
    out <- pct_fn(x, y, digits, na)
  }

  out
}


#' Convert between URIs and CURIEs
#'
#' Convert between URIs and CURIEs for a limited set of predicates, commmonly
#' found in Open Biological and Biomedical Ontology (OBO) Foundry ontologies.
#' OBO Foundry ontology identifiers should make the round trip from
#' URI-to-CURIE and back via the `obo:` prefix but more specific ontology CURIEs
#' may not be recognized (e.g. `UBERON:0000002`).
#'
#' @param x A character vector of URIs or CURIEs.
#' @param to The desired output format, either "curie" or "uri".
#' @param bracket Whether to include angle brackets around the output.
#'
#' @returns A character vector of the converted URIs or CURIEs.
#'
#' @examples
#' .curie <- c(
#'   "rdfs:comment", "dc:date", "terms:license", "owl:deprecated",
#'   "oboInOwl:id", "UBERON:0000002", "DOID:0001816", "doid:DO_AGR_slim"
#' )
#'
#' uri_curie(.curie, to = "uri")
#'
#' .uri <- c(
#'   "http://www.w3.org/2000/01/rdf-schema#comment",
#'   "http://purl.org/dc/elements/1.1/date",
#'   "http://purl.org/dc/terms/license",
#'   "http://www.w3.org/2002/07/owl#deprecated",
#'   "http://www.geneontology.org/formats/oboInOwl#id",
#'   "http://purl.obolibrary.org/obo/UBERON_0000002",
#'   "http://purl.obolibrary.org/obo/DOID_0001816",
#'   "http://purl.obolibrary.org/obo/doid#DO_AGR_slim",
#'   "<http://www.geneontology.org/formats/oboInOwl#hasDbXref>"
#' )
#'
#' uri_curie(.uri, to = "curie")
#'
#' @md
#' @export
uri_curie <- function(x, to = "curie", bracket = FALSE) {
  box::use(purrr, stringr)

  to <- match.arg(to, c("curie", "uri"))

  sanitized <- stringr$str_trim(x) |>
    stringr$str_remove_all("^<|>$")

  recode_vctr <- c(
    `http://www.geneontology.org/formats/oboInOwl#` = "oboInOwl:",
    `http://www.w3.org/1999/02/22-rdf-syntax-ns#` = "rdf:",
    `http://www.w3.org/2000/01/rdf-schema#` = "rdfs:",
    `http://www.w3.org/2004/02/skos/core#` = "skos:",
    `http://purl.obolibrary.org/obo/DOID_` = "DOID:",
    `http://purl.obolibrary.org/obo/IAO_` = "IAO:",
    `http://www.w3.org/2001/XMLSchema#` = "xsd:",
    `http://purl.org/dc/elements/1.1/` = "dc:",
    `http://purl.obolibrary.org/obo/` = "obo:",
    `http://www.w3.org/2002/07/owl#` = "owl:",
    `http://purl.org/dc/terms/` = "terms:"
  )

  if (to == "uri") {
    recode_vctr <- purrr$set_names(names(recode_vctr), recode_vctr)
  }

  out <- stringr$str_replace_all(sanitized, recode_vctr)

  if (bracket) {
    out <- stringr$str_replace_all(out, c("^<?" = "<", ">$" = ">"))
  }

  out
}


# pct_match() helpers --------------------------------------------------------

# pct_fn() compares only 2 vector sets
pct_fn <- function(.x, .y, digits = 2, na = "omit") {
  stopifnot(".x & .y must be atomic vectors" = is.atomic(.x) && is.atomic(.y))

  if (na != "omit") max_len <- max(c(length(.x), length(.y)))
  if (na %in% c("omit", "not_match")) {
    .x <- .x[!is.na(.x)]
    .y <- .y[!is.na(.y)]
  }
  if (na == "omit") max_len <- max(c(length(.x), length(.y)))

  # return NA if both sets are empty or only include NA values after applying
  # the `na` argument
  if (max_len == 0) {
    return(NA_real_)
  }

  round(length(intersect(.x, .y)) / max_len * 100, digits = digits)
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

    # paste_present() tests --------------------------------------------------
    test_that("paste_present() works", {
      .x <- c(1, NA, NA, "a")
      .y <- c(NA, 2, NA, "b")
      expect_equal(paste_present(.x, .y), c("1", "2", NA, "a b"))
      expect_equal(paste_present(.x, .y, sep = "|"), c("1", "2", NA, "a|b"))
      expect_equal(paste_present(.x, .y, collapse = "|"), c("1|2|a b"))
      expect_equal(paste_present(rep(NA, 2), rep(NA, 2)), rep(NA_character_, 2))
      expect_equal(
        paste_present(rep(NA, 2), rep(NA, 2), collapse = "|"),
        NA_character_
      )
      # works with more than 2 inputs
      .z <- letters[1:4]
      expect_equal(paste_present(.x, .y, .z), c("1 a", "2 b", "c", "a b d"))
      # ONLY recycle length 1 inputs (differs from paste())
      expect_equal(paste_present(.x, .y, "z"), c("1 z", "2 z", "z", "a b z"))
      expect_error(paste_present(.x, .y[-1]))
      # length 0 inputs are ignored
      expect_equal(paste_present(.x, .y, character()), c("1", "2", NA, "a b"))
    })

    test_that("paste_present(dominant = FALSE) works", {
      .x <- c(1, 2, NA, "a")
      .y <- c(NA, 3, NA, "b")
      expect_equal(
        paste_present(.x, .y, dominant = FALSE),
        c(NA, "2 3", NA, "a b")
      )
      expect_equal(
        paste_present(.x, .y, sep = "|", dominant = FALSE),
        c(NA, "2|3", NA, "a|b")
      )
      expect_equal(
        paste_present(.x, .y, collapse = "|", dominant = FALSE),
        "2 3|a b"
      )
      # no effect when all NAs
      expect_equal(
        paste_present(rep(NA, 2), rep(NA, 2), dominant = FALSE),
        rep(NA_character_, 2)
      )
      expect_equal(
        paste_present(rep(NA, 2), rep(NA, 2), collapse = "|", dominant = FALSE),
        NA_character_
      )
      # works with more than 2 inputs
      .z <- letters[1:4]
      expect_equal(
        paste_present(.x, .y, .z, dominant = FALSE),
        c(NA, "2 3 b", NA, "a b d")
      )
      # ONLY recycle length 1 inputs (differs from paste())
      expect_equal(
        paste_present(.x, .y, "z", dominant = FALSE),
        c(NA, "2 3 z", NA, "a b z")
      )
      expect_error(paste_present(.x, .y[-1], dominant = FALSE))
      # length 0 inputs are ignored
      expect_equal(
        paste_present(.x, .y, character(), dominant = FALSE),
        c(NA, "2 3", NA, "a b")
      )
    })

    # pct_match() tests ------------------------------------------------------
    test_that("pct_match() works", {
      expect_equal(pct_match(1:2, 2:3), 50)
      expect_equal(pct_match(c("a", "b", "c"), c("c", "d")), 33.33)
      expect_equal(pct_match(list(1:2, 1), list(2:3, 1:5)), c(50, 20))
    })

    test_that("pct_match() duplicate handling remains the same", {
      # could maybe solve with https://cran.r-project.org/package=vecsets
      expect_equal(pct_match(c(2L, 2L), 2:3), 50) # okay here
      expect_equal(pct_match(c("c", "b", "c"), c("c", "d", "c")), 33.33) # unintuitive here, expect = 66.67
    })

    test_that("pct_match() na arg works", {
      # na = "omit" (default) maintains backward compatibility (with
      # expectations... since NAs weren't appropriately handled before)
      expect_equal(pct_match(NA, NA, na = "omit"), NA_real_)
      expect_equal(pct_match(NA, NA, na = "include"), 100)
      expect_equal(pct_match(NA, NA, na = "not_match"), 0)
      expect_equal(pct_match(c(1, NA), c(1, 2, NA), na = "omit"), 50)
      expect_equal(pct_match(c(1, NA), c(1, 2, NA), na = "include"), 66.67)
      expect_equal(pct_match(c(1, NA), c(1, 2, NA), na = "not_match"), 33.33)
      expect_equal(
        pct_match(list(1:2, c(1, NA)), list(2:3, c(1:5, NA)), na = "omit"),
        c(50, 20)
      )
      expect_equal(
        pct_match(list(1:2, c(1, NA)), list(2:3, c(1:5, NA)), na = "include"),
        c(50, 33.33)
      )
      expect_equal(
        pct_match(list(1:2, c(1, NA)), list(2:3, c(1:5, NA)), na = "not_match"),
        c(50, 16.67)
      )
    })

    # uri_curie() tests ------------------------------------------------------
    test_that("uri_curie() works", {
      .curie <- c(
        "rdfs:comment", "dc:date", "terms:license", "owl:deprecated",
        "oboInOwl:id", "UBERON:0000002", "DOID:0001816", "doid:DO_AGR_slim"
      )

      expect_equal(
        uri_curie(.curie, to = "uri"),
        c(
          "http://www.w3.org/2000/01/rdf-schema#comment",
          "http://purl.org/dc/elements/1.1/date",
          "http://purl.org/dc/terms/license",
          "http://www.w3.org/2002/07/owl#deprecated",
          "http://www.geneontology.org/formats/oboInOwl#id",
          "UBERON:0000002",
          "http://purl.obolibrary.org/obo/DOID_0001816",
          "doid:DO_AGR_slim"
        )
      )

      .uri <- c(
        "http://www.w3.org/2000/01/rdf-schema#comment",
        "http://purl.org/dc/elements/1.1/date",
        "http://purl.org/dc/terms/license",
        "http://www.w3.org/2002/07/owl#deprecated",
        "http://www.geneontology.org/formats/oboInOwl#id",
        "http://purl.obolibrary.org/obo/UBERON_0000002",
        "http://purl.obolibrary.org/obo/DOID_0001816",
        "http://purl.obolibrary.org/obo/doid#DO_AGR_slim",
        "<http://www.geneontology.org/formats/oboInOwl#hasDbXref>"
      )
      expect_equal(
        uri_curie(.uri, to = "curie"),
        c(
          "rdfs:comment", "dc:date", "terms:license", "owl:deprecated",
          "oboInOwl:id", "obo:UBERON_0000002", "DOID:0001816",
          "obo:doid#DO_AGR_slim", "oboInOwl:hasDbXref"
        )
      )
    })
}
