#' requires ROBOT accessible from command line, https://robot.obolibrary.org/

# Check if ROBOT is accessible from command line
check_robot <- function() {
  if (Sys.which("robot") == "") {
    stop("ROBOT is not available. Please install according to https://robot.obolibrary.org/")
  }
}


#' Create a ROBOT template
#'
#' Creates a ROBOT template from a standardized data frame with columns
#' `source_id`, `predicate`, `translation_lang`, and `translation_text`.
#'
#' @param .df A standardized data frame.
#' @param path Optional path to save the output robot template file. If `NULL`,
#' the data is not saved.
#'
#' @returns A tibble formatted as a stand alone robot template (see
#' https://robot.obolibrary.org/template).
#'
#' @md
#' @family ROBOT-requiring functions
#' @export
create_robot_template <- function(.df, path = NULL) {
  box::use(
    dplyr, purrr, readr, stringr, tibble, tidyr, tidyselect,
    ./general
  )

  rt_data <- readr$read_tsv(
    "data/robot_template_headers.tsv",
    col_types = "c"
  )

  rt_pivot <- rt_data |>
    dplyr$mutate(
      predicate = stringr$str_extract(template, "[[:alnum:]]+:[[:alnum:]]+")
    ) |>
    dplyr$select(-"template")

  lang <- unique(.df$translation_lang)

  out <- .df |>
    dplyr$left_join(rt_pivot, by = "predicate") |>
    dplyr$mutate(
      header = general$glue_pair(.data$header, lang = .data$translation_lang)
    ) |>
    tidyr$pivot_wider(
      names_from = "header",
      values_from = "translation_text",
      values_fn = ~ paste0(.x, collapse = "|")
    ) |>
    dplyr$rename(id = "source_id")

  ### ADD TEMPLATE ROW ###
  # match template_row & out cols, prior to adding (all cols must be present in
  # both)
  template_row <- build_lang_template_row(rt_data, lang)
  out_cols <- names(out)
  extra_cols <- setdiff(out_cols, names(template_row))
  empty_df <- data.frame(
    matrix(ncol = length(extra_cols), nrow = 1),
    stringsAsFactors = FALSE
  )
  colnames(empty_df) <- extra_cols
  tr <- dplyr$select(template_row, dplyr$any_of(out_cols)) |>
    dplyr$bind_cols(empty_df)
  out <- tibble$add_row(out, tr, .before = 1)

  if (!is.null(path)) {
    delim <- switch(tools::file_ext(path),
      "tsv" = "\t",
      "csv" = ","
    )
    readr$write_delim(out, path, delim, na = "")
  }

  out
}


#' Get text from an OBO ontology file using ROBOT
#'
#' Gets ontology text (limited to English) from an Open Biological and
#' Biomedical Ontology (OBO) Foundry file including labels, definitions, and
#' synonyms with their predicates.
#'
#' @param path Path to the ontology file.
#' @param save Optional path to save the output TSV file. If `NULL`, the data
#' is not saved.
#' @param id_as Whether to return IDs as CURIEs or bracketed URIs.
#' @param lang A language tag, as specified by
#' [RFC 5646](https://datatracker.ietf.org/doc/html/rfc5646), by which to
#' filter the text by. Default is "en" which will also uniquely return text
#' that has no language tag (because OBO ontologies are English by default).
#' All matching language tag variants will be returned (uses SPARQL
#' [langMatches](https://www.w3.org/TR/sparql11-query/#func-langMatches)
#' function internally).
#'
#' @returns A tibble with 5 columns: `source_id`, `predicate`, `source_text`,
#' `synonym_type` (only applicable when predicate is an `oboInOwl` synonym
#' scope), and `deprecated`.
#'
#' @family ROBOT-requiring functions
#'
#' @md
#' @export
get_obo_text <- function(path, save = NULL, id_as = "curie", lang = "en") {
  id_as <- match.arg(id_as, c("curie", "uri"))
  check_robot()

  box::use(
    dplyr, glue, readr, stringr, tibble,
    ./general
  )

  glueV <- function(...) glue$glue(..., .open = "!<<", .close = ">>!")
  if (lang == "en") {
    lang_filter <- 'FILTER(lang(?text) = "" || langMatches(lang(?text), "en"))'
  } else {
    lang_filter <- glueV('FILTER(langMatches(lang(?text), "!<<lang>>!"))')
  }
  query <- glueV('
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?source_id ?predicate ?source_text ?synonym_type ?deprecated
    WHERE {
      VALUES ?predicate {
        rdfs:label
        obo:IAO_0000115
        oboInOwl:hasExactSynonym
        oboInOwl:hasBroadSynonym
        oboInOwl:hasNarrowSynonym
        oboInOwl:hasRelatedSynonym
      }

      ?source_id a owl:Class ;
        ?predicate ?text .

      !<<lang_filter>>!
      BIND(str(?text) AS ?source_text)
      OPTIONAL { ?source_id owl:deprecated ?deprecated }
      OPTIONAL {
        [] owl:annotatedSource ?source_id ;
          owl:annotatedProperty ?predicate ;
          owl:annotatedTarget ?text ;
          oboInOwl:hasSynonymType ?synonym_type .
      }
    }')

  query_file <- tempfile(fileext = ".rq")
  writeLines(query, query_file)

  if (is.null(save)) save <- tempfile(fileext = ".tsv")
  system2(
    "robot",
    c(
      "query",
      "--input", path,
      "--query", query_file, save
    )
  )

  out <- readr$read_tsv(save, show_col_types = FALSE)
  if (nrow(out) == 0) {
    out <- c(rep(list(character(0)), 3), list(logical(0)))
    names(out) <- c("source_id", "predicate", "source_text", "synonym_type",
                    "deprecated")
    return(tibble$as_tibble(out))
  }

  out <- out |>
    dplyr$rename_with(
      .cols = everything(),
      .fn = ~ stringr$str_remove(.x, "^\\?"),
    )

  if (id_as == "curie") {
    out <- out |>
      dplyr$mutate(
        dplyr$across(
          c("source_id", "predicate", "synonym_type"),
          general$uri_curie
        )
      )
  }

  out
}


# create_robot_template() helpers --------------------------------------------

# create headers and robot template codes for one or more languages
build_lang_template_row <- function(rt_data, lang) {
  box::use(dplyr, purrr, tidyr)

  purrr$map(lang, ~ lang_replace(rt_data, .x)) |>
    dplyr$bind_rows() |>
    unique() |>
    tidyr$pivot_wider(names_from = "header", values_from = "template")
}

# replace lang placeholder in internal template with specified language code
lang_replace <- function(rt_data, lang) {
  box::use(
    dplyr, purrr,
    ./general
  )
  dplyr$mutate(
    rt_data,
    dplyr$across(
      dplyr$everything(),
      ~ general$glue_pair(.x, lang = lang)
    )
  )
}
