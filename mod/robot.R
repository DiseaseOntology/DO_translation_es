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
#'
#' @returns A tibble with columns `source_id`, `predicate`, `source_text`,
#' and `deprecated`.
#'
#' @family ROBOT-requiring functions
#'
#' @md
#' @export
get_obo_text <- function(path, save = NULL, id_as = "curie") {
  id_as <- match.arg(id_as, c("curie", "uri"))
  check_robot()

  box::use(
    dplyr, readr, stringr,
    ./general
  )

  query <- '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?source_id ?predicate ?source_text ?deprecated
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
        ?predicate ?source_text .
      FILTER(LANG(?source_text) IN ("", "en"))
      OPTIONAL { ?source_id owl:deprecated ?deprecated }
    }'

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

  out <- readr$read_tsv(save, show_col_types = FALSE) |>
    dplyr$rename_with(
      .cols = everything(),
      .fn = ~ stringr$str_remove(.x, "^\\?"),
    ) |>
    dplyr$mutate(source_text = stringr$str_remove(.data$source_text, "@en$"))

  if (id_as == "curie") {
    out <- out |>
      dplyr$mutate(
        dplyr$across(c("source_id", "predicate"), general$uri_curie)
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
