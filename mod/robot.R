#' requires ROBOT accessible from command line, https://robot.obolibrary.org/

# Check if ROBOT is accessible from command line
check_robot <- function() {
  if (Sys.which("robot") == "") {
    stop("ROBOT is not available. Please install according to https://robot.obolibrary.org/")
  }
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
#'
#' @returns A tibble with columns `class`, `predicate`, `text`, and
#' `deprecated`.
#'
#' @family ROBOT-requiring functions
#'
#' @md
#' @export
get_obo_text <- function(path, save = NULL) {
  box::use(dplyr, readr, stringr)
  check_robot()

  query <- '
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>

    SELECT ?class ?predicate ?text
    WHERE {
      VALUES ?predicate {
        rdfs:label
        obo:IAO_0000115
        oboInOwl:hasExactSynonym
        oboInOwl:hasBroadSynonym
        oboInOwl:hasNarrowSynonym
        oboInOwl:hasRelatedSynonym
      }

      ?class a owl:Class ;
        ?predicate ?text .
      FILTER(LANG(?text) IN ("", "en"))
      OPTIONAL { ?class owl:deprecated ?deprecated }
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
      .fn = ~ stringr$str_remove(.x, ".*\\?"),
    ) |>
    dplyr$mutate(text = stringr$str_remove(.data$text, "@en$"))

  out
}
