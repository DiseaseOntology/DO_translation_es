# pre-process SYMP, TRANS, DISDRIV translation data

box::use(./mod)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)


# Files & versions ---------------------------------------------------------

data_dir <- here::here("data/symp-trans-disdriv")

doid_release <- "2024-04-30"
ontologies <- c("symp", "trans", "disdriv")

output_file <- file.path(data_dir, "symp-trans-disdriv-TSG-es-complete.tsv")


# Custom functions --------------------------------------------------------

tag_lang <- function(.df, lang) {
  dplyr::rename_with(.df, ~ paste0(.x, "_", lang), .cols = dplyr::everything())
}

validate_en_es <- function(.df, cols_sans_lang) {
  out <- dplyr::mutate(
    .df,
    class_valid = .data$class_en == .data$class_es
  )
  res <- list()
  for (.x in cols_sans_lang) {
    res[[paste0(.x, "_valid")]] <- is.na(out[[paste0(.x, "_en")]]) == is.na(out[[paste0(.x, "_es")]])
  }
  out <- dplyr::bind_cols(out, res)

  status <- "valid"
  if (any(!out$class_valid)) {
    status <- "invalid"
    warning(
      "Class IDs do not match between English and Spanish label/definition data:\n  ",
      paste0(which(!out$class_valid), collapse = ", ")
    )
  }

  text_valid_cols <- paste0(cols_sans_lang, "_valid")
  for (col in text_valid_cols) {
    if (any(!out[[col]])) {
      status <- "invalid"
      warning(
        "Mismatch in NA presence between English and Spanish for '",
        gsub("_valid$", "", col), "'\n  Rows: ",
        paste0(which(!out[[col]]), collapse = ", ")
      )
    }
  }

  invisible(list(data = out, status = status))
}

get_import_version <- function(path) {
  x <- readr::read_file(path)
  version <- stringr::str_match(
    x,
    "Sourced from (http:[^<]+)"
  )[,2] |>
    stats::na.omit()
  if (length(version) == 0) {
    version <- stringr::str_match(
      x,
      'owl:versionIRI.*"(http.*\\.owl)'
    )[,2] |>
      stats::na.omit()
  }
  version
}

standardize_in_part <- function(.df, translator, trans_date, status) {
  dplyr::select(.df, -"class_es") |>
    tidyr::pivot_longer(
      cols = -"class_en",
      names_to = c("type", ".value"),
      names_sep = "_",
      values_drop_na = TRUE
    ) |>
    dplyr::rename(
      source_id = "class_en", source_text = "en", translation_text = "es"
    ) |>
    dplyr::mutate(
      translator = translator,
      translation_date = trans_date,
      source_lang = "en",
      translation_lang = "es",
      status = status,
      status_history = NA
    )
}


# Data processing ---------------------------------------------------------

# 1. load originally submitted English & translated Spanish by type, merge, &
#   validate

# 1a. label & definition data
en_labdef <- readr::read_csv(
  file.path(data_dir, "SYMP-TRANS-DISDRIV-LabDef-en.csv")
) |>
  tag_lang("en")

es_labdef <- readxl::read_excel(
  file.path(data_dir, "SYMPTRANS-DISDRIV-LabDef-TSG-Es-1.xlsx"),
  sheet = "Hoja 1 - 0805091722SYMPTRANS-DI"
) |>
  tag_lang("es")

labdef <- dplyr::bind_cols(en_labdef, es_labdef)
valid <- validate_en_es(labdef, c("label", "definition"))
if (valid$status == "invalid") {
  stop("Review data for errors before proceeding.")
}

# 1b. synonyms
en_syn <- readr::read_csv(
  file.path(data_dir, "SYMP-TRANS-DISDRIV-Syn-en.csv")
) |>
  tag_lang("en")

es_syn <- readxl::read_excel(
  file.path(data_dir, "SYMPTRANS-DISDRIV-Syn-TSG-Es.xlsx"),
  sheet = "Hoja 1 - 0805091722SYMPTRANS-DI"
) |>
  tag_lang("es")

syn <- dplyr::bind_cols(en_syn, es_syn)
valid <- validate_en_es(syn, "syn")
if (valid$status == "invalid") {
  stop("Review data for errors before proceeding.")
}


# 2. standardize to "final" format & merge into single data.frame
en_es <- purrr:::map(
  list(labdef, syn),
  ~ standardize_in_part(
    .x,
    translator = "The Spanish Group LLC (https://thespanishgroup.org/)",
    trans_date = "2024-05-21",
    status = "MANUALLY TRANSLATED"
  )
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    translator_info = "Translator: Alexander Largaespada; Reviewer: Salvador G. Ordorica",
    source_id = mod$general$uri_curie(source_id, to = "curie")
  )


# 3. get original ontology data (for predicate info) & validate match to translation data

# 3a. get original ontology data for SYMP, TRANS, & DISDRIV
## NOTE: This data was originally extracted by querying the doid-merged.owl data
##   via the SPARQL endpoint. Thus, the imports will be downloaded from the DOID
##   release site and used as original data (these 3 ontologies are imported to
##   DO in full). Their original sources will be identified directly from their
##   metadata and added as source_version.
import_path <- purrr::map_chr(
  ontologies,
  function(.prefix) {
    file_path <- file.path(data_dir, paste0(.prefix, "_import-", doid_release, ".owl"))
    purl <- paste0(
      "http://purl.obolibrary.org/obo/doid/releases/", doid_release, "/imports/",
      .prefix, "_import.owl"
    )
    res <- utils::download.file(purl, file_path)
    if (res != 0) {
      stop("Error downloading ontology file: ", purl)
    }
    file_path
  }
) |>
  purrr::set_names(ontologies)

ont_data <- purrr::map(
  import_path,
  ~ mod$robot$get_obo_text(
      .x,
      id_as = "curie",
      lang = "any"
    ) |>
    dplyr::mutate(
      source_version = get_import_version(.x),
      deprecated = ifelse(is.na(.data$deprecated), FALSE, .data$deprecated)
    )
) |>
  dplyr::bind_rows(.id = "ontology")

# 3b. validate ontology data
# confirm source_lang is "en" or NA only, then drop it
if(!all(is.na(ont_data$source_lang) | ont_data$source_lang == "en")) {
  stop("Unexpected source_lang values in ontology data")
}
ont_df <- ont_data |>
  # drop text expected to be untranslated (obsolete, ontology metadata)
  #  --> not submitted to TSG
  dplyr::filter(
    !stringr::str_detect(.data$source_id, ".owl$"),
    !deprecated
  ) |>
  dplyr::select(-"source_lang", -"deprecated")


# 4. merge translation & ontology data, and validate
# 4a. merge
full_df <- dplyr::full_join(
  en_es,
  ont_df,
  by = c("source_id", "source_text")
) |>
  # ignore non-DISDRIV terms that were translated
  dplyr::filter(
    stringr::str_detect(
      .data$source_id,
      paste0(stringr::str_to_upper(ontologies), collapse = "|")
    )
  )

# 4b. validate
# 4b-i. check all source_id/source_text pairs have matching ontology data
invalid <- dplyr::filter(
  full_df,
  is.na(.data$translation_text) | is.na(.data$predicate),
  # 3 additional CHEBI terms got pulled in from CHEBI import, not DISDRIV
  # !stringr::str_detect(.data$source_id, "obo:CHEBI_(35681|36835|8884)$")
)
if (nrow(invalid) > 0) {
  stop(
    "Some source_id/source_text pairs do not have matching ontology data:\n  ",
    paste0(invalid$source_id, collapse = ", ")
  )
}

# 4b-ii. check all text 'type' corresponds to expected predicates
invalid <- dplyr::mutate(
  full_df,
  expected_type = dplyr::case_match(
    .data$predicate,
    "rdfs:label" ~ "label",
    "IAO:0000115" ~ "definition",
    "oboInOwl:hasExactSynonym" ~ "syn",
    "oboInOwl:hasRelatedSynonym" ~ "syn",
    "oboInOwl:hasBroadSynonym" ~ "syn",
    "oboInOwl:hasNarrowSynonym" ~ "syn",
    .default = NA_character_
  )
) |>
  dplyr::filter(.data$type != .data$expected_type)
if (nrow(invalid) > 0) {
  stop(
    "Some text 'type' values do not match expected predicate types:\n  ",
    paste0(invalid$source_id, " (", invalid$predicate, " != ", invalid$type, ")", collapse = ", ")
  )
}


# 5. write final standardized data to file
#   --> official columns to be added with or after automated scoring:
#        "auto_review_score", "final_reviewer", "review_notes"
stdd_records <- full_df |>
  dplyr::relocate("predicate", .after = "source_id") |>
  dplyr::relocate(
    "synonym_type", "source_version", "translator_info",
    .before = "status_history"
  ) |>
  dplyr::relocate("ontology", "type", .before = 1) |>
  dplyr::arrange(
    .data$ontology, .data$type,
    .data$source_id, .data$predicate, .data$source_text
  )

readr::write_tsv(stdd_records, output_file)
