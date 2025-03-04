# Combine DO data (except physical disorder branch) for translation review
#
# NOTE: All translation files from TSG had errors that required manual or
# semi-automatic correction. The details of those errors and the process to
# correct them is contained in two Google Sheets. The following is a summary of
# those efforts:
#
# 1. Label & definition translation file
#   - Google sheet: https://docs.google.com/spreadsheets/d/1PNr751jEWm0C4CIDuokZ9vXZDuIS49MZjbWJM1Zln1k/
#   - Data file: data/remaining_diseases/label_def-TSG-ES-all_tidy1.csv
#   - Description: The label & definition translation file had numerous
#   formatting errors that significantly complicated comparison with English. An
#   attempt was made to tidy the initial dataset with the best attempt results
#   saved in the "tidy1" sheet (see "methods" sheet for tidying description),
#   which was then downloaded. TSG is also attempting to fix this file and will
#   provide a new version for processing.
#
# 2. Synonym translation file
#   - Google sheet: https://docs.google.com/spreadsheets/d/166ZhR6h1UEodfnGH9WRjkUM5A4aB0gUE9GbDg7iyjPs/
#   - Data file: data/remaining_diseases/syn_en_es-fixed.csv
#   - Description: Subtle but significant errors in the synonym translation file
#   also necessitated correction (e.g. missing translations, duplicated text).
#   These resulted in English-Spanish misalignment so it was necessary to join
#   the English and Spanish and then fix the issues (see "summary" sheet for
#   details). Some synonyms remain untranslated and are listed in the
#   "missing_translation" sheet, while the final product in the "en_es_fixed"
#   sheet was downloaded.
#
# Additional needed preprocessing is completed here, so this data is ready to
# enter the review pipeline.

box::use(
  here, dplyr, purrr, readr, stringr, tidyr,
  ./mod
)


# INPUT FILES -------------------------------------------------------------

data_dir <- here$here("data/remaining_diseases")

# Label & definitions
en_ld_path <- file.path(
    data_dir,
    "Label_Def-All-diseases-except-physical-disorders.csv"
)

es_ld_path <- file.path(
    data_dir,
    "label_def-TSG-ES-all_tidy1.csv"
)

# already paired synonyms
en_es_syn_path <- file.path(
    data_dir,
    "syn_en_es-fixed.csv"
)

# ontology (version used for original English sent to TSG)
doid_path <- file.path(data_dir, "doid-v2024-02-28.owl")

# output file prefix
out_prefix <- file.path(data_dir, "disease_en_es-")


# Read ontology data ------------------------------------------------------

# Combining and reformatting these to make sure data matches what's in the
# ontology up front... will change back to wide format for review after

doid_data <- mod$robot$get_obo_text(doid_path, id_as = "curie", lang = "any") |>
  dplyr$rename(predicate_id = "predicate") |>
  dplyr$mutate(
    # newlines in text are double-escaped in this file for some reason -> fix
    source_text = stringr$str_replace_all(source_text, "\\\\n", "\n"),
    # 1 "fr", ~ 1/2 "en" --> all determined to be English = standardize
    source_lang = "en"
  ) |>
  # no synonym types in this release, added later; no deprecated diseases (as expected) = not needed
  dplyr$select(-"synonym_type", -"deprecated") |>
  unique()


# Read label & definition data ---------------------------------------------
# NOTE: Can't use mod$bind because of data quality issues!!

# LABELS & DEFINITIONS
# order can't be preserved for these since translation file was a mess but
# there's only one value for each per class => can be joined
en_ld <- readr$read_csv(en_ld_path) |>
  dplyr$rename(source_id = "class") |>
  tidyr$pivot_longer(
    cols = c("label", "def"),
    names_to = "predicate_id",
    values_to = "source_text",
    values_drop_na = TRUE
  ) |>
  dplyr$mutate(
    source_id = mod$general$uri_curie(source_id, to = "curie"),
    predicate_id = dplyr$case_match(
      predicate_id,
      "label" ~ "rdfs:label",
      "def" ~ "IAO:0000115"
    )
  ) |>
  dplyr$left_join(doid_data, by = c("source_id", "predicate_id", "source_text"))

es_ld <- readr$read_csv(es_ld_path)|>
  dplyr$rename(source_id = "clase") |>
  tidyr$pivot_longer(
    cols = c("etiqueta", "def"),
    names_to = "predicate_id",
    values_to = "target_text",
    values_drop_na = TRUE
  ) |>
  dplyr$mutate(
    source_id = mod$general$uri_curie(source_id, to = "curie"),
    predicate_id = dplyr$case_match(
      predicate_id,
      "etiqueta" ~ "rdfs:label",
      "def" ~ "IAO:0000115"
    ),
    target_lang = "es"
  )

label_def <- dplyr$full_join(
  en_ld,
  es_ld,
  by = c("source_id", "predicate_id")
) |>
  dplyr$relocate(
    dplyr$ends_with("lang"),
    "target_text",
    tsg_set = "set",
    .after = dplyr$last_col()
  ) |>
  dplyr$arrange(.data$predicate_id, .data$source_id, .data$target_text)

readr$write_csv(label_def, paste0(out_prefix, "label_def-tidy1.csv"))


# Tidying paired synonym data ---------------------------------------------

# Google Sheets corrupted some of the English during alignment/error correction
# so that it doesn't match the original exactly --> fuzzy string match matching
# (restricted to matching DOID) is implemented to restore the original English
bespoke_match <- function(x, id) {
  stopifnot("`id` must be same length as `x`" = length(id) == length(x))
  box::use(stringdist)
  y_sets <- purrr$map(
    id,
    ~ dplyr$filter(
      doid_data,
      .data$source_id == .x,
      stringr$str_detect(.data$predicate_id, stringr$coll("Synonym"))
    )$source_text
  )
  purrr::map2_chr(
    x,
    y_sets,
    function(.x, .y) {
      best_pos <- stringdist$amatch(.x, .y, maxDist = 3)
      .y[best_pos]
    }
  )
}

# see remaining_disease-TSG_encoding_fix.nb.html for description of need for
# Spanish character error correction
syn_es_fix <- c(
  "\\?" = "ɣ", "Î²" = "β", "î" = "Ó", "ê" = "Í", "ò" = "Ú",
  "\u0083" = "É", "\u008d" = "c", "\u0096" = "ñ", "\u009f" = "ü"
)

syn <- readr$read_csv(en_es_syn_path) |>
  dplyr$select(
    source_id = "class", source_text = "synonym", target_text = "sinónimo"
  ) |>
  dplyr$mutate(
    source_id = mod$general$uri_curie(source_id, to = "curie"),
    target_lang = "es",
    # fix Spanish character errors
    target_text = stringr$str_replace_all(target_text, syn_es_fix)
  )

# fix English text
syn_en_fix <- syn |>
  dplyr::filter(!.data$source_text %in% doid_data$source_text) |>
  dplyr::mutate(
    closest = bespoke_match(.data$source_text, .data$source_id)
  )

syn_tidy <- syn |>
  dplyr$left_join(
    syn_en_fix,
    by = c("source_id", "source_text", "target_text", "target_lang")
  ) |>
  dplyr$mutate(
    source_text = dplyr$if_else(
      !is.na(.data$closest),
      .data$closest,
      .data$source_text)
  ) |>
  dplyr$select(-"closest") |>
  dplyr$left_join(
    dplyr$filter(doid_data, stringr$str_detect(predicate_id, "Synonym")),
    by = c("source_id", "source_text")
  ) |>
  dplyr::relocate("predicate_id", .after = "source_id") |>
  dplyr$relocate("source_lang", "target_lang", .before = "target_text") |>
  dplyr$arrange(.data$source_id, .data$source_text)

readr$write_csv(syn_tidy, paste0(out_prefix, "syn-tidy1.csv"))


# Reformat for review -----------------------------------------------------

# Reformat to label/def/synonym column names in English & Spanish for automated
# review

label_init <- label_def |>
  dplyr::filter(.data$predicate_id == "rdfs:label") |>
  dplyr::select(
    "tsg_set", class = "source_id", label = "source_text", clase = "source_id",
    etiqueta = "target_text"
  )

readr$write_csv(label_init, paste0(out_prefix, "label-init1.csv"))


def_init <- label_def |>
  dplyr::filter(.data$predicate_id == "IAO:0000115") |>
  dplyr::select(
    "tsg_set", class = "source_id", definition = "source_text",
    clase = "source_id", definición = "target_text"
  )

readr$write_csv(def_init, paste0(out_prefix, "def-init1.csv"))


syn_init <- syn |>
  dplyr::select(
    class = "source_id", synonym = "source_text", clase = "source_id",
    sinónimo = "target_text"
  )

readr$write_csv(syn_init, paste0(out_prefix, "syn-init1.csv"))

