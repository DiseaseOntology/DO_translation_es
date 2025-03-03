# Combine DO data (except physical disorder branch) for translation review
#
# NOTE: The file from TSG for label & definition translations had all kinds of
# formatting errors. An attempt was made to tidy the initial dataset in a
# Google Sheet (https://docs.google.com/spreadsheets/d/1PNr751jEWm0C4CIDuokZ9vXZDuIS49MZjbWJM1Zln1k/"),
# with the best attempt results saved in the "tidy1" sheet (see "methods" sheet
# for tidying description). The "tidy1" sheet was then downloaded as
# label_def-TSG-ES-all_tidy1.csv. TSG is attempting to fix this file and will
# provide a new version for processing.
#
# The synonyms file was formatted correctly and is used as is here.

box::use(
  here, dplyr, purrr, readr, stringr, tidyr,
  ./mod
)


# INPUT FILES -------------------------------------------------------------

data_dir <- here$here("data/remaining_diseases")

# original English
en_paths <- file.path(
  data_dir,
  c(
    "Label_Def-All-diseases-except-physical-disorders.csv",
    "Syn-All-diseases-except-physical-disorders.csv"
  )
)

es_paths <- file.path(
  data_dir,
  c(
    "label_def-TSG-ES-all_tidy1.csv",
    "Syn-All-diseases-except-physical-disorders-TSG-Es.csv"
  )
)

# ontology (version used for original English sent to TSG)
doid_path <- file.path(data_dir, "doid-v2024-02-28.owl")

# output file prefix
out_prefix <- file.path(data_dir, "disease_en_es-no_pd-")


# Read, tidy, & combine data ----------------------------------------------

# NOTE: Can't use mod$bind because of data quality issues!!
# Combining and reformatting these to make sure data matches what's in the
# ontology up front... will change back to wide format for review after

# LABELS & DEFINITIONS
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

# order can't be preserved for these since translation file was a mess but
# there's only one value for each per class => can be joined
en_ld <- readr$read_csv(en_paths[1]) |>
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

es_ld <- readr$read_csv(es_paths[1])|>
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


# SYNONYMS
# merge synonyms first; original English had some duplicates that may have been
# translated differently => unique() first causes mismatch in records
en_syn <- readr$read_csv(en_paths[2]) |>
  dplyr$rename(synonym = "syn")
es_syn <- readr$read_csv(
  es_paths[2],
  locale = readr$locale(encoding = "latin1")
) |>
  dplyr$rename(sinónimo = "syn") |>
  dplyr$mutate(sinónimo = stringr$str_squish(sinónimo))
syn <- dplyr$bind_cols(en_syn, es_syn) |>
  dplyr$mutate(matches = class == clase)

# confirm classes match
dplyr$count(syn, matches)

which(!syn$matches) |> DO.utils::to_range()

# Write data to file ------------------------------------------------------

# keeping these separate since there were issues with labels & definitions data
readr$write_csv(label_def, file.path(data_dir, "remaining_disease-label_def-es_en-1.csv"))
readr$write_csv(syn, file.path(data_dir, "remaining_disease-syn-es_en-1.csv"))
