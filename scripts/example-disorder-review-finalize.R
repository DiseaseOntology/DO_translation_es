# Finalize & standardize Spanish translation review for DOID in preparation for
# inclusion in the DOID ontology.

# NOTES:
# 	 1. translation status probably needs to be revised
# 	 2. need to add predicates earlier in this process (at very beginning; next
#     time this is run)
# 	 3. no check currently for english text changes (should do this at beginning
# 		too)
box::use(
  dplyr, googlesheets4, here, purrr, stringr, readr, tidyr,
  ./mod
)

do_repo_path <- "~/Documents/Ontologies/HumanDiseaseOntology"


# Get initial data with Google Translations from GS --------------------------

gs <- "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg"
review <- mod$gs_review$read_gs_review(gs, ss_prefix = "disorder2")

# replace label with disorder3_en_es_label
review$label <- googlesheets4$read_sheet(gs, sheet = "disorder3_en_es_label")


# Determine final translation changes ----------------------------------------

review$label <- mod$gs_review$finalize_review_df(review$label)


# Standardize to format for translation --------------------------------------

std <- purrr$map(review, mod$gs_review$standardize_review_df) |>
  dplyr$bind_rows() |>
  dplyr$mutate(source_id = mod$general$uri_curie(source_id, to = "curie"))

# get predicate data from original version of doid.owl that was provided to
# TSG for translation (should do this first for other data sets)

# get acronym annotations from latest ontology (didn't exist when translations
# were made)
acronym <- mod$robot$get_obo_text(
  file.path(do_repo_path, "src/ontology/doid.owl") # v2024-12-18
) |>
  dplyr$select(-"deprecated")

doid_text <- mod$robot$get_obo_text(
  here$here("data/physical_disorder/doid-v2023-10-21.owl")
) |>
  dplyr$select(-"synonym_type") |>
  # add acronym annotations (hopefully mostly the same)
  dplyr$left_join(
    acronym,
    by = c("source_id", "predicate", "source_text")
  )

# fix double-escaped return (not sure why this issue is happening)
doid_text[16500, "source_text"] <- doid_text[[16500, "source_text"]] |>
  stringr$str_replace(stringr$coll("\\n"), "\n")

# check to make sure all match (should be 0)
dplyr$anti_join(std, doid_text, by = c("source_id", "source_text"))

# add predicates
std <- std |>
  dplyr$select(-"predicate") |>
  dplyr$left_join(doid_text, by = c("source_id", "source_text")) |>
  dplyr$relocate("predicate", .after = "source_id")

# ensure none deprecated, then drop deprecated column
if (all(is.na(std$deprecated))) {
  std$deprecated <- NULL
} else {
  stop("Some data are deprecated")
}

# replace newline chars with pipe (newlines cause errors in tsv outputs)
std <- std |>
  dplyr$mutate(
    dplyr$across(
      dplyr$where(is.character),
      ~ stringr$str_replace_all(.x, "\n", "|")
    )
  )

# Write to file --------------------------------------------------------------

# in this repo
local_data <- here$here("data/physical_disorder")
if (!dir.exists(local_data)) dir.create(local_data)
readr$write_tsv(std, file.path(local_data, "doid-es-pd.tsv"))

# in the DOID repo
do_trans_dir <- file.path(do_repo_path, "src/translations")
if (!dir.exists(do_trans_dir)) dir.create(do_trans_dir)

readr$write_tsv(std, do_trans_dir, na = "")


# Generate robot template ----------------------------------------------------

if (!dir.exists(file.path(do_repo_path, "build/translations"))) {
  dir.create(file.path(do_repo_path, "build/translations"), recursive = TRUE)
}
rt <- mod$robot$create_robot_template(
  std,
  file.path(do_repo_path, "build/translations/doid-es-rt.tsv")
)
