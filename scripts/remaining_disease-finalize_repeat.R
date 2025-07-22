# This script should flexibly wrap up the whole translation process for all
# diseases & can be run multiple times if needed. It does the following:
#
# 1. Remerges the remaining diseases that were split into multiple sheets during
# review
# 2. Combines the translations of the physical disorder branch with all
# remaining disease translations
# 3. Finalizes the whole set (adding final comparisons, predicates, & acronym
# data; and standardizing the format)
# 4. Adds diseases to -es translation file in DO repo that haven't yet been
# added (to avoid duplication)

box::use(
  dplyr, googledrive, googlesheets4, here, purrr, stringr, readr, tibble, tidyr,
  ./mod
)


# DATA VARIABLES ----------------------------------------------------------

# local data
remaining_input <- here$here("data/remaining_diseases/doid-v2024-02-28.owl")

# DO repo path
do_repo_path <- "~/Documents/Ontologies/HumanDiseaseOntology"

# local output
final_dir <- here$here("data/final")



# Custom functions --------------------------------------------------------

# Standardized processing for DOID input data
process_doid_input <- function(.df) {
  box::use(dplyr, stringr)

  .df |>
    # fix newlines in text that are double-escaped
    dplyr$mutate(
      source_text = stringr$str_replace_all(source_text, "\\\\n", "\n")
    ) |>
    # drop synonyms when there are label-synonym matches to avoid duplication
    dplyr$filter(
      !(any(.data$predicate == "rdfs:label") &
          stringr$str_detect(.data$predicate, "Synonym")),
      .by = c("source_id", "source_text")
    ) |>
    # Drop undesirable columns:
    # 1. no synonym types in original inputs, added later;
    # 2. deprecation for these versions is not relevant
    # 3. source_lang is duplicated, already captured during creation of review
    dplyr$select(-"synonym_type", -"deprecated", -"source_lang") |>
    unique()
}

# converts empty columns from boolean to character
std_col_classes <- function(.df) {
  box::use(dplyr)

  chr_col <- c(
    class = "character", google_translate_to_en = "character",
    clase = "character", google_translate_to_es = "character",
    en_align = "character", match_en = "character", es_align = "character",
    match_es = "character", final_reviewer = "character", review_notes = "character",
    # input specific - labels
    label = "character", etiqueta = "character",
    etiqueta_passed = "character", etiqueta_final = "character",
    # input specific - definitions
    definition = "character", definición = "character",
    definición_passed = "character", definición_final = "character",
    # input specific - synonyms
    synonym = "character", sinónimo = "character",
    sinónimo_passed = "character", sinónimo_final = "character"
  )

  num_col <- c(
    match_rating_en = "numeric", match_rating_es = "numeric",
    match_rating_overall = "numeric"
  )

  dplyr$mutate(
    .df,
    dplyr$across(dplyr$any_of(names(chr_col)), as.character),
    dplyr$across(dplyr$any_of(names(num_col)), as.numeric)
  )
}

# Google Sheets corrupts some of the English so that it doesn't match the
# original exactly (treats it as dates or converts symbols like ɣ --> fuzzy
# string match matching (restricted to matching DOID) is implemented to restore
# the original English to get the exact match
bespoke_id_syn_match <- function(x, id, doid_df) {

  stopifnot("`id` must be same length as `x`" = length(id) == length(x))

  box::use(dplyr, purrr, stringdist, stringr)

  y_sets <- purrr$map(
    id,
    ~ dplyr$filter(
      doid_df,
      .data$source_id == .x,
      stringr$str_detect(.data$predicate, stringr$coll("Synonym"))
    )$source_text
  )
  purrr$map2_chr(
    x,
    y_sets,
    function(.x, .y) {
      best_pos <- stringdist$amatch(.x, .y, maxDist = 3)
      .y[best_pos]
    }
  )
}

# Merge pre-standardized translation review with original DOID data
merge_w_doid <- function(std_df, original) {
  box::use(dplyr)

  std_prep <- std_df |>
    # drop empty predicate col, if it exists
    dplyr$select(-dplyr$any_of("predicate"))

  out <- std_prep |>
    dplyr$left_join(
      original,
      by = c("source_id", "source_text"),
      relationship = "one-to-one"
    ) |>
    dplyr$relocate("predicate", .after = "source_id")

  missing <- is.na(out$predicate)
  if (!any(missing)) return(out)

  warning(
    "Some English text did not match the original exactly, using approximate matching...",
    immediate. = TRUE
  )
  # subset to avoid approximate matching when unnecessary (expensive)
  std_en_fix <- std_prep[missing, ] |>
    dplyr$mutate(
      replacement = bespoke_id_syn_match(.data$source_text, .data$source_id, original),
    ) |>
    dplyr$select(
      "source_id", "source_text", "translation_text", "translation_lang",
      "replacement"
    )

  out <- std_prep |>
    dplyr$left_join(
      std_en_fix,
      by = c("source_id", "source_text", "translation_text", "translation_lang"),
      relationship = "one-to-one"
    ) |>
    dplyr$mutate(
      source_text = dplyr$if_else(
        is.na(.data$replacement),
        .data$source_text,
        .data$replacement
      )
    ) |>
    dplyr$select(-"replacement") |>
    dplyr$left_join(
      original,
      by = c("source_id", "source_text"),
      relationship = "one-to-one"
    ) |>
    dplyr$relocate("predicate", .after = "source_id")

  still_missing <- is.na(out$predicate)
  if (!any(still_missing)) {
    attr(out, "replacement") <- std_en_fix |>
      dplyr::select("source_id", "source_text", "replacement") |>
      dplyr::mutate(index = which(missing))
    return(out)
  }

  sm_n <- sum(still_missing)
  if (sm_n > 5) {
    still_missing <- still_missing[1:5]
    msg_suffix <- paste0(" ... (+", sm_n - 5, " more)")
  } else {
    msg_suffix <- NULL
  }
  msg <- paste0(
    "Some English text could not be matched in rows: ",
    paste0(which(still_missing), collapse = ", "),
    msg_suffix
  )
  stop(msg)
}

# Identifies the best translation for each source text, with first preference
# for the best status (as specified by status_order; best first) and then by the
# highest auto_review_score for the best status (first chosen if multiple
# translations have the same status and score)
preferred_translation <- function(status, score,
                                  status_order = c("final",
                                                   "passed automated review",
                                                   "manually translated")) {
  in_order <- status %in% status_order
  if (!all(in_order)) {
    msg <- paste0(
      "All `status` values must be in `status_order`. Missing: ",
      paste0(unique(status[!in_order]), collapse = ", ")
    )
    stop(msg)
  }

  status_n <- as.integer(factor(status, levels = status_order))
  best_status <- status_n == min(status_n)
  if (sum(best_status) == 1) return(best_status)

  best_both <- best_status & score == max(score[best_status])
  if (sum(best_both) == 1) return(best_both)

  best_both_pos <- which(best_both)
  best_both_first <- best_both
  best_both_first[best_both_pos[-1]] <- FALSE
  best_both_first
}



# LOAD DOID DATA ----------------------------------------------------------

# Latest --> needed for acronym identification & to ensure no deprecated diseases
doid_latest <- mod$robot$get_obo_text(
  file.path(do_repo_path, "src/ontology/doid.owl"), # v2025-03-31
  id_as = "curie",
  lang = "any"
) |>
  dplyr$select(-"source_lang")


# Original translation input for "remaining diseases"
doid_remaining <- mod$robot$get_obo_text(
  remaining_input,
  id_as = "curie",
  lang = "any"
)  |>
  process_doid_input() |>
  # add acronym annotations & deprecation status
  dplyr$left_join(
    doid_latest,
    by = c("source_id", "predicate", "source_text"),
    relationship = "one-to-one"
  )



# LOAD REVIEW DATA --------------------------------------------------------

# upper level review dir for remaining diseases
upper_dir <- "https://drive.google.com/drive/folders/1GqYzQ6ZI5I3pFC82XLKG5P61STxqwn4b"

# list files & folders
upper_obj <- googledrive$drive_ls(upper_dir)

# load original datasets
gs_rem_unsplit <- upper_obj |>
  dplyr$filter(!googledrive$is_folder(upper_obj)) |>
  dplyr$mutate(datatype = stringr$str_remove(name, ".*_")) |>
  dplyr$rowwise() |>
  dplyr$mutate(
    sheets = list(googlesheets4$sheet_names(.data$id)),
    data = list(googlesheets4$read_sheet(ss = .data$id, sheet = .data$sheets[1]))
  )

rem_unsplit <- purrr$set_names(
  gs_rem_unsplit$data,
  gs_rem_unsplit$datatype
)

# identify & load split datasets
rem_split_dirs <- upper_obj |>
  dplyr$filter(googledrive$is_folder(upper_obj))

gs_rem_split <- rem_split_dirs |>
  dplyr$select(datatype = "name", dir_id = "id") |>
  dplyr$rowwise() |>
  dplyr$mutate(gs = list(googledrive$drive_ls(.data$dir_id))) |>
  tidyr$unnest("gs") |>
  dplyr$rowwise() |>
  dplyr$mutate(
    sheets = list(googlesheets4$sheet_names(.data$id)),
    data = list(
      purrr$map(
        .data$sheets,
        ~ if (stringr$str_detect(.x, "^Set[0-9]+|^all$")) {
          googlesheets4$read_sheet(ss = .data$id, sheet = .x) |>
            std_col_classes()
        } else {
          NULL
        }
      ) |>
        dplyr$bind_rows(.id = "set")
    )
  )

remaining_full <- gs_rem_split |>
  dplyr$select("data") |>
  unlist(recursive = FALSE, use.names = FALSE) |>
  split(gs_rem_split$datatype) |>
  purrr$map(dplyr$bind_rows)

# make sure none of records have been lost
remaining_missing <- purrr$map(
  names(rem_unsplit),
  function(.x) {
    cols <- names(rem_unsplit[[.x]])[1:13] # include only non-curator columns
    dplyr$anti_join(
      rem_unsplit[[.x]],
      remaining_full[[.x]],
      by = cols
    )
  }
) |>
  purrr$set_names(nm = names(rem_unsplit))

# if missing due to split, find out which set (exact, passed, or pending/failed)
if (any(purrr$map_int(remaining_missing, nrow) > 0)) {
  add_sets <- function(.df) {
      .df |>
          dplyr$mutate(
              set = dplyr$case_when(
                  .data$match_rating_overall == 1 ~ "exact",
                  .data$match_rating_overall > 0.75 ~ "passed",
                  TRUE ~ "failed"
              )
          )
  }

  purrr$map(
    remaining_missing
    ~ .x |> add_sets() |> dplyr$count(set)
  )

  stop("Some data were lost during the split process. REVIEW...")
} else {
  message("All data were successfully remerged.")
}



# FINALIZE REVIEW ---------------------------------------------------------

remaining_final <- purrr$map(remaining_full, mod$gs_review$finalize_review_df)

# save as R object (for analysis only at this point)
if (!dir.exists(final_dir)) dir.create(final_dir)
save(
  remaining_final,
  file = file.path(final_dir, "finalized_reviews.rda")
)



# STANDARDIZE & MERGE (w/PREDICATES) --------------------------------------

rem_full <- purrr$map(remaining_final, mod$gs_review$standardize_review_df) |>
  dplyr$bind_rows() |>
  dplyr$mutate(source_id = mod$general$uri_curie(source_id, to = "curie")) |>
  unique() |>
  # keep only best translation for each source text
  dplyr$filter(
    preferred_translation(.data$status, .data$auto_review_score),
    .by = c("source_id", "source_text")
  ) |>
  merge_w_doid(doid_remaining)

translation_full <- rem_full



# POST-PROCESS FOR SAVE TO ONTOLOGY ---------------------------------------

# identify translations for deprecated terms
trans_deprecated <- translation_full |>
  dplyr$filter(.data$deprecated)


translation <- translation_full |>
  dplyr$filter(
    is.na(.data$deprecated) | !.data$deprecated,
    status != "manually translated"
  ) |>
  dplyr::select(-"deprecated") |>
  # fix for problems in translation text (extra " & tabs)
  dplyr::mutate(
    translation_text = stringr::str_remove_all(
      .data$translation_text,
      "\\t|\""
    )
  )



# WRITE OUTPUT ------------------------------------------------------------

readr$write_tsv(
  trans_deprecated,
  file.path(final_dir, "doid-es-deprecated-2025-03-31.tsv"),
  na = ""
)

readr$write_tsv(translation, file.path(final_dir, "doid-es.tsv"), na = "")

# in the DOID repo
do_trans_dir <- file.path(do_repo_path, "src/translations")
if (!dir.exists(do_trans_dir)) dir.create(do_trans_dir)

es_exist <- readr$read_tsv(
  file.path(do_trans_dir, "doid-es.tsv")
)

translation_new <- dplyr::anti_join(
  translation,
  es_exist,
  by = c("source_id", "predicate", "source_text")
) |>
  dplyr::mutate(
    review_notes = stringr::str_replace_all(
      .data$review_notes,
      "\\n",
      "; "
    )
  )

if (!interactive()) {
  readr$write_tsv(
    translation_new,
    file.path(do_trans_dir, "doid-es.tsv"),
    na = "",
    append = TRUE
  )

  # CHECK doid-es.tsv -------------------------------------------------------

  es_tsv <- readr$read_tsv(
    file.path(do_trans_dir, "doid-es.tsv"),
    show_col_types = FALSE
  )
}





