# This script should flexibly wrap up the whole translation process for all
# diseases, include compare text with the latest version. It's designed to be
# run multiple times and overwrite data in the DO src/translations dir.
# It does the following:
#
# 1. Merges disease translation & review data (original set for physical
# disorder and "remaining" diseases)
# 3. Finalizes the whole set (adding final comparisons, predicates, & acronym
# data; and standardizing the format)
# 4. Compares translated & reviewed text with text in the latest doid.owl
# (ASSUMPTION: this will be run after the standard release procedure)
# 5. Creates the following files locally (with datestamp) and in the DO
# src/translations directory:
#   1) doid-es-all.tsv: everything!!
#   2) doid-es-deprecated.tsv: deprecated text
#   3) doid-es-untranslated.tsv: untranslated text that does NOT match
#   previously translated text
#   4) doid-es-changed.tsv: untranslated text that matches previously translated
#   text
#   5) doid-es-translated.tsv: translated text that has not been accepted for
#   production release
#   6) doid-es.tsv: translated text that has been accepted for production release
#
# FUTURE UPDATES:
# 1. Convert more data from finalized review to standardized output
#   - Add translation dates (& review dates)
#   - Include all translations (original TSG, google, and final)
#   - Keep backtranslations?
# 2. Include translations from additional translation & review (e.g. for new
# terms) *** high priority ***
# 3. Create procedure for approximate matching to recommend candidate translations


box::use(
  dplyr, googledrive, googlesheets4, here, purrr, stringr, readr, tibble, tidyr,
  ./mod
)


# DATA VARIABLES ----------------------------------------------------------

# sheets with latest versions of the physical disorder review
pd_sheets <- c(
  "disorder3_en_es_label", "disorder2_en_es_definition", "disorder2_en_es_synonym"
)

# DO repo path
do_repo_path <- "~/Documents/Ontologies/HumanDiseaseOntology"

# local output
final_dir <- here$here("data/final")



# CUSTOM FUNCTIONS --------------------------------------------------------

#' Access & merge Google Sheets datasets safely
#'
#' @param gs A Google Sheet (workbook) identifier.
#' @param sheets_list The sheet names of each Google Sheet, as a character
#' vector.
merge_gs <- function(gs, sheets_list) {
  gs_vals <- purrr$map(sheets_list, ~ access_gs(gs, .x))
  failed <- purrr$map(gs_vals, ~ attr(.x, "failed")) |>
    unlist()

  if (length(failed) > 0) {
    rlang::warn(
      c(
        paste0("One or more sheets for", gs, " could not be accessed:"),
        purrr$set_names(failed, rep("x", length(failed)))
      )
    )
    return(gs_vals)
  }
  dplyr$bind_rows(gs_vals, .id = "set")
}

# attempts to access all sheets listed from gs (limited to "Set" or "all")
access_gs <- function(gs, .sheet) {
  if (!stringr$str_detect(.sheet, "^Set[0-9]+|^all$")) return(NULL)

  gs_res <- read_gs_safely(ss = gs, sheet = .sheet)
  if (is.null(gs_res$result) || !is.null(gs_res$error)) {
    status <- try(gs_res$error$resp$status)
    if (class(status) != "try-error" && status == "429") {
      Sys.sleep(120)
      gs_res <- read_gs_safely(ss = gs, sheet = .sheet)
      if (is.null(gs_res$result) || !is.null(gs_res$error)) {
        attr(gs_res, "failed") <- .sheet
        return(gs_res)
      }
    } else {
      attr(gs_res, "failed") <- .sheet
      return(gs_res)
    }
  }
  std_col_classes(gs_res$result)
}

read_gs_safely <- purrr$safely(googlesheets4$read_sheet)

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


# save files
write_output <- function(.df, output_dir, lang = "es") {
  box::use(readr, stringr)

  if (stringr$str_detect(output_dir, "HumanDiseaseOntology")) {
    datestamp <- ""
  } else {
    # if output_dir is not in DO repo, assumed to be local (add datestamp)
    datestamp <- paste0("-", format(Sys.Date(), "%Y%m%d"))
  }

  .df_nm <- as.character(substitute(.df))
  if (stringr$str_detect(.df_nm, "^all$|_all")) {
    file_nm <- paste0("doid-", lang, "-all", datestamp, ".tsv")
  } else if (stringr$str_detect(.df_nm, "production")) {
    file_nm <- paste0("doid-", lang, datestamp, ".tsv")
  } else {
    file_nm <- paste0("doid-", lang, "-", .df_nm, datestamp, ".tsv")
  }

  file_path <- file.path(output_dir, file_nm)

  # maintain standard sort order
  .df |>
    dplyr$arrange(.data$source_id, .data$predicate, .data$source_text) |>
    readr$write_tsv(file_path, na = "", quote = "needed")
}


# LOAD DATA ---------------------------------------------------------------

#### Latest DOID ###
# needed for acronym identification & to assess text changes
doid_latest <- mod$robot$get_obo_text(
  file.path(do_repo_path, "src/ontology/doid.owl"), # v2025-03-31
  id_as = "curie",
  lang = "any"
) |>
  dplyr$select(-"source_lang") |>
  dplyr$mutate(deprecated = tidyr$replace_na(.data$deprecated, FALSE))


### REVIEW BATCH: physical disorder ###

gs_pd <- googledrive$as_dribble(
  "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg"
) |>
  dplyr$mutate(
    sheets = list(googlesheets4$sheet_names(.data$id)),
  ) |>
  tidyr$unnest("sheets") |>
  dplyr$filter(.data$sheets %in% pd_sheets) |>
  dplyr$mutate(
    datatype = stringr$str_remove(.data$sheets, ".*_en_es_"),
    .before = "sheets"
  ) |>
  dplyr$rowwise() |>
  dplyr$mutate(
    data = list(googlesheets4$read_sheet(ss = .data$id, sheet = .data$sheets))
  )


### REVIEW BATCH: remaining diseases ###

# Remaining disease data is in 2 groups with the data, all contained in
# this upper level review dir
upper_dir <- "https://drive.google.com/drive/folders/1GqYzQ6ZI5I3pFC82XLKG5P61STxqwn4b"

upper_obj <- googledrive$drive_ls(upper_dir)

# load "unsplit" datasets --> no review occurring here
# these are included ONLY to make sure no records are lost during review in
# "split" datasets
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

# load split datasets --> where review actually happens
# these were split into different "files" based on data quality (auto_review_score)
# & split further into "sheets" in those files to allow work in chunks
# --> splitting by data quality helps in review, but chunking doesn't seem to
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
    data = list(merge_gs(.data$id, .data$sheets))
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
    remaining_missing,
    ~ .x |> add_sets() |> dplyr$count(set)
  )

  stop("Some data were lost during the split process. REVIEW...")
} else {
  message("All split 'remaining' data were successfully remerged.")
}


# get 3rd round of translations
new_trans <- googlesheets4::read_sheet(
  "1t9AyvuJJeLZa4cBS4pNi2-dVdDf0ZrYSDQ-dARmr2r0",
  "labels",
  skip = 1
) |>
  dplyr::mutate(
    translator = "Google Translate",
    source_lang = "en",
    translation_lang = "es",
    status = dplyr::if_else(!is.na(final_reviewer), "final", "untranslated")
  ) |>
  dplyr::filter(status == "final") |>
  dplyr::select(dplyr::any_of(names(rem_full)))



# FINALIZE REVIEWS --------------------------------------------------------

pd_final <- purrr$map(
  gs_pd$data,
  ~ mod$gs_review$finalize_review_df(std_col_classes(.x))
) |>
  purrr$set_names(gs_pd$datatype)

remaining_final <- purrr$map(remaining_full, mod$gs_review$finalize_review_df)



# STANDARDIZE & MERGE -----------------------------------------------------

pd_full <- purrr$map(pd_final, mod$gs_review$standardize_review_df) |>
  dplyr$bind_rows()

rem_full <- purrr$map(remaining_final, mod$gs_review$standardize_review_df) |>
  dplyr$bind_rows()

trans_full <- dplyr$bind_rows(pd_full, rem_full, new_trans) |>
  dplyr$mutate(source_id = mod$general$uri_curie(source_id, to = "curie")) |>
  unique() |>
  # add latest predicate for comparison
  dplyr$left_join(
    dplyr$rename(doid_latest, predicate_latest = "predicate"),
    by = c("source_id", "source_text")
  ) |>
  # ensure all text from deprecated terms is marked as deprecated
  # --> it wasn't being marked when the text changed (e.g. "obsolete" added to label)
  dplyr$mutate(
    deprecated = any(.data$deprecated),
    .by = "source_id"
  )



# POST-PROCESS FOR SAVE TO ONTOLOGY ---------------------------------------

# modify trans_full to include potential translation CANDIDATES for text that
# has changed
trans_candidate <- trans_full |>
  dplyr$select("source_id":"review_notes") |>
  dplyr$rename("source_id_new" = "source_id", "predicate_new" = "predicate") |>
  # identify preferred translation
  dplyr$mutate(
    best = preferred_translation(
      .data$status, .data$auto_review_score,
      status_order = c("final", "passed automated review", "manually translated")
    ),
    .by = "source_text"
  ) |>
  dplyr$mutate(
    final_reviewer = NA_character_,
    review_notes = NA_character_
  )

# get latest ontology text that was NOT in original review & add possible
# translation CANDIDATES ()
latest_add <- doid_latest |>
  # keep only active terms
  dplyr$filter(!.data$deprecated) |>
  # keep only non-exact matches
  dplyr$anti_join(
    trans_full,
    by = c("source_id", "predicate", "source_text")
  ) |>
  dplyr$left_join(
    trans_candidate,
    by = "source_text",
    na_matches = "never",
    relationship = "many-to-many"
  ) |>
  dplyr$mutate(
    preferred = dplyr$case_when(
      is.na(.data$source_id_new) ~ 0,
      # this can't happen because it's been filtered out
      # .data$source_id == .data$source_id_new & .data$predicate == .data$predicate_new ~ 1,
      .data$source_id == .data$source_id_new ~ 1,
      .data$predicate == .data$predicate_new ~ 2,
      .data$best ~ 3,
      TRUE ~ 4
    )
  ) |>
  dplyr$filter(
    .data$preferred == min(.data$preferred),
    .by = c("source_id", "predicate", "source_text")
  ) |>
  dplyr$mutate(
    source_lang = "en",
    translation_lang = "es",
    status = dplyr$case_when(
      .data$preferred == 0 & !.data$source_id %in% trans_full$source_id ~ "UNTRANSLATED: new entity",
      .data$preferred == 0 ~ "UNTRANSLATED",
      .data$preferred == 1 ~ paste0("CANDIDATE: from ", .data$predicate_new),
      .data$preferred == 2 ~ paste0("CANDIDATE: from ", .data$source_id_new),
      .data$preferred > 2 ~ paste0("CANDIDATE: from ", .data$source_id_new, "-", .data$predicate_new)
    )
  ) |>
  dplyr$select(-"best", -"preferred", -"source_id_new", -"predicate_new")

# compare existing translations with latest ontology text and add new
# text needing translation
trans_all <- trans_full |>
  # downgrade status for quick formatting reviews (e.g. reviewer = JAB-quick)
  # --> did not include translation quality review
  dplyr$mutate(
    status = dplyr$if_else(
      !is.na(.data$final_reviewer) &
        stringr$str_detect(.data$final_reviewer, stringr$coll("quick")),
      "manually translated",
      .data$status
    )
  ) |>
  dplyr$mutate(
    status_history = dplyr$if_else(
      .data$predicate != .data$predicate_latest | is.na(.data$predicate_latest) |
        .data$deprecated,
      toupper(.data$status),
      NA_character_
    ),
    status = dplyr$case_when(
      .data$deprecated ~ "DEPRECATED-ENTITY",
      is.na(.data$predicate_latest) ~ "DEPRECATED-SOURCE TEXT",
      .data$predicate == .data$predicate_latest ~ toupper(.data$status),
      paste0(.data$source_id, .data$source_text) %in%
        paste0(doid_latest$source_id, doid_latest$source_text) ~ "DEPRECATED-PREDICATE",
      paste0(.data$predicate, .data$source_text) %in%
        paste0(doid_latest$predicate, doid_latest$source_text) ~ "DEPRECATED-SOURCE ID",
      .data$source_text %in% doid_latest$source_text ~ "DEPRECATED-SOURCE ID, PREDICATE",
    )
  ) |>
  dplyr$bind_rows(latest_add) |>
  dplyr$select(-"deprecated", -"predicate_latest")

### TEMPORARY fixes ###
# 1. retain ontology description translation
onto_desc <- readr$read_tsv(
  file.path(do_repo_path, "src/translations/doid-es.tsv"),
  show_col_types = FALSE
) |>
  dplyr::filter(!stringr$str_detect(.data$source_id, "DOID")) |>
  dplyr$mutate(status = toupper(.data$status))

# 2. remove single quotes in text --> lead to errors
trans_all <- trans_all |>
  dplyr$mutate(
    translation_text = dplyr$if_else(
      stringr::str_count(.data$translation_text, '"') == 1,
      stringr$str_remove(.data$translation_text, '"'),
      .data$translation_text
    )
  ) |>
  dplyr::filter(
    !(.data$source_id == "obo:doid.owl" & .data$predicate == "dc:description")
  ) |>
  dplyr$bind_rows(onto_desc)


# PREPARE SUBSETS ---------------------------------------------------------

### Create subset files from "all" for specific uses ###
# 1. text to ignore
deprecated <- trans_all |>
  dplyr$filter(stringr$str_detect(.data$status, "DEPRECATED"))

# not for saving, but useful for remaining outputs
active <- trans_all |>
  dplyr$filter(!stringr$str_detect(.data$status, "DEPRECATED"))

# 2. text needing translation
untranslated <- active |>
  dplyr$filter(
    stringr$str_detect(.data$status, "UNTRANSLATED")
  )

# 3. text that moved --> simple review
changed <- active |>
  dplyr$filter(
    stringr$str_detect(.data$status, "CANDIDATE")
  )

# 4. translated but not yet accepted for release
translated <- active |>
  dplyr$filter(
    .data$status == "MANUALLY TRANSLATED" &
      (
        is.na(.data$final_reviewer) |
          # quick review means its accepted for production w/o full review
          !stringr$str_detect(
            .data$final_reviewer,
            stringr$coll("quick", ignore_case = TRUE)
          )
      )
  )

# 5. production ready
production <- active |>
  dplyr$filter(
    .data$status %in% c("PASSED AUTOMATED REVIEW", "FINAL") |
      # quick review means its accepted for production w/o full review
      stringr$str_detect(
        .data$final_reviewer,
        stringr$coll("quick", ignore_case = TRUE)
      )
  )

# check that all records are accounted for across subsets, error if missing
subset_remerged <- dplyr$bind_rows(
  deprecated, untranslated, changed, translated, production
)

missing <- dplyr$anti_join(trans_all, subset_remerged, by = names(trans_all))
if (nrow(missing) > 0) {
  print(missing)
  stop("Some data were lost during the split process. REVIEW...")
} else {
  message("All data successfully accounted for across subsets.")
}

# check that no records are in more than one subset
duplicates <- duplicated(subset_remerged) |
  duplicated(subset_remerged, fromLast = TRUE)
if (any(duplicates)) {
  print(subset_remerged[duplicates, ])
  stop("Some data were duplicated across subsets. REVIEW...")
} else {
  message("No duplicates found across subsets.")
}



# WRITE OUTPUT ------------------------------------------------------------

# write all locally (with datestamp)

if (!dir.exists(final_dir)) dir.create(final_dir)
write_output(trans_all, final_dir)
write_output(deprecated, final_dir)
write_output(untranslated, final_dir)
write_output(changed, final_dir)
write_output(translated, final_dir)
write_output(production, final_dir)

# write in the DOID repo
do_trans_dir <- file.path(do_repo_path, "src/translations")
if (!dir.exists(do_trans_dir)) dir.create(do_trans_dir)
write_output(trans_all, do_trans_dir)
write_output(deprecated, do_trans_dir)
write_output(untranslated, do_trans_dir)
write_output(changed, do_trans_dir)
write_output(translated, do_trans_dir)
write_output(production, do_trans_dir)
