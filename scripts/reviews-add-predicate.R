# add predicates back to data - one time, so it doesn't need to be done repeatedly
# also adding review_date column

box::use(
  dplyr, googledrive, googlesheets4, here, purrr, stringr, readr, tibble, tidyr,
  ./mod
)


# CUSTOM FUNCTIONS --------------------------------------------------------

# standardize processing for DOID input data from file
get_doid_data <- function(doid_path) {
  # box::use( # only needed if making independent from this script
  #   dplyr, stringr,
  #   ./mod
  # )

  mod$robot$get_obo_text(doid_path, id_as = "curie", lang = "any") |>
    # fix newlines in text that are double-escaped
    dplyr$mutate(
      source_text = stringr$str_replace_all(source_text, "\\\\n", "\n")
    ) |>
    # Drop undesirable columns:
    # 1. no synonym types in original inputs, added after translation;
    # 2. deprecation for these versions is not relevant, deprecated weren't translated;
    # 3. source_lang is duplicated, already captured during creation of review
    dplyr$select(-"synonym_type", -"deprecated", -"source_lang") |>
    unique()
}

# restore predicates to the translation data
restore_predicate <- function(df, type, doid_data) {
  box::use(dplyr, stringr)

  # standardize IDs to CURIEs (physical disorder review data has URI)
  df <- df |>
    dplyr$mutate(class = mod$general$uri_curie(.data$class, to = "curie"))

  # ensure predicate column is only run once... even if this is run again
  if ("predicate" %in% names(df)) {
    stop("Predicate column already exists in the data frame.")
  }

  # add predicate directly for labels & defs
  if (type == "label") {
    return(dplyr$mutate(df, predicate = "rdfs:label"))
  }
  if (type == "definition") {
    return(dplyr$mutate(df, predicate = "IAO:0000115"))
  }

  # join for synonyms
  doid_syn <- dplyr$filter(
    doid_data,
    stringr$str_detect(.data$predicate, stringr$coll("Synonym"))
  )
  dplyr$left_join(
      df,
      doid_syn,
      by = c("class" = "source_id", "synonym" = "source_text"),
      relationship = "one-to-one"
  )
}

write_predicate_all <- function(gs_df) {
  gs_df |>
    dplyr$rowwise() |>
    dplyr$transmute(
      name = .data$name,
      sheets = .data$sheets[1],
      pred_col_letter = write_predicate(.data$data, .data$id, .data$sheets)
    )
}

write_predicate <- function(df, gs, sheet) {
  box::use(googlesheets4, dplyr)

  col_letter <- LETTERS[ncol(df)]
  pred <- dplyr$select(df, "predicate")
  sht_id <- googlesheets4$range_write(
    data = pred,
    ss = gs,
    sheet = sheet,
    range = col_letter
  )
  invisible(col_letter)
}


# LOAD DOID DATA ----------------------------------------------------------

# original translation input for 'physical disorders'
pd_input <- here$here("data/physical_disorder/doid-v2023-10-21.owl")
doid_pd <- get_doid_data(pd_input)

# original translation input for "remaining diseases"
remaining_input <- here$here("data/remaining_diseases/doid-v2024-02-28.owl")
doid_remaining <- get_doid_data(remaining_input)



# LOAD REVIEW DATA --------------------------------------------------------

### BATCH: PHYSICAL DISORDER ###

# latest versions of the physical disorder review
pd_sheets <- c(
  "disorder3_en_es_label", "disorder2_en_es_definition", "disorder2_en_es_synonym"
)

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


### BATCH: REMAINING DISEASES ###

# upper level review dir for remaining diseases
upper_dir <- "https://drive.google.com/drive/folders/1GqYzQ6ZI5I3pFC82XLKG5P61STxqwn4b"

# list files & folders
upper_obj <- googledrive$drive_ls(upper_dir)

# load original datasets (before splitting into subsets of ~ 500)
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

gs_rem_split.by_sht <- rem_split_dirs |>
  dplyr$select(datatype = "name", dir_id = "id") |>
  dplyr$rowwise() |>
  dplyr$mutate(gs = list(googledrive$drive_ls(.data$dir_id))) |>
  tidyr$unnest("gs") |>
  dplyr$rowwise() |>
  dplyr$mutate(sheets = list(googlesheets4$sheet_names(.data$id))) |>
  tidyr$unnest("sheets") |>
  dplyr$filter(stringr$str_detect(.data$sheets, "^Set[0-9]+|^all$")) |>
  dplyr$mutate(
    data = purrr$map2(
        .data$id,
        .data$sheets,
        ~ googlesheets4$read_sheet(ss = .x, sheet = .y)
    )
  )

remaining_full <- gs_rem_split.by_sht |>
  dplyr$mutate(data = purrr$map(.data$data, std_col_classes)) |>
  dplyr$select("data") |>
  unlist(recursive = FALSE, use.names = FALSE) |>
  split(gs_rem_split.by_sht$datatype) |>
  purrr$map(dplyr$bind_rows)

# make sure none of records from split subsets have been lost
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



# ADD PREDICATES TO IN-MEMORY DATA ----------------------------------------

### BATCH: PHYSICAL DISORDER ###
gs_pd.new <- gs_pd |> # already grouped rowwise -> purr::map errors when rowwise
  dplyr$mutate(
    data = list(restore_predicate(.data$data, .data$datatype, doid_pd))
  )

# checks
pd_chk <- purrr$set_names(
  gs_pd.new$data,
  gs_pd.new$datatype
) |>
  dplyr$bind_rows(.id = "datatype") |>
  dplyr$count(.data$datatype, .data$predicate)

pd_chk
if (any(is.na(pd_chk$predicate))) {
  stop("Some predicates were not restored to the physical disorder review data.")
}


### BATCH: REMAINING DISEASES ###

gs_rem_unsplit.new <- gs_rem_unsplit |>
  dplyr$rowwise() |>
  dplyr$mutate(
    data = list(restore_predicate(.data$data, .data$datatype, doid_remaining))
  )

gs_rem_split.new <- gs_rem_split.by_sht |>
  dplyr$rowwise() |>
  dplyr$mutate(
    data = list(restore_predicate(.data$data, .data$datatype, doid_remaining))
  )

# checks
rem_us_chk <- purrr$set_names(
  gs_rem_unsplit.new$data,
  gs_rem_unsplit.new$datatype
) |>
  dplyr$bind_rows(.id = "datatype") |>
  dplyr$count(.data$datatype, .data$predicate)

rem_us_chk
if (any(is.na(rem_us_chk$predicate))) {
  stop("Some predicates were not restored to the physical disorder review data.")
}


rem_s_chk <- purrr$set_names(
  gs_rem_split.new$data,
  gs_rem_split.new$datatype
) |>
  dplyr$bind_rows(.id = "datatype") |>
  dplyr$count(.data$datatype, .data$predicate)

rem_s_chk
if (any(is.na(rem_s_chk$predicate))) {
  stop("Some predicates were not restored to the physical disorder review data.")
}

if (!identical(rem_us_chk, rem_s_chk)) {
  stop("Predicate data for remaining diseases are not identical between unsplit and split datasets.")
}


# WRITE TO GOOGLE SHEETS --------------------------------------------------

pd_pred <- gs_pd.new |>
  dplyr$rowwise() |>
  dplyr$transmute(
    name = name,
    sheets = sheets,
    pred_col_letter = write_predicate(.data$data, .data$id, .data$sheets)
  )
pd_pred

rem_us_pred <- write_predicate_all(gs_rem_unsplit.new)
rem_us_pred

rem_s_pred <- write_predicate_all(gs_rem_split.new)
rem_s_pred
