# Example script to auto-compare Spanish translations using physical disorder
# data with human (TSG company) and google translated labels, definitions, and
# synonyms, and google backtranslation of TSG human translations.
#
# ASSUMPTIONS:
#   1. Review Google Sheet was generated with Google translation formulas
#       previously.
#   2. The three separate sets of labels, definitions, and synonyms are present.
box::use(
  dplyr, googlesheets4, here, purrr,
  ./mod
)

# Get initial data with Google Translations from GS -----------------------

init <- mod$gs_review$read_gs_review(
  "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg",
  "disorder"
)

# One-time fixes for extra columns and blank rows -------------------------

# Fix for blank rows saved in definition sheet
init[["definition"]] <- init[["definition"]] |>
  dplyr$filter(!is.na(.data[["definition"]]))


# Removal of extra columns, don't want to lose data ==> DON'T OVERWRITE!!!
init_tidy <- init
init_tidy$label <- init$label |>
  dplyr$select(
    "class", "label", "google_translate_to_en", "clase",
    "etiqueta", "google_translate_to_es"
  )
init_tidy <- purrr$map(
  init_tidy,
  ~ dplyr$select(.x, -dplyr$starts_with("compare_")
  )
)

cur_data <- dplyr::mutate(
  init[[1]],
  curator_match = paste0(
    dplyr$if_else(
      !is.na(.data[["cur_match_en"]]),
      paste0("en:", .data[["cur_match_en"]]),
      ""
    ),
    dplyr$if_else(
      !is.na(.data[["who_bilingual"]]),
      paste0("-", .data[["who_bilingual"]]),
      ""
    ),
    dplyr$if_else(
      !is.na(.data[["cur_match_en"]]) & !is.na(.data[["cur_match_es"]]),
      ";", ""
    ),
    dplyr$if_else(
      !is.na(.data[["cur_match_es"]]),
      paste0("es:", .data[["cur_match_es"]]),
      ""
    ),
    dplyr$if_else(
      !is.na(.data[["who_es"]]),
      paste0("-", .data[["who_es"]]),
      ""
    )
  ),
  review_notes = paste0(
    dplyr$if_else(
      !is.na(.data[["review_notes"]]),
      paste0("en:\n", .data[["review_notes"]]),
      ""
    ),
    dplyr$if_else(
      !is.na(.data[["review_notes"]]) & !is.na(.data[["notes"]]),
      "\n\n", ""
    ),
    dplyr$if_else(
      !is.na(.data[["notes"]]),
      paste0("es:\n", .data[["notes"]]),
      ""
    )
  )
) |>
  dplyr::select(
    "class", "label", "google_translate_to_en", "curator_match", "review_notes"
  )


# Automated review --------------------------------------------------------

review <- purrr$map(
  init_tidy,
  ~ mod$gs_review$auto_review_df(.x, alignment = "both")
)


# Combine with captured curator data --------------------------------------

# reformat data we previously curated and fit it in to designated curator cols
out <- review
out[[1]] <- review[[1]] |>
  dplyr::select(-"curator_match", -"review_notes") |>
  dplyr::full_join(cur_data, by = c("class", "label", "google_translate_to_en"))


# Write to GS -------------------------------------------------------------

mod$gs_review$write_gs_review(
  out,
  "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg/edit?gid=1590954027#gid=1590954027",
  "disorder1"
)
