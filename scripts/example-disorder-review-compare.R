# Example script to auto-compare Spanish translations using physical disorder
# data with human (TSG company) and google translated labels, definitions, and
# synonyms, and google backtranslation of TSG human translations.
#
# ASSUMPTIONS:
#   1. Review Google Sheet was generated with Google translation formulas
#       previously.
#   2. The three separate sets of labels, definitions, and synonyms are present.
box::use(
  dplyr, googlesheets4, here, purrr, stringr,
  ./mod
)

# Get initial data with Google Translations from GS --------------------------

gs <- "https://docs.google.com/spreadsheets/d/1Fvnmz_3KNXuLvLtJGm9eKkqvV33mIZUESsPWv28uUAg"
init <- mod$gs_review$read_gs_review(gs, ss_prefix = "disorder1")


# One-time copy of curation notes --------------------------------------------

# This section is included only to capture the manual curation notes created 
# during a first review attempt which pre-dated full automated character &
# word comparison
# --> Don't want to lose that data = rename original label sheet in Google Sheet
# file to "disorderOBS_en_es_label" to retain and avoid overwriting initial
# attempt
obs_label <- googlesheets4$read_sheet(gs, "disorderOBS_en_es_label")
label_notes <- dplyr$mutate(
  obs_label,
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
  curator_match = dplyr$na_if(curator_match, ""),
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
  ),
  # replace names with initials
  review_notes = stringr$str_replace_all(
    review_notes,
    c("Allen" = "JAB", "Claudia" = "CSBJ")
  ),
  review_notes = dplyr$na_if(review_notes, "")
) |>
  dplyr$select(
    "class", "label", "google_translate_to_en", "curator_match", "review_notes"
  )


# Automated review --------------------------------------------------------

# auto_review_df() & write_gs_review() are used here because data has already
# been read in from Google Sheets in order to add the previous curation
# information, and to write the data to a new sheet to show the results of
# this review separately from the init step.

# Review could also be accomplished with the single wrapper function
# auto_review() which will read in the data, generate the automated review, and
# write the review back to the same sheet
review <- purrr$map(
  init,
  ~ mod$gs_review$auto_review_df(.x, alignment = "both")
)

# add in curation note data from original obsoleted approach
review$label <- dplyr$full_join(
  # remove blank placeholder for review_notes & add in review_notes &
  # curator_match data from prior, obsolete review attempt
  dplyr$select(review$label, -"review_notes", -"curator_match"),
  label_notes,
  by = c("class", "label", "google_translate_to_en")
)

mod$gs_review$write_gs_review(review, gs, ss_prefix = "disorder2")
