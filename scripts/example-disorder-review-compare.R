
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
init[["definition"]] <- init[[2]] |>
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


# Automated review --------------------------------------------------------

review <- purrr$map(
  init_tidy,
  ~ mod$gs_review$auto_review(.x, alignment = "both")
)


mod$gs_review$
label_align <- label_scomp2 |>
  dplyr$mutate(
    g_align = paste0(
      as.character(pwalign$alignedPattern(g_alignment)), "\n",
      as.character(pwalign$alignedSubject(g_alignment))
    ),
    g_score = pwalign$score(g_alignment)
  )

# Write to GS -------------------------------------------------------------

# still need to capture the data we previously curated and figure out how
# to fit it in with this new data
