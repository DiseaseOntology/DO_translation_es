# Script for generating review Google Sheets (label, definition, synonym) for
# all diseases excluding physical disorder branch (done previously)
box::use(
  googledrive, googlesheets4, here, purrr, stringr,
  ./mod
)

data_dir <- here$here("data/remaining_diseases")
en_es_files <- list.files(data_dir, pattern = ".*init1.csv", full.names = TRUE)

# create empty Google Sheets in translation folder
gs_id <- en_es_files |>
  basename() |>
  stringr$str_replace_all(c("-init1.csv" = "", "-" = "_")) |>
  purrr$map(
    function(.nm) {
      gs_id <- googlesheets4::gs4_create(name = .nm)
      googledrive$drive_mv(
        gs_id,
        "https://drive.google.com/drive/folders/1GqYzQ6ZI5I3pFC82XLKG5P61STxqwn4b/"
      )
      gs_id
    }
  )

# initialize review (add data to Google Sheets)
gs_out <- purrr$map2(
  en_es_files,
  gs_id,
  ~ mod$gs_review$create_gs_review(
    .x,
    gs = .y,
    "disease1"
  ) |>
    unlist(recursive = FALSE)
)

