# Script for generating review Google Sheets (label, definition, synonym) for
# all diseases excluding physical disorder branch (done previously)
box::use(
  googledrive, googlesheets4, here, purrr, stringr,
  ./mod
)

data_dir <- here$here("data/remaining_diseases")
en_es_files <- list.files(data_dir, pattern = ".*init1.csv", full.names = TRUE)
progress_file <- file.path(data_dir, "progress.rda")

if (file.exists(progress_file)) {
  load(progress_file)
}

if (!exists("gs_out")) {
  # create empty Google Sheets in translation folder
  gs_id <- en_es_files |>
    basename() |>a
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

  save(gs_out, file = progress_file)
}


# auto1_opts <- c("label", "definition", "synonym", "all", "skip")
# auto1 <- NULL
# while (!any(auto1 %in% auto1_opts)) {
#   auto1_raw <- readline("For which data would you like to execute auto-comparison?  \n  --> As unquoted, comma-separated values (or all).")
#   auto1_parse <- stringr$str_split_1(auto1_raw, ", *")
#   auto1 <- auto1_parse[auto1_parse %in% auto1_opts]
# }
#
# if (auto1 != "skip") {

exec_compare <- readline("Execute automatic comparison for review? (y/n)  ")
if (exec_compare == "y") {
  # custom auto_review function (minor difference from other function)
  auto_review <- function(gs_info) {
    .df <- googlesheets4$read_sheet(ss = gs_info[[1]], sheet = gs_info[[2]])
    out <- mod$gs_review$auto_review_df(.df)
    googlesheets4$write_sheet(
        data = out,
        ss = gs_info[[1]],
        sheet = gs_info[[2]]
      )
    .df
  }

  df_nm <- purrr::map_chr(
    gs_out,
    ~ stringr::str_extract(.x[[2]], "label|definition|synonym")
  )

  review_list <- purrr$map(
    gs_out,
    ~ auto_review(.x)
  ) |>
    purrr::set_names(df_nm)
}
