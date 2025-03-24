# just to make sure I didn't miss any records when splitting

box::use(
  dplyr, googledrive, googlesheets4, here, purrr, stringr, tibble,
  ./mod
)

# gs_out
data_dir <- here$here("data/remaining_diseases")
progress_file <- file.path(data_dir, "progress.rda")


df_nm <- purrr::map_chr(gs_out, ~ stringr::str_remove(.x[[2]], ".*_"))
original <- purrr::map(
  gs_out,
  ~ googlesheets4$read_sheet(ss = .x[[1]], sheet = .x[[2]])
) |>
  purrr::set_names(df_nm)


# upper level review dir
upper_dir <- "https://drive.google.com/drive/folders/1GqYzQ6ZI5I3pFC82XLKG5P61STxqwn4b"

# list files &
upper_obj <- googledrive::drive_ls(upper_dir)

# load original datasets
original_df <- upper_obj |>
  dplyr::filter(!googledrive::is_folder(upper_obj)) |>
  dplyr::mutate(datatype = stringr::str_remove(name, ".*_")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    sheets = list(googlesheets4::sheet_names(.data$id)),
    data = list(googlesheets4::read_sheet(ss = .data$id, sheet = .data$sheets[1]))
  )

# identify & load split datasets
dt_dir <- upper_obj |>
  dplyr::filter(googledrive::is_folder(upper_obj))

dt_files <- dt_dir |>
  dplyr::select(datatype = "name", dir_id = "id") |>
  dplyr::rowwise() |>
  dplyr::mutate(gs = list(googledrive::drive_ls(.data$dir_id))) |>
  tidyr::unnest("gs") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    sheets = list(googlesheets4::sheet_names(.data$id)),
    data = list(
      purrr::map(
        .data$sheets,
        ~ googlesheets4::read_sheet(ss = .data$id, sheet = .x)
      ) |>
        dplyr::bind_rows(.id = "set")
    )
  )

dt_merged <- dt_files |>
  dplyr::select("data") |>
  unlist(recursive = FALSE, use.names = FALSE) |>
  split(dt_files$datatype) |>
  purrr::map(~ dplyr::bind_rows(.x))

original <- purrr::set_names(
  original$data,
  original$datatype
)

# comparison
res <- purrr::map(
  names(original),
  ~ dplyr::anti_join(original[[.x]], dt_merged[[.x]])
)
