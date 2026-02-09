# save spec as tsv

library(dplyr)
library(googlesheets4)
library(here)
library(janitor)
library(tidyr)
library(vroom)

# input
gs <- "https://docs.google.com/spreadsheets/d/1wYjZNme9Bc7rJKWyrZLxlJCmgvM7UpjNLqXQmj2jB48/"
sheet_nm <- "v1-TSV-v2"

# output

data_out <- here::here("mod", "data")
data_raw_out <- here::here("data-raw")


# tidy data from google sheet
df_raw <- googlesheets4::read_sheet(gs, sheet = sheet_nm, skip = 1, col_types = "c")

df <- df_raw |>
  dplyr::select(-dplyr::starts_with("v1")) |>
  dplyr::rename_with(~ sub("v2", "bp", .x), dplyr::starts_with("v2")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(dplyr::contains("order"), as.integer)
  )

# save babelon-compatible spec
babelon <- dplyr::select(df, dplyr::starts_with("babelon")) |>
  dplyr::filter(!is.na(.data$babelon)) |>
  dplyr::rename(col = "babelon") |>
  dplyr::rename_with(~ sub("babelon_", "", .x), dplyr::starts_with("babelon_"))


vroom::vroom_write(
  babelon,
  file.path(data_raw_out, "babelon_spec.tsv"),
  delim = "\t",
  na = ""
)
save(babelon, file = file.path(dir_out, "babelon.rda"))


# save babelonplus spec
## NOTE: bp_order 1-7 identify "core" columns for match_id generation
babelonplus <- df |>
  tidyr::unite(
    col = "bp_value",
    bp_value,
    babelon_value,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  dplyr::mutate(bp_value = dplyr::na_if(.data$bp_value, "")) |>
  dplyr::select(dplyr::starts_with("bp")) |>
  dplyr::relocate("bp_value", .after = "bp_datatype") |>
  dplyr::filter(!is.na(.data$bp)) |>
  dplyr::rename(col = "bp") |>
  dplyr::rename_with(~ sub("bp_", "", .x), dplyr::starts_with("bp_"))


vroom::vroom_write(
  babelonplus,
  file.path(data_raw_out, "babelonplus_spec.tsv"),
  delim = "\t",
  na = ""
)
save(babelonplus, file = file.path(dir_out, "babelonplus.rda"))

