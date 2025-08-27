# one time *rough* comparison of gene symbols & chromosome coordinates in definitions

box::use(
  dplyr, googlesheets4,
  ./mod
)

gs <- "https://docs.google.com/spreadsheets/d/1sV0NTBh_Nf4q_3LnBjKZNQvpnDPxpY7o5yFcseZeREU/"
sheet <- "all"

def <- googlesheets4$read_sheet(gs, sheet = sheet)

w_gc <- def |>
  dplyr$mutate(
    genetic_chr = mod$text$compare_regex_grp(
      def$definition,
      def$definición,
      regex = "\\b[A-Z0-9]{2,}\\b|\\b[0-9XY]+([pq][0-9]+)?(\\.[0-9]+)?\\b",
      as_string = TRUE
    )
  )

new_col_rng <- LETTERS[ncol(w_gc)] |>
  rep(2) |>
  paste0(collapse= ":")

googlesheets4$range_write(
  ss = gs,
  data = dplyr$select(w_gc, "genetic_chr"),
  sheet = sheet,
  range = new_col_rng
)
