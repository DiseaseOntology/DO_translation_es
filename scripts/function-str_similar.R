# converts numbers to lowercase roman numerals (identifying roman numerals and
#   converting to numbers is much harder and less precise)
to_roman_lc <- function(x) {
    has_number <- stringr::str_detect(x, "[0-9]+")
    numbers <- stringr::str_extract_all(x, "[0-9]+")
    rn <- purrr::map(
        numbers,
        ~ stringr::str_to_lower(utils::as.roman(.x))
    )
    replace_list <- purrr::map2(
        numbers,
        rn,
        function(num, rn) {
            if (length(num) == 0) {
                NA
            } else {
                pattern <- paste0("(^|[^0-9])", num, "([^0-9]|$)")
                purrr::set_names(rn, num)
            }
        }
    )

    purrr::map2_chr(
        x,
        replace_list,
        function(input, patt_repl) {
            if (all(is.na(patt_repl))) {
                input
            } else {
                stringr::str_replace_all(input, patt_repl)
            }
        }
    )
}

# compares 2 strings and specifies the type of match (i.e. 'exact' or one or
# more of 'case', 'space', 'punct', 'num_format' with multiple pipe delimited)
str_similar <- function(x, y) {
    stopifnot("length of `y` must be same as `x`" = length(x) == length(y))

    df <- tibble::tibble(
        x = x,
        y = y,
        comp = dplyr::if_else(x == y, "exact", NA_character_)
    )

    # single conversion tests
    df <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = c(x, y),
                .fns = list(
                    lc = ~ stringr::str_to_lower(.x),
                    space = ~ stringr::str_remove_all(.x, "[[:space:]]+"),
                    punct = ~ stringr::str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = dplyr::case_when(
                !is.na(comp) ~ comp,
                x_lc == y_lc ~ "case",
                x_space == y_space ~ "space",
                x_punct == y_punct ~ "punct",
                x_num == y_num ~ "num_format",
                .default = NA_character_
            )
        )

    # double conversion tests
    df <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::ends_with("lc"),
                .fns = list(
                    space = ~ stringr::str_remove_all(.x, "[[:space:]]+"),
                    punct = ~ stringr::str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            dplyr::across(
                .cols = dplyr::ends_with("space"),
                .fns = list(
                    punct = ~ stringr::str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            dplyr::across(
                .cols = dplyr::ends_with("punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = dplyr::case_when(
                !is.na(comp) ~ comp,
                x_lc_space == y_lc_space ~ "case|space",
                x_lc_punct == y_lc_punct ~ "case|punct",
                x_lc_num == y_lc_num ~ "case|num_format",
                x_space_punct == y_space_punct ~ "space|punct",
                x_space_num == y_space_num ~ "space|num_format",
                x_punct_num == y_punct_num ~ "punct|num_format",
                .default = NA_character_
            )
        )

    # triple conversion tests
    df <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::ends_with("lc_space"),
                .fns = list(
                    punct = ~ stringr::str_remove_all(.x, "[[:punct:]]+"),
                    num = ~ to_roman_lc(.x)
                )
            ),
            dplyr::across(
                .cols = dplyr::ends_with("lc_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            dplyr::across(
                .cols = dplyr::ends_with("space_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),

            comp = dplyr::case_when(
                !is.na(comp) ~ comp,
                x_lc_space_punct == y_lc_space_punct ~ "case|space|punct",
                x_lc_space_num == y_lc_space_num ~ "case|space|num_format",
                x_lc_punct_num == y_lc_punct_num ~ "case|punct|num_format",
                x_space_punct_num == y_space_punct_num ~ "space|punct|num_format",
                .default = NA_character_
            )
        )

    # all conversion tests
    df <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::ends_with("lc_space_punct"),
                .fns = list(
                    num = ~ to_roman_lc(.x)
                )
            ),
            comp = dplyr::case_when(
                !is.na(comp) ~ comp,
                x_lc_space_punct_num == y_lc_space_punct_num ~ "case|space|punct|num_format",
                .default = NA_character_
            )
        )

    df$comp
}
