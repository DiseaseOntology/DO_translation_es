#' Rate a String Comparison
#'
#' This function generates a rating for string comparisons created by
#' [str_compare_all()], using the specified weights.
#'
#' @param x A character vector of comparison types from [str_compare_all()].
#' @param chr_wt The value to deduct for each character transformation.
#' @param token_wt The value to deduct if word tokenization is required for
#' a match.
#' @param word_wt The value to deduct for each word transformation occurring
#' in conjunction with word tokenization.
#'
#' @section Token/Word Weight Note:
#' `token_wt` and `word_wt` are scaled by the word match percentage. For
#' example, if the word match percentage is 100%, the full `token_wt` and any
#' applicable `word_wt` are deducted. If the word match percentage is 75%
#' (wordToken[75%]), 75% of `token_wt` and any applicable `word_wt` are
#' deducted.
#'
#' @examples
#' x <- c(
#'   "exact", "chr:numeral", "chr:case", "chr:space", "chr:punct",
#'   "chr:diacritic", "chr:case|punct", "chr:case|space",
#'   "chr:numeral|space|diacritic", "wordToken[100%]", "wordToken[75%]",
#'   "wordToken[100%]:wpunct", "wordToken[100%]:stemmed",
#'   "wordToken[100%]:stopwords", "wordToken[100%]:wpunct|stopwords",
#'   "wordToken[100%]:wpunct|stemmed|stopwords",
#'   "wordToken[75%]:wpunct|stemmed|stopwords",
#'   "chr:case|space;wordToken[33.33%]:wpunct"
#' )
#'
#' rate_comparison(x)
#'
#' @md
#' @export
rate_comparison <- function(x, chr_wt = 0.02, token_wt = 0.1, word_wt = 0.05) {
  box::use(
    dplyr, stringr, tidyr,
    ./mutate
  )

  x_chr <- stringr$str_extract(x, "chr:[^;]+")
  x_word <- stringr$str_extract(x, "wordToken.+")

  chr_n <- stringr$str_count(
    x_chr,
    paste0(mutate$str_mutate_opts, collapse = "|")
  ) |>
    tidyr$replace_na(0)

  word_n <- stringr$str_count(
    x_word,
    paste0(mutate$tokenize_words_opts, collapse = "|")
  ) |>
    tidyr$replace_na(0)

  word_pct <- as.numeric(stringr$str_extract(x_word, "[0-9]+")) / 100 |>
    tidyr$replace_na(0)

  dplyr$case_when(
    x == "exact" ~ 1,
    is.na(x_chr) & is.na(x_word) ~ 0,
    chr_n > 0 & word_n == 0 ~ 1 - (chr_n * chr_wt),
    word_n > 0 ~ word_pct - token_wt * word_pct - (word_n * word_wt * word_pct),
  )
}