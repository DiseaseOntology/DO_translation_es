# one time *rough* comparison of inheritance & mutations in definitions

# a number of definitions were mistranslated with regard to inheritance
# (added or mistranslation in some other way)

library(stringr)
box::use(
  dplyr, googlesheets4,
  ./mod
)


# CUSTOM FUNCTIONS --------------------------------------------------------

# normalize spanish text (remove diacritics, lower case, unify spaces, remove gendered endings)
normalize_es <- function(.es) {
  box::use(
    stringr,
    # ./mod
  )
  mod$mutate$str_mutate(.es, c("diacritic", "case"), locale = "es") |>
    stringr::str_replace_all(c("[:space:]" = " ", "[ao]\\b" = ""))
}


# counts each term in each string of a character vector; output = list of data.frames
count_terms <- function(x, terms) {
  box::use(purrr, stringr, tibble)
  pattern <- paste0("\\b(", paste0(terms, collapse = "|"), ")\\b")
  x_list <- stringr::str_extract_all(
    x,
    stringr::regex(pattern, ignore_case = TRUE)
  )
  purrr::map(
    x_list,
    function(.x) {
      counts <- table(.x)
      if (length(counts) == 0) {
        tibble::tibble(term = NA_character_, n = 0)
      } else {
        tibble::as_tibble(counts, .name_repair = ~ c("term", "n"))
      }
    }
  )
}


# compares lists of data.frames; output = 'en: es: both:' string)
compare_term_counts <- function(en_counts, es_counts, term_map_es) {
  box::use(dplyr, purrr, tidyr)
  stopifnot("`en_counts` & `es_counts` must be the same length" = length(en_counts) == length(es_counts))

  # genetic_terms and abbreviation mapping --> shorter output
  abbr <- c(
    "autosomal" = "aut", "recessive" = "rec", "mutation" = "mut", "dominant" = "dom",
    "inheritance" = "inh", "variation" = "vrtn", "heterozygous" = "het", "homozygous" = "hom",
    "compound heterozygous" = "het-cmp", "hemizygous" = "hem", "variant" = "var", #"allele" = "al",
    "chromosome" = "chrom", "region" = "reg", "x-linked" = "Xl", #"locus" = "loc", "gene" = "gen",
    "y-linked" = "Yl", "mitochondrial" = "mito", "de novo" = "dn", "mosaic" = "mos", "somatic" = "som",
    "germline" = "gl", "penetrance" = "pen", "monogenic" = "mono", "polygenic" = "poly",
    "oligogenic" = "oligo", "digenic" = "di", "monosomy" = "monos", "trisomy" = "tris",
    "autosomal recessive" = "AR", "autosomal dominant" = "AD", "semi-dominant" = "semidom"
  )

  purrr$map2_chr(
    en_counts,
    es_counts,
    function(.x, .y) {
      en_std <- dplyr$rename(.x, "en" = "n")
      es_std <- .y |>
        dplyr$mutate(term = dplyr$recode(.data$term, !!!term_map_es)) |>
        dplyr$rename("es" = "n")

      .df <- dplyr$full_join(
        en_std,
        es_std,
        by = "term",
        na_matches = "never",
        relationship = "one-to-one"
      ) |>
        tidyr$replace_na(list(en = 0, es = 0)) |>
        dplyr$mutate(term = dplyr$recode(.data$term, !!!abbr)) |>
        dplyr$mutate(
          both = purrr::map2_int(.data$en, .data$es, min),
          en = .data$en - .data$both,
          es = .data$es - .data$both
        ) |>
        tidyr$pivot_longer(
          c("both", "en", "es"),
          names_to = "type",
          values_to = "n"
        ) |>
        dplyr$arrange(.data$term) |>
        dplyr$mutate(
          type = factor(type, levels = c("en", "es", "both")),
          out = dplyr$case_when(
            .data$n != 0 & .data$type == "both" ~ paste0(.data$term, "=", .data$n),
            .data$n != 0 ~ paste0(.data$term, "+", .data$n),
            TRUE ~ NA_character_
          )
        ) |>
        dplyr$summarize(
          out = paste0(na.omit(.data$out), collapse = " | "),
          .by = "type"
        ) |>
        dplyr$arrange(.data$type) |>
        dplyr$transmute(out = paste0(.data$type, ": ", .data$out))

      paste0(.df$out, collapse = "\n")
    }
  )
}


# compare genetic terms in two languages
check_genetic_term <- function(en, es) {
  box::use(
    purrr, stringr,
    # ./mod
  )

  # expected translations; pre-sorted by length (longest first) to avoid partial matches
  terms <- c(
    `compound heterozygous` = "heterocigota compuesta", `autosomal recessive` = "autosómica recesiva",
    `autosomal dominant` = "autosómica dominante", mitochondrial = "mitocondrial",
    `semi-dominant` = "semidominante", heterozygous = "heterocigoto",
    inheritance = "herencia", homozygous = "homocigoto", hemizygous = "hemicigoto",
    chromosome = "cromosoma", penetrance = "penetrancia", oligogenic = "oligogénico",
    autosomal = "autosómica", recessive = "recesiva", variation = "variación",
    monogenic = "monogénico", polygenic = "poligénico", mutation = "mutación",
    dominant = "dominante", `X-linked` = "ligado al cromosoma X",
    `Y-linked` = "ligado al cromosoma Y", germline = "linaje germinal",
    monosomy = "monosomía", variant = "variante", `de novo` = "de novo",
    somatic = "somático", digenic = "digenico", trisomy = "trisomía",
    allele = "alelo", region = "región", mosaic = "mosaico", locus = "locus",
    gene = "gen", homozygous = "homocigótica", hemizygous = "hemicigótica"
  )

  # standardize for matching (rough)
  terms_std <- setNames(normalize_es(terms), tolower(names(terms)))
  en_std <- tolower(en)
  es_std <- normalize_es(es)

  # get term counts
  en_counts <- count_terms(en_std, names(terms_std))
  es_counts <- count_terms(es_std, terms_std)

  terms_es <- setNames(names(terms_std), terms_std)
  compare_term_counts(en_counts, es_counts, terms_es)
}



# Add to definition review ------------------------------------------------

gs <- "https://docs.google.com/spreadsheets/d/1sV0NTBh_Nf4q_3LnBjKZNQvpnDPxpY7o5yFcseZeREU/"
sheet <- "all"

def <- googlesheets4$read_sheet(gs, sheet = sheet)

w_gc <- def |>
  dplyr$mutate(
    genetic_terms = check_genetic_term(.data$definition, .data$definición)
  )

new_col_rng <- LETTERS[ncol(w_gc) + 1] |>
  rep(2) |>
  paste0(collapse= ":")

googlesheets4$range_write(
  ss = gs,
  data = dplyr$select(w_gc, "genetic_terms"),
  sheet = sheet,
  range = new_col_rng
)
