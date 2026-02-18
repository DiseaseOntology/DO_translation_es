#!/usr/bin/env Rscript

# =============================================================================
# Initial triager use (for babelon-compatible implementation)
# =============================================================================

# SETUP -----------------------------------------------------------------------

library(dplyr)
library(googlesheets4)
library(here)
library(readr)
library(triager) # from DiseaseOntology/triager@v0.1.0

data_dir <- here::here("data/symp-trans-disdriv")
input_path <- file.path(data_dir, "symp-trans-disdriv-TSG-es-complete.tsv")

my_sheet <- "https://docs.google.com/spreadsheets/d/1JgjxxCM94gLvoACN6foNuJofjpKu4brVj54fS_yf04w/"



# EXECUTE WORKFLOW ------------------------------------------------------------

# STEP 1: Load Translation Data

translations <- readr::read_tsv(input_path, show_col_types = FALSE)



# STEP 2: Get New Forward Translation Records (via Google Translate in Sheets)

forward_prep <- triager::prepare_forward_translations(translations) |>
  triager::add_gs_translate_formula("translate")

googlesheets4::sheet_write(forward_prep, ss = my_sheet, sheet = "forward")

# translations are initiated automatically... brief wait to ensure complete
Sys.sleep(60)
forward_completed <- googlesheets4::read_sheet(my_sheet, sheet = "forward") |>
  triager::add_triager_id()

# CHECKPOINT: Verify triager_id is 15 characters
stopifnot(all(nchar(forward_completed$triager_id) == 15))



# STEP 3: Get Backtranslations for Original Translations (via Google Translate in Sheets)

backtrans_prep <- triager::prepare_backtranslations(translations) |>
  triager::add_gs_translate_formula("backtranslate")

googlesheets4::sheet_write(backtrans_prep, ss = my_sheet, sheet = "backtrans")

# translations are initiated automatically... brief wait to ensure complete
Sys.sleep(60)
backtrans_completed <- googlesheets4::read_sheet(my_sheet, sheet = "backtrans") |>
  triager::add_triager_id()



# STEP 4: Triage!!!

# Now compare original translations with Google Translate versions
# This will:
# 1. Match records by triager_id
# 2. Compare source_value with backtranslation (from original translator)
# 3. Compare translation_value with Google's translation_value
# 4. Calculate confidence scores
# 5. Update translation_status to AUTO_APPROVED for high-scoring translations

triaged <- triager::triage_translations(
  .df = backtrans_completed,
  .additional = forward_completed,
  cutoff = 0.75
)


# STEP 5: Examine & Record Results

dplyr::count(triaged, translator_expertise, predicate_id, translation_status) |>
  dplyr::mutate(
    pct = round(n / sum(n) * 100, 2),
    .by = c("translator_expertise", "predicate_id")
  )

# Look at confidence_details for a triaged record
cat(triaged$confidence_details[[1]], "\n\n")

# See which records were auto-approved
auto_approved <- triaged %>%
  dplyr::filter(translation_status == "AUTO_APPROVED")

cat("Auto-approved records:\n")
dplyr::select(
  auto_approved, subject_id, predicate_id, translation_value,
  translation_status, reviewer
)

# See which records still need manual review
needs_review <- triaged %>%
  dplyr::filter(
    translation_status == "CANDIDATE",
    translator_expertise != "ALGORITHM"
  )

cat("\nRecords needing manual review:\n")
dplyr::select(
  needs_review, subject_id, predicate_id, translation_value,
  translation_status
)

# Write triaged results
output_file <- file.path(data_dir, "symp-trans-disdriv-triaged.tsv")
readr::write_tsv(triaged, output_file, quote = "needed", na = "")

googlesheets4::write_sheet(
  triaged,
  ss = my_sheet,
  sheet = "triaged"
)
