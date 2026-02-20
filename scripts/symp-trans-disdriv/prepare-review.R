library(here)
library(readr)
library(triager)
library(googlesheets4)

# data paths
data_dir <- here::here("data/symp-trans-disdriv")
triaged_path <- file.path(data_dir, "symp-trans-disdriv-triaged.tsv")

# Google Sheet: symp-translation-review
gs <- "https://docs.google.com/spreadsheets/d/1eD1ATiMumtvbtA_7n5P2yVfov_6dKhiCNTW8yo-nolU/edit?gid=0#gid=0"


# split by ontology
triaged <- readr::read_tsv(triaged_path) |>
  dplyr::mutate(
    ontology = stringr::str_remove(subject_id, ":.+")
  ) |>
  tidyr::nest(.by = "ontology")

symp <- dplyr::filter(triaged, ontology == "SYMP")$data[[1]]

# Create review sheet for CANDIDATE records (below confidence threshold)
symp_candidate <- prepare_review_sheet(symp, status = "CANDIDATE")
 # !FIX: need to have option to keep non-triager/babelon columns (dropping type)
 # !UPDATE: option in triage_translations() to drop or retain non-triager cols --> should sort to end

# Create separate sheet for confirming AUTO_APPROVED records
symp_approved <- prepare_review_sheet(symp, status = "AUTO_APPROVED")

# Upload to Google Sheets

write_sheet(review_candidate, gs, sheet = "Review-CANDIDATE")
write_sheet(review_approved, gs, sheet = "AUTO_APPROVED")


## Step 2: Set Up Data Validation (Optional but Recommended)

Add data validation in Google Sheets to reduce errors:

### translation_status dropdown
Valid values from the BabelonPlus spec:
- UNTRANSLATED
- CANDIDATE
- AUTO_APPROVED
- OFFICIAL
- DEPRECATED
- NOT_TRANSLATED

### reviewer_expertise dropdown
Valid values:
- ALGORITHM
- DOMAIN_EXPERT
- DOMAIN_STUDENT
- LAYPERSON
- PROFESSIONAL_TRANSLATOR
- TECHNICAL_SPECIALIST

You can add these via the Google Sheets UI or programmatically if needed.

## Step 3: Human Review

Reviewers should:

1. **Review translations** - Compare `translation_value` against `source_value`, `backtranslation`, and `alternate_translation`

2. **Make decisions**:
   - If the translation is acceptable: Set `translation_status` to OFFICIAL (or CANDIDATE if needs more work)
   - If the translation needs minor edits: Enter corrected text in `reviewer_translation`
   - If the translation is problematic: Set status to REJECTED or NOT_TRANSLATED

3. **Always fill**:
   - `reviewer`: Your name or username
   - `reviewer_expertise`: Your expertise level
   - `comment` (optional but helpful): Notes about your decision

## Step 4: Restore from Review

r
# Read reviewed data from Google Sheets
reviewed_candidate <- read_sheet(gs, sheet = "Review-CANDIDATE")
reviewed_approved <- read_sheet(gs, sheet = "Review-AUTO_APPROVED")

# Restore both sets
restored_candidate <- restore_from_review(triaged_data, reviewed_candidate)
restored_approved <- restore_from_review(triaged_data, reviewed_approved)

# Combine results
all_restored <- dplyr::bind_rows(
  restored_candidate,
  restored_approved
) |>
  dplyr::distinct()  # Remove any duplicates

# Save updated dataset
readr::write_tsv(all_restored, "path/to/reviewed-translations.tsv")


## What Happens During Restore

### For records with `reviewer_translation` filled:
1. **New record created** with:
   - `translation_value` = `reviewer_translation`
   - `translator` = `reviewer`
   - `translator_expertise` = `reviewer_expertise`
   - `translation_date` = current date
   - New `triager_id` (recalculated from core columns)
   - `translation_confidence` =
     - If edit is minor (>85% similar): `original_confidence + 0.15` (max 0.98)
     - If substantially different: `NA` (human-verified, no automated score)
   - `confidence_details` = YAML documenting human review:
     yaml
     - method: human-review
       supersedes_triager_id: <old_id>
       review_date: 2026-02-18
       reviewer: <reviewer_name>
       reviewer_expertise: PROFESSIONAL_TRANSLATOR

   - `status_history` = Initial entry with reviewer's chosen status

2. **Original record marked DEPRECATED**:
   - `translation_status` → DEPRECATED
   - `status_history` updated with deprecation entry

### For records without `reviewer_translation` but with status changes:
- `translation_status`, `comment`, `reviewer`, `reviewer_expertise` updated
- `status_history` updated with new status entry
- Original `translation_value` and `triager_id` preserved

### For comparison CANDIDATE records (AUTO_GENERATED):
- Left unchanged (they serve as historical reference)

## Status History Schema

The `status_history` column stores a YAML list tracking translation lifecycle:

### Required fields:
- `date`: ISO 8601 date (YYYY-MM-DD) when status changed
- `status`: One of the valid translation_status values
- `actor`: Who/what made the change (username, "triager", URI)

### Optional fields:
- `confidence`: Translation confidence at this status (0-1)
- `actor_type`: Expertise level (ALGORITHM, PROFESSIONAL_TRANSLATOR, etc.)
- `comment`: Free text (e.g., "Supersedes triager_id abc123")
- `superseded_by`: triager_id of new record (for DEPRECATED status)

### Example:
yaml
- date: 2026-02-14
  status: CANDIDATE
  confidence: 0.75
  actor: triager
  actor_type: ALGORITHM
- date: 2026-02-18
  status: OFFICIAL
  actor: jsmith
  actor_type: PROFESSIONAL_TRANSLATOR
  comment: "Minor capitalization fixes"
  confidence: 0.90


## Tips for Reviewers

1. **Context matters**: `subject_id` and `predicate_id` tell you what's being translated (label, definition, synonym)

2. **Trust but verify**: `alternate_translation` is machine-generated - it's useful context but shouldn't be blindly accepted

3. **Minor edits only**: If you need to completely rewrite a translation, consider that a separate translation task rather than a "review"

4. **Be consistent**: Use the same `reviewer` identifier across all your reviews for auditability

5. **Comment liberally**: Future you (or other reviewers) will appreciate notes about tricky decisions

## Example Review Scenarios

### Scenario 1: Translation is perfect

translation_status     → OFFICIAL
reviewer              → jsmith
reviewer_expertise    → PROFESSIONAL_TRANSLATOR
reviewer_translation  → (leave empty)
comment               → Verified accurate


### Scenario 2: Minor capitalization fix needed

translation_status     → OFFICIAL
reviewer              → jsmith
reviewer_expertise    → PROFESSIONAL_TRANSLATOR
reviewer_translation  → Mecanismos Ambientales... (corrected)
comment               → Fixed capitalization


### Scenario 3: Translation is problematic

translation_status     → NOT_TRANSLATED
reviewer              → jsmith
reviewer_expertise    → PROFESSIONAL_TRANSLATOR
reviewer_translation  → (leave empty)
comment               → Incorrect terminology, needs professional translation


### Scenario 4: Prefer the alternate translation

translation_status     → OFFICIAL
reviewer              → jsmith
reviewer_expertise    → PROFESSIONAL_TRANSLATOR
reviewer_translation  → (copy alternate_translation here)
comment               → Alternate translation is more idiomatic

