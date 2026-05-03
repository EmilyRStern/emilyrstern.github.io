
## 0. Setup: Packages and Configuration


# Install missing packages (run once, then comment out)
# packageneeded <- c("pdftools", "tidyverse", "RColorBrewer",
#                    "quanteda", "quanteda.textstats", "quanteda.textplots",
#                    "quanteda.textmodels", "readtext", "topicmodels",
#                    "seededlda", "stm", "ldatuning", "tictoc", "scales")
# new_packages <- packageneeded[!packageneeded %in% installed.packages()[, "Package"]]
# if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(tidyverse)
library(pdftools)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(topicmodels)
library(scales)
library(tictoc)


PATH_DATA <- "knowledge_mining/hackathon/job_data"

pdf_files <- list.files(
  PATH_DATA,
  pattern = "\\.pdf$",
  full.names = TRUE,
  ignore.case = TRUE
)

cat("Found", length(pdf_files), "PDF files\n")

safe_pdf_text <- function(path) {
  out <- tryCatch(
    paste(pdf_text(path), collapse = "\n"),
    error = function(e) NA_character_
  )
  
  tibble(
    doc_id = basename(path),
    text = out,
    nchar_text = ifelse(is.na(out), NA_integer_, nchar(out))
  )
}

SECTION_PATTERN <- "(?m)^(ADMINISTRATION|AMERICAN GOVERNMENT AND POLITICS|COMPARATIVE POLITICS|INTERNATIONAL RELATIONS|METHODOLOGY|NON-ACADEMIC|OTHER|POLITICAL THEORY|PUBLIC ADMINISTRATION|PUBLIC LAW|PUBLIC POLICY|OPEN)"

# ── 1. Extract raw text from PDFs ────────────────────────────────────────────
extract <- map_dfr(pdf_files, safe_pdf_text)

extract_clean <- extract %>%
  filter(!is.na(text), nchar_text > 0)

# ── 2. Split into ads, assign section, extract metadata ──────────────────────
extract_ads <- extract_clean %>%
  mutate(
    ads = map(text, ~ {
      
      # Section header positions in the full PDF text
      section_matches <- str_locate_all(.x, SECTION_PATTERN)[[1]]
      section_names   <- str_extract_all(.x, SECTION_PATTERN)[[1]] %>% str_trim()
      
      # Split into per-ad chunks on trailing eJobs ID line
      chunks              <- str_split(.x, "(?<=eJobs ID:\\s{0,5}\\d{1,6}\\s{0,5}\n)")[[1]]
      chunk_end_positions <- cumsum(nchar(chunks))
      
      # Assign the most recent section header preceding each chunk
      assign_section <- function(pos) {
        if (nrow(section_matches) == 0 || !any(section_matches[, "start"] <= pos))
          return(NA_character_)
        section_names[max(which(section_matches[, "start"] <= pos))]
      }
      
      tibble(
        ad_text  = chunks,
        ejobs_id = str_extract(chunks, "eJobs ID:\\s*(\\d+)", group = 1),
        section  = map_chr(chunk_end_positions, assign_section)
      )
    })
  ) %>%
  select(-text) %>%
  unnest(ads) %>%
  filter(!is.na(ejobs_id), nchar(ad_text) > 200) %>%
  
  # ── 3. Extract per-ad metadata ──────────────────────────────────────────────
  mutate(
    # Footer fields
    start_date   = str_extract(ad_text, "(?<=Start Date:)[^\n]+")           %>% str_trim(),
    app_deadline = str_extract(ad_text, "(?<=Application Deadline:)[^\n]+") %>% str_trim(),
    date_posted  = str_extract(ad_text, "(?<=Date Posted:)[^\n]+")          %>% str_trim(),
    salary       = str_extract(ad_text, "(?<=Salary:)[^\n]+")               %>% str_trim(),
    
    # Header fields
    institution = str_extract(ad_text, "([^\n]+)\n(?:Rank:)", group = 1) %>% str_trim(),
    rank         = str_extract(ad_text, "(?<=Rank:)[^\n]+")                 %>% str_trim(),
    subfields    = str_extract(ad_text, "(?<=Subfield\\(s\\):)[^\n]+")      %>% str_trim(),
    
    # Parsed dates — derived from date_posted, not filename
    date_posted_dt = lubridate::mdy(date_posted),
    year           = lubridate::year(date_posted_dt),
    month          = lubridate::month(date_posted_dt),
    period         = ifelse(date_posted_dt < as.Date("2020-03-01"), "pre_covid", "post_covid")
  )

# Validate extraction effort

cat("PDFs found:       ", length(pdf_files), "\n")
cat("PDFs extracted:   ", nrow(extract_clean), "\n")
cat("Total ads parsed: ", nrow(extract_ads), "\n")
cat("Ads per PDF — summary:\n")
extract_ads %>% count(doc_id) %>% pull(n) %>% summary() %>% print()

extract_ads %>%
  summarise(across(c(institution, rank, subfields, section,
                     start_date, app_deadline, date_posted,
                     salary, date_posted_dt, year, period),
                   ~ scales::percent(mean(is.na(.x)), accuracy = 0.1))) %>%
  glimpse()

cat("\nSection counts (including NA):\n")
extract_ads %>%
  count(section, sort = TRUE) %>%
  mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  print(n = Inf)

cat("\nDate range:\n")
cat("  Earliest: ", format(min(extract_ads$date_posted_dt, na.rm = TRUE)), "\n")
cat("  Latest:   ", format(max(extract_ads$date_posted_dt, na.rm = TRUE)), "\n")

cat("\nAds per year:\n")
extract_ads %>%
  count(year, period) %>%
  print(n = Inf)

set.seed(42)
extract_ads %>%
  slice_sample(n = 10) %>%
  select(doc_id, ejobs_id, section, institution, rank, date_posted_dt, period) %>%
  print(width = Inf)

cat("\nFull text of one ad:\n")
cat(extract_ads$ad_text[sample(nrow(extract_ads), 1)])

## Address high number of NA institutions with help of Claude code
# Three-pass recovery strategy:
#   Pass 1 — clean non-NA values (trim two-column PDF bleed from raw extraction)
#   Pass 2 — sequence signal: the previous ad's right-column overflow contains
#             the current ad's institution header (two-column PDF layout artefact)
#   Pass 3 — body-text signals: EEO statements, "X seeks applications", "X is a university"

# ── Shared institution suffix pattern ─────────────────────────────────────────
inst_sfx <- paste0(
  "(?:University|College|Institute|School|Academy|",
  "Institution|Brookings|RAND|Naval|Council|Foundation)"
)

# ── Signal A: EEO statement (fixed for "University of X" format) ──────────────
# Original pattern failed on "University of West Florida" because it ends in
# "Florida" not "University". New version allows trailing words after the suffix.
eeo_pat <- paste0(
  "((?:[A-Z][\\w&',-]*\\s+){1,7}", inst_sfx,
  "(?:[\\w\\s,.-]{0,30})?)\\s+is an? [Ee]qual [Oo]pportunity"
)

# ── Signal B: "X seeks / invites / encourages applications" ───────────────────
seeks_pat <- paste0(
  "((?:[A-Z][\\w&',-]*\\s+){1,7}", inst_sfx,
  "(?:[\\w\\s,.-]{0,20})?)\\s+",
  "(?:seeks|invites|encourages|welcomes|is seeking|is accepting)"
)

# ── Signal C: "X is a/an [adj] university/college" ────────────────────────────
is_a_pat <- paste0(
  "([A-Z][\\w\\s&',-]{3,55})\\s+is an?\\s+[\\w\\s]{0,25}",
  "(?:[Uu]niversity|[Cc]ollege|[Ii]nstitut|[Aa]cademy)"
)

# ── Signal D: previous ad right-column — institution header of current ad ─────
# In two-column PDFs, the header for ad N (institution + "Rank:") appears in
# the right column of ad N-1's extracted text. Detect via heavy indentation
# followed by a "Rank:" label on the next indented line.
prev_right_pat <- paste0(
  "(?m)\\s{25,}([A-Z][A-Za-z.,'&\\s-]{3,55}", inst_sfx,
  "[A-Za-z.,'&\\s-]{0,20})\\s*\\n\\s{10,}Rank:"
)

# ── Recovery function (body-text signals only, used as fallback) ──────────────
recover_body_text <- function(ad_text) {
  h <- str_match(ad_text, eeo_pat)[, 2]
  if (!is.na(h)) return(str_squish(str_remove(h, "^The\\s+")))
  h <- str_match(ad_text, seeks_pat)[, 2]
  if (!is.na(h)) return(str_squish(str_remove(h, "^The\\s+")))
  h <- str_match(ad_text, is_a_pat)[, 2]
  if (!is.na(h)) return(str_squish(str_remove(h, "^The\\s+")))
  return(NA_character_)
}

# ── Apply all passes ──────────────────────────────────────────────────────────
extract_ads <- extract_ads %>%
  # Pass 1: clean raw non-NA values (strip right-column bleed after institution name)
  mutate(
    institution_clean = if_else(
      !is.na(institution),
      str_squish(str_extract(institution, "^[^\\n]+")),
      NA_character_
    )
  ) %>%
  # Pass 2: sequence signal — look at previous ad's text within same PDF
  group_by(doc_id) %>%
  mutate(
    prev_right_col = str_squish(
      str_match(lag(ad_text), prev_right_pat)[, 2]
    )
  ) %>%
  ungroup() %>%
  # Pass 3: body-text signals as final fallback
  mutate(
    institution_recovered = case_when(
      !is.na(institution_clean) ~ NA_character_,          # already have it
      !is.na(prev_right_col)    ~ prev_right_col,         # sequence signal
      TRUE                      ~ map_chr(ad_text, recover_body_text)  # body text
    ),
    institution_clean = coalesce(institution_clean, institution_recovered)
  )

# ── Check results ─────────────────────────────────────────────────────────────
extract_ads %>%
  summarise(
    total             = n(),
    originally_na     = sum(is.na(institution)),
    recovered_seq     = sum(is.na(institution) & !is.na(prev_right_col)),
    recovered_text    = sum(is.na(institution) & is.na(prev_right_col) & !is.na(institution_recovered)),
    total_recovered   = sum(is.na(institution) & !is.na(institution_clean)),
    still_na          = sum(is.na(institution_clean)),
    pct_na_final      = scales::percent(mean(is.na(institution_clean)), accuracy = 0.1)
  )

