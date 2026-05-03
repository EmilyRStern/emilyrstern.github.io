# ============================================================================
# Political Science Jobs — Quanteda Analysis Starter
# ============================================================================
# This script loads the pre-processed job ads data and walks through
# three analyses you can run and customize:
#
#   1. Top words — what terms appear most in job ads?
#   2. Keyness  — what words are distinctive in one group vs another?
#   3. Trends   — how has a term's usage changed over time?
#
# To get started: change the path in "SETUP" to wherever you saved the .rds
# Then run the whole script top to bottom (Ctrl+A, Ctrl+Enter)
# ============================================================================


# ── SETUP ────────────────────────────────────────────────────────────────────

# Install packages if you haven't already — only need to do this once
# install.packages(c("tidyverse", "quanteda", "quanteda.textstats",
#                    "quanteda.textplots", "showtext", "ggplot2"))

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(showtext)

# !! CHANGE THIS to wherever you saved the .rds file !!
PATH_RDS <- "knowledge_mining/hackathon/job_data/extract_ads_final.rds"

# Load the data
ads <- readRDS(PATH_RDS)


cat("Loaded", nrow(ads), "job ads\n")
cat("Columns available:", paste(colnames(ads), collapse = ", "), "\n")

# Preview
glimpse(ads)


# ── THEME (for nice plots) ───────────────────────────────────────────────────

font_add_google("Source Sans 3", "sourcesans")
font_add_google("Playfair Display", "playfair")
showtext_auto()

civic_cat <- c("#2D6A5F", "#D4A843", "#6BAF9E", "#E8C97A", "#0F3830", "#C8BFA8")

theme_civic <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background  = element_rect(fill = "#F5F0E8", color = NA),
      panel.background = element_rect(fill = "#FFFFFF",  color = NA),
      panel.grid.major = element_line(color = "#C8BFA8", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = "#C8BFA8", linewidth = 0.5),
      plot.title       = element_text(family = "playfair", size = base_size * 1.4,
                                      color = "#1C2B2B", face = "bold", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = base_size * 0.95, color = "#3A4A45",
                                      margin = margin(b = 12)),
      plot.caption     = element_text(family = "sourcesans", size = base_size * 0.72,
                                      color = "#7A8C85", hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(family = "sourcesans", size = base_size * 0.78,
                                      color = "#7A8C85"),
      axis.text        = element_text(family = "sourcesans", size = base_size * 0.75,
                                      color = "#3A4A45"),
      axis.ticks       = element_line(color = "#C8BFA8"),
      legend.background = element_rect(fill = "#F5F0E8", color = NA),
      legend.text      = element_text(family = "sourcesans", size = base_size * 0.75,
                                      color = "#3A4A45"),
      strip.background = element_rect(fill = "#EDE8DC", color = "#C8BFA8"),
      strip.text       = element_text(family = "sourcesans", size = base_size * 0.78,
                                      color = "#1C2B2B"),
      plot.margin      = margin(16, 16, 16, 16)
    )
}


# ── BUILD THE CORPUS, TOKENS, AND DFM ────────────────────────────────────────
# You only need to run this block once per session.
# These three objects are the foundation for all analyses below.

# Step 1: corpus — wraps the text + keeps all your metadata attached
ads <- ads %>%
  mutate(doc_id = paste0("ad_", row_number()))

job_corpus <- corpus(ads, text_field = "ad_text", docid_field = "doc_id")

# Step 2: tokens — splits text into words, removes noise

## Add entries to this section to tell the analysis to ignore a certain word. Below are what I've removed to start.

custom_stops <- c(
  # foreign stopwords
  stopwords("fr"), stopwords("es"),
  
  # academic year patterns
  "2018-2019", "2019-2020", "2020-2021", "2021-2022",
  
  # boilerplate legal/admin terms
  "affirmative", "employer", "action", "send", "applications",
  "equal", "opportunity", "discrimination", "regardless",
  
  # institution name artifacts — add as you spot them
  "hampden-sydney", "kapsarc", "kspp", "fudan", "qss",
  
  # other noise
  "three", "m",
  # metadata field labels
  "ejobs", "date", "deadline", "start", "salary", "rank",
  
  # universal boilerplate
  "application", "applications", "applicants", "candidates", "must",
  
  # too universal to be informative
  "university", "college", "department",
  # HTML/CSS artifacts
  "style", "margin", "margin-bottom", "margin-top", "font-size",
  "font-family", "sans-serif", "arial", "montserrat", "span",
  "0001pt", "10.0pt", "lt", "gt",
  
  # association acronyms
  "sfpe", "ias", "idinsight", "aarhus", "earlham"
)

job_tokens <- tokens(
  job_corpus,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url     = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(custom_stops)

job_dfm <- dfm(job_tokens) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2)

cat("\nCorpus built:", ndoc(job_corpus), "documents,",
    nfeat(job_dfm), "unique terms\n")


# ============================================================================
# ANALYSIS 1 — TOP WORDS
# What terms appear most often across all job ads?
# ============================================================================

# ── 1a. Overall top words ────────────────────────────────────────────────────

top_words <- topfeatures(job_dfm, n = 30) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(count = 2) %>%
  arrange(desc(count))

ggplot(top_words, aes(x = reorder(term, count), y = count)) +
  geom_col(fill = "#2D6A5F") +
  coord_flip() +
  labs(
    title    = "Most common words in political science job ads",
    subtitle = "All sections, all years",
    x        = NULL,
    y        = "Total occurrences",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ── 1b. Top words by section ─────────────────────────────────────────────────
# Change the section name below to explore different subfields

# Options: ADMINISTRATION, COMPARATIVE POLITICS, INTERNATIONAL RELATIONS,
#          METHODOLOGY, NON-ACADEMIC, OTHER, POLITICAL THEORY,
#          PUBLIC ADMINISTRATION, PUBLIC LAW, PUBLIC POLICY, OPEN

SECTION_TO_EXPLORE <- "METHODOLOGY"   # ← CHANGE THIS

top_by_section <- job_dfm %>%
  dfm_subset(section == SECTION_TO_EXPLORE) %>%
  topfeatures(n = 20) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(count = 2)

ggplot(top_by_section, aes(x = reorder(term, count), y = count)) +
  geom_col(fill = "#D4A843") +
  coord_flip() +
  labs(
    title    = paste("Top words in", SECTION_TO_EXPLORE, "ads"),
    x        = NULL,
    y        = "Total occurrences",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ============================================================================
# ANALYSIS 2 — KEYNESS
# What words are distinctively more common in one group vs another?
# ============================================================================

# ── 2a. Pre-COVID vs Post-COVID ───────────────────────────────────────────────

#Remove rows where period is na 
job_dfm_dated <- job_dfm %>%
  dfm_subset(!is.na(docvars(job_dfm, "period")))

textstat_keyness(
  job_dfm_dated,
  target = docvars(job_dfm_dated, "period") == "post_covid"
) %>%
  textplot_keyness(
    n     = 20,
    color = c("#2D6A5F", "#D4A843")
  ) +
  labs(
    title    = "Words distinctive of post-COVID vs pre-COVID job ads",
    subtitle = "Green = more common post-COVID  |  Gold = more common pre-COVID",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ── 2b. One section vs all others ────────────────────────────────────────────
# Change SECTION_TO_EXPLORE above, or set a new one here

TARGET_SECTION <- "NON-ACADEMIC"   # ← CHANGE THIS

textstat_keyness(
  job_dfm,
  target = docvars(job_dfm, "section") == TARGET_SECTION
) %>%
  textplot_keyness(n = 20, color = c("#2D6A5F", "#D4A843")) +
  labs(
    title    = paste("Words distinctive of", TARGET_SECTION, "ads"),
    subtitle = paste(TARGET_SECTION, "vs all other sections"),
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ============================================================================
# ANALYSIS 3 — TRENDS OVER TIME
# How has the use of specific terms changed across years?
# ============================================================================

# ── 3a. Track terms of interest ──────────────────────────────────────────────
# Add or remove terms from this list

TERMS <- c("remote", "online", "diversity", "equity", "data")   # ← CHANGE THIS

trend_data <- job_dfm %>%
  dfm_group(groups = year) %>%
  dfm_select(pattern = TERMS) %>%
  convert(to = "data.frame") %>%
  rename(year = doc_id) %>%
  filter(!is.na(year)) %>%
  pivot_longer(-year, names_to = "term", values_to = "count") %>%
  mutate(year = as.integer(year))

ggplot(trend_data, aes(x = year, y = count, color = term)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = civic_cat) +
  labs(
    title    = "Term frequency trends in political science job ads",
    subtitle = paste("Tracking:", paste(TERMS, collapse = ", ")),
    x        = "Year",
    y        = "Total occurrences",
    color    = NULL,
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ── 3b. Normalize by number of ads per year (relative frequency) ─────────────
# Raw counts can be misleading if some years have more ads than others.
# This shows occurrences per 1,000 ads instead.

ads_per_year <- ads %>%
  filter(!is.na(year)) %>%
  count(year, name = "n_ads")

trend_data_norm <- trend_data %>%
  left_join(ads_per_year, by = "year") %>%
  mutate(per_1000 = (count / n_ads) * 1000)

ggplot(trend_data_norm, aes(x = year, y = per_1000, color = term)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = civic_cat) +
  labs(
    title    = "Term frequency trends (normalized)",
    subtitle = paste("Occurrences per 1,000 ads — tracking:", paste(TERMS, collapse = ", ")),
    x        = "Year",
    y        = "Occurrences per 1,000 ads",
    color    = NULL,
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic()


# ============================================================================
# QUICK REFERENCE — things you can change
# ============================================================================
#
#  SECTION_TO_EXPLORE   any of: ADMINISTRATION, COMPARATIVE POLITICS,
#                        INTERNATIONAL RELATIONS, METHODOLOGY, NON-ACADEMIC,
#                        OTHER, POLITICAL THEORY, PUBLIC ADMINISTRATION,
#                        PUBLIC LAW, PUBLIC POLICY, OPEN
#
#  TARGET_SECTION       same options — used for keyness comparison
#
#  TERMS                any words you want to track over time
#                        e.g. c("qualitative", "quantitative", "machine", "learn")
#
#  period               "pre_covid" or "post_covid"
#
#  year                 any integer from 2015 to 2026
#
# ============================================================================