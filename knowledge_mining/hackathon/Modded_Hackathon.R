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
# install.packages(c("tidyverse","tidytext", "quanteda", "quanteda.textstats",
#                    "quanteda.textplots", "showtext", "ggplot2"))

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(showtext)
library(tidytext)

# !! CHANGE THIS to wherever you saved the .rds file !!
PATH_RDS <- "~/R Docs/EPPS6323/WordFish/jobads/extract_ads_final.rds"

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
# ANALYSIS 4 — AD VOLUME OVER TIME
# How many job listings were posted each year?
# ============================================================================

ads_per_year_plot <- ads %>%
  filter(!is.na(year)) %>%
  count(year, name = "n_ads") %>%
  mutate(year = as.integer(year))

ggplot(ads_per_year_plot, aes(x = year, y = n_ads)) +
  geom_col(fill = "#2D6A5F") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "#0F3830", linewidth = 0.8) +
  geom_label(aes(x = 2020, y = max(n_ads) * 0.92, label = "Peak COVID (2020)"),
             hjust       = -0.08,
             family      = "sourcesans",
             size        = 3,
             color       = "#0F3830",
             fill        = "#F5F0E8",
             label.size  = 0.3) +
  geom_text(
    aes(label = n_ads),
    vjust  = -0.5,
    family = "sourcesans",
    size   = 3.2,
    color  = "#3A4A45"
  ) +
  scale_x_continuous(breaks = unique(ads_per_year_plot$year)) +
  labs(
    title    = "Number of Political Science Job Ads Posted Per Year",
    subtitle = "Total listings across all sections",
    x        = "Year",
    y        = "Number of ads",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ============================================================================
# ANALYSIS 5 — AD VOLUME BY SECTION OVER TIME
# How has posting volume changed within each job type across years?
# ============================================================================

section_year_counts <- ads %>%
  filter(!is.na(year), !is.na(section)) %>%
  count(year, section, name = "n_ads") %>%
  mutate(year = as.integer(year))

# ── 5a. Faceted line chart — one panel per section ───────────────────────────

ggplot(section_year_counts, aes(x = year, y = n_ads, color = section, group = section)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ section, scales = "free_y", ncol = 3) +
  scale_color_manual(values = colorRampPalette(civic_cat)(n_distinct(section_year_counts$section))) +
  scale_x_continuous(breaks = unique(section_year_counts$year)) +
  labs(
    title    = "Job ad Postings by Section Over Time",
    subtitle = "Y-axis is free-scaled per section to highlight relative trends",
    x        = "Year",
    y        = "Number of ads",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic() +
  theme(
    legend.position  = "none",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    strip.text       = element_text(size = 8)
  )


# ── 5b. Stacked area chart — overall composition shift at a glance ───────────

ggplot(section_year_counts, aes(x = year, y = n_ads, fill = section)) +
  geom_area(position = "stack", alpha = 0.85, color = "#F5F0E8", linewidth = 0.3) +
  scale_fill_manual(values = colorRampPalette(civic_cat)(n_distinct(section_year_counts$section))) +
  scale_x_continuous(breaks = unique(section_year_counts$year)) +
  labs(
    title    = "Composition of Job Ad Postings by Section Over Time",
    subtitle = "Stacked area — shows both total volume and share per section",
    x        = "Year",
    y        = "Number of ads",
    fill     = NULL,
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "lines")
  ) +
  guides(fill = guide_legend(nrow = 3))

# ============================================================================
# ANALYSIS 6 — EMERGING vs DECLINING KEYWORDS BY SECTION
# Compares the earliest years vs most recent years within each section
# to surface vocabulary that is genuinely entering or leaving the field.
# ============================================================================

# ── Parameters ───────────────────────────────────────────────────────────────
# Define how many years to treat as "early" and "late" windows.
# E.g., N_YEARS = 2 means earliest 2 years vs latest 2 years in each section.

N_YEARS   <- 2   # ← CHANGE THIS to widen/narrow the comparison window
N_TERMS   <- 10  # ← top N emerging + top N declining per section

# ── Helper: run keyness for one section ──────────────────────────────────────

get_section_keyness <- function(section_name, dfm_obj, n_years, n_terms) {
  
  sub_dfm <- dfm_obj %>%
    dfm_subset(section == section_name & !is.na(year))
  
  if (ndoc(sub_dfm) < 10) return(NULL)   # skip tiny sections
  
  years_present <- sort(unique(as.integer(docvars(sub_dfm, "year"))))
  
  if (length(years_present) < (n_years * 2)) return(NULL)  # not enough years
  
  early_years <- head(years_present, n_years)
  late_years  <- tail(years_present, n_years)
  
  sub_dfm_filtered <- sub_dfm %>%
    dfm_subset(as.integer(year) %in% c(early_years, late_years))
  
  if (ndoc(sub_dfm_filtered) < 4) return(NULL)
  
  is_late <- as.integer(docvars(sub_dfm_filtered, "year")) %in% late_years
  
  ks <- tryCatch(
    textstat_keyness(sub_dfm_filtered, target = is_late, measure = "chi2"),
    error = function(e) NULL
  )
  
  if (is.null(ks) || nrow(ks) < 2) return(NULL)
  
  emerging  <- ks %>% filter(chi2 > 0) %>% slice_max(chi2,  n = n_terms)
  declining <- ks %>% filter(chi2 < 0) %>% slice_min(chi2,  n = n_terms)
  
  bind_rows(
    emerging  %>% mutate(direction = "Emerging"),
    declining %>% mutate(direction = "Declining")
  ) %>%
    mutate(
      section      = section_name,
      early_window = paste(early_years, collapse = "–"),
      late_window  = paste(late_years,  collapse = "–")
    )
}

# ── Run across all sections ───────────────────────────────────────────────────

all_sections <- unique(na.omit(docvars(job_dfm, "section")))

keyness_all <- map_dfr(all_sections, ~ get_section_keyness(
  section_name = .x,
  dfm_obj      = job_dfm,
  n_years      = N_YEARS,
  n_terms      = N_TERMS
))

# ── 6a. Faceted diverging bar chart ──────────────────────────────────────────
# One panel per section. Bars point right (emerging) or left (declining).

keyness_plot_data <- keyness_all %>%
  filter(!section %in% c("OPEN", "NON-ACADEMIC")) %>%
  mutate(
    chi2_signed = if_else(direction == "Declining", -abs(chi2), abs(chi2)),
    direction   = factor(direction, levels = c("Emerging", "Declining"))
  ) %>%
  group_by(section, direction) %>%
  slice_max(abs(chi2_signed), n = 4) %>%
  ungroup() %>%
  mutate(term = reorder_within(feature, chi2_signed, section))

ggplot(keyness_plot_data, aes(x = chi2_signed, y = term, fill = direction)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, color = "#C8BFA8", linewidth = 0.5) +
  scale_y_reordered() +
  scale_fill_manual(
    values = c("Emerging" = "#2D6A5F", "Declining" = "#D4A843"),
    name   = NULL
  ) +
  facet_wrap(~ section, scales = "free_y", ncol = 2) +
  labs(
    title    = "Emerging and Declining Keywords by Section",
    subtitle = paste0(
      "Earliest ", N_YEARS, " vs latest ", N_YEARS,
      " years within each section  |  χ² keyness, target = recent"
    ),
    x        = "← Declining   |   Emerging →",
    y        = NULL,
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic(base_size = 10) +
  theme(
    legend.position  = "top",
    legend.key.size  = unit(0.6, "lines"),
    strip.text       = element_text(size = 8, face = "bold"),
    axis.text.y      = element_text(size = 7),
    axis.text.x      = element_text(size = 7),
    panel.spacing    = unit(1.2, "lines")
  )


# ── 6b. Salary / pay mentions by section ─────────────────────────────────────

SALARY_TERMS <- c("salary", "pay", "compensation", "stipend", "wage")

salary_by_section <- job_dfm %>%
  dfm_select(pattern = SALARY_TERMS) %>%
  convert(to = "data.frame") %>%
  bind_cols(section = docvars(job_dfm, "section")) %>%
  filter(!is.na(section), !section %in% c("OPEN", "NON-ACADEMIC")) %>%
  pivot_longer(-c(doc_id, section), names_to = "term", values_to = "count") %>%
  group_by(section, term) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  group_by(section) %>%
  mutate(section_total = sum(total)) %>%
  ungroup()

salary_by_section <- salary_by_section %>%
  left_join(ads %>% count(section, name = "n_ads"), by = "section") %>%
  mutate(total = (total / n_ads) * 1000)

ggplot(salary_by_section, aes(x = reorder(section, section_total), y = total, fill = term)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = civic_cat, name = "Term") +
  labs(
    title    = "Salary and Pay-Related Term Mentions by Section",
    subtitle = "Raw counts across all ads per section",
    x        = NULL,
    y        = "Total mentions",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.6, "lines")
  )

salary_by_section_period <- job_dfm %>%
  dfm_select(pattern = SALARY_TERMS) %>%
  convert(to = "data.frame") %>%
  bind_cols(
    section = docvars(job_dfm, "section"),
    period  = docvars(job_dfm, "period")
  ) %>%
  filter(
    !is.na(section), !is.na(period),
    !section %in% c("OPEN", "NON-ACADEMIC")
  ) %>%
  pivot_longer(-c(doc_id, section, period), names_to = "term", values_to = "count") %>%
  group_by(section, period, term) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  mutate(period = factor(period, levels = c("pre_covid", "post_covid"),
                         labels = c("Pre-COVID", "Post-COVID")))

ggplot(salary_by_section_period, aes(x = period, y = total, fill = term)) +
  geom_col(width = 0.65, position = "stack") +
  facet_wrap(~ section, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = civic_cat, name = "Term") +
  labs(
    title    = "Salary and Pay-Related Mentions: Pre- vs Post-COVID",
    subtitle = "Raw counts per section split by period",
    x        = NULL,
    y        = "Total mentions",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic(base_size = 10) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.6, "lines"),
    strip.text      = element_text(size = 8, face = "bold"),
    panel.spacing   = unit(1.2, "lines")
  )

# ── 6c. Chi-squared test for "remote" pre- vs post-COVID──────────────────────

remote_counts <- job_dfm %>%
  dfm_subset(!is.na(docvars(job_dfm, "period"))) %>%
  dfm_select(pattern = "remote") %>%
  convert(to = "data.frame") %>%
  bind_cols(period = docvars(
    job_dfm %>% dfm_subset(!is.na(docvars(job_dfm, "period"))), 
    "period"
  )) %>%
  group_by(period) %>%
  summarise(
    remote_mentions = sum(remote),
    total_words     = n(),
    .groups         = "drop"
  ) %>%
  mutate(not_remote = total_words - remote_mentions)

# Build the contingency table
contingency_table <- remote_counts %>%
  select(period, remote_mentions, not_remote) %>%
  column_to_rownames("period") %>%
  as.matrix()

# Run the chi-squared test
chi_result <- chisq.test(contingency_table)

# Print readable results
cat("=== Chi-squared test: 'remote' pre- vs post-COVID ===\n\n")
cat("Observed counts:\n")
print(contingency_table)
cat("\nExpected counts (if no difference):\n")
print(round(chi_result$expected, 1))
cat("\nChi-squared statistic:", round(chi_result$statistic, 3), "\n")
cat("Degrees of freedom:  ", chi_result$parameter, "\n")
cat("P-value:             ", format.pval(chi_result$p.value, digits = 4), "\n")
cat("\nConclusion: The difference is",
    if_else(chi_result$p.value < 0.05, 
            "STATISTICALLY SIGNIFICANT (p < 0.05)", 
            "not statistically significant (p >= 0.05)"), "\n")

# ── Visualization: "remote" pre- vs post-COVID ────────────────────────────────

remote_plot_data <- remote_counts %>%
  mutate(
    period       = factor(period, 
                          levels = c("pre_covid", "post_covid"),
                          labels = c("Pre-COVID", "Post-COVID")),
    expected     = chi_result$expected[, "remote_mentions"]
  ) %>%
  pivot_longer(
    cols      = c(remote_mentions, expected),
    names_to  = "type",
    values_to = "count"
  ) %>%
  mutate(type = factor(type,
                       levels = c("remote_mentions", "expected"),
                       labels = c("Observed", "Expected (if no difference)")))

# ── 6c. Observed vs Expected grouped bar ─────────────────────────────────────

ggplot(remote_plot_data, aes(x = period, y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.9) +
  geom_text(
    aes(label = round(count, 1)),
    position = position_dodge(width = 0.6),
    vjust    = -0.5,
    family   = "sourcesans",
    size     = 3.2,
    color    = "#3A4A45"
  ) +
  annotate(
    "text",
    x      = 1.5,
    y      = max(remote_plot_data$count) * 1.12,
    label  = paste0("χ² = ", round(chi_result$statistic, 2),
                    "  |  p ", ifelse(chi_result$p.value < 0.001, "< 0.001",
                                      paste0("= ", round(chi_result$p.value, 3)))),
    family = "sourcesans",
    size   = 3.2,
    color  = "#0F3830"
  ) +
  scale_fill_manual(
    values = c("Observed"                    = "#2D6A5F",
               "Expected (if no difference)" = "#D4A843"),
    name   = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Observed vs Expected Mentions of 'remote'",
    subtitle = "If COVID had no effect, observed and expected bars would be equal",
    x        = NULL,
    y        = "Number of mentions",
    caption  = "Source: APSA eJobs"
  ) +
  theme_civic() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.6, "lines")
  )
