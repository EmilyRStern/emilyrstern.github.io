### EPPS 6323 Workshop: Text Analytics- US DOD China Military Power Reports (1999–2025)
### Tools: quanteda, quanteda.textmodels, quanteda.textstats, stm
### Author: Karl Ho, University of Texas at Dallas

## 0. Setup: Packages and Configuration


# Install missing packages (run once, then comment out)
packageneeded <- c("pdftools", "tidyverse", "RColorBrewer",
                    "quanteda", "quanteda.textstats", "quanteda.textplots",
                    "quanteda.textmodels", "readtext", "topicmodels",
                    "seededlda", "stm", "ldatuning", "tictoc", "scales")
new_packages <- packageneeded[!packageneeded %in% installed.packages()[, "Package"]]
 if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(tidyverse)
library(pdftools)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(topicmodels)
library(scales)
library(tictoc)

# Configuration: Set path to your PDF directory
# Change this single line to run on any machine
PATH_DATA <- "knowledge_mining/lab03/USDOD/USDOD"  # where the military report files are

# Seed for reproducibility
SEED <- 6323

## 1. DATA INGESTION


# List all PDF files
pdf_files <- list.files(PATH_DATA, pattern = "\\.pdf$", full.names = TRUE, 
                        ignore.case = TRUE)
cat("Found", length(pdf_files), "PDF files\n")

# Read PDFs using pdftools with error handling
# readtext can fail on non-standard PDFs (e.g., 1999 GPO report)
# pdftools::pdf_text() is more robust for government PDFs

read_pdf_safe <- function(filepath) {
  tryCatch({
    pages <- pdftools::pdf_text(filepath)
    text <- paste(pages, collapse = "\n")
    return(text)
  }, error = function(e) {
    warning("Failed to read: ", basename(filepath), " — ", e$message)
    return(NA_character_)
  })
}

DODq <- tibble(
  doc_id = basename(pdf_files),
  text   = map_chr(pdf_files, read_pdf_safe)
)

# Report any failures
n_failed <- sum(is.na(DODq$text))
if (n_failed > 0) {
  cat("WARNING:", n_failed, "file(s) failed to read:\n")
  print(DODq$doc_id[is.na(DODq$text)])
  DODq <- filter(DODq, !is.na(text))
}

# Extract year from filenames (first 4-digit sequence)
DODq$year <- as.numeric(str_extract(DODq$doc_id, "\\d{4}"))

# Create clean document IDs
DODq$doc_id <- paste0("USDOD_", DODq$year, ".pdf")

# Sort by year for consistent ordering
DODq <- arrange(DODq, year)

cat("Successfully loaded", nrow(DODq), "reports spanning", 
    min(DODq$year), "to", max(DODq$year), "\n")



## 2. CORPUS CONSTRUCTION WITH METADATA


corp_DOD <- corpus(DODq, text_field = "text")

# Assign president by year (updated for second Trump term)
corp_DOD$president <- case_when(
  corp_DOD$year <= 2000                          ~ "Clinton",
  corp_DOD$year >= 2001 & corp_DOD$year <= 2008  ~ "Bush",
  corp_DOD$year >= 2009 & corp_DOD$year <= 2016  ~ "Obama",
  corp_DOD$year >= 2017 & corp_DOD$year <= 2020  ~ "Trump",
  corp_DOD$year >= 2021 & corp_DOD$year <= 2024  ~ "Biden",
  corp_DOD$year >= 2025                          ~ "Trump II",
  TRUE                                           ~ NA_character_
)

# Verify metadata
summary(corp_DOD, 5)
table(corp_DOD$president)



## 3. TOKENIZATION AND DFM


# Tokenize with preprocessing
toks_DOD <- tokens(corp_DOD, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(c("page", "figure", "table", "appendix", "chapter",
                   "https", "www", "pdf", "gov"))  # Remove boilerplate

# Create DFM
dfmat_DOD <- dfm(toks_DOD)

cat("DFM dimensions:", dim(dfmat_DOD), "\n")
cat("Documents:", ndoc(dfmat_DOD), "| Features:", nfeat(dfmat_DOD), "\n")



## 4. EXPLORATORY TEXT ANALYSIS


# --- 4a. Top features ---

topfeat <- topfeatures(dfmat_DOD, 30)
data.frame(feature = names(topfeat), freq = topfeat) %>%
  ggplot(aes(x = reorder(feature, freq), y = freq)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 30 Features across All DOD China Reports",
       x = NULL, y = "Frequency") +
  theme_bw(base_size = 18) +
  theme(text=element_text(family="Palatino"))

# --- 4b. Track key terms over time ---

# Create a year-grouped DFM for trend analysis
dfmat_year <- dfm_group(dfmat_DOD, groups = corp_DOD$year)

# Track specific terms
key_terms <- c("taiwan", "pla", "nuclear", "cyber", "missile", 
               "threat", "aircraft", "carrier", "space", "ai")

term_trends <- convert(dfmat_year, to = "data.frame") %>%
  dplyr::select(doc_id, any_of(key_terms)) %>%
  mutate(year = as.numeric(str_extract(doc_id, "\\d{4}"))) %>%
  pivot_longer(-c(doc_id, year), names_to = "term", values_to = "count")

# Normalize by document length
doc_lengths <- ntoken(dfmat_year)
term_trends <- term_trends %>%
  left_join(tibble(doc_id = names(doc_lengths), total = doc_lengths), by = "doc_id") %>%
  mutate(rate = count / total * 10000)  # Rate per 10,000 words

ggplot(term_trends, aes(x = year, y = rate, color = term)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~term, scales = "free_y", ncol = 2) +
  labs(title = "Key Term Frequency in DOD China Reports (per 10,000 words)",
       x = "Year", y = "Rate per 10,000 words") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none", text=element_text(family="Palatino"))


# --- 4c. Keyness: Trump vs. Obama/Bush ---

tstat_key <- textstat_keyness(
  dfmat_DOD, 
  target = corp_DOD$president %in% c("Trump", "Trump II"),
  measure = "lr"
)

textplot_keyness(tstat_key, n = 15,
                 color = c("steelblue", "slategray"), font="Palatino") +
  labs(title = "Keyness: Trump-era vs. Other Administrations",
       subtitle = "Log-likelihood ratio (LLR)") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", text=element_text(family="Palatino"))


# --- 4d. Collocations (bigrams and trigrams) ---

tstat_col2 <- textstat_collocations(toks_DOD, size = 2, min_count = 50)
head(tstat_col2, 20)

tstat_col3 <- textstat_collocations(toks_DOD, size = 3, min_count = 30)
head(tstat_col3, 15)



## 5. WORDFISH SCALING


# Trim DFM for Wordfish (remove very rare and very common features)
dfmat_wf <- dfm_trim(dfmat_DOD, min_termfreq = 10, max_docfreq = 0.95,
                       docfreq_type = "prop")

# Identify anchor documents by name (not position)
# Use an early report and a recent report to set scale direction
anchor_early <- which(docnames(dfmat_wf) == "USDOD_2000.pdf")
anchor_late  <- which(docnames(dfmat_wf) == "USDOD_2023.pdf")

cat("Anchor documents:", 
    docnames(dfmat_wf)[anchor_early], "(early) and",
    docnames(dfmat_wf)[anchor_late], "(late)\n")

# Fit Wordfish
tmod_wf <- textmodel_wordfish(dfmat_wf, dir = c(anchor_early, anchor_late))
summary(tmod_wf)

# --- 5a. Document positions (1D) ---
textplot_scale1d(tmod_wf)

# --- 5b. Document positions by president ---
textplot_scale1d(tmod_wf, groups = dfmat_wf$president, 
                 highlighted_color = "firebrick")

# --- 5c. Feature positions with domain-specific highlights ---
textplot_scale1d(tmod_wf, margin = "features",
                 highlighted = c("taiwan", "pla", "nuclear", "missile",
                                  "cyber", "space", "carrier", "ai"),
                 highlighted_color = "firebrick")

# --- 5d. Theta over time (custom ggplot) ---

doc_pos <- data.frame(
  document  = docnames(dfmat_wf),
  theta     = tmod_wf$theta,
  year      = as.numeric(str_extract(docnames(dfmat_wf), "\\d{4}")),
  president = dfmat_wf$president,
  stringsAsFactors = FALSE
) %>% arrange(year)

# Color palette for all presidents
pres_colors <- c(
  "Clinton"  = "#9467bd",
  "Bush"     = "#1f78b4",
  "Obama"    = "#33a02c",
  "Trump"    = "#e31a1c",
  "Biden"    = "#ff7f00",
  "Trump II" = "#d62728"
)

ggplot(doc_pos, aes(x = year, y = theta)) +
  geom_line(color = "gray60", linetype = "dashed") +
  geom_point(aes(color = president), size = 3) +
  geom_text(aes(label = year), vjust = -1.2, size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "gray40", 
              alpha = 0.15, linewidth = 0.5) +
  scale_color_manual(values = pres_colors) +
  labs(title = "DOD China Report Positioning by Year (Wordfish)",
       subtitle = expression("Estimated latent position" ~ hat(theta) ~ "over time"),
       x = "Year", y = expression(hat(theta)),
       color = "President") +
  theme_bw(base_size = 16) +
  theme(text=element_text(family="Palatino"), plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

# --- 5e. Diagnostic: Check 2001 report ---
# The 2001 report may be an outlier because:
# - Different format (first edition was only 23 pages vs. 200+ later)
# - Different authoring convention pre-9/11
# Check document length as a potential confound:
cat("\nDocument lengths (tokens):\n")
print(sort(ntoken(dfmat_wf)))

# 6. CORRESPONDENCE ANALYSIS (MULTI-DIMENSIONAL)

tmod_ca <- textmodel_ca(dfmat_wf)

# 1D scale grouped by president
textplot_scale1d(tmod_ca, groups = dfmat_wf$president)

# 2D biplot
# NOTE: coef(tmod_ca, doc_dim = 2) returns empty for dim2.
# Access document coordinates directly from the model object:
#   tmod_ca$rowcoord is a matrix (documents × dimensions)
#   tmod_ca$colcoord is a matrix (features × dimensions)

dat_ca <- data.frame(
  dim1      = tmod_ca$rowcoord[, 1],
  dim2      = tmod_ca$rowcoord[, 2],
  document  = rownames(tmod_ca$rowcoord),
  stringsAsFactors = FALSE
)
dat_ca$year      <- as.numeric(str_extract(dat_ca$document, "\\d{4}"))
dat_ca$president <- dfmat_wf$president

ggplot(dat_ca, aes(x = dim1, y = dim2, color = president)) +
  geom_point(size = 3) +
  geom_text(aes(label = year), vjust = -1, size = 3) +
  scale_color_manual(values = pres_colors) +
  labs(title = "Correspondence Analysis: DOD China Reports",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", text=element_text(family = "Palatino"))



## 7. TOPIC MODELS


# --- 7a. LDA via topicmodels ---

# Prepare: paragraph-level DFM for richer topic structure
toks_para <- tokens(corpus_reshape(corp_DOD, to = "paragraphs"),
                    remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("english"))

dfmat_para <- dfm(toks_para) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 3)

dtm_para <- convert(dfmat_para, to = "topicmodels")

set.seed(SEED)
tic("LDA fitting (k=5)")
lda_model <- LDA(dtm_para, method = "Gibbs", k = 5, 
                 control = list(seed = SEED, burnin = 500, iter = 1000))
toc()

terms(lda_model, 10)

# --- 7b. Find optimal K using ldatuning ---

# library(ldatuning)
# tic("Finding optimal K")
# result <- FindTopicsNumber(
#   dtm_para,
#   topics = seq(from = 3, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = SEED),
#   verbose = TRUE
# )
# toc()
# FindTopicsNumber_plot(result)


# --- 7c. Structural Topic Model (STM) ---

# library(stm)
# 
# dfmat_stm <- dfmat_DOD %>%
#   dfm_remove(stopwords("english")) %>%
#   dfm_trim(min_termfreq = 5)
# 
# stm_input <- convert(dfmat_stm, to = "stm")
# stm_input$meta$president <- dfmat_DOD$president
# stm_input$meta$year <- as.numeric(str_extract(docnames(dfmat_DOD), "\\d{4}"))
# 
# # Search for optimal K
# set.seed(SEED)
# k_result <- searchK(
#   documents = stm_input$documents,
#   vocab = stm_input$vocab,
#   K = 5:15,
#   prevalence = ~ president + s(year),
#   data = stm_input$meta,
#   verbose = TRUE
# )
# plot(k_result)
# 
# # Fit with chosen K
# stm_model <- stm(
#   documents = stm_input$documents,
#   vocab = stm_input$vocab,
#   K = 10,
#   prevalence = ~ president + s(year),
#   data = stm_input$meta,
#   seed = SEED
# )
# 
# labelTopics(stm_model)
# 
# # Estimate effect of president on topic prevalence
# prep <- estimateEffect(1:10 ~ president, stm_model, 
#                        metadata = stm_input$meta)
# summary(prep)
# 
# # Visualize topic correlations
# plot(topicCorr(stm_model))



## 8. SENTIMENT / POLARITY ANALYSIS


# Using Lexicoder Sentiment Dictionary (LSD2015)
dfmat_sent <- dfm_lookup(dfmat_DOD, dictionary = data_dictionary_LSD2015)

sent_df <- convert(dfmat_sent, to = "data.frame") %>%
  mutate(
    year      = as.numeric(str_extract(doc_id, "\\d{4}")),
    net       = positive - negative,
    total     = ntoken(corp_DOD),
    sent_rate = net / total,
    president = corp_DOD$president
  )

ggplot(sent_df, aes(x = year, y = sent_rate, color = president)) +
  geom_line(color = "gray60", linetype = "dashed") +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "gray40", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = pres_colors) +
  labs(title = "Net Sentiment of DOD China Reports over Time",
       subtitle = "LSD2015: (positive − negative) / total tokens",
       x = "Year", y = "Net Sentiment Rate",
       color = "President") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", text=element_text(family="Palatino"))



## 9. SUMMARY OUTPUT


cat("\n========== ANALYSIS SUMMARY ==========\n")
cat("Reports analyzed:", ndoc(corp_DOD), "\n")
cat("Year range:", min(DODq$year), "-", max(DODq$year), "\n")
cat("Presidents:", paste(unique(corp_DOD$president), collapse = ", "), "\n")
cat("DFM dimensions:", dim(dfmat_DOD), "\n")
cat("Wordfish theta range:", round(range(tmod_wf$theta), 3), "\n")
cat("=======================================\n")

