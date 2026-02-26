# ============================================================================
# helpers.R -- Shared constants, color palettes, and utility functions
# ============================================================================

library(tidyverse)
library(ggrepel)
library(scales)
library(viridis)
library(patchwork)

# === Paths ===
SOURCE_PATH <- "/home/user/United-Nations-General-Assembly-Votes-and-Ideal-Points/"
PROJECT_PATH <- "/home/user/United-Nations-General-Assembly-Votes-and-Ideal-Points/"

# === ASEAN Country Definitions (COW codes) ===
ASEAN_CODES <- c(775, 800, 811, 812, 816, 820, 830, 835, 840, 850)
REFERENCE_CODES <- c(2, 710) # US, China
ALL_CODES <- c(ASEAN_CODES, REFERENCE_CODES)

ASEAN_LABELS <- c(
  "775" = "Myanmar", "800" = "Thailand", "811" = "Cambodia",
  "812" = "Laos", "816" = "Vietnam", "820" = "Malaysia",
  "830" = "Singapore", "835" = "Brunei", "840" = "Philippines",
  "850" = "Indonesia"
)

REFERENCE_LABELS <- c("2" = "United States", "710" = "China")

# ASEAN founding year: 1967 = session 22
ASEAN_START_SESSION <- 22

# === Color palette: 10 distinguishable colors for ASEAN countries ===
ASEAN_COLORS <- c(
  "Myanmar" = "#E41A1C", "Thailand" = "#377EB8", "Cambodia" = "#4DAF4A",
  "Laos" = "#984EA3", "Vietnam" = "#FF7F00", "Malaysia" = "#A65628",
  "Singapore" = "#F781BF", "Brunei" = "#66C2A5", "Philippines" = "#999999",
  "Indonesia" = "#E6AB02"
)
REF_COLORS <- c("United States" = "#0000FF", "China" = "#FF0000")

# === Session to Year conversion ===
session_to_year <- function(session) session + 1945

# === Common ggplot theme ===
theme_asean <- function() {
  theme_light() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold")
    )
}

# === Data loading helper ===
# Reads a source ideal-point CSV and filters to ASEAN + US + China
load_and_filter <- function(filename, issue_label = "All") {
  filepath <- paste0(SOURCE_PATH, "Output/", filename)
  df <- read.csv(filepath, stringsAsFactors = FALSE)

  # Find the IdealPoint and NVotes columns (handle issue-suffixed names)
  ip_col <- grep("^IdealPoint", names(df), value = TRUE)[1]
  nv_col <- grep("^NVotes", names(df), value = TRUE)[1]
  cn_col <- grep("^Countryname", names(df), value = TRUE)[1]

  df <- df %>%
    filter(ccode %in% ALL_CODES) %>%
    select(ccode, session,
           IdealPoint = all_of(ip_col),
           NVotes = all_of(nv_col),
           Countryname = all_of(cn_col)) %>%
    mutate(
      year = session_to_year(session),
      issue = issue_label,
      country_label = case_when(
        ccode %in% ASEAN_CODES ~ ASEAN_LABELS[as.character(ccode)],
        ccode == 2 ~ "United States",
        ccode == 710 ~ "China",
        TRUE ~ Countryname
      ),
      is_asean = ccode %in% ASEAN_CODES
    )

  return(df)
}
