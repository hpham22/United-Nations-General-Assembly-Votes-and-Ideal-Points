# ============================================================================
# 02_alignment_analysis.R -- Compute alignment metrics (US/China distance,
#                            intra-ASEAN cohesion) for all issue areas
# ============================================================================

source("/home/user/ASEAN-UNGA-Voting-Analysis/scripts/helpers.R")

cat("Computing alignment metrics...\n")

df_long <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_by_issue.csv"),
                    stringsAsFactors = FALSE)

# --- Compute distances from US and China for each issue area ---

# Extract reference ideal points
us_ip <- df_long %>%
  filter(ccode == 2) %>%
  select(session, issue, us_ip = IdealPoint)

china_ip <- df_long %>%
  filter(ccode == 710) %>%
  select(session, issue, china_ip = IdealPoint)

# Join distances with ASEAN data
asean_dist <- df_long %>%
  filter(is_asean == TRUE) %>%
  left_join(us_ip, by = c("session", "issue")) %>%
  left_join(china_ip, by = c("session", "issue")) %>%
  mutate(
    dist_us = abs(IdealPoint - us_ip),
    dist_china = abs(IdealPoint - china_ip),
    signed_dist_us = IdealPoint - us_ip,
    signed_dist_china = IdealPoint - china_ip,
    closer_to = case_when(
      is.na(dist_china) ~ NA_character_,
      dist_us < dist_china ~ "United States",
      dist_us > dist_china ~ "China",
      TRUE ~ "Equidistant"
    )
  )

write.csv(asean_dist, paste0(PROJECT_PATH, "data/asean_distances.csv"), row.names = FALSE)

# --- Compute ASEAN summary statistics per session per issue ---

asean_summary <- df_long %>%
  filter(is_asean == TRUE) %>%
  group_by(session, year, issue) %>%
  summarise(
    n_members = n(),
    asean_mean = mean(IdealPoint, na.rm = TRUE),
    asean_sd = sd(IdealPoint, na.rm = TRUE),
    asean_min = min(IdealPoint, na.rm = TRUE),
    asean_max = max(IdealPoint, na.rm = TRUE),
    asean_range = max(IdealPoint) - min(IdealPoint),
    .groups = "drop"
  ) %>%
  left_join(us_ip %>% rename(us_idealpoint = us_ip), by = c("session", "issue")) %>%
  left_join(china_ip %>% rename(china_idealpoint = china_ip), by = c("session", "issue")) %>%
  mutate(
    mean_dist_us = abs(asean_mean - us_idealpoint),
    mean_dist_china = abs(asean_mean - china_idealpoint)
  )

write.csv(asean_summary, paste0(PROJECT_PATH, "data/asean_summary.csv"), row.names = FALSE)

# --- Compute mean distance per session (for "closer-to" analysis) ---

mean_dist <- asean_dist %>%
  group_by(session, year, issue) %>%
  summarise(
    mean_dist_us = mean(dist_us, na.rm = TRUE),
    mean_dist_china = mean(dist_china, na.rm = TRUE),
    pct_closer_china = mean(closer_to == "China", na.rm = TRUE),
    .groups = "drop"
  )

write.csv(mean_dist, paste0(PROJECT_PATH, "data/asean_mean_distances.csv"), row.names = FALSE)

cat("Alignment analysis complete.\n")
cat("  Distance file:     ", nrow(asean_dist), "rows\n")
cat("  Summary file:      ", nrow(asean_summary), "rows\n")
cat("  Mean distance file:", nrow(mean_dist), "rows\n")
