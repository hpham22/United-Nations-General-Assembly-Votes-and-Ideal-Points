# ============================================================================
# 03_visualizations.R -- All ggplot2 visualizations for ASEAN UNGA analysis
# ============================================================================

source("/home/user/ASEAN-UNGA-Voting-Analysis/scripts/helpers.R")

cat("Generating visualizations...\n")

# Load pre-computed data
df_all      <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_all.csv"), stringsAsFactors = FALSE)
df_long     <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_by_issue.csv"), stringsAsFactors = FALSE)
df_wide     <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_wide.csv"), stringsAsFactors = FALSE)
asean_dist  <- read.csv(paste0(PROJECT_PATH, "data/asean_distances.csv"), stringsAsFactors = FALSE)
asean_summ  <- read.csv(paste0(PROJECT_PATH, "data/asean_summary.csv"), stringsAsFactors = FALSE)
mean_dist   <- read.csv(paste0(PROJECT_PATH, "data/asean_mean_distances.csv"), stringsAsFactors = FALSE)

fig_path <- paste0(PROJECT_PATH, "figures/")

# ============================================================================
# PLOT 1: All ASEAN countries' ideal points over time (overall)
# ============================================================================

asean_df <- df_all %>% filter(is_asean == TRUE)
ref_df   <- df_all %>% filter(is_asean == FALSE)

asean_df <- asean_df %>%
  group_by(country_label) %>%
  mutate(end_label = ifelse(session == max(session), country_label, NA)) %>%
  ungroup()

p1 <- ggplot() +
  geom_line(data = ref_df, aes(x = year, y = IdealPoint, linetype = country_label),
            color = "grey40", linewidth = 0.6) +
  geom_line(data = asean_df, aes(x = year, y = IdealPoint, color = country_label),
            linewidth = 0.7) +
  geom_text_repel(data = asean_df %>% filter(!is.na(end_label)),
                  aes(x = year, y = IdealPoint, label = end_label, color = country_label),
                  nudge_x = 2, size = 3, segment.color = "grey60", show.legend = FALSE) +
  scale_color_manual(values = ASEAN_COLORS) +
  scale_linetype_manual(values = c("United States" = "dashed", "China" = "dotted")) +
  guides(color = "none") +
  labs(title = "ASEAN Member States: UNGA Ideal Points Over Time",
       subtitle = "Dashed = US, Dotted = China (from 1971)",
       x = "Year", y = "Ideal Point Estimate", linetype = "Reference") +
  theme_asean()

ggsave(paste0(fig_path, "01_asean_idealpoints_overall.png"), p1,
       width = 11, height = 6, dpi = 300)
cat("  [1/12] asean_idealpoints_overall.png\n")


# ============================================================================
# PLOT 2: ASEAN mean vs US vs China
# ============================================================================

summ_all <- asean_summ %>% filter(issue == "All")

p2 <- ggplot() +
  geom_ribbon(data = summ_all,
              aes(x = year, ymin = asean_mean - asean_sd, ymax = asean_mean + asean_sd),
              fill = "darkgreen", alpha = 0.15) +
  geom_line(data = summ_all, aes(x = year, y = asean_mean, color = "ASEAN Mean"),
            linewidth = 1.2) +
  geom_line(data = summ_all, aes(x = year, y = us_idealpoint, color = "United States"),
            linewidth = 0.9) +
  geom_line(data = summ_all %>% filter(!is.na(china_idealpoint)),
            aes(x = year, y = china_idealpoint, color = "China"),
            linewidth = 0.9) +
  scale_color_manual(values = c("ASEAN Mean" = "darkgreen",
                                "United States" = "blue",
                                "China" = "red")) +
  labs(title = "ASEAN Mean Ideal Point vs United States and China",
       subtitle = "Shaded region = +/- 1 SD of ASEAN member ideal points",
       x = "Year", y = "Ideal Point Estimate", color = "") +
  theme_asean() +
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "02_asean_mean_vs_us_china.png"), p2,
       width = 10, height = 6, dpi = 300)
cat("  [2/12] asean_mean_vs_us_china.png\n")


# ============================================================================
# PLOT 3: ASEAN mean vs US vs China — faceted by issue area
# ============================================================================

# Build combined data: ASEAN mean + reference countries
asean_mean_long <- asean_summ %>%
  select(session, year, issue, IdealPoint = asean_mean) %>%
  mutate(country_label = "ASEAN Mean")

ref_long <- df_long %>%
  filter(is_asean == FALSE) %>%
  select(session, year, issue, IdealPoint, country_label)

combined <- bind_rows(asean_mean_long, ref_long)

# Factor for ordering facets
issue_order <- c("All", "Human Rights", "Nuclear", "Middle East", "Important Votes", "Non-Nuclear")
combined$issue <- factor(combined$issue, levels = issue_order)

p3 <- ggplot(combined, aes(x = year, y = IdealPoint, color = country_label)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ issue, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("ASEAN Mean" = "darkgreen",
                                "United States" = "blue",
                                "China" = "red")) +
  labs(title = "ASEAN Mean vs US vs China Ideal Points by Issue Area",
       x = "Year", y = "Ideal Point", color = "") +
  theme_asean() +
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "03_asean_mean_vs_refs_by_issue.png"), p3,
       width = 12, height = 10, dpi = 300)
cat("  [3/12] asean_mean_vs_refs_by_issue.png\n")


# ============================================================================
# PLOT 4: Distance from US — all ASEAN countries (overall)
# ============================================================================

dist_all <- asean_dist %>% filter(issue == "All")

p4 <- ggplot(dist_all, aes(x = year, y = dist_us, color = country_label)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_color_manual(values = ASEAN_COLORS) +
  labs(title = "ASEAN Countries: Distance from US in UNGA Voting",
       subtitle = "Absolute difference in ideal point estimates (overall votes)",
       x = "Year", y = "|Ideal Point Distance from US|", color = "Country") +
  theme_asean() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

ggsave(paste0(fig_path, "04_asean_distance_from_us.png"), p4,
       width = 10, height = 6, dpi = 300)
cat("  [4/12] asean_distance_from_us.png\n")


# ============================================================================
# PLOT 5: Distance from China — all ASEAN countries (overall)
# ============================================================================

p5 <- ggplot(dist_all %>% filter(!is.na(dist_china)),
             aes(x = year, y = dist_china, color = country_label)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_color_manual(values = ASEAN_COLORS) +
  labs(title = "ASEAN Countries: Distance from China in UNGA Voting",
       subtitle = "Absolute difference in ideal point estimates (overall votes, from 1971)",
       x = "Year", y = "|Ideal Point Distance from China|", color = "Country") +
  theme_asean() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

ggsave(paste0(fig_path, "05_asean_distance_from_china.png"), p5,
       width = 10, height = 6, dpi = 300)
cat("  [5/12] asean_distance_from_china.png\n")


# ============================================================================
# PLOT 6: Mean ASEAN distance from US vs China over time
# ============================================================================

mean_dist_all <- mean_dist %>% filter(issue == "All")

mean_dist_long <- mean_dist_all %>%
  pivot_longer(cols = c(mean_dist_us, mean_dist_china),
               names_to = "reference", values_to = "distance") %>%
  mutate(reference = recode(reference,
                            "mean_dist_us" = "Distance from US",
                            "mean_dist_china" = "Distance from China"))

p6 <- ggplot(mean_dist_long %>% filter(!is.na(distance)),
             aes(x = year, y = distance, color = reference)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Distance from US" = "blue",
                                "Distance from China" = "red")) +
  labs(title = "Mean ASEAN Distance from US vs China Over Time",
       subtitle = "Average absolute ideal point distance across all ASEAN members (overall votes)",
       x = "Year", y = "Mean |Ideal Point Distance|", color = "") +
  theme_asean() +
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "06_asean_mean_distance_us_vs_china.png"), p6,
       width = 10, height = 6, dpi = 300)
cat("  [6/12] asean_mean_distance_us_vs_china.png\n")


# ============================================================================
# PLOT 7: Intra-ASEAN cohesion (SD) over time — overall
# ============================================================================

cohesion_all <- asean_summ %>% filter(issue == "All")

p7 <- ggplot(cohesion_all, aes(x = year, y = asean_sd)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(aes(size = n_members), color = "darkgreen", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "forestgreen",
              fill = "lightgreen", alpha = 0.3, linewidth = 0.5) +
  scale_size_continuous(range = c(1, 4), breaks = c(3, 5, 7, 10)) +
  labs(title = "Intra-ASEAN Voting Cohesion Over Time",
       subtitle = "Lower SD = more cohesive; point size = number of ASEAN members in session",
       x = "Year", y = "SD of ASEAN Ideal Points", size = "N Members") +
  theme_asean()

ggsave(paste0(fig_path, "07_asean_cohesion.png"), p7,
       width = 10, height = 6, dpi = 300)
cat("  [7/12] asean_cohesion.png\n")


# ============================================================================
# PLOT 8: Intra-ASEAN cohesion by issue area
# ============================================================================

asean_summ$issue <- factor(asean_summ$issue, levels = issue_order)

p8 <- ggplot(asean_summ %>% filter(!is.na(asean_sd)),
             aes(x = year, y = asean_sd, color = issue)) +
  geom_line(linewidth = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", linewidth = 0.4) +
  labs(title = "Intra-ASEAN Voting Cohesion by Issue Area",
       subtitle = "Standard deviation of ASEAN ideal points per session",
       x = "Year", y = "SD of ASEAN Ideal Points", color = "Issue Area") +
  theme_asean() +
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "08_asean_cohesion_by_issue.png"), p8,
       width = 10, height = 6, dpi = 300)
cat("  [8/12] asean_cohesion_by_issue.png\n")


# ============================================================================
# PLOT 9: Heatmap — ASEAN ideal points (country x year)
# ============================================================================

# Order countries by ASEAN joining date
country_order <- c("Thailand", "Philippines", "Indonesia", "Malaysia", "Singapore",
                   "Brunei", "Vietnam", "Laos", "Myanmar", "Cambodia")
asean_df$country_label <- factor(asean_df$country_label, levels = rev(country_order))

p9 <- ggplot(asean_df, aes(x = year, y = country_label, fill = IdealPoint)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0, name = "Ideal\nPoint") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(title = "ASEAN Ideal Points Heatmap (All Votes)",
       subtitle = "Countries ordered by ASEAN accession date; white gaps = no data",
       x = "Year", y = "") +
  theme_asean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(fig_path, "09_asean_heatmap.png"), p9,
       width = 14, height = 5, dpi = 300)
cat("  [9/12] asean_heatmap.png\n")


# ============================================================================
# PLOT 10: Which power is each ASEAN country closer to? (tile chart)
# ============================================================================

closer_data <- asean_dist %>%
  filter(issue == "All", !is.na(closer_to)) %>%
  mutate(country_label = factor(country_label, levels = rev(country_order)))

p10 <- ggplot(closer_data, aes(x = year, y = country_label, fill = closer_to)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("China" = "#FF4444", "United States" = "#4444FF",
                               "Equidistant" = "grey70"),
                    name = "Closer To") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(title = "ASEAN Countries: Closer to US or China in UNGA Voting?",
       subtitle = "Based on absolute ideal point distance (overall votes, from 1971)",
       x = "Year", y = "") +
  theme_asean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(fig_path, "10_asean_closer_to_us_china.png"), p10,
       width = 14, height = 5, dpi = 300)
cat("  [10/12] asean_closer_to_us_china.png\n")


# ============================================================================
# PLOT 11: Correlation heatmap of ASEAN ideal points
# ============================================================================

asean_wide_corr <- asean_df %>%
  select(session, country_label, IdealPoint) %>%
  pivot_wider(names_from = country_label, values_from = IdealPoint)

cor_matrix <- cor(asean_wide_corr %>% select(-session), use = "pairwise.complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Country1 = Var1, Country2 = Var2, Correlation = Freq)

p11 <- ggplot(cor_long, aes(x = Country1, y = Country2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.8) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Pairwise Correlation of ASEAN Ideal Points",
       subtitle = "Across all overlapping sessions (1967-2019)",
       x = "", y = "") +
  theme_asean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(fig_path, "11_asean_correlation_heatmap.png"), p11,
       width = 8, height = 7, dpi = 300)
cat("  [11/12] asean_correlation_heatmap.png\n")


# ============================================================================
# PLOT 12: ASEAN individual countries — faceted by issue area
# ============================================================================

asean_issues <- df_long %>% filter(is_asean == TRUE)
asean_issues$issue <- factor(asean_issues$issue, levels = issue_order)
asean_issues$country_label <- factor(asean_issues$country_label, levels = country_order)

# Expand US + China data so they appear as reference lines in EVERY ASEAN country panel
us_issues <- df_long %>%
  filter(ccode == 2) %>%
  select(session, year, issue, ref_ip = IdealPoint)
china_issues <- df_long %>%
  filter(ccode == 710) %>%
  select(session, year, issue, ref_ip = IdealPoint)

# Cross-join with ASEAN country labels so the reference lines appear in each row
us_expanded <- crossing(country_label = factor(country_order, levels = country_order)) %>%
  cross_join(us_issues)
us_expanded$issue <- factor(us_expanded$issue, levels = issue_order)

china_expanded <- crossing(country_label = factor(country_order, levels = country_order)) %>%
  cross_join(china_issues)
china_expanded$issue <- factor(china_expanded$issue, levels = issue_order)

p12 <- ggplot() +
  geom_line(data = us_expanded,
            aes(x = year, y = ref_ip), color = "blue", linewidth = 0.3,
            alpha = 0.4, linetype = "dashed") +
  geom_line(data = china_expanded,
            aes(x = year, y = ref_ip), color = "red", linewidth = 0.3,
            alpha = 0.4, linetype = "dotted") +
  geom_line(data = asean_issues,
            aes(x = year, y = IdealPoint), color = "darkgreen", linewidth = 0.5) +
  facet_grid(country_label ~ issue, scales = "free_y") +
  labs(title = "ASEAN Individual Countries: Ideal Points by Issue Area",
       subtitle = "Blue dashed = US, Red dotted = China",
       x = "Year", y = "Ideal Point") +
  theme_asean() +
  theme(strip.text.y = element_text(angle = 0, size = 8),
        axis.text = element_text(size = 7))

ggsave(paste0(fig_path, "12_asean_individual_by_issue.png"), p12,
       width = 16, height = 14, dpi = 300)
cat("  [12/12] asean_individual_by_issue.png\n")


cat("\nAll visualizations saved to figures/ directory.\n")
