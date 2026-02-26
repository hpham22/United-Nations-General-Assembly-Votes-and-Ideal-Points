# ============================================================================
# 04_vietnam_divergence.R -- Explaining the Vietnam Divergence Puzzle
#
# Vietnam has NEGATIVE ideal point correlations with ASEAN founding members
# (Fig 11) yet all are closer to China than the US. This script decomposes
# why: correlation measures co-MOVEMENT, not level proximity. Vietnam and
# founders moved in opposite directions across eras even while occupying
# the same side of the ideal point spectrum.
# ============================================================================

source("scripts/helpers.R")
library(zoo)

cat("Generating Vietnam divergence analysis...\n")

# === Load data ===
df_all  <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_all.csv"),
                    stringsAsFactors = FALSE)
df_long <- read.csv(paste0(PROJECT_PATH, "data/asean_idealpoints_by_issue.csv"),
                    stringsAsFactors = FALSE)

fig_path <- paste0(PROJECT_PATH, "figures/")

# === Constants ===
FOUNDER_CODES <- c(800, 840, 850, 820, 830)
FOUNDER_NAMES <- c("Thailand", "Philippines", "Indonesia", "Malaysia", "Singapore")
VIETNAM_CODE  <- 816
CHINA_CODE    <- 710
US_CODE       <- 2

COLD_WAR_END  <- 1991
VIETNAM_ASEAN <- 1995
VIETNAM_START <- 1977
DATA_END      <- 2019

# ============================================================================
# DERIVED DATA
# ============================================================================

# Vietnam ideal points (issue = "All")
vietnam_ts <- df_all %>%
  filter(ccode == VIETNAM_CODE) %>%
  select(year, vietnam_ip = IdealPoint)

# Founding members' mean per year
founder_mean_ts <- df_all %>%
  filter(ccode %in% FOUNDER_CODES) %>%
  group_by(year) %>%
  summarise(founder_mean_ip = mean(IdealPoint, na.rm = TRUE),
            founder_sd = sd(IdealPoint, na.rm = TRUE),
            n_founders = n(),
            .groups = "drop")

# China ideal points
china_ts <- df_all %>%
  filter(ccode == CHINA_CODE) %>%
  select(year, china_ip = IdealPoint)

# US ideal points
us_ts <- df_all %>%
  filter(ccode == US_CODE) %>%
  select(year, us_ip = IdealPoint)

# Master time series
master_ts <- founder_mean_ts %>%
  left_join(vietnam_ts, by = "year") %>%
  left_join(china_ts, by = "year") %>%
  left_join(us_ts, by = "year") %>%
  arrange(year) %>%
  mutate(
    delta_vietnam  = vietnam_ip - lag(vietnam_ip),
    delta_founders = founder_mean_ip - lag(founder_mean_ip),
    delta_china    = china_ip - lag(china_ip)
  )

# Save derived data
write.csv(master_ts,
          paste0(PROJECT_PATH, "data/vietnam_divergence_ts.csv"),
          row.names = FALSE)
cat("  Saved data/vietnam_divergence_ts.csv\n")

# Wide format for correlations: one column per country
founders_wide <- df_all %>%
  filter(ccode %in% c(FOUNDER_CODES, VIETNAM_CODE, CHINA_CODE)) %>%
  select(year, country_label, IdealPoint) %>%
  pivot_wider(names_from = country_label, values_from = IdealPoint) %>%
  arrange(year)


# ============================================================================
# PLOT 13: Trajectory Comparison — Vietnam vs Founding Mean vs China
# ============================================================================

plot_data <- master_ts %>% filter(year >= VIETNAM_START)

# Individual founding members for background context
founders_individual <- df_all %>%
  filter(ccode %in% FOUNDER_CODES, year >= VIETNAM_START)

p13 <- ggplot() +
  # Cold War era shading
  annotate("rect", xmin = VIETNAM_START, xmax = COLD_WAR_END,
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  annotate("text", x = 1984, y = 2.5, label = "Cold War Era",
           fontface = "italic", size = 3.5, color = "grey50") +
  # Founding members ribbon (SD band)
  geom_ribbon(data = plot_data,
              aes(x = year,
                  ymin = founder_mean_ip - founder_sd,
                  ymax = founder_mean_ip + founder_sd),
              fill = "darkgreen", alpha = 0.12) +
  # Individual founders as thin grey lines
  geom_line(data = founders_individual,
            aes(x = year, y = IdealPoint, group = country_label),
            color = "grey70", linewidth = 0.3, alpha = 0.6) +
  # US reference line
  geom_line(data = plot_data,
            aes(x = year, y = us_ip, linetype = "United States"),
            color = "grey50", linewidth = 0.5) +
  # China
  geom_line(data = plot_data %>% filter(!is.na(china_ip)),
            aes(x = year, y = china_ip, color = "China"),
            linewidth = 0.9) +
  # Founding mean
  geom_line(data = plot_data,
            aes(x = year, y = founder_mean_ip, color = "Founding ASEAN Mean"),
            linewidth = 0.9) +
  # Vietnam
  geom_line(data = plot_data,
            aes(x = year, y = vietnam_ip, color = "Vietnam"),
            linewidth = 1.1) +
  # Vietnam joins ASEAN marker
  geom_vline(xintercept = VIETNAM_ASEAN, linetype = "longdash",
             color = "#FF7F00", alpha = 0.6) +
  annotate("text", x = VIETNAM_ASEAN + 0.5, y = 2.3,
           label = "Vietnam joins\nASEAN (1995)",
           hjust = 0, size = 3, color = "#FF7F00") +
  scale_color_manual(values = c("Vietnam" = "#FF7F00",
                                "Founding ASEAN Mean" = "darkgreen",
                                "China" = "#FF0000")) +
  scale_linetype_manual(values = c("United States" = "dashed")) +
  labs(title = "The Vietnam Divergence: Same Side, Different Trajectories",
       subtitle = "Vietnam and founders are both closer to China than the US, but their year-to-year movements diverge",
       x = "Year", y = "Ideal Point Estimate",
       color = "", linetype = "Reference") +
  theme_asean() +
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "13_vietnam_trajectory_comparison.png"), p13,
       width = 11, height = 7, dpi = 300)
cat("  [13] vietnam_trajectory_comparison.png\n")


# ============================================================================
# PLOT 14: Rolling 10-Year Correlations
# ============================================================================

window_size <- 10

# Filter to Vietnam's data range
fw <- founders_wide %>% filter(year >= VIETNAM_START)

# Manual rolling correlation function
rolling_cor <- function(x, y, width) {
  n <- length(x)
  result <- rep(NA_real_, n)
  for (i in width:n) {
    idx <- (i - width + 1):i
    xi <- x[idx]; yi <- y[idx]
    valid <- !is.na(xi) & !is.na(yi)
    if (sum(valid) >= 5) {
      result[i] <- cor(xi[valid], yi[valid])
    }
  }
  result
}

# Compute rolling correlations for each partner vs Vietnam
partners <- c(FOUNDER_NAMES, "China")
partners <- partners[partners %in% names(fw)]

rolling_results <- tibble(year = fw$year)
for (partner in partners) {
  rolling_results[[partner]] <- rolling_cor(fw$Vietnam, fw[[partner]], window_size)
}

rolling_long <- rolling_results %>%
  pivot_longer(-year, names_to = "partner", values_to = "correlation") %>%
  filter(!is.na(correlation))

# Colors: founders get ASEAN_COLORS, China gets red
partner_colors <- c(ASEAN_COLORS[FOUNDER_NAMES], "China" = "#FF0000")

p14 <- ggplot(rolling_long, aes(x = year, y = correlation, color = partner)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey60", linewidth = 0.4) +
  geom_line(aes(linewidth = ifelse(partner == "China", "bold", "normal")),
            alpha = 0.85) +
  scale_linewidth_manual(values = c("bold" = 1.2, "normal" = 0.7), guide = "none") +
  geom_vline(xintercept = COLD_WAR_END, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = VIETNAM_ASEAN, linetype = "longdash",
             color = "#FF7F00", alpha = 0.5) +
  annotate("text", x = COLD_WAR_END - 0.5, y = -0.85,
           label = "End of\nCold War", hjust = 1, size = 2.8, color = "grey40") +
  annotate("text", x = VIETNAM_ASEAN + 0.5, y = -0.85,
           label = "Vietnam\njoins ASEAN", hjust = 0, size = 2.8, color = "#FF7F00") +
  scale_color_manual(values = partner_colors) +
  labs(title = "Rolling 10-Year Correlation: Vietnam vs Each Founding Member and China",
       subtitle = paste0("Window = ", window_size,
                         " years; shows when negative correlations intensify or reverse"),
       x = "Year (end of window)", y = "Pearson Correlation",
       color = "Partner Country") +
  theme_asean() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

ggsave(paste0(fig_path, "14_vietnam_rolling_correlations.png"), p14,
       width = 11, height = 6, dpi = 300)
cat("  [14] vietnam_rolling_correlations.png\n")


# ============================================================================
# PLOT 15: Era-Specific Correlation Heatmaps (Cold War vs Post-Cold War)
# ============================================================================

target_codes <- c(FOUNDER_CODES, VIETNAM_CODE, CHINA_CODE)
col_order <- c(FOUNDER_NAMES, "Vietnam", "China")

build_cor_matrix <- function(data, year_min, year_max) {
  wide <- data %>%
    filter(ccode %in% target_codes, year >= year_min, year <= year_max) %>%
    select(year, country_label, IdealPoint) %>%
    pivot_wider(names_from = country_label, values_from = IdealPoint)

  cols <- col_order[col_order %in% names(wide)]
  cor(wide[, cols], use = "pairwise.complete.obs")
}

cor_cold <- build_cor_matrix(df_all, VIETNAM_START, COLD_WAR_END - 1)
cor_post <- build_cor_matrix(df_all, COLD_WAR_END, DATA_END)

cor_to_long <- function(mat, era_label) {
  df <- as.data.frame(as.table(mat))
  names(df) <- c("Country1", "Country2", "Correlation")
  df$era <- era_label
  # Maintain factor order
  df$Country1 <- factor(df$Country1, levels = col_order)
  df$Country2 <- factor(df$Country2, levels = col_order)
  df
}

cor_cold_long <- cor_to_long(cor_cold,
                             paste0("Cold War (", VIETNAM_START, " -", COLD_WAR_END - 1, ")"))
cor_post_long <- cor_to_long(cor_post,
                             paste0("Post-Cold War (", COLD_WAR_END, " -", DATA_END, ")"))

make_heatmap <- function(cor_data, title_text) {
  ggplot(cor_data, aes(x = Country1, y = Country2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.8) +
    scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = title_text, x = "", y = "") +
    theme_asean() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}

h1 <- make_heatmap(cor_cold_long,
                   paste0("Cold War (", VIETNAM_START, " -", COLD_WAR_END - 1, ")"))
h2 <- make_heatmap(cor_post_long,
                   paste0("Post-Cold War (", COLD_WAR_END, " -", DATA_END, ")"))

p15 <- (h1 | h2) +
  plot_annotation(
    title = "How Correlations Shifted: Cold War vs Post-Cold War",
    subtitle = "Vietnam's correlations with founding ASEAN members and China differ sharply between eras",
    theme = theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 11, color = "grey30"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "15_vietnam_era_correlation_heatmaps.png"), p15,
       width = 14, height = 7, dpi = 300)
cat("  [15] vietnam_era_correlation_heatmaps.png\n")


# ============================================================================
# PLOT 16: Year-over-Year Changes (Delta Scatter)
# ============================================================================

delta_data <- master_ts %>%
  filter(!is.na(delta_vietnam), year >= VIETNAM_START + 1)

# Panel A: Vietnam delta vs Founding Mean delta
delta_founders <- delta_data %>% filter(!is.na(delta_founders))
cor_vf <- cor(delta_founders$delta_vietnam, delta_founders$delta_founders,
              use = "complete.obs")

scatter_a <- ggplot(delta_founders, aes(x = delta_founders, y = delta_vietnam)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "#FFE0B2", alpha = 0.3) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = "#FFE0B2", alpha = 0.3) +
  geom_point(aes(color = year < COLD_WAR_END), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.5,
              linetype = "dashed") +
  scale_color_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#1976D2"),
                     labels = c("TRUE" = "Cold War", "FALSE" = "Post-Cold War"),
                     name = "Era") +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = paste0("r = ", round(cor_vf, 3)),
           size = 4, fontface = "bold", color = "grey30") +
  labs(title = "Vietnam vs Founding Members",
       subtitle = "Shaded quadrants = anti-correlated movement",
       x = expression(Delta ~ "Founding ASEAN Mean"),
       y = expression(Delta ~ "Vietnam")) +
  theme_asean() +
  theme(legend.position = "bottom")

# Panel B: Vietnam delta vs China delta
delta_china <- delta_data %>% filter(!is.na(delta_china))
cor_vc <- cor(delta_china$delta_vietnam, delta_china$delta_china,
              use = "complete.obs")

scatter_b <- ggplot(delta_china, aes(x = delta_china, y = delta_vietnam)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#C8E6C9", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#C8E6C9", alpha = 0.3) +
  geom_point(aes(color = year < COLD_WAR_END), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.5,
              linetype = "dashed") +
  scale_color_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#1976D2"),
                     labels = c("TRUE" = "Cold War", "FALSE" = "Post-Cold War"),
                     name = "Era") +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = paste0("r = ", round(cor_vc, 3)),
           size = 4, fontface = "bold", color = "grey30") +
  labs(title = "Vietnam vs China",
       subtitle = "Shaded quadrants = co-movement",
       x = expression(Delta ~ "China"),
       y = expression(Delta ~ "Vietnam")) +
  theme_asean() +
  theme(legend.position = "bottom")

p16 <- (scatter_a | scatter_b) +
  plot_annotation(
    title = "Year-over-Year Ideal Point Changes: Anti-Correlated with Founders, Correlated with China?",
    subtitle = "Each point = one year; orange shading = opposite-direction movements, green = same-direction",
    theme = theme(plot.title = element_text(size = 13, face = "bold"),
                  plot.subtitle = element_text(size = 10, color = "grey30"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "16_vietnam_delta_scatter.png"), p16,
       width = 12, height = 6, dpi = 300)
cat("  [16] vietnam_delta_scatter.png\n")


# ============================================================================
# PLOT 17: Issue Decomposition — Vietnam's Correlations by Issue Area
# ============================================================================

issue_order <- c("All", "Human Rights", "Nuclear", "Middle East",
                 "Important Votes", "Non-Nuclear")

# Vietnam-Founders correlation per issue
issue_corr_founders <- df_long %>%
  filter(ccode %in% c(FOUNDER_CODES, VIETNAM_CODE)) %>%
  group_by(year, issue) %>%
  summarise(
    vietnam_ip = IdealPoint[ccode == VIETNAM_CODE][1],
    founder_mean = mean(IdealPoint[ccode %in% FOUNDER_CODES], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(vietnam_ip), !is.na(founder_mean)) %>%
  group_by(issue) %>%
  summarise(
    correlation = cor(vietnam_ip, founder_mean, use = "complete.obs"),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(pair = "Vietnam vs Founding Mean")

# Vietnam-China correlation per issue
issue_corr_china <- df_long %>%
  filter(ccode %in% c(CHINA_CODE, VIETNAM_CODE)) %>%
  select(year, issue, ccode, IdealPoint) %>%
  pivot_wider(names_from = ccode, values_from = IdealPoint,
              names_prefix = "c") %>%
  filter(!is.na(c816), !is.na(c710)) %>%
  group_by(issue) %>%
  summarise(
    correlation = cor(c816, c710, use = "complete.obs"),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(pair = "Vietnam vs China")

issue_corr_combined <- bind_rows(
  issue_corr_founders %>% select(issue, correlation, n_years, pair),
  issue_corr_china %>% select(issue, correlation, n_years, pair)
)
issue_corr_combined$issue <- factor(issue_corr_combined$issue, levels = issue_order)

p17 <- ggplot(issue_corr_combined,
              aes(x = issue, y = correlation, fill = pair)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", correlation),
                vjust = ifelse(correlation >= 0, -0.5, 1.5)),
            position = position_dodge(width = 0.7), size = 3.2) +
  scale_fill_manual(values = c("Vietnam vs Founding Mean" = "darkgreen",
                               "Vietnam vs China" = "#FF0000")) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Vietnam Correlations by Issue Area",
       subtitle = "Which issue domains drive the negative correlation with founding members?",
       x = "Issue Area", y = "Pearson Correlation", fill = "") +
  theme_asean() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(paste0(fig_path, "17_vietnam_issue_decomposition.png"), p17,
       width = 10, height = 6, dpi = 300)
cat("  [17] vietnam_issue_decomposition.png\n")


# ============================================================================
# PLOT 18: The Distance-Correlation Paradox
# ============================================================================

# Panel A: Distance over time
dist_panel <- master_ts %>%
  filter(year >= VIETNAM_START) %>%
  mutate(
    dist_vietnam_founders = abs(vietnam_ip - founder_mean_ip),
    dist_vietnam_china = abs(vietnam_ip - china_ip)
  ) %>%
  select(year, dist_vietnam_founders, dist_vietnam_china) %>%
  pivot_longer(-year, names_to = "pair", values_to = "distance") %>%
  mutate(pair = recode(pair,
    "dist_vietnam_founders" = "Vietnam - Founding Mean",
    "dist_vietnam_china" = "Vietnam - China"
  )) %>%
  filter(!is.na(distance))

panel_a <- ggplot(dist_panel, aes(x = year, y = distance, color = pair)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Vietnam - Founding Mean" = "darkgreen",
                                "Vietnam - China" = "#FF0000")) +
  labs(title = "Level Proximity (Distance)",
       subtitle = "Vietnam is often closer to China than to founding members",
       x = "Year", y = "|Ideal Point Distance|", color = "") +
  theme_asean() +
  theme(legend.position = "bottom")

# Panel B: Correlation summary by era
full <- master_ts %>% filter(year >= VIETNAM_START)
cold <- master_ts %>% filter(year >= VIETNAM_START, year < COLD_WAR_END)
post <- master_ts %>% filter(year >= COLD_WAR_END)

era_labels <- c(
  paste0("Full Period\n(", VIETNAM_START, " -", DATA_END, ")"),
  paste0("Cold War\n(", VIETNAM_START, " -", COLD_WAR_END - 1, ")"),
  paste0("Post-Cold War\n(", COLD_WAR_END, " -", DATA_END, ")")
)

corr_summary <- tibble(
  pair = rep(c("Vietnam -Founders", "Vietnam -China"), each = 3),
  era = rep(era_labels, 2),
  correlation = c(
    cor(full$vietnam_ip, full$founder_mean_ip, use = "complete.obs"),
    cor(cold$vietnam_ip, cold$founder_mean_ip, use = "complete.obs"),
    cor(post$vietnam_ip, post$founder_mean_ip, use = "complete.obs"),
    cor(full$vietnam_ip, full$china_ip, use = "complete.obs"),
    cor(cold$vietnam_ip, cold$china_ip, use = "complete.obs"),
    cor(post$vietnam_ip, post$china_ip, use = "complete.obs")
  )
)
corr_summary$era <- factor(corr_summary$era, levels = era_labels)

panel_b <- ggplot(corr_summary, aes(x = era, y = correlation, fill = pair)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", correlation),
                vjust = ifelse(correlation >= 0, -0.5, 1.5)),
            position = position_dodge(width = 0.7), size = 3.5) +
  scale_fill_manual(values = c("Vietnam -Founders" = "darkgreen",
                               "Vietnam -China" = "#FF0000")) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Temporal Co-Movement (Correlation)",
       subtitle = "Anti-correlated with founders, correlated with China",
       x = "", y = "Pearson Correlation", fill = "") +
  theme_asean() +
  theme(legend.position = "bottom")

p18 <- (panel_a | panel_b) +
  plot_annotation(
    title = "The Distance -Correlation Paradox: Proximity Does Not Imply Co-Movement",
    subtitle = "Vietnam is close to all in ideal-point space (left) but anti-correlated with founders over time (right)",
    theme = theme(plot.title = element_text(size = 13, face = "bold"),
                  plot.subtitle = element_text(size = 10, color = "grey30"))
  ) +
  plot_layout(widths = c(1.2, 1), guides = "collect") &
  theme(legend.position = "bottom")

ggsave(paste0(fig_path, "18_vietnam_distance_paradox.png"), p18,
       width = 14, height = 7, dpi = 300)
cat("  [18] vietnam_distance_paradox.png\n")


cat("\nVietnam divergence analysis complete. Figures 13-18 saved.\n")
cat("Derived data saved to data/vietnam_divergence_ts.csv\n")
