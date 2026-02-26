# ============================================================================
# 01_extract_data.R -- Extract and consolidate ASEAN + US + China ideal points
# ============================================================================

source("/home/user/ASEAN-UNGA-Voting-Analysis/scripts/helpers.R")

cat("Loading ideal point data from source repository...\n")

# Load each issue-specific dataset
df_all <- load_and_filter("IdealpointestimatesAll_Apr2020.csv", "All")
df_hr  <- load_and_filter("IdealpointestimatesHumanRights_Apr2020.csv", "Human Rights")
df_nu  <- load_and_filter("IdealpointestimatesNuclear_Apr2020.csv", "Nuclear")
df_me  <- load_and_filter("IdealpointestimatesMiddleEast_Apr2020.csv", "Middle East")
df_imp <- load_and_filter("IdealpointestimatesImportant_Apr2020.csv", "Important Votes")
df_nn  <- load_and_filter("IdealpointestimatesNoNukes_Apr2020.csv", "Non-Nuclear")

# Filter to sessions >= ASEAN founding (session 22 = 1967)
df_all <- df_all %>% filter(session >= ASEAN_START_SESSION)
df_hr  <- df_hr  %>% filter(session >= ASEAN_START_SESSION)
df_nu  <- df_nu  %>% filter(session >= ASEAN_START_SESSION)
df_me  <- df_me  %>% filter(session >= ASEAN_START_SESSION)
df_imp <- df_imp %>% filter(session >= ASEAN_START_SESSION)
df_nn  <- df_nn  %>% filter(session >= ASEAN_START_SESSION)

# Combine all into long format
df_long <- bind_rows(df_all, df_hr, df_nu, df_me, df_imp, df_nn)

# Create wide format (one row per country-session, columns per issue)
df_wide <- df_all %>%
  select(ccode, session, year, country_label, is_asean,
         IdealPoint_All = IdealPoint, NVotes_All = NVotes) %>%
  left_join(df_hr  %>% select(ccode, session, IdealPoint_HR = IdealPoint, NVotes_HR = NVotes),
            by = c("ccode", "session")) %>%
  left_join(df_nu  %>% select(ccode, session, IdealPoint_Nuclear = IdealPoint, NVotes_Nuclear = NVotes),
            by = c("ccode", "session")) %>%
  left_join(df_me  %>% select(ccode, session, IdealPoint_ME = IdealPoint, NVotes_ME = NVotes),
            by = c("ccode", "session")) %>%
  left_join(df_imp %>% select(ccode, session, IdealPoint_Important = IdealPoint, NVotes_Important = NVotes),
            by = c("ccode", "session")) %>%
  left_join(df_nn  %>% select(ccode, session, IdealPoint_NoNukes = IdealPoint, NVotes_NoNukes = NVotes),
            by = c("ccode", "session"))

# Save outputs
write.csv(df_all,  paste0(PROJECT_PATH, "data/asean_idealpoints_all.csv"), row.names = FALSE)
write.csv(df_long, paste0(PROJECT_PATH, "data/asean_idealpoints_by_issue.csv"), row.names = FALSE)
write.csv(df_wide, paste0(PROJECT_PATH, "data/asean_idealpoints_wide.csv"), row.names = FALSE)

cat("Data extraction complete.\n")
cat("  Overall (long):   ", nrow(df_all), "rows\n")
cat("  All issues (long):", nrow(df_long), "rows\n")
cat("  Wide format:      ", nrow(df_wide), "rows\n")
cat("  Countries: ", paste(unique(df_all$country_label), collapse = ", "), "\n")
cat("  Sessions:  ", min(df_all$session), "-", max(df_all$session), "\n")
