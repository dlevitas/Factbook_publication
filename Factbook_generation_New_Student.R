params <- list()

## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------

past_years <- seq(year - 4, year, 1)


## ----include = FALSE------------------------------------------------------------------------------------------------------------------------------

# 1). Import data
admissions_data_filename <- "sdm_application_mv_modified.csv"
admissions_file <- paste0(data_dir, admissions_data_filename)
admissions_df_orig <- read.csv(admissions_file, header = TRUE)

# 2). Ensure specific columns are factors and deal with any potential typos
admissions_df_orig <- admissions_df_orig %>%
  mutate(
    `Fall semester` = floor(as.numeric(admissions_df_orig$snapshot_term_code_ipeds_modified) / 10),
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("M", "F"), labels = c("Male", "Female")),
    snapshot_term_ipeds_modified = factor(snapshot_term_ipeds_modified),
    student_age = as.numeric(student_age),
    career = factor(career, levels = c("Undergraduate", "Graduate")),
    stdnt_race_ipeds = factor(stdnt_race_ipeds),
    student_age_group = cut(
      student_age,
      breaks = c(-Inf, 17, 19, 21, 24, 29, 34, 39, 49, 64, Inf),  
      labels = c("Under 18", "18-19", "20-21", "22-24", "25-29", "30-34", "35-39", "40-49", "50-64", "65+"),
      right = TRUE  
    ),
    new_student_group = factor(
      new_student_group, levels = c(
        "First-Time Degree-Seeking Bachelor's",
        "Transfer",
        "Master's",
        "Doctorate",
        "Law"
      )
    )
  )

# 3). Create two separate data frames to be used for the Factbook tables and charts: admissions_df_trends and admissions_df
# 3a). admisisons_df_trends
admissions_df_trends <- admissions_df_orig %>%
  filter(
    substr(snapshot_term_ipeds_modified, 1,4) == "Fall", # Only care about Fall terms for the FactBook
    `Fall semester` %in% past_years
  )


## ----include = FALSE------------------------------------------------------------------------------------------------------------------------------

counter <- 0
table_counter <- 0
figure_counter <- 0

# 1). New student groups
for (g in sort(unique(admissions_df_trends$new_student_group))) {
  print(paste0("Creating ", g, " visualizations"))
  counter <- counter + 1
  
  first_word_match <- tolower(str_match(g, "([\\w-]+)")[,2]) %>%
    str_extract("^[^\\-'\\s]+") # Stop at -, space, but not '
  
  col_matcher <- paste0("new_", first_word_match)
  
  group_df <- admissions_df_trends %>%
  filter(
    new_student_group == g
  ) %>%
  mutate(across(starts_with(col_matcher), ~ifelse(. == "Y", 1, 0)))
    
  relevant_cols <- grep(col_matcher, names(group_df), value = TRUE)
  
  group_summary_df <- group_df  %>%
    group_by(`Fall semester`) %>%
    summarize(
      Applied = sum(!!sym(relevant_cols[2])),
      Admitted = sum(!!sym(relevant_cols[1])),
      `Admitted vs Applied (%)` = round((Admitted / Applied) * 100, 1),
      Enrolled = sum(!!sym(relevant_cols[3])),
      `Enrolled vs Admitted (%)` = round((Enrolled / Admitted) * 100, 1)
    ) %>%
    mutate(`Fall semester` = as.character(`Fall semester`)) %>%  # Ensure is character before binding
    bind_rows(
      summarize(
        ., 
        `Fall semester` = "All",
        Applied = sum(Applied),
        Admitted = sum(Admitted),
        `Admitted vs Applied (%)` = round((sum(Admitted) / sum(Applied)) * 100, 1),
        Enrolled = sum(Enrolled),
        `Enrolled vs Admitted (%)` = round((sum(Enrolled) / sum(Admitted)) * 100, 1)
      )
    ) %>%
    as.data.frame()
  
  group_summary_df$Applied <- fmt(group_summary_df$Applied)
  group_summary_df$Admitted <- fmt(group_summary_df$Admitted)
  group_summary_df$Enrolled <- fmt(group_summary_df$Enrolled)
  
  time_var <- colnames(group_summary_df)[1]
  cols <- colnames(group_summary_df)[2:ncol(group_summary_df)]
  context = ""
  
  # 1a). Create and save tables
  table_counter <- table_counter + 1
  table <- generate_table_admissions(group_summary_df, time_var, cols, context)
  html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_", g, ".html")
  save_table(table, html_file)
  
  # 1b). Create and save figures
  counter <- counter + 1
  figure_counter <- figure_counter + 1
  figure <- generate_line_ggplots_admissions(group_df, "Fall semester", relevant_cols[2], relevant_cols[1], relevant_cols[3], g)
  output_file_svg <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_", g, ".svg")
  output_file_html <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_", g, ".html")
  save_figure(figure, output_file_svg, output_file_html)
}


# 2). New first-time degree-seeking Bachelor's (NFTB) SAT scores
print("Creating SAT table")
NFTB_df <- admissions_df_trends %>%
  filter(
    new_student_group == "First-Time Degree-Seeking Bachelor's",
    new_firstTime_degreeSeeking_bachelors_enroll_flag == "Y"
  ) %>%
  mutate(across(starts_with("new_firstTime"), ~ifelse(. == "Y", 1, 0)))

SAT_summary_df <- NFTB_df %>%
  group_by(`Fall semester`) %>%
  summarize(
    `Students with Unreported SAT` = sum(is.na(sat_super2_ipeds))  # Count missing SAT scores before filtering
  ) %>%
  left_join(  # Merge with filtered data set containing SAT-specific stats
    NFTB_df %>%
      filter(test_decision_indicator == "SAT", !is.na(acceptance_status)) %>%
      group_by(`Fall semester`) %>%
      summarize(
        `Avg Math` = round(mean(sat_math_ipeds, na.rm = TRUE)),
        `Avg Read` = round(mean(sat_read_ipeds, na.rm = TRUE)),
        `Avg Combined` = round(mean(sat_super2_ipeds, na.rm = TRUE)),
        `25th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.25, na.rm = TRUE)),
        `50th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.50, na.rm = TRUE)),
        `75th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.75, na.rm = TRUE)),
        `Students where SAT score used in admissions decision` = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)
      ),
    by = "Fall semester"
  ) %>%
  mutate(`Fall semester` = as.character(`Fall semester`)) %>%  # Ensure is character before binding
  bind_rows(
    NFTB_df %>%  # Refer to the original data frame NFTB_df for accurate calculations
      summarize(
        `Fall semester` = "Total",
        `Students with Unreported SAT` = sum(is.na(sat_super2_ipeds)),
        `Avg Math` = round(mean(sat_math_ipeds, na.rm = TRUE)),
        `Avg Read` = round(mean(sat_read_ipeds, na.rm = TRUE)),
        `Avg Combined` = round(mean(sat_super2_ipeds, na.rm = TRUE)),
        `25th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.25, na.rm = TRUE)),
        `50th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.50, na.rm = TRUE)),
        `75th percentile (Combined)` = round(quantile(sat_super2_ipeds, 0.75, na.rm = TRUE)),
      `Students where SAT score used in admissions decision` = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag[test_decision_indicator == "SAT"])
    )
  ) %>%
  relocate(`Students with Unreported SAT`, .before = `Students where SAT score used in admissions decision`) %>% # Move Unreported to 2nd last column
  as.data.frame()

SAT_summary_df$`Students with Unreported SAT` <- fmt(SAT_summary_df$`Students with Unreported SAT`)
SAT_summary_df$`Students where SAT score used in admissions decision` <- fmt(SAT_summary_df$`Students where SAT score used in admissions decision`)

time_var <- colnames(SAT_summary_df )[1]
cols <- colnames(SAT_summary_df )[2:ncol(SAT_summary_df )]
counter <- counter + 1
context = "SAT"

# 2a). Create and save table
table_counter <- table_counter + 1
table <- generate_table_admissions(SAT_summary_df , time_var, cols, context)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_SAT.html")
save_table(table, html_file)


# 3). New first-time degree-seeking Bachelor's (NFTB) ACT scores
print("Creating ACT table")
ACT_summary_df <- NFTB_df %>%
  group_by(`Fall semester`) %>%
  summarize(
    `Students with Unreported ACT` = sum(is.na(act_composite_ipeds))  # Count missing ACT scores before filtering
  ) %>%
  left_join(  # Merge with filtered data set containing ACT-specific stats
    NFTB_df %>%
      filter(test_decision_indicator == "ACT", !is.na(acceptance_status)) %>%
      group_by(`Fall semester`) %>%
      summarize(
        `Avg Math` = round(mean(act_math, na.rm = TRUE)),
        `Avg English` = round(mean(act_english, na.rm = TRUE)),
        `Avg Composite` = round(mean(act_composite_ipeds, na.rm = TRUE), 1),
        `25th percentile (Composite)` = round(quantile(act_composite_ipeds, 0.25, na.rm = TRUE)),
        `50th percentile (Composite)` = round(quantile(act_composite_ipeds, 0.50, na.rm = TRUE)),
        `75th percentile (Composite)` = round(quantile(act_composite_ipeds, 0.75, na.rm = TRUE)),
        `Students where ACT score used in admissions decision` = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)
      ),
    by = "Fall semester"
  ) %>%
  mutate(`Fall semester` = as.character(`Fall semester`)) %>%
  bind_rows(
    NFTB_df %>%
      summarize(
        `Fall semester` = "Total",
        `Students with Unreported ACT` = sum(is.na(act_composite_ipeds)),  # Total unreported count across all semesters
        `Avg Math` = round(mean(act_math, na.rm = TRUE)),
        `Avg English` = round(mean(act_english, na.rm = TRUE)),
        `Avg Composite` = round(mean(act_composite_ipeds, na.rm = TRUE), 1),
        `25th percentile (Composite)` = round(quantile(act_composite_ipeds[test_decision_indicator == "ACT"], 0.25, na.rm = TRUE)),
        `50th percentile (Composite)` = round(quantile(act_composite_ipeds[test_decision_indicator == "ACT"], 0.50, na.rm = TRUE)),
        `75th percentile (Composite)` = round(quantile(act_composite_ipeds[test_decision_indicator == "ACT"], 0.75, na.rm = TRUE)),
        `Students where ACT score used in admissions decision` = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag[test_decision_indicator == "ACT"])
      )
  ) %>%
  relocate(`Students with Unreported ACT`, .before = `Students where ACT score used in admissions decision`) %>%  # Move Unreported to 2nd last column
  as.data.frame()

ACT_summary_df$`Students with Unreported ACT` <- fmt(ACT_summary_df$`Students with Unreported ACT`)
ACT_summary_df$`Students where ACT score used in admissions decision` <- fmt(ACT_summary_df$`Students where ACT score used in admissions decision`)

time_var <- colnames(ACT_summary_df )[1]
cols <- colnames(ACT_summary_df )[2:ncol(ACT_summary_df )]
counter <- counter + 1
context = "ACT"

# 3a). Create and save table
table_counter <- table_counter + 1
table <- generate_table_admissions(ACT_summary_df , time_var, cols, context)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_ACT.html")
save_table(table, html_file)


# 4 NFTB high school GPAs
print("Creating HS GPA table")
GPA_summary_df <- NFTB_df %>%
  group_by(`Fall semester`) %>%
  summarize(
    `Avg HS GPA` = round((mean(weighted_hs_gpa, na.rm = TRUE)), 2),
    `GPA < 2.0` = sum(weighted_hs_gpa < 2.0, na.rm = TRUE),
    `GPA 2.0 - 2.9` = sum(weighted_hs_gpa >= 2.0 & weighted_hs_gpa < 3.0, na.rm = TRUE),
    `GPA >= 3.0` = sum(weighted_hs_gpa >= 3.0, na.rm = TRUE),
    `GPAs reported (%)` = round((sum(!is.na(weighted_hs_gpa)) / sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)) * 100, 1),
    Students = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)
  ) %>%
  mutate(`Fall semester` = as.character(`Fall semester`)) %>%  # Ensure is character before binding
  bind_rows(
    summarize(
      NFTB_df, # Refer to NFTB_df for accurate calculations
      `Fall semester` = "All",
      `Avg HS GPA` = round((mean(weighted_hs_gpa, na.rm = TRUE)), 2),
      `GPA < 2.0` = sum(weighted_hs_gpa < 2.0, na.rm = TRUE),
      `GPA 2.0 - 2.9` = sum(weighted_hs_gpa >= 2.0 & weighted_hs_gpa < 3.0, na.rm = TRUE),
      `GPA >= 3.0` = sum(weighted_hs_gpa >= 3.0, na.rm = TRUE),
      `GPAs reported (%)` = round((sum(!is.na(weighted_hs_gpa), na.rm = TRUE) / sum(NFTB_df$new_firstTime_degreeSeeking_bachelors_enroll_flag, na.rm = TRUE)) * 100, 1),
      Students = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag, na.rm = TRUE)
    )
  ) %>%
  as.data.frame()

GPA_summary_df$Students <- fmt(GPA_summary_df$Students)

time_var <- colnames(GPA_summary_df)[1]
cols <- colnames(GPA_summary_df)[2:ncol(GPA_summary_df)]
counter <- counter + 1
context <- ""

# 4a). Create and save table
table_counter <- table_counter + 1
table <- generate_table_admissions(GPA_summary_df , time_var, cols, context)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_HS GPA.html")
save_table(table, html_file)


# 5 High school class rank percentile
print("Creating HS Percentile table")
HS_rank_summary_df <- NFTB_df %>%
  group_by(`Fall semester`) %>%
  summarize(
    `10th percentile` = quantile(hs_percentile, 0.10, na.rm = TRUE),
    `25th percentile` = quantile(hs_percentile, 0.25, na.rm = TRUE),
    `50th percentile` = quantile(hs_percentile, 0.50, na.rm = TRUE),
    `75th percentile` = quantile(hs_percentile, 0.75, na.rm = TRUE),
    Unreported = sum(is.na(hs_percentile)),
    Students = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)
  ) %>%
  mutate(`Fall semester` = as.character(`Fall semester`)) %>%  # Ensure is character before binding
  bind_rows(
    summarize(
      NFTB_df, 
      `Fall semester` = "Total",
      `10th percentile` = quantile(hs_percentile, 0.10, na.rm = TRUE),
      `25th percentile` = quantile(hs_percentile, 0.25, na.rm = TRUE),
      `50th percentile` = quantile(hs_percentile, 0.50, na.rm = TRUE),
      `75th percentile` = quantile(hs_percentile, 0.75, na.rm = TRUE),
      Unreported = sum(is.na(hs_percentile)),
      Students = sum(new_firstTime_degreeSeeking_bachelors_enroll_flag)
    )
  )

HS_rank_summary_df$Students <- fmt(HS_rank_summary_df$Students)

time_var <- colnames(HS_rank_summary_df )[1]
cols <- colnames(HS_rank_summary_df )[2:ncol(HS_rank_summary_df )]
counter <- counter + 1

# 5a). Create and save table
table_counter <- table_counter + 1
table <- generate_table_admissions(HS_rank_summary_df , time_var, cols, context)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_HS rank.html")
save_table(table, html_file)
