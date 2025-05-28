params <- list()

## -------------------------------------------------------------------------------------------------------------------------------------------------

past_years <- seq(year - 4, year, 1)

## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1). Import data
employee_data_filename <- "sdm_employment_mv_modified.csv"
employee_file <- paste0(data_dir, employee_data_filename)
employee_df_orig <- read.csv(employee_file, header = TRUE)


# 2). Create filtered data frame (employee_df_trends) that contains past 5 years worth of data
employee_df_trends <- employee_df_orig %>%
  filter(
    EMPL_EXCLUDE_FROM_IPEDS_FLAG == "N",
    SNAPSHOT_YEAR %in% past_years
  )


# 3). Ensure specific columns are factors
employee_df_trends <- employee_df_trends %>%
  mutate(
    SNAPSHOT_YEAR = factor(SNAPSHOT_YEAR),
    IPEDS_FT_PT_MODIFIED = factor(IPEDS_FT_PT_MODIFIED, levels = c("Full-Time", "Part-Time")),
    EMPLOYEE_TYPE = factor(EMPLOYEE_TYPE, levels = c("EHRA", "SHRA", "NPE")),
    GENDER_MODIFIED = factor(GENDER_MODIFIED, levels = c("Male", "Female")),
    OCCUPATIONAL_STATUS = factor(OCCUPATIONAL_STATUS),
    ACADEMIC_RANK_IPEDS = factor(ACADEMIC_RANK_IPEDS, levels = c("Professor", "Associate Professor", "Assistant Professor", "Instructor", "Lecturer", "No Academic Rank", "Without Faculty Status", "Unknown")),
    EMPL_HIGHEST_DEGREE_LEVEL = factor(EMPL_HIGHEST_DEGREE_LEVEL, levels = c("Bachelor's", "Master's", "Doctoral, Professional Practice", "Doctoral, Research/Scholarship", "Unknown")),
    EMPL_HOME_DEPARTMENT = factor(EMPL_HOME_DEPARTMENT),
    EMPLOYEE_RACE_IPEDS = factor(EMPLOYEE_RACE_IPEDS),
    EMPLOYEE_AGE_BINNED = factor(EMPLOYEE_AGE_BINNED, levels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")),
    TENURE_STATUS_MODIFIED = factor(TENURE_STATUS_MODIFIED, levels = c("EPA - Permanent Tenure", "EPA - Not Tenured but on Tenure Track", "EPA - Not on Tenure Track", "Without Faculty Status", "Phased Retirement Program"))
  )


employee_df <- employee_df_trends %>%
  filter(
    SNAPSHOT_TERM == paste0("Fall ", year)
  )

## -------------------------------------------------------------------------------------------------------------------------------------------------
# These are ordered

visual_info <- list(
  c("Full & Part Time", "IPEDS_FT_PT_MODIFIED", "table"),
  c("Full & Part Time", "IPEDS_FT_PT_MODIFIED", "figure"),
  c("Employee Type", "EMPLOYEE_TYPE", "table"),
  c("Gender", "GENDER_MODIFIED", "table"),
  c("Academic Rank", "ACADEMIC_RANK_IPEDS", "table"),
  c("Highest Degree Earned", "EMPL_HIGHEST_DEGREE_LEVEL", "table"),
  c("Ethnicity & Race", "EMPLOYEE_RACE_IPEDS", "table"),
  c("Tenure Status", "TENURE_STATUS_MODIFIED", "table"),
  c("Age Group", "EMPLOYEE_AGE_BINNED", "table")
)


## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1 Employees
# 1a). Total Employee count
total_employees <- tibble(
  Metric = "Total Employees",
  Category = "--",
  N = nrow(employee_df),
  `%` = 100
)

# 1b). Create separate summary tables
summary_full_part_time <- summarize_metric(employee_df, "IPEDS_FT_PT_MODIFIED")
summary_gender <- summarize_metric(employee_df, "GENDER_MODIFIED")
summary_ethnicity <- summarize_metric(employee_df, "EMPLOYEE_RACE_IPEDS")
summary_type <- summarize_metric(employee_df, "EMPLOYEE_TYPE")
summary_academic_rank <- summarize_metric(employee_df, "ACADEMIC_RANK_IPEDS")
summary_tenure_status <- summarize_metric(employee_df, "TENURE_STATUS_MODIFIED")


# 1c). Append (stack) them together
summary_combined_employees <- bind_rows(
  total_employees,
  summary_full_part_time,
  summary_gender,
  summary_ethnicity,
  summary_type,
  summary_academic_rank,
  summary_tenure_status
) %>%
  group_by(Metric) %>%
  mutate(Metric = ifelse(row_number() == 1, Metric, "")) %>%
  ungroup() %>%
  mutate(
    Metric = case_when(
      Metric == "IPEDS_FT_PT_MODIFIED" ~ "Full/Part Time",
      Metric == "GENDER_MODIFIED" ~ "Gender",
      Metric == "EMPLOYEE_RACE_IPEDS" ~ "Ethnicity/Race",
      Metric == "EMPLOYEE_TYPE" ~ "Employee Type",
      Metric == "ACADEMIC_RANK_IPEDS" ~ "Academic Rank",
      Metric == "TENURE_STATUS_MODIFIED" ~ "Tenure status",
      TRUE ~ Metric
    )
  ) %>%
  # Add blank row for consistency with other tables
  bind_rows(
    .,
    summarise(., across(everything(), ~NA[1]))  
  )

# 1d). Generate full table
f2 <- NULL
table_summary_employment <- generate_table_from_summary(summary_combined_employees, f2) %>%
  add_header_row(values = paste0("Faculty & Staff, Fall ", year), colwidths = ncol(summary_combined_employees)) %>%
  hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>%
  vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header") %>%
  line_spacing(space = 0.9)

# 1e). Save this table
html_file <- paste0(intermediary_dir,"s1_General Information_2_table2_Employees.html")
save_table(table_summary_employment, html_file)


## -------------------------------------------------------------------------------------------------------------------------------------------------

counter <- 0
table_counter <- 0
figure_counter <- 0

# Generate tables
for (v in visual_info) {
  print(paste0("Creating ", v[1], " visualizations"))
  print(paste0("Creating ", v[1], " ", v[3]))
  
  counter <- counter + 1
  if (v[3] == "table") {
    table_counter <- table_counter + 1
  } else {
    figure_counter <- figure_counter + 1
  }
  
  if (v[1] == "Full & Part Time") {
    if (v[3] == "table") {
        visual <- generate_table_1cat(employee_df_trends, v[2], "SNAPSHOT_YEAR")
    } else {
      cat_var <- v[2]
      title <- NULL
      xlabel <- "Academic Year"
      visual <- generate_line_1cat_total_ggplot(employee_df_trends, "SNAPSHOT_YEAR", v[2], title, xlabel)
    }
  } else if (v[1] == "Employee Type") {
    f2 <- "Note: EHRA denotes faculty/higher-level positions, SHRA denotes staff, and NRE denotes non-permanent employees"
    if (v[3] == "table") {
      visual <- generate_table_1cat(employee_df_trends, v[2], "SNAPSHOT_YEAR", f2)
    }
  } else {
    if (v[3] == "table") {
      f2 <- NULL
      visual <- generate_table_1cat(employee_df_trends, v[2], "SNAPSHOT_YEAR", f2)
    }
  }
  
  if (v[3] == "table") {
    html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_", v[1], ".html")
    save_table(visual, html_file)
  } else {
    output_file_svg <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_", v[1], ".svg")
    output_file_html <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_", v[1], ".html")
    save_figure(visual, output_file_svg, output_file_html)
  }
}
