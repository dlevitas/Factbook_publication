params <- list()

## -------------------------------------------------------------------------------------------------------------------------------------------------

past_academic_years <- paste0(seq(year - 5, year, 1), "-", seq(year - 4, year + 1, 1))

## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1). Import data
finaid_data_filename <- "sdm_finaid_yr_awrd_dtl_full_modified.csv"
finaid_file <- paste0(data_dir, finaid_data_filename)
finaid_df_orig <- read.csv(finaid_file, header = TRUE)


# 2). Create data frame
finaid_df_trends <- finaid_df_orig %>%
  filter(
    academic_year %in% past_academic_years
  ) %>%
  mutate(
    academic_year = factor(academic_year),
    award_source = factor(award_source, levels = c("Federal", "State", "Institutional", "External or Private")),
    award_name = factor(award_name),
    award_type_inst = factor(award_type_inst),
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("M", "F"), labels = c("Male", "Female")),
    snapshot_term = factor(snapshot_term),
    career = factor(career, levels = c("Undergraduate", "Graduate")),
    stdnt_race_ipeds = factor(stdnt_race_ipeds)
  )

## -------------------------------------------------------------------------------------------------------------------------------------------------

counter <- 0
table_counter <- 0
figure_counter <- 0

# 1). Student financial aid by broad metrics for Undergraduates
print("Creating Student financial aid by broad metrics for Undergraduates table")
counter <- counter + 1
table_counter <- table_counter + 1
summary_table1 <- finaid_df_trends %>%
  filter(career == "Undergraduate", award_type == "Grant or Scholarship") %>%
  group_by(academic_year) %>%
  summarise(
    `Undergraduate students awarded aid` = n_distinct(student_pidm),
    `Total Aid` = sum(amount_paid_year, na.rm = TRUE),
    `Undergraduates Pell Grant` = n_distinct(
      if_else(award_name == "Federal Pell Grant", as.character(student_pidm), NA_character_),
      na.rm = TRUE
    )
  ) %>%
  bind_rows(
    summarise(
      .,
      academic_year = "Total",
      `Undergraduate students awarded aid` = sum(`Undergraduate students awarded aid`, na.rm = TRUE),
      `Total Aid` = sum(`Total Aid`, na.rm = TRUE),
      `Undergraduates Pell Grant` = sum(`Undergraduates Pell Grant`, na.rm = TRUE)
    ) %>%
    mutate(academic_year = "Total")
  ) %>%
  # Format the total row if needed
  mutate(
    `Undergraduate students awarded aid` = fmt(`Undergraduate students awarded aid`),
    `Total Aid` = gsub(" ", "", paste0("$", fmt(round(`Total Aid`)))),
    `Undergraduates Pell Grant` = fmt(`Undergraduates Pell Grant`),
  ) %>%
  rename(
    `Academic Year` = academic_year,
    `Financial Aid Awarded to All Undergraduates` = `Total Aid`,
    `Undergraduates Awarded Federal Pell Grant` = `Undergraduates Pell Grant`
  )

f2 <- NULL
table_summary <- generate_table_from_summary(summary_table1, f2)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Financial Aid (Undergraduate).html")
save_table(table_summary, html_file)


# 2). Student financial aid by award type (inst)
print("Creating Student financial aid by award type (inst) table")
counter <- counter + 1
table_counter <- table_counter + 1
summary_table3 <- finaid_df_trends %>%
  filter(academic_year == paste0(year-1, "-", year), career == "Undergraduate") %>%
  group_by(award_source, award_type_inst) %>%
  summarise(
    Students = n_distinct(student_pidm),  # Count of unique students for each award source and award type (inst)
    `Total Amount Paid` = round(sum(amount_paid_year, na.rm = TRUE)),
    `Avg Amount Paid` = round(`Total Amount Paid` / Students),
    .groups = "drop"
  ) %>%
  group_by(award_source) %>%
  mutate(award_source = if_else(row_number() == 1, as.character(award_source), "")) %>%
  ungroup() %>%
  bind_rows(
    summarise(
      .,
      award_source = "Total",
      award_type_inst = "",
      Students = sum(Students, na.rm = TRUE),
      `Total Amount Paid` = round(sum(`Total Amount Paid`, na.rm = TRUE)),
      `Avg Amount Paid` = round(sum(`Total Amount Paid`, na.rm = TRUE) / sum(Students, na.rm = TRUE))
    )
  ) %>%
  # Format the total row values
  mutate(
    Students = fmt(Students),
    `Total Amount Paid` = gsub(" ", "", paste0("$", fmt(round(`Total Amount Paid`)))),
    `Avg Amount Paid` = gsub(" ", "", paste0("$", fmt(round(`Avg Amount Paid`))))
  ) %>%
  # Rename columns
  rename(
    `Award Source` = award_source,
    `Award Type` = award_type_inst
  )

f2 <- NULL
table_summary <- generate_table_from_summary(summary_table3, f2)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Award Type (Undergraduate).html")
save_table(table_summary, html_file)


# 3). Aid Type line plot
print("Creating Aid Type plot")
counter <- counter + 1
figure_counter <- figure_counter + 1

color_mapping <- c(
  "Non-Pell grants/scholarships" = "#8b2331",
  "Federal Pell grants" = "#a24e5a",
  "Federal loans" = "#c59198"
)

finaid_df_trends$academic_year = as.character(finaid_df_trends$academic_year)

aidType_plot <- generate_line_ggplots_aidType(
  finaid_df_trends,
  color_mapping,
  "academic_year",
  "aidtype_fed_other_grants_flag",
  "aidtype_fed_pell_grant_flag",
  "aidtype_fed_loan_flag",
  title = ""
)

svg_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Aid Type (Undergraduate).svg")
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Aid Type (Undergraduate).html")
save_figure(aidType_plot, svg_file, html_file)


# 4). Award source line plot
print("Creating Award source plot")
counter <- counter + 1
figure_counter <- figure_counter + 1

color_mapping <- c(
  "Federal" = "#6b1e28",
  "Institutional" = "#862633",
  "State" = "#aa6770",
  "External or Private" = "#cea8ad"
)

award_source_plot <- generate_line_ggplots_1cat(
    finaid_df_trends,
    color_mapping,
    "award_source",
    "academic_year",
    title = NULL
)

svg_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Award Source (Undergraduate).svg")
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Award Source (Undergraduate).html")
save_figure(award_source_plot, svg_file, html_file)
