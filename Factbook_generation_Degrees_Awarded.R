params <- list()

## -------------------------------------------------------------------------------------------------------------------------------------------------

past_academic_years_grad_ret <- paste0(seq(year - 9, year, 1), "-", seq(year - 8, year + 1, 1))
past_academic_years_da <- paste0(seq(year - 5, year, 1), "-", seq(year - 4, year + 1, 1))
ret_grad_data_filename <- "oira_gradret.txt"
degrees_awarded_filename <- "sdm_completion_major_mv_modified.csv"

## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1). Import data
ret_grad_file <- paste0(data_dir, ret_grad_data_filename)
ret_grad_df_orig <- fread(ret_grad_file, header = TRUE)

degrees_awarded_file <- paste0(data_dir, degrees_awarded_filename)
degrees_awarded_df_orig <- read.csv(degrees_awarded_file, header = TRUE)


# 2). Filter for past 5 years data and make modifications
# 2a). Retention & Graduation df
ret_grad_df_trends <- ret_grad_df_orig %>%
  mutate(
    year = as.integer(substr(snapshot_term_code_campus, 1, 4)),
    academic_year =  paste(year-1, year, sep = "-"),
    # a) Last Department
    Last_Department = ifelse(
      Last_Department %in% c("", "NONE") | is.na(Last_Department),
      "Unknown",
      Last_Department
    ),
    # b). Last College
    Last_College = ifelse(
      Last_College %in% c("", "NONE") | is.na(Last_College),
      "Unknown",
      Last_College
    ),
    # c). Academic year shorthand
    academic_year_shorthand = sub("^20([0-9]{2})-20([0-9]{2})$", "\\1-\\2", academic_year),
    # d). factor specific columns (necessary for constructing tabular & flextable objects)
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("M", "F"), labels = c("Male", "Female")),
    Career = factor(Career, levels = c("Undergraduate", "Graduate")),
    Last_Department = factor(Last_Department),
    Last_College = factor(Last_College),
    stdnt_race_ipeds = factor(stdnt_race_ipeds),
    Student_Type = factor(Student_Type, levels = c("First Time Bachelor's", "Transfer", "Master's", "Doctoral, Professional Practice", "Doctoral, Research/Scholarship"))
  ) %>%
  filter(
    academic_year %in% past_academic_years_grad_ret
  ) %>%
  mutate(
    academic_year = factor(academic_year),
    academic_year_shorthand = factor(academic_year_shorthand)
  ) %>%
  select(-year)


# 2b). Degrees Awarded df
degrees_awarded_df <- degrees_awarded_df_orig %>%
  filter(
    comp_term_academic_year %in% past_academic_years_da
  ) %>%
  mutate(
    career = factor(career),
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("M", "F"), labels = c("Male", "Female")),
    degree_level = factor(degree_level, levels = c("Bachelor's", "Master's", "Doctoral, Professional Practice", "Doctoral, Research/Scholarship")),
    comp_term_academic_year = factor(comp_term_academic_year),
    program_college = factor(program_college),
    program_department = factor(program_department),
    stdnt_race_ipeds = factor(stdnt_race_ipeds)
  )
  


## -------------------------------------------------------------------------------------------------------------------------------------------------

counter <- 0
table_counter <- 0
figure_counter <- 0

# 1). Retention
print("Creating Retention table")
counter <- counter + 1
table_counter <- table_counter + 1
ret_summary <- retention_graduation_summary_df(ret_grad_df_trends, "Year_", "After")
f2 <- "Note: Cohort numbers are based on first-time degree-seeking Bachelor's students."
ret_summary_table <- generate_ret_grad_tables(ret_summary, f2)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Retention", ".html")
save_table(ret_summary_table, html_file)
  

# 2). 1-yr Retention Rates
print("Creating 1-yr Retention table")
counter <- counter + 1
table_counter <- table_counter + 1
f2 <- "Note: Cohort numbers are based on first-time degree-seeking Bachelor's students."
ret1_summary <- ret_summary %>%
  select(c("Cohort", "Headcount", "After 1 (#)", "After 1 (%)")) %>%
  generate_ret_grad_tables(f2)

html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_1 Yr Retention.html")
save_table(ret1_summary, html_file)


# 3). Graduation Rates
print("Creating Graduation table")
counter <- counter + 1
table_counter <- table_counter + 1
grad_summary <- retention_graduation_summary_df(ret_grad_df_trends, "In_", "In")
f2 <- "Note: Cohort numbers are based on first-time degree-seeking Bachelor's students."
grad_summary_table <- generate_ret_grad_tables(grad_summary, f2)
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Graduation.html")
save_table(grad_summary_table, html_file)


# 4). Degree Level
print("Creating Degree Level visualizations")
# 4a). table
counter <- counter + 1
table_counter <- table_counter + 1
table_career <- generate_table_1cat(degrees_awarded_df, "degree_level", "comp_term_academic_year")
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Degree Level.html")
save_table(table_career, html_file)

# 4b). figure
counter <- counter + 1
figure_counter <- figure_counter + 1
title <- NULL
figure_degree_level <- generate_line_1cat_total_ggplot(degrees_awarded_df, "comp_term_academic_year", "degree_level", title, "Academic year")
output_file_svg <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Degree Level.svg")
output_file_html <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Degree Level.html")
save_figure(figure_degree_level, output_file_svg, output_file_html)


# 5). Gender
print("Creating Gender visualizations")
# 5a). table
counter <- counter + 1
table_counter <- table_counter + 1
table_gender <- generate_table_1cat(degrees_awarded_df, "student_gender_ipeds", "comp_term_academic_year")
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Gender.html")
save_table(table_gender, html_file)

# 5b). figure
counter <- counter + 1
figure_counter <- figure_counter + 1
title <- NULL
figure_gender <- generate_line_1cat_total_ggplot(degrees_awarded_df, "comp_term_academic_year", "student_gender_ipeds", title, "Academic year")
output_file_svg <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Gender.svg")
output_file_html <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_figure", figure_counter, "_Gender.html")
save_figure(figure_gender, output_file_svg, output_file_html)


# 6). Department Table
print("Creating Department visualizations")
# 6a). undergraduate
counter <- counter + 1
table_counter <- table_counter + 1

table_department_u <- degrees_awarded_df %>%
  filter(
    career == "Undergraduate",
  ) %>%
  generate_table_1cat("program_department", "comp_term_academic_year") %>%
  line_spacing(space = 0.65) %>%
  width(j = 1, width = 1.5, unit = "in")

html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Department (Undergraduate).html")
save_table(table_department_u, html_file)

# 6b). graduate
counter <- counter + 1
table_counter <- table_counter + 1

table_department_g <- degrees_awarded_df %>%
  filter(
    career == "Graduate",
  ) %>%
  generate_table_1cat("program_department", "comp_term_academic_year") %>%
  line_spacing(space = 0.85) %>%
  width(j = 1, width = 1.5, unit = "in")

html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Department (Graduate).html")
save_table(table_department_g, html_file)


# 7). College Table
print("Creating College visualizations")
# 7a). Undergraduate
counter <- counter + 1
table_counter <- table_counter + 1
table_college_u <- degrees_awarded_df %>%
  filter(
    career == "Undergraduate",
  ) %>%
  generate_table_1cat("program_college", "comp_term_academic_year") %>%
  line_spacing(space = 0.7) %>%
  width(j = 1, width = 1.5, unit = "in")

html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_College (Undergraduate).html")
save_table(table_college_u, html_file)

# 7b). Graduate
counter <- counter + 1
table_counter <- table_counter + 1
table_college_g <- degrees_awarded_df %>%
  filter(
    career == "Graduate",
  ) %>%
  generate_table_1cat("program_college", "comp_term_academic_year") %>%
  line_spacing(space = 0.7) %>%
  width(j = 1, width = 1.5, unit = "in")


html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_College (Graduate).html")
save_table(table_college_g, html_file)


# 8). Ethnicity/Race Table
print("Creating Ethnicity/Race visualizations")
counter <- counter + 1
table_counter <- table_counter + 1
table_race <- degrees_awarded_df %>%
  generate_table_1cat("stdnt_race_ipeds", "comp_term_academic_year")
html_file <- paste0(intermediary_dir, section_id, "_", section, "_", counter, "_table", table_counter, "_Ethnicity & Race.html")
save_table(table_race, html_file)


## -------------------------------------------------------------------------------------------------------------------------------------------------

fall_cohorts_ret1_rates <- ret_summary %>%
  select(all_of(c("Cohort", "After 1 (%)"))) %>%
  filter(grepl("Fall", Cohort)) %>%
  rename("After 1" = `After 1 (%)`) %>%
  pivot_longer(
    cols = c("After 1"),
    names_to = "Category",
    values_to = "%"
  ) %>%
  mutate(Metric = "1st Year Retention Rates of Fall Cohorts") %>%
  select(Metric, everything())


fall_cohorts_grad_rates <- grad_summary %>%
  select(all_of(c("Cohort", "In 4 (%)", "In 6 (%)"))) %>%
  filter(grepl("Fall", Cohort)) %>%
  rename(
    "In 4 yrs" = `In 4 (%)`,
    "In 6 yrs" = `In 6 (%)`
  ) %>%
  pivot_longer(
    cols = c("In 4 yrs", "In 6 yrs"),
    names_to = "Category",
    values_to = "%"
  ) %>%
  mutate(Metric = "Graduation Rates of Fall Cohorts") %>%
  select(Metric, everything()) %>%
  arrange(Category)


# 1c). Append (stack) them together
summary_combined_student_success <- bind_rows(
  fall_cohorts_ret1_rates,
  fall_cohorts_grad_rates
  ) %>%
  # Add blank row for consistency with other tables
  bind_rows(
    .,
    summarise(., across(everything(), ~NA[1]))  
  )

# 1d). Generate full table
f2 <- "Note: information pertains specifically to First-time degree-seeking Bachelor's students"
table_summary_student_success <- generate_table_from_summary(summary_combined_student_success, f2) %>%
  add_header_row(values = "Student Success", colwidths = ncol(summary_combined_student_success)) %>%
  hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>%
  line_spacing(space = 0.8) %>%
  vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header")

# 1e). Save this table
html_file <- paste0(intermediary_dir,"s1_General Information_3_table3_Student Success.html")
save_table(table_summary_student_success, html_file)
