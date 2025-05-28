params <- list()

## -------------------------------------------------------------------------------------------------------------------------------------------------

section <- "Fall Enrollment"
past_years <- seq(year - 4, year, 1)
topN <- 10


## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1). Import data
enroll_data_filename <- "sdm_career_admit_modified.csv"
enroll_file <- paste0(data_dir, enroll_data_filename)
enroll_df_orig <- read.csv(enroll_file, header = TRUE)


# 2). Filter by specific criteria and make necessary modifications

# 2a). enroll_df_trends
enroll_df_trends <- enroll_df_orig %>%
  filter(
    snapshot_term_type == "Fall",
    snapshot_year %in% past_years
  ) %>%
  mutate(
    student_citizenship = factor(student_citizenship),
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("Male", "Female")),
    snapshot_term = factor(gsub("Fall ", "", snapshot_term)),
    student_age = as.numeric(student_age),
    student_full_part_time = factor(student_full_part_time, levels = c("Part-Time", "Full-Time")),
    major_1_campus_code = factor(major_1_campus_code),
    enrollment_status_ipeds = factor(enrollment_status_ipeds),
    career = factor(career, levels = c("Undergraduate", "Graduate")),
    fa_pell_offer_flag = factor(fa_pell_offer_flag, levels = c("Y", "N"), labels = c("Yes", "No")),
    first_generation_fafsa = factor(first_generation_fafsa, levels = c("N", "Y"), labels = c("No/Unlikely", "Yes/Probable")),
    Online.vs.In.Person = factor(Online.vs.In.Person, levels = c("In-Person", "Online")),
    major_1_cip_stem_flag = factor(major_1_cip_stem_flag, levels = c("Y", "N"), labels = c("STEM", "Non-STEM")),
    stdnt_race_ipeds = factor(stdnt_race_ipeds),
    student_fte = factor(student_fte),
    class_level = factor(
      class_level, levels = c(
        "First Year Undergraduate",
        "Second Year Undergraduate",
        "Third Year Undergraduate",
        "Fourth Year Undergraduate",
        "Second Bachelor's",
        "Graduate",
        "Unclassified"
      )
    ),
    student_age_group = cut(
      student_age,
      breaks = c(-Inf, 17, 19, 21, 24, 29, 34, 39, 49, 64, Inf),
      labels = c("Under 18", "18-19", "20-21", "22-24", "25-29", "30-34", "35-39", "40-49", "50-64", "65+"),
      right = TRUE
    ),
    major_1_college = factor(major_1_college),
    county_of_residence = factor(county_of_residence, levels = c(
      setdiff(sort(unique(county_of_residence)), "Outside of North Carolina"),
      "Outside of North Carolina"
    )),
    # state_of_residence = fct_relevel(state_of_residence, "Foreign", after = Inf),
    major_1_department = factor(major_1_department)
  )

# 2b). enroll_df
enroll_df <- enroll_df_trends %>%
  filter(
    snapshot_year == year
  ) %>%
  mutate(
    career = factor(career, levels = c("Undergraduate", "Graduate")),
    student_gender_ipeds = factor(student_gender_ipeds, levels = c("Male", "Female")),
    stdnt_race_ipeds = factor(stdnt_race_ipeds),
    residency = factor(residency),
    degree_seeking_flag = factor(degree_seeking_flag, levels = c("Y", "N"), labels = c("Degree-seeking", "Non Degree-seeking")),
    major_1_department = factor(major_1_department),
    major_1_inst = factor(major_1_inst),
    class_level = factor(
      class_level, levels = c(
        "First Year Undergraduate",
        "Second Year Undergraduate",
        "Third Year Undergraduate",
        "Fourth Year Undergraduate",
        "Second Bachelor's",
        "Graduate",
        "Unclassified"
      )
    ),
    student_full_part_time = factor(student_full_part_time),
    student_age = as.numeric(student_age),
    student_age_group = cut(
      student_age,
      breaks = c(-Inf, 17, 19, 21, 24, 29, 34, 39, 49, 64, Inf),
      labels = c("Under 18", "18-19", "20-21", "22-24", "25-29", "30-34", "35-39", "40-49", "50-64", "65+"),
      right = TRUE
    ),
    new_firstTime_degreeSeeking_bachelors_enroll_flag = ifelse(
      enrollment_status_code_ipeds %in% c(1,5),
      "Yes",
      "No"
    ),
    new_transfer_bachelors_enroll_flag = ifelse(
      enrollment_status_code_ipeds %in% c(2,6),
      "Yes",
      "No"
    )
  ) %>%
  distinct(student_pidm, .keep_all = TRUE)


# 3). Specify a Law School data frame (needed for Law school figure/table)
law_df <- enroll_df_trends %>%
    filter(major_1_inst == "Law")


# 4). Specify a New Transfers data frame
new_transfers_df <- enroll_df_trends %>%
  filter(str_detect(enrollment_status_ipeds, "Transfer"))


## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1). Load data
sch_data_filename <- "sdm_enrollment_modified.csv"
sch_file <- paste0(data_dir, sch_data_filename)
sch_df_orig <- read.csv(sch_file, header = TRUE)

sch_df_orig$year <- floor(as.numeric(sch_df_orig$snapshot_term_code) / 10)

# 1). Filter by specific criteria
sch_df_trends <- sch_df_orig %>%
  filter(
    year %in% past_years
  ) %>%
  mutate(
    career = factor(career),
    snapshot_term = factor(gsub("Fall", "", snapshot_term))
  )

## -------------------------------------------------------------------------------------------------------------------------------------------------

# Ordered list
visual_info <- list(
  c("Career", "career", "table"),
  c("Career", "career", "figure"),
  c("Gender by Career", "student_gender_ipeds", "table"),
  c("Gender by Career", "student_gender_ipeds", "figure"),
  c("Full & Part Time", "student_full_part_time", "table"),
  c("Full & Part Time", "student_full_part_time", "figure"),
  c("Full-Time Equivalent", "student_fte", "table"),
  c("Ethnicity & Race", "stdnt_race_ipeds", "table"),
  c("Online vs In-Person majors", "Online.vs.In.Person", "table"),
  c("Online vs In-Person majors", "Online.vs.In.Person", "figure"),
  c(paste0("Age - Fall ", year), "student_age_group", "table"),
  c(paste0("Age - Fall ", year), "student_age_group", "figure"),
  c("Citizenship", "student_citizenship", "table"),
  c("Enrollment Category per IPEDS", "enrollment_status_ipeds", "table"),
  c("Pell Offer", "fa_pell_offer_flag", "table"),
  c("Pell Offer", "fa_pell_offer_flag", "figure"),
  c("First Generation", "first_generation_fafsa", "table"),
  c("First Generation", "first_generation_fafsa", "figure"),
  c("STEM", "major_1_cip_stem_flag", "table"),
  c("STEM", "major_1_cip_stem_flag", "figure"),
  c("College", "major_1_college", "table"),
  c("Top 10 Departments (Undergraduate)", "major_1_department", "figure"),
  c("Top 10 Majors (Undergraduate)", "major_1_inst", "figure"),
  c("All Student Credit Hours for Course Enrollments at NCCU at Census", "career", "table"),
  c("All Student Credit Hours for Course Enrollments at NCCU at Census", "career", "figure"),
  c("New Transfers", "enrollment_status_ipeds", "figure"),
  c("Law School", "major_1_inst", "figure")
)

## -------------------------------------------------------------------------------------------------------------------------------------------------

counter <- 0
table_counter <- 0
figure_counter <- 0

# Generate tables
for (v in visual_info) {
  print(paste0("Creating ", v[1], " ", v[3]))

  counter <- counter + 1
  if (v[3] == "table") {
    table_counter <- table_counter + 1
  } else {
    figure_counter <- figure_counter + 1
  }

  if (v[1] %in% c("Career", "All Student Credit Hours for Course Enrollments at NCCU at Census")) {
    if (v[3] == "table") {
      if (v[1] == "Career") {
        visual <- generate_table_1cat(enroll_df_trends, v[2], "snapshot_term")
      } else {
        visual <- generate_table_1cat(sch_df_trends, v[2], "snapshot_term")
      }
    } else {
      cat_var <- v[2]
      title <- NULL
      xlabel <- "Fall semester"
      if (v[1] == "Career") {
        visual <- generate_line_1cat_total_ggplot(enroll_df_trends, "snapshot_term", v[2], title, xlabel)
      } else {
        visual <- generate_line_1cat_total_ggplot(sch_df_trends, "snapshot_term", v[2], title, xlabel)
      }
    }
  } else if (v[1] == "Law School") {
    if (v[3] == "table") {
      visual <- generate_table_law(law_df, v[2], "snapshot_term")
    } else {
      visual <- generate_law_bar_ggplots(law_df, "snapshot_term")
    }
  } else if (v[1] == "New Transfers" && v[3] == "figure") {
    visual <- generate_law_bar_ggplots(new_transfers_df, "snapshot_term")
  } else if (v[1] == paste0("Age - Fall ", year) && v[3] == "figure") {
    visual <- generate_age_bar_ggplots(enroll_df, v[2], "career")
  } else if (v[1] %in% c("Top 10 Majors (Undergraduate)", "Top 10 Departments (Undergraduate)") && v[3] == "figure") {
    visual <- topN_horizontal_barplots(enroll_df, v[2], topN)
  } else {
    if (v[3] == "table") {
    visual <- generate_table_2cats(enroll_df_trends, v[2], "career", "snapshot_term")
    } else {
      visual <- generate_line_ggplots(enroll_df_trends, "snapshot_term", v[2], "career")
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

## -------------------------------------------------------------------------------------------------------------------------------------------------

# 1 Enrollment
# 1a). Total Enrollment count
total_enrollment <- tibble(
  Metric = "Total Enrollment",
  Category = "--",
  N = nrow(enroll_df),
  `%` = 100
)

# 1b). Create separate summary tables
summary_degree_seeking <- summarize_metric(enroll_df, "degree_seeking_flag")
summary_career <- summarize_metric(enroll_df, "career")
summary_gender <- summarize_metric(enroll_df, "student_gender_ipeds")
summary_ethnicity <- summarize_metric(enroll_df, "stdnt_race_ipeds")
summary_class_level <- summarize_metric(enroll_df, "class_level")
summary_residency <- summarize_metric(enroll_df, "residency")
summary_full_part_time <- summarize_metric(enroll_df, "student_full_part_time")
summary_pell_offer <- summarize_metric(enroll_df, "fa_pell_offer_flag")
summary_NFTB <- summarize_metric(enroll_df, "new_firstTime_degreeSeeking_bachelors_enroll_flag")
summary_new_transfers <- summarize_metric(enroll_df, "new_transfer_bachelors_enroll_flag")

# 1c). Append (stack) them together
summary_combined_fall_enrollment <- bind_rows(
  total_enrollment,
  summary_degree_seeking,
  summary_career,
  summary_gender,
  summary_ethnicity,
  summary_class_level,
  summary_residency,
  summary_full_part_time,
  summary_pell_offer,
  summary_NFTB,
  summary_new_transfers,
) %>%
  group_by(Metric) %>%
  mutate(Metric = ifelse(row_number() == 1, Metric, "")) %>%
  ungroup() %>%
  mutate(
    Metric = case_when(
      Metric == "degree_seeking_flag" ~ "Degree-seeking status",
      Metric == "career" ~ "Career",
      Metric == "student_gender_ipeds" ~ "Gender",
      Metric == "stdnt_race_ipeds" ~ "Ethnicity/Race",
      Metric == "class_level" ~ "Class Level",
      Metric == "new_transfer_bachelors_enroll_flag" ~ "New Transfers",
      Metric == "residency" ~ "Residency",
      Metric == "fa_pell_offer_flag" ~ "Offered Federal Pell Grant(s)",
      Metric == "student_full_part_time" ~ "Full/Part Time",
      Metric == "new_firstTime_degreeSeeking_bachelors_enroll_flag" ~ "New First-Time Degree-seeking Bachelors",
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
table_summary_fall_enrollments <- generate_table_from_summary(summary_combined_fall_enrollment, f2) %>%
  add_header_row(values = paste0("Fall ", year, " Enrollment"), colwidths = ncol(summary_combined_fall_enrollment)) %>%
  hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>% 
  line_spacing(space = 0.7) %>%
  vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header")

# 1e). Save table
html_file <- paste0(intermediary_dir,"s1_General Information_1_table1_Fall Enrollment.html")
save_table(table_summary_fall_enrollments, html_file)

