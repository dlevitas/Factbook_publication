params <- list()

# 1). Get tuition information (if the PDF document is provided in OIRA O drive)
fy_finance_file <- paste0(university_finances_dir, "NCCU_FY_", year, "-", year+1, ".xlsx")
if (file.exists(fy_finance_file)) {

  fy_financial_df <- read.xlsx(fy_finance_file, sheet = 1)
  fy_financial_df <- as.data.frame(fy_financial_df)
  
  # 1). Revenue
  print("Revenue information")
  key_categories <- c("Operating Revenues", "Nonoperating Revenues", "Revenues and Other Additions")
  
  rev_slice_start <- which(fy_financial_df$North.Carolina.Central.University == "Operating Revenues")
  rev_slice_end <- which(fy_financial_df$North.Carolina.Central.University == "Total all revenues and other additions")
  
  revenues_df <- fy_financial_df %>%
    slice(rev_slice_start:rev_slice_end) %>%
    select(3:4) %>% 
    set_names(c("Category", "Revenue")) %>%
    filter(
      if_any(everything(), ~ !is.na(.)),
      !grepl("04a|04b|28a|28b|operating:|Total operating and non|12-Month Student FTE|Total oper|Total nonoper|Total other", Category)
    ) %>%
    mutate(
      Category = gsub(", after.*| \\(Do NOT.*", "", Category),
      `Source` = ifelse(Category %in% key_categories, Category, NA)
    ) %>%
    tidyr::fill(`Source`, .direction = "down") %>%
    mutate(
      Percent = round((as.numeric(Revenue) / as.numeric(Revenue[n()])) * 100, 1),
      Percent = ifelse(row_number() == n(), NA, Percent),
      "%" = ifelse(is.na(Percent), "--", Percent),
      Revenue = ifelse(is.na(Revenue) | Revenue == 0, "--", gsub(" ", "", paste0("$", fmt(as.numeric(Revenue)))))
    ) %>%
    filter(
      !grepl(gsub(",", "|", paste(key_categories, collapse = ',')), Category),
      Revenue != "--"
    ) %>%
    select(`Source`, Category, Revenue, `%`)
  
  revenues_df$`Source`[nrow(revenues_df)] <- "Total"
  revenues_df$Category[nrow(revenues_df)] <- ""
  revenues_df$`%`[nrow(revenues_df)] <- "100"

  f2 <- NULL
  table_summary_university_revenue <- generate_table_from_summary(revenues_df, f2) %>%
    add_header_row(values = paste0("University Revenues, Fiscal Year ", year, "-", year+1), colwidths = ncol(revenues_df)) %>%
    hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>% 
    line_spacing(space = 0.3) %>%
    width(width = 2, unit = "in") %>%
    vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header")
  
  html_file <- paste0(intermediary_dir, section_id, "_", section, "_1_table1_Revenue.html")
  save_table(table_summary_university_revenue, html_file)
  
  
  # 2). Expenditures
  print("Expenditures information")
  key_categories <- c("Operating Expenses", "Nonoperating Expenses")
  
  exp_slice_start <- which(fy_financial_df$North.Carolina.Central.University == "Operating Expenses")
  exp_slice_end <- which(fy_financial_df$North.Carolina.Central.University == "Total expenses and deductions")[2]
  
  expenditures_df <- fy_financial_df %>%
    slice(exp_slice_start:exp_slice_end) %>%
    select(3:4) %>% 
    set_names(c("Category", "Expenditure")) %>%
    filter(
      if_any(everything(), ~ !is.na(.)),
      !grepl("Total operating expenses", Category)
    ) %>%
    mutate(
      Category = gsub(",.*", "", Category),
      `Source` = ifelse(Category %in% key_categories, Category, NA)
    ) %>%
    tidyr::fill(`Source`, .direction = "down") %>%
    mutate(
      Percent = round((as.numeric(Expenditure) / as.numeric(Expenditure[n()])) * 100, 1),
      Percent = ifelse(row_number() == n(), NA, Percent),
      "%" = ifelse(is.na(Percent), "--", Percent),
      Expenditure = ifelse(is.na(Expenditure) | Expenditure == 0, "--", gsub(" ", "", paste0("$", fmt(as.numeric(Expenditure)))))
    ) %>%
    filter(
      !grepl(gsub(",", "|", paste(key_categories, collapse = ',')), Category),
      Expenditure != "--"
    ) %>%
    select(`Source`, Category, Expenditure, `%`)
  
  expenditures_df$`Source`[nrow(expenditures_df)] <- "Total"
  expenditures_df$Category[nrow(expenditures_df)] <- ""
  expenditures_df$`%`[nrow(expenditures_df)] <- "100"

  f2 <- NULL
  table_summary_university_expenditures <- generate_table_from_summary(expenditures_df, f2) %>%
    add_header_row(values = paste0("University Expenditures, Fiscal Year ", year, "-", year+1), colwidths = ncol(expenditures_df)) %>%
    hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>% 
    line_spacing(space = 0.3) %>%
    width(width = 2, unit = "in") %>%
    vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header")
  
  html_file <- paste0(intermediary_dir, section_id, "_", section, "_2_table2_Expenditures.html")
  save_table(table_summary_university_expenditures, html_file)
}


