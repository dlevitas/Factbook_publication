params <- list()

# 1). Get tuition information (if the PDF document is provided in OIRA O: drive)
if (file.exists(tuition_pdf_file)) {
  print("Extracting Tuition costs")
  tuition_matrix <- matrix(nrow = 5, ncol = 3)
  colnames(tuition_matrix) <- c("Student group", "In-State", "Out-of-State")
  
  tuition_info <- pdf_text(tuition_pdf_file)
  
  target_text_undergrad_instate <- paste0("UNDERGRADUATE STUDENTS - COHORT FALL ", year, " OR SPRING ", year+1, " - I N S T A T E")
  target_text_undergrad_outstate <- paste0("UNDERGRADUATE STUDENTS - FALL ", year, " OR SPRING ", year+1, " - OUT OF S T A T E")
  target_text_grad_instate <- "GRADUATE - IN STATE"
  target_text_grad_outstate <- "GRADUATE - OUT OF STATE"
  target_text_phd_instate <- "PhD PROGRAM - IN STATE"
  target_text_phd_outstate <- "PhD PROGRAM - OUT OF STATE"
  target_text_law_instate <- paste0("LAW STUDENTS ADMITTED FALL ", year, " OR SPRING ", year+1, " SEMESTER - IN STATE")
  target_text_law_outstate <- paste0("LAW STUDENTS ADMITTED FALL ", year, " OR SPRING ", year+1, " SEMESTER - OUT OF STATE")
  
  target_groups <- c(
    target_text_undergrad_instate,
    target_text_undergrad_outstate,
    target_text_grad_instate,
    target_text_grad_outstate,
    target_text_phd_instate,
    target_text_phd_outstate,
    target_text_law_instate,
    target_text_law_outstate
  )
  
  page_numbers <- c()
  for (target in target_groups) {
    matching_pages <- which(grepl(target, tuition_info, fixed = TRUE))[1]
    page_numbers <- append(page_numbers, matching_pages)
  }
  
  counter <- 0
  for (p in seq_along(page_numbers)) {
    
    if (p %in% c(1,3,5,7)) { # c(1,25,27,29)
      counter <- counter + 1
    }
    
    if (p %in% c(1,2)) {
      student_group = "Undergraduate"
    } else if (p %in% c(3,4)) {
      student_group = "Graduate"
    } else if (p %in% c(5,6)) {
      student_group = "PhD"
    } else if (p %in% c(7,8)) {
      student_group = "Law"
    }
    
    if (p %in% c(2,4,6,8)) {
      tuition_rate = "Out-of-State"
    } else {
      tuition_rate = "In-State"
    }
    
    page <- page_numbers[p]
    
    page_lines <- strsplit(tuition_info[page], "\n")[[1]][8:40]
    tuition_table <- read_table(paste(page_lines, collapse = "\n"), col_names = FALSE)
    
    # Find which row contains accident insurance and remove it's value from the tuition total.
    accident_row <- tuition_table[tuition_table$X1 == "Accident", -1]
    accident_amount <- as.numeric(tail(na.omit(as.vector(unlist(accident_row))), 1))
    
    tuition <- paste0("$", fmt(round((tuition_table$X13[nrow(tuition_table)] - accident_amount) * 2)))
    
    tuition_matrix[counter, 1] <- student_group
    if (tuition_rate == "In-State") {
      tuition_matrix[counter, 2] <- tuition
    } else {
      tuition_matrix[counter, 3] <- tuition
    }
  }
  tuition_df <- as.data.frame(tuition_matrix)
  
  f2 <- "Note: Rates can vary year-to-year." 
  tuition_summary <- generate_table_from_summary(tuition_df, f2) %>%
    add_header_row(values = "Tuition costs", colwidths = ncol(tuition_df)) %>%
    hline(i = 1, part = "header", border = fp_border(color = "white", width = 3)) %>% 
    vline(i = 1, j = 1, border = fp_border(color = NCCU_maroon), part = "header")
  
  html_file <- paste0(intermediary_dir,"s1_General Information_4_table4_Tuition Costs.html")
  save_table(tuition_summary, html_file)
}


