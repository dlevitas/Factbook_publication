params <- list()

## -----------------------------------------------------------------------------------------------------------------------------------------

factbook_filename_html <- paste0(intermediary_dir, "NCCU_Factbook_", year, "-", year+1, ".html")
factbook_filename_pdf <- paste0(intermediary_dir, "NCCU_Factbook_", year, "-", year+1, ".pdf")
table_of_contents_html <- paste0(intermediary_dir, "s0_Table of Contents.html")

eagle_facts_filename_html <- paste0(intermediary_dir, "Eagle_Facts_in_Brief_", year, "-", year+1, ".html")
eagle_facts_filename_pdf <- paste0(intermediary_dir, "Eagle_Facts_in_Brief_", year, "-", year+1, ".pdf")

if (file.exists(factbook_filename_pdf)) {
  file.remove(factbook_filename_pdf)
  file.remove(factbook_filename_html)
}

if (file.exists(table_of_contents_html)) {
  file.remove(table_of_contents_html)
}

if (file.exists(eagle_facts_filename_pdf)) {
  file.remove(eagle_facts_filename_pdf)
  file.remove(eagle_facts_filename_html)
}


## -----------------------------------------------------------------------------------------------------------------------------------------

### Set several variables pertaining to height (and width) of html components. Values are typically in pixels (px)
dpi <- 96 # from powershell cmd: Get-WmiObject -Class Win32_DesktopMonitor | Select-Object ScreenWidth, ScreenHeight, PixelsPerXLogicalInch, PixelsPerYLogicalInch

page_Width_in <- 8.5
page_height_in <- 11
page_width_px <- page_Width_in * dpi
# page_height_px <- page_height_in * dpi # Assumed total height for an A4 page in pixels
page_height_px <- 1000 # for now

header_footer_font_size <- 12
footer_height <- 20
header_height <- 30
header_padding_top <- 10
footer_padding_bottom <- header_padding_top
rectangle_height <- 10
table_header_height <- 18
img_padding_bottom <- 20

NCCU_photo_height <- 433 # in pixels

header_section_height <- header_height + header_padding_top

header_section_height <- header_height + header_padding_top
footer_section_height <- footer_height + footer_padding_bottom

# List HTML files in order by number
html_files <- grep("Eagle", 
                   list.files(path = intermediary_dir, pattern = "\\.html$", full.names = TRUE), 
                   invert = TRUE, value = TRUE
                   )
section_ids <- as.numeric(str_extract(basename(html_files), "(?<=s)\\d+"))
file_numbers <- as.numeric(str_extract(basename(html_files), "(?<=_)\\d+(?=_)"))
html_files <- html_files[order(section_ids, file_numbers)]

# Randomly sort the NCCU photo shelter files
photo_shelter_files <- list.files(path = photo_shelter_dir, pattern = ".*\\.html$", full.names = TRUE)
photo_shelter_files <- grep("NCCU_logo", photo_shelter_files, invert = TRUE, value = TRUE)
photo_shelter_files_indices <- seq(1, length(photo_shelter_files))
photo_shelter_files_indices <- sample(photo_shelter_files_indices, length(photo_shelter_files_indices), replace = FALSE)
photo_shelter_files <- photo_shelter_files[photo_shelter_files_indices]

# Count occurrences of each unique tale/figure name (substring following the final underscore)
extracted_substrings <- sub(".*_(.*)", "\\1", html_files)
substring_counts <- table(extracted_substrings)
substring_counts[paste0("Age - Fall ", year, ".html")] = 1 # Do this to space out the Age visualizations

# Initialize blank list of ordered files
combined_files <- c()
photo_shelter_counter <- 1
photo_shelter_use <- seq(1,100,3) # Only add picture ever 3 instance there isn't a pairing, otherwise the Factbook would become too large
unpair_counter <- 0
# Loop through each file in the original list
for (h in html_files) {
  combined_files <- c(combined_files, h)  # Add the current file to the modified list

  # Extract the enrollment number and check for corresponding figure/table
  name <- sub(".*_(.*)", "\\1", h)
  
  if (name == "Tuition Costs.html") {
    photo_shelter_file <- photo_shelter_files[photo_shelter_counter]
    photo_shelter_counter <- photo_shelter_counter + 1
    combined_files <- c(combined_files, photo_shelter_file)
  }

  # If a table exists but no figure add a specific placeholder file randomly after the table
  if (substring_counts[name] == 1 & !grepl(")", name) & !grepl("SAT|ACT|HS|Enrollment Category per IPEDS|New Transfers|Law School|Top 10 Departments (Undergraduate)|Top 10 Majors (Undergraduate)|Award Name|Award Type (Undergraduate)|Title Page", name) & !grepl("General Information", h)) {
    unpair_counter <-  unpair_counter + 1

    if (unpair_counter %in% photo_shelter_use | name %in% c("New Transfers.html", "Full-Time Equivalent.html", "Graduation.html", "Financial Aid (Undergraduate).html", "Highest Degree Earned.html", "Age Group.html")) { # Specify when to insert NCCU photo shelter images 
      photo_shelter_file <- photo_shelter_files[photo_shelter_counter]
      photo_shelter_counter <- photo_shelter_counter + 1
      combined_files <- c(combined_files, photo_shelter_file)
    }
  }
}

page_number <- 1
page_height <- page_height_px

# combined_files <- combined_files[1:11] # NOTE: COMMENT OUT (OR DELETE) WHEN DONE TESTING

info_matrix <- matrix(nrow = length(combined_files), ncol = 11)
colnames(info_matrix) <- c("index", "section", "filepath", "title_text", "title_label", "img_components_height", "padding_bottom", "available_height", "pos_id", "new_page", "page_number")

file_titles <- sub(".*_", "", combined_files)

### Go through combined files list and create a matrix of key information for matrix
previous_pos_ids <- c()
previous_new_pages <- c()
for (i in seq_along(combined_files)) {
  current_file <- combined_files[i]
  page_counter <- 1
  new_page <- "N"
  
  # Determine section name
  if (grepl("Title Page", current_file)) {
    section <- "Title Page"
  } else if (grepl("s1_General", current_file)) {
    section <- "General Information"
  } else if (grepl("s1_Eagle", current_file)) {
    section <- "Eagle Facts in Brief"
  } else if (grepl("s2_", current_file)) {
    section <- "New Student"
  } else if (grepl("s3_", current_file)) {
    section <- "Fall Enrollment"
  } else if (grepl("s4_", current_file)) {
    section <- "Degrees Awarded"
  } else if (grepl("s5_", current_file)) {
    section <- "Student Financial Aid"
  } else if (grepl("s6_", current_file)) {
    section <- "Employees"
  } else if (grepl("s7_", current_file)) {
    section <- "University Finances"
  } else if (grepl("s8_", current_file)) {
    section <- "End of Document"
  } else {
    section <- "N/A"
  }
  
  # Get necessary information for determine layout of Factbook
  if (grepl("Title Page", current_file)) {
    is_title_page <- "Y"
    title_text <- "NA"
    title_label <- "NA"
    img_components_height <- 0
    padding_bottom <- 0
    available_height <- 0
    pos_id <- 1
    new_page <- "Y"
    page_number <- page_number
  } else {
    is_title_page <- "N"
    
    result <- extract_file_text_info(current_file)
    title_text <- result$title_text
    title_label <- result$title_label
    
    img_height <- calculate_img_height(current_file, dpi)
    
    if (i == 1 | title_text %in% c("Tuition Costs", "Fall Enrollment", "Employees", "Student Success", "New Transfers", "Enrollment Category per IPEDS", "College", "Ethnicity & Race", "Department (Undergraduate)", "Department (Graduate)", "Award Name") | grepl("table|figure", current_file) && grepl("Age - Fall", current_file)) {
      pos_id <- 1
      if (grepl("Tuition Costs|New Transfers", current_file) | (grepl("Age - Fall", current_file) & grepl("figure", current_file))) {
        new_page <- "N"
      } else if (title_text == "Ethnicity & Race" & section %in% c("Employees", "Degrees Awarded")) {
        new_page <- "N"
      } else {
        new_page <- "Y"
      }
    } else if (grepl("photo", current_file)) {
      pos_id <- 2
    } else {
      if (previous_pos_ids[i-1] == 1) {
        if (grepl("Title Page", combined_files[i-1]) | previous_new_pages[i-1] == "Y") {
          pos_id = 1
        } else {
          pos_id = 2
        }
      } else if (previous_pos_ids[i-1] == 2) {
        pos_id <- 1
      } else {
        pos_id <- 2
      }
    }
    
    if (i == length(combined_files)) {
      other_img_height <- 0
    } else {
      other_img_index <- ifelse(pos_id == 1, i + 1, i - 1)
      other_img_height <- calculate_img_height(combined_files[other_img_index], dpi)
    }
    
    if (pos_id == 1) {
      img_components_height <- table_header_height + rectangle_height + img_height + img_padding_bottom
      other_img_components_height <- table_header_height + rectangle_height + other_img_height
    } else {
      img_components_height <- table_header_height + rectangle_height + img_height
      other_img_components_height <- table_header_height + rectangle_height + other_img_height + img_padding_bottom
    }
    
    available_height <- page_height - header_section_height - footer_section_height - img_components_height - other_img_components_height
    if (is.na(available_height)) {
      available_height <- -1
    }
    
    if (available_height <= 0) {
      if (pos_id == 1) {
        available_height <- page_height - header_section_height - footer_section_height - img_components_height
      } else {
        bleedover_space <- ((other_img_components_height / page_height) - floor(other_img_components_height / page_height)) * page_height
        check_height <- page_height - header_section_height - footer_section_height - bleedover_space
        if (check_height - NCCU_photo_height <= NCCU_photo_height) {
          available_height <- page_height - header_section_height - footer_section_height - img_components_height
        } else {
          # pass
        }
      }
    }
  
    padding_bottom <- img_padding_bottom
    if (pos_id == 1) {
      if (available_height > 100) {
        padding_bottom <- img_padding_bottom + (available_height - 100)
      }
    } else {
      padding_bottom <- 0
    }
    
    if (title_text %in% c("New Transfers", "Law School", "State", "Top 10 Departments (Undergraduate)", "Top 10 Majors (Undergraduate)", "Ethnicity & Race", "Citizenship", "Graduation", "Revenue", "Expenditures") | grepl(paste0("Age - Fall ", year), current_file)) {
      padding_bottom <- 0
    }
    if (title_text == "Age Group" & section == "Employees") {
      padding_bottom <- 0
    }
    if (title_text == "Retention") {
      padding_bottom <- 10
    }
    
    if (new_page == "N") {
      if (pos_id == 2 | previous_new_pages[i-1] == "N") {
        new_page <- "Y"
      }
    }
    if (title_text == "Ethnicity & Race" & section == "Degrees Awarded") {
      new_page <- "Y"
    }
    
    if (available_height <= 0) {
      page_counter <- ceiling(abs(available_height / page_height))
    }
  }
  
  # Remove NCCU photo placeholders if they follow a 2nd placed visualization (meaning they'd be first on page which we don't want)
  use_in_report <- "Y"
  if (i != 1) {
    if (grepl("photo", current_file) & previous_new_pages[i-1] == "Y") {
      use_in_report <- "N"
    }
  }
    
  previous_pos_ids <- c(previous_pos_ids, pos_id)
  previous_new_pages <- c(previous_new_pages, new_page)

  print(paste0(i, ": ", basename(current_file), ", title_text: ", title_text, ", pos_id: ", pos_id, ", new_page: ", new_page, ", page: ", page_number))
  
  if (use_in_report == "Y") {
    info_matrix[i, 1] <- i
    info_matrix[i, 2] <- section
    info_matrix[i, 3] <- combined_files[i]
    info_matrix[i, 4] <- title_text
    info_matrix[i, 5] <- title_label
    info_matrix[i, 6] <- img_components_height
    info_matrix[i, 7] <- padding_bottom
    info_matrix[i, 8] <- available_height
    info_matrix[i, 9] <- pos_id
    info_matrix[i, 10] <- new_page
    info_matrix[i, 11] <- page_number
  
    if (new_page == "Y") {
      page_number <- page_number + page_counter
    }
  }
}

info_df <- as.data.frame(info_matrix)
info_df <- info_df[rowSums(is.na(info_df)) != ncol(info_df), ] # Remove rows with all NA values

# Determine pages of specific sections for table of contents
table_of_contents <- info_df %>%
  filter(
    grepl("_0_", filepath)
  ) %>%
  mutate(
    section = str_extract(filepath, "(?<=s\\d?_).+?(?=_0_)")
  ) %>%
  mutate(
    page_number = as.character(as.numeric(page_number) + 1)
  )

# Generate table of contents page and place info into the df
output_file <- paste0(intermediary_dir, "s0_Table of Contents.html")
generate_table_of_contents_page(table_of_contents, output_file, bg_color)

table_of_contents_df <- table_of_contents[1,]
table_of_contents_df$filepath <- output_file
table_of_contents_df$section <- "Table of Contents"
table_of_contents_df$page_number <- "2"

info_df <- InsertRow(info_df, table_of_contents_df, RowNum = 2)
info_df$page_number <- c(info_df$page_number[1:2], as.character(as.numeric(info_df$page_number[3:nrow(info_df)]) + 1))
info_df$index <- c(info_df$index[1:2], as.character(as.numeric(info_df$index[3:nrow(info_df)]) + 1))

# Create a dataframe for the Eagle Facts in Brief (same as the General Information section essentially)
last_index <- max(which(info_df$section == "General Information"))
eagle_facts_df <- info_df[1:last_index, ]
eagle_facts_df <- eagle_facts_df[3:nrow(eagle_facts_df), ]
first_page_num <- as.numeric(eagle_facts_df$page_number[1])
eagle_facts_df$filepath[1] <- paste0(intermediary_dir, "s1_Eagle Facts in Brief_0_Title Page.html")
eagle_facts_df$page_number <- as.numeric(eagle_facts_df$page_number) - first_page_num + 1

## -------------------------------------------------------------------------------------------------------------------------------------------------

print("")
print("-----------------------------------")
print("Assembling HTML documents and converting to PDF format")
print("-----------------------------------")

# Create or clear the combined HTML file(s)
file.create(factbook_filename_html)
file.create(eagle_facts_filename_html)

### Some HTML components

# NCCU logo
NCCU_logo <- paste0(photo_shelter_dir, "NCCU_logo.html")

# rectangle above each image
html_rect_img_info <- paste0("<div class='rectangle' style='margin: 0; height: ", rectangle_height, "px;'></div>")

# table-header
html_table_header_info <- paste0("<div class='table-header' style='display: flex; justify-content: space-between; margin: 0; padding: 0; width: 100%; height: ", table_header_height, "px;'>")

# footer
footer_text <- paste0("North Carolina Central University OIRA Fact Book ", year, "-", year + 1)
html_footer_info <- paste0("<div class='footer' style='color: #5B6770; font-size: ", header_footer_font_size, "px; text-align: center; font-family: ", font_type, "; font-weight: bold; padding-bottom: ", footer_padding_bottom, "px; height: ", footer_height, "px;'>", footer_text, "</div>")

# page number
html_page_number_info_ref <- paste0("<div class='page-number' style='color: #485517; font-size: ", header_footer_font_size, "px; text-align: right; padding-right: 20px; padding-top: ", header_padding_top, "px; height: ", header_height, "px; font-weight: bold; font-family: ", font_type, ";'>")

df_list <- list(info_df, eagle_facts_df)

for (df in df_list) {
  
  if (grepl("Main", df$filepath[1])) {
    print("Converting FactBook from HTML to PDF")
    html_file <- factbook_filename_html
    pdf_file <- factbook_filename_pdf
  } else {
    print("Converting Eagle Facts in Brief from HTML to PDF")
    html_file <- eagle_facts_filename_html
    pdf_file <- eagle_facts_filename_pdf
  }

  # Append content from each HTML file to the combined HTML file
  for (i in 1:nrow(df)) {
    
    section <- df$section[i]
    filepath <- df$filepath[i]
    padding_bottom <- df$padding_bottom[i]
    page_num <- df$page_number[i]
    new_page <- df$new_page[i]
    pos_id <- df$pos_id[i]
    title_text <- df$title_text[i]
    title_label <- df$title_label[i]
    
    if (title_text == "photo") {
      title_text <- ""
      title_label <- ""
    }
    
    # Special case for Title Pages: Only add page number, skip other processes
    # if (section == "Title Page") {
    if (section %in% c("Title Page", "Table of Contents")) {
      
      # Ensure a new page starts
      cat("<div style='page-break-before: always;'></div>", file = html_file, sep = "\n", append = TRUE)
  
      # Optionally, wrap it in a page div for styling consistency
      cat("<div class='page'>", file = html_file, sep = "\n", append = TRUE)
      
      html_content <- readLines(filepath, warn = FALSE)
      
      if (section != "Table of Contents") {
        logo_html_content <- readLines(NCCU_logo, warn = FALSE)
        logo_html_content <- paste(logo_html_content, collapse = "\n")
        
        html_content <- gsub(
          paste0("<object class='html-content' data='", NCCU_logo, "' type='text/html'></object>"),
          logo_html_content,
          html_content
        )
      }
      
      cat(html_content, file = html_file, sep = "\n", append = TRUE)
      
      html_page_number_info <- paste0(html_page_number_info_ref, "Page ", page_num, "</div>")
      cat(html_page_number_info, file = html_file, sep = "\n", append = TRUE)
      
      # Close the page div
      cat("</div>", file = html_file, sep = "\n", append = TRUE)
      
    } else {
    
      if (pos_id == 1) {
        
        # Create a new page
        cat("<div style='page-break-before: always;'></div>", file = html_file, sep = "\n", append = TRUE)
        cat("<div class='page'>", file = html_file, sep = "\n", append = TRUE)
        
        # Add header
        html_header_info <- paste0("<div class='header' style='color: #485517; font-size: ", header_footer_font_size, "px; text-align: left; padding-left: 20px; padding-top: ", header_padding_top, "; font-family: ", font_type, "; font-weight: bold; height: ", header_height, "px;'>", section, "</div>")
        cat(html_header_info, file = html_file, sep = "\n", append = TRUE)
        
        # Add a container for the tables/figure
        cat("<div class='content-container'>", file = html_file, sep = "\n", append = TRUE)
      }
      
      # Wrap the rectangle and the content in a new container
      cat("<div class='table-wrapper'>", file = html_file, sep = "\n", append = TRUE)
      
      # Add the rectangle and title
      cat(
        html_table_header_info,
        sprintf(paste0("<span class='table-title' style='color: ", rich_maroon, "; font-weight: bold;'>%s</span>"), title_text),
        sprintf(paste0("<span class='table-label' style='color: ", black, "; font-weight: bold;'>%s</span>"), title_label),
        "</div>",
        html_rect_img_info,
        file = html_file, sep = "\n", append = TRUE
      )
      
      # Insert the HTML content
      html_content <- readLines(filepath, warn = FALSE)
      cat(paste0("<div class='table-container' style='margin: 0; padding-bottom: ", padding_bottom, "px;'>"), file = html_file, sep = "\n", append = TRUE)
      cat(html_content, file = html_file, sep = "\n", append = TRUE)
      cat("</div>", file = html_file, sep = "\n", append = TRUE)
      
      # Close the table-wrapper div
      cat("</div>", file = html_file, sep = "\n", append = TRUE)
      
      # Close the content-container if it's the end of a set
      if (pos_id == 2 || new_page == "Y" || i == nrow(df)) {
        cat("</div>", file = html_file, sep = "\n", append = TRUE)  # Close content-container
        
        # Add the footer and page number
        cat(
          html_footer_info,
          file = html_file, sep = "\n", append = TRUE
        )
        
        html_page_number_info <- paste0(html_page_number_info_ref, "Page ", page_num, "</div>")
        cat(
          html_page_number_info,
          file = html_file, sep = "\n", append = TRUE
        )
        
        # Close the page div
        cat("</div>", file = html_file, sep = "\n", append = TRUE)
      }
    }
    # Footer and page number style information
    footer_style <- c(
      paste0(
        "<style>",
        "body { margin: 0; padding: 0; background-color: ", bg_color, "; }",
        ".page { position: relative; margin-bottom: 0px; min-height: 100vh; height: auto; text-align: center; }",
        ".content-container { display: flex; flex-wrap: wrap; justify-content: center; padding: 0 50px; }",
        ".table-container { flex: 0 0 48%; text-align: center; margin: 20px; }",
        ".footer { position: absolute; bottom: 0; left: 0; right: 0; text-align: center; color: ", NCCU_gray, "; font-size: 12px; margin: 0; background-color: ", bg_color, "; }",
        ".page-number { position: absolute; top: 0px; right: 0px; width: 100%; text-align: right; }",
        ".rectangle { height: ", rectangle_height, "px; background-color: ", NCCU_gray, "; width: 100%; box-sizing: border-box; margin: 20px auto; }",
        ".table-header { margin: 0; padding: 0; width: 100%; }",
        ".table-header span { display: block; margin: 0; padding: 0; }",
        "</style>"
      )
    )
  
    # Write the footer and page number style information to the combined HTML file
    cat(footer_style, file = html_file, sep = "\n", append = TRUE)
  }
  
  # Convert the combined HTML file to PDF
  retry_chrome_print(html_file, pdf_file)
}
