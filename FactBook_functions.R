# This file contains all relevant functions needed for the FactBook generation code


# 1). Install & load packages
load_packages <- function() {
  packages <- c(
    "base64enc",
    "DataCombine",
    "data.table",
    "dplyr",
    "forcats",
    "flextable",
    "fs",
    "ggplot2",
    "haven",
    "httr",
    "magick",
    "officer",
    "openxlsx",
    "pagedown",
    "pdftools",
    "png",
    "purrr",
    "readr",
    "readxl",
    "rvest",
    "servr",
    "stringdist",
    "stringr",
    "tables",
    "tidyr",
    "tools",
    "webshot"
  )
  
  have <- packages %in% rownames(installed.packages())
  
  if (any(!have)) {
    install.packages(packages[!have], dependencies = TRUE, ask = FALSE)
  }
  
  # webshot::install_phantomjs() # This installs the PhantomJS binary needed by webshot. Unsure if this line is needed
  
  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }
}


# 2). Format for adding commas to thousandths place of large numbers
fmt <- function(x) {
  sprintf("%s", format(x, big.mark = ","))
}


# 3). Saves flextable objects to HTML format, accounting for occasionally odd formatting issues
save_table <- function(flextable_obj, output_html_file) {
  # Initial save
  save_as_html(x = flextable_obj, path = output_html_file)
  
  # Read the HTML file content back into R
  html_content <- readLines(output_html_file, warn = FALSE)
  
  # Remove the strange "x" that appears in top left corner of html
  cleaned_html_content <- gsub(pattern = ">\\s*x\\s*<", replacement = "><", x = html_content)
  
  # Remove the <h1 class="title toc-ignore"> </h1> tag (with extra spaces)
  cleaned_html_content <- 
    gsub(pattern = "<h1 class=\"title toc-ignore\">\\s*</h1>", replacement = "", x = cleaned_html_content)
  
  # Remove the <h2></h2> tag (with extra spaces)
  cleaned_html_content <- sub(pattern = "<h2></h2>", replacement = "", x = cleaned_html_content)
  
  # Write the cleaned HTML content back to the file
  writeLines(cleaned_html_content, con = output_html_file)
}


 # 4). Save ggplot objects to HTML format (with save to SVG format as intermediary step)
save_figure <- function(ggplot_obj, output_file_svg, output_file_html) {
  ggsave(output_file_svg, ggplot_obj, width = 2150, height = 1200, units ="px", dpi = "retina")
  
  # Create minimal HTML content with the SVG embedded
  svg_content <- readLines(output_file_svg)
  html_content <- sprintf(
    "<html>
     <head>
       <title>NCCU Fact Book</title>
     </head>
     <body>
       %s
     </body>
    </html>",
    paste(svg_content, collapse = "\n")  # Embed SVG content
  )
  
  # Write the minimal HTML content to a file
  writeLines(html_content, output_file_html)
  
  # Remove SVG (intermediary) file
  file.remove(output_file_svg)
}


# 5). Save jpg images to html format
save_jpg_to_html <- function(jpg_file, output_file_html) {
  
  aspect_ratio <- 1.5
  width <- 650
  height <- round(width/aspect_ratio)
  
  # Resize the image to reduce its size
  img <- image_read(jpg_file) %>%
    image_resize(paste0(width, "x", height)) %>%
    image_write(format = "jpeg")
  
  # Encode the resized image as base64
  base64_image <- base64encode(img)
  
  # Create an HTML file with the embedded image
  html_content <- paste0(
    "<html><body>",
    "<img src='data:image/jpeg;base64,", base64_image, "' ",
    "style='display: block; margin: auto; width: ", width, "px; height: ", height, "px;' />",
    "</body></html>"
  )
  
  # Save the HTML content to a file
  writeLines(html_content, output_file_html)
}


# 6). Calculate the height of HTML content
calculate_img_height <- function(html_file, dpi) {
  
  if (grepl("table", html_file)) {
    # Create a temporary file to save the screenshot
    temp_image <- paste0(intermediary_dir, "test.png")
    file.create(temp_image)
    
    # Take a screenshot of the specified element using the selector
    webshot::webshot(
      url = html_file,
      file = temp_image
    )
    
    # Read the image to get its dimensions
    img <- readPNG(temp_image, native = TRUE)
    height <- attr(img, "dim")[1]
    file.remove(temp_image)
  } else if (grepl("figure", html_file)) {
    # Parse the HTML file
    html_doc <- read_html(html_file)
    
    # Attempt to locate the SVG with the "svglite" class
    svg_element <- html_doc %>% html_node(".svglite")
    
    height_attr <- html_attr(svg_element, "height")
    
    # Convert dimensions to numeric and from pt to px
    height <- as.numeric(strsplit(height_attr, split="pt")[1]) * (dpi / 72)
  } else {
    # Parse the HTML file
    html_doc <- read_html(html_file)
    
    element <- html_doc %>% html_node("img")
    
    height_attr <- html_attr(element, "style")
    
    # Convert dimensions to numeric and from pt to px
    height <- as.numeric(sub(".*height: ([0-9.]+)px.*", "\\1", height_attr))
    
  }
  return(height)
}


# 7). Extract file text information
extract_file_text_info <- function(file) {
  filename <- basename(file)
  components <- strsplit(filename, "_")[[1]]  # Split by underscores
  figure_table_number <- str_extract(filename, "(figure|table)(\\d+)") %>% str_extract("\\d+")
  ending <- components[-(1:3)]
  substr(ending, 1,1) <- toupper(substr(ending, 1,1))
  if (grepl("table", filename)) {
    type <- "table"
  } else if (grepl("figure", filename)) {
    type <- "figure"
  } else {
    type <- "photo"
  }
  
  # Define the title based on the type
  # NCCU photo shelter images don't have the table/figure naming format, so don't give them titles or labels
  if (!(grepl("table", type, ignore.case = TRUE) || grepl("figure", type, ignore.case = TRUE))) {
    title_text <- "photo"
    title_label <- "photo"
  } else {
    title_text <- sub("\\.html$", "", ending)[2]
    title_label <- ifelse(grepl("table", type, ignore.case = TRUE), paste("Table", figure_table_number), paste("Figure", figure_table_number))
  }
  return(list(title_text = title_text, title_label = title_label))
}

# 8). Specifies ggplot theme information 
ggplot_theme_info <- function() {
  theme_info <- (
    theme_minimal() +
      # theme_fivethirtyeight() +
      theme(
        text = element_text(family = "Roboto"),
        plot.title = element_text(hjust = 0.5, size = 12), # Center the title
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(face = "bold"),
        legend.margin = margin(c(0, 0, 0, 0)),
        plot.background = element_rect("White"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(1,1,1,1),
        axis.line.x = element_line(color = "black", linewidth = 0.1),
        axis.line.y = element_line(color = "black", linewidth = 0.1),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black")
      )
  )
  return(theme_info)
}


# 9). Base flextable styling information
base_flextable <- function(df, anchor_color, f1 = "Source: Student Data Mart", f2 = NULL, vert_line_list, as_flex = TRUE) {
  
  if (as_flex) {
    table <- as_flextable(df)
  } else {
    table <- flextable(df)
  }
  
  table <- table %>%
    theme_booktabs(bold_header = TRUE) %>%
    bg(bg = anchor_color, part = "header") %>%
    bg(bg = "White", part = "body") %>%
    bg(i = nrow(df), bg = anchor_color, part = "body") %>%
    color(color = "White", part = "header") %>%
    color(color = black, part = "body") %>%
    color(i = nrow(df), color = "White", part = "body") %>%
    bold(i = nrow(df)) %>%
    align(align = "center", part = "all") %>%
    # align(j = 1, align = "left") %>%
    valign(valign = "center", part = "all") %>% # Doesn't seem to work
    # width(j = 1, 2.5, unit = "in") %>% # REFERENCE
    width(j = 1, 1.5, unit = "in") %>%
    font(fontname = font_type, part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    border_remove() %>%
    # hline_top(part = "header", border = fp_border(color = anchor_color, width = 0)) %>%
    # hline_bottom(part = "header", border = fp_border(color = anchor_color, width = 0)) %>%
    # hline(i = 1:(nrow(df) - 2), j = seq(numRowCols+1, ncol(df)), border = fp_border(color = black, width = 1)) %>%
    vline(j = vert_line_list[-length(vert_line_list)], border = fp_border(color = "white", width = 3)) %>%
    add_footer_lines(values = c(f1, f2)) %>%
    italic(part = "footer") %>%
    fontsize(size = 8, part = "footer") %>%
    line_spacing(space = 0, part = "footer") %>%
    set_table_properties(layout = "autofit")
  
  return(table)
}


# 10). Create line plots with a longitudinal variable and a categorical (2 levels, one combined/total)
generate_line_1cat_total_ggplot <- function(df, time_var, cat_var, title, xlabel) {
  
  if ("comp_term_academic_year" %in% colnames(df)) {
    y_axis_text <- "Degrees Awarded"
  } else {
    y_axis_text <- "Headcount"
  }
  
  # Specify plotting information
  ggplot_info_df <- bind_rows(
    data.frame(Level = "Total", LineType = "solid", Color = black),
    data.frame(
      Level = levels(df[[cat_var]]),
      LineType = "dotted",
      Color = c(NCCU_maroon, NCCU_gray)[seq_along(levels(df[[cat_var]]))]
    )
  )
  
  # Summarize counts across time and career variable levels/factors
  summary <- df %>%
    group_by(!!sym(time_var), !!sym(cat_var)) %>%
    summarise(count = n(), .groups = "drop")
  
  # Summarize total count across time
  total_summary <- summary %>%
    group_by(!!sym(time_var)) %>%
    summarise(
      count = sum(count),
      !!sym(cat_var) := "Total",  # Label for total line
      .groups = "drop"
    )
  
  # Combine the two summaries
  combined_summary <- bind_rows(summary, total_summary)
  
  # Ensure the career_var is a factor for consistent ordering in the plot
  combined_summary[[cat_var]] <- factor(combined_summary[[cat_var]], 
                                        levels = ggplot_info_df$Level)
  
  # Specify which labels we want to display in the plot
  summary_labels <- combined_summary %>%
    mutate(across(all_of(time_var), as.numeric)) %>%
    filter(
      !!sym(time_var) == min(!!sym(time_var), na.rm = TRUE) |
        !!sym(time_var) == max(!!sym(time_var), na.rm = TRUE)
    )
  
  # Get custom colors
  custom_colors <- setNames(ggplot_info_df$Color, ggplot_info_df$Level)
  
  # Create line chart of trends by category levels/factors across time
  p <- ggplot(combined_summary, aes(
    x = !!sym(time_var),
    y = count,
    color = !!sym(cat_var),
    group = !!sym(cat_var),
    linetype = !!sym(cat_var) # Use line type aesthetic
  )) +
    geom_line(linewidth = 1) +  # Draw lines for each group
    geom_point(aes(color = !!sym(cat_var)), size = 3, shape = 21, fill = "white") +
    geom_text(
      data = summary_labels,
      aes(label = fmt(count)),
      vjust = -0.8,
      size = 3,
      show.legend = FALSE,
      check_overlap = TRUE,
      color = "black",
      fontface = "bold"
    ) +
    scale_color_manual(values = custom_colors) +  # Set custom colors
    scale_linetype_manual(values = setNames(ggplot_info_df$LineType, ggplot_info_df$Level)) +  # Define line types
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    labs(
      x = xlabel,
      y = y_axis_text ,
      title = title
    ) +
    ggplot_theme_info()
  
  return(p)
}


# NEW STUDENTS

# 11). Create ggplot line plots for new student groups (NSG)
generate_line_ggplots_admissions <- function(df, time_var, apply_col, admit_col, enroll_col, group) {
  
  # Group by year and summarize
  summary <- df %>%
    group_by(!!sym(time_var)) %>%
    summarize(
      Applied = sum(!!sym(apply_col)),
      Admitted = sum(!!sym(admit_col)),
      Enrolled = sum(!!sym(enroll_col))
    )
  
  # Reshape the data for a proper legend and plot
  summary_long <- summary %>%
    pivot_longer(cols = c(Applied, Admitted, Enrolled),
                 names_to = "metric", values_to = "count")
  
  # Convert metric to factor with correct levels (Fixes legend color issue)
  summary_long$metric <- factor(summary_long$metric, levels = c("Applied", "Admitted", "Enrolled"))
  
  # Specify which labels we want to display in the plot
  summary_labels <- summary_long %>%
    group_by(metric) %>%
    filter(
      !!sym(time_var) == min(!!sym(time_var)) | !!sym(time_var) == max(!!sym(time_var))
    )
  
  # Create the plot using the LONG format data
  p <- ggplot(summary_long, aes(x = !!sym(time_var), y = count, color = metric, group = metric)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c(Applied = "#c59198", Admitted = "#a24e5a", Enrolled = "#8b2331")) +
    scale_x_continuous(breaks = unique(summary_long[[time_var]])) +
    geom_text(
      data = summary_labels,
      aes(label = fmt(count)),
      hjust = 0.5,
      vjust = -0.7,
      size = 3,
      show.legend = FALSE,
      color = "black",
      fontface = "bold"
    ) +
    labs(
      # title = paste0(group, " trends"),
      title = NULL,
      x = "Fall semester",
      y = "Headcount"
    ) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    ggplot_theme_info()
  
  return(p)
}


# 12). Create tables for new student groups (NSG). Pertains to admissions
generate_table_admissions <- function(df, df_column_for_row, cols_list, context) {
  
  df[[df_column_for_row]] <- factor(df[[df_column_for_row]])
  
  # Create a tabular object that can dynamically handle various columns
  # a). Wrap column names in backticks
  cols_expr <- paste(sapply(cols_list, function(x) paste0("`", x, "`")), collapse = " + ")
  
  # b). Wrap df_column_for_row in backticks if it contains spaces
  row_var <- paste0("`", df_column_for_row, "`")
  
  # c). Construct the formula dynamically, including Heading() and identity
  formula <- paste(row_var, "~", "(", cols_expr, ")", "* Heading() * identity")
  
  # d). Convert to formula object
  formula <- as.formula(formula)
  
  # e). Apply the tabular function with the dynamically created formula
  tab <- tabular(
    formula, 
    data = df
  )
  
  rowLabels(tab)[nrow(tab), 1] <- "Total"
  
  # Footer information
  f1 <- "Source: Student Data Mart"
  if (context %in% c("ACT", "SAT")) {
    f2 <- paste0("Note: Average and percentile calculations based on students for whom ", context, " was used in the admission decision")
  } else {
    f2 <- NULL
  }
  
  # Information for table style
  numRowCols <- 1
  numTabCols <- ncol(tab) + numRowCols
  vert_line_list <- seq(numRowCols, (length(cols_list) + 1))
  
  # Convert to flextable() format to enhance aesthetics
  table <- base_flextable(tab, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list) %>%
    align(i = which(df[[df_column_for_row]] == "Fall semester"), align = "center", part = "body") %>%
    hline(i = 1:nrow(tab), j = seq(numRowCols+1, numTabCols), border = fp_border(color = black, width = 1))
  
  return(table)
}


# 13). Generate flextable objects (with tabular object intermediary) with 1 categorical variable for rows and another categorical variable for columns
generate_table_1cat <- function(df, df_column_for_row, df_column_for_col, f2 = NULL) {
  # Create tabular() table, similar to SAS's PROC TABULATE functionality
  tab <- tabular(
    Heading() * as.name(df_column_for_row) * DropEmpty(empty = "--") + 1 ~
      Heading() * as.name(df_column_for_col) * (Heading("#") * 1 * Format(fmt()) + Heading("%") * Format(sprintf("%.1f"))*Percent("col")),
    data = df
  )
  
  rowLabels(tab)[nrow(tab), 1] <- "Total"
  
  # Footer information
  f1 <- "Source: Student Data Mart"
  f2 <- f2
  
  # # Information for table style
  numRowCols <- 1
  numTabCols <- ncol(tab) + numRowCols
  vert_line_list <- seq(numRowCols, (length(unique(df[[df_column_for_col]])) + 1) * 2, 2)
  
  # Convert to flextable() format to enhance aesthetics
  table <- base_flextable(tab, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list) %>%
    # align(j = 1, align = "left") %>% #NOTE: Should all be aligned center, per base_flextable
    hline(i = 1:(nrow(tab) - 2), j = seq(numRowCols+1, numTabCols), border = fp_border(color = black, width = 1))

  return(table)
}


# FALL ENROLLMENT

# 14). Create line plots where time_var == snapshot_term, cat1_var == variable of interest, and cat2_var == career
generate_line_ggplots <- function(df, time_var, cat1_var, cat2_var) {
  
  if (cat1_var == "fa_pell_offer_flag") {
    df <- df %>%
      filter(
        degree_seeking_flag == "Y",
        !(career == "Graduate" & fa_pell_offer_flag == "Yes")
      )
  }
  
  # Summarize enrollment counts across time and both categorical variables
  summary <- df %>%
    group_by(!!sym(time_var), !!sym(cat1_var), !!sym(cat2_var)) %>%
    summarise(Headcount = n(), .groups = "drop")
  
  # Combine the two categorical variables for the color aesthetic
  summary$combined_category <- interaction(summary[[cat1_var]], summary[[cat2_var]], sep = " - ")
  
  # Determine the last time point
  last_time_point <- df[[time_var]][length(df[[time_var]])]
  
  # Get the enrollment count at the last time point for each combined category
  end_values <- summary %>%
    filter(!!sym(time_var) == last_time_point) %>%
    arrange(desc(Headcount))
  
  # Reorder the levels of the combined_category based on the enrollment count at the last time point
  summary$combined_category <- factor(summary$combined_category, levels = end_values$combined_category)
  
  # Specify which labels we want to display in the plot
  summary_labels <- summary %>%
    mutate(across(all_of(time_var), as.numeric)) %>%
    filter(
      !!sym(time_var) == min(!!sym(time_var), na.rm = TRUE) |
        !!sym(time_var) == max(!!sym(time_var), na.rm = TRUE)
    )
  
  # Define custom colors for the cat2_var levels
  custom_colors <- c(NCCU_maroon, NCCU_gray)
  
  # Define linetypes based on the levels of cat1_var
  cat1_levels <- levels(df[[cat1_var]])
  linetype_mapping <- c("solid", "dotted")
  
  # Create a color mapping for the combined categories
  color_mapping <- sapply(levels(summary$combined_category), function(x) {
    if (grepl("Undergraduate", x)) {
      custom_colors[1]
    } else {
      custom_colors[2]
    }
  })
  
  # Create a line type mapping for the combined categories
  linetype_mapping_combined <- sapply(levels(summary$combined_category), function(x) {
    if (startsWith(x, cat1_levels[1])) {
      linetype_mapping[1]
    } else {
      linetype_mapping[2]
    }
  })
  
  # Create line chart of enrollment trends by the combined categories across time
  p <- ggplot(summary, aes(
    x = !!sym(time_var),
    y = Headcount,
    color = combined_category,
    linetype = combined_category,
    group = combined_category
  )) +
    geom_line(linewidth = 1, show.legend = TRUE) +
    geom_point(shape = 19, size = 3, show.legend = FALSE) +
    geom_text(
      data = summary_labels,
      aes(label = fmt(Headcount)),
      vjust = -0.8,
      size = 3,
      show.legend = FALSE,
      check_overlap = TRUE,
      color = "black",
      fontface = "bold"
    ) +
    labs(
      title = "",
      x = "Fall semester",
      y = "Headcount"
    ) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = linetype_mapping_combined) + 
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    guides(color = guide_legend(override.aes = list(linetype = linetype_mapping_combined))) +
    ggplot_theme_info()
  
  return(p)
}

# 15). Generate tabular tables with following structure: 2 categorical vars (2nd is career) and 1 time var (snapshot term)
generate_table_2cats <- function(df, df_column_for_row1, df_column_for_row2, df_column_for_col) {
  # Create tabular() table, similar to SAS proc tabulate functionality
  
  if (df_column_for_row1 == "fa_pell_offer_flag") {
    df <- df %>%
      filter(
        degree_seeking_flag == "Y",
        !(career == "Graduate" & fa_pell_offer_flag == "Yes")
      )
  } else if (df_column_for_row1 == "first_generation_fafsa_clean") {
    df <- df %>%
      filter(career == "Undergraduate")
  }
  
  tab <- tabular(
    Heading() * as.name(df_column_for_row1) * Heading() * as.name(df_column_for_row2) * DropEmpty(empty = "--") + 1 ~
      Heading() * as.name(df_column_for_col) * (Heading("#") * 1 * Format(fmt()) + Heading("%") * Format(sprintf("%.1f"))*Percent("col")),
    data = df
  )
  
  rowLabels(tab)[nrow(tab), 1] <- "Total"
  rowLabels(tab)[nrow(tab), 2] <- ""
  
  # Footer information
  f1 <- "Source: Student Data Mart"
  if (df_column_for_row1 == "fa_pell_offer_flag") {
    f2 <- "Note: Pell grant offers only applicable to Undergraduate degree-seeking students. Total numbers thus do not reflect full enrollment count"
  } else if (df_column_for_row1 == "major_1_cip_stem_flag") {
    f2 <- "Note: 'STEM' is an acronym for Science, Technology, Engineering, and Mathematics"
  } else if (df_column_for_row1 == "major_1_college") {
    f2 <- "Note: Several colleges have been phased out (e.g. UNC Exchange) whereas others have recently been added"
  } else if (df_column_for_row1 == "state_of_residence") {
    f2 <- "Note: 'Foreign' encompasses foreign students and those stationed outside U.S. border"
  } else if (df_column_for_row1 == "first_generation_fafsa_clean") {
    f2 <- "Note: First generation only applicable to Undergraduate students. Total numbers thus do not reflect full enrollment count"
  } else if (df_column_for_row1 == "online") {
    f2 <- "Note: Distinction only pertains to students' primary (1st) major"
  } else if (df_column_for_row1 == "student_fte") {
    f2 <- "Note: 'FTE' is an acronym for Full-Time Equivalent, a metric based on semester credit hours"
  } else if (df_column_for_row1 == "enrollment_status_ipeds") {
    f2 <- "Note: Transfer students are counted for Fall semester if matriculated in Summer"
  } else {
    f2 <- NULL
  }
  
  # Information for table style
  numRowCols <- 2
  numTabCols <- ncol(tab) + numRowCols
  vert_line_list <- seq(numRowCols, (length(past_years) + 1) * 2, 2)
  
  info <- as.matrix(tab)[,2]
  rowCol1_hline_indices <- c()
  for (i in 1:length(info)) {
    val <- info[i]
    if (val %in% c("", "All") || i == numRowCols + 1) {
      # pass
    } else {
      if (val == "Undergraduate") {
        rowCol1_hline_indices <- c(rowCol1_hline_indices, i-1)
      } else {
        prev_val <- info[i-1]
        if (val == "Graduate" && prev_val == "Graduate") {
          rowCol1_hline_indices <- c(rowCol1_hline_indices, i-1)
        }
      }
    }
  }
  
  # Convert to flextable
  table <- base_flextable(tab, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list) %>%
    # Add horizontal borders under each row in the body except for the row cols
    hline(i = 1:(nrow(tab) - 2), j = seq(numRowCols+1, numTabCols), border = fp_border(color = black, width = 1)) %>%
    # Add horizontal border under the last 2nd rowCol factor for the 1st rowCol
    hline(i = rowCol1_hline_indices - numRowCols, j = seq(1, numRowCols+1), part = "body", border = fp_border(color = black, width = 1))
  
  # Need to shrink the College table a bit since it just barely bleeds into a second page.
  if (df_column_for_row1 == "major_1_college") {
      table <- table %>% fontsize(size = 9, part = "all")
  }
    
  return(table)
}


# 16). Create table for Law School Enrollment
generate_table_law <- function(df, df_column_for_row, df_column_for_col) {
  # Create tabular() table, similar to SAS proc tabulate functionality
  
  tab <- tabular(
    Heading("Law") * as.name(df_column_for_row) * DropEmpty(empty = "--") + 1 ~
      Heading() * as.name(df_column_for_col) * (Heading() * 1 * Format(fmt())),
    data = df
  )
  
  rowLabels(tab)[nrow(tab), 1] <- "Total"
  
  # Footer information
  f1 <- "Source: Student Data Mart"
  f2 <- NULL
  
  # Information for sylying table
  numRowCols <- 1
  numTabCols <- ncol(tab) + numRowCols
  vert_line_list <- seq(numRowCols, (length(past_years) + 1))
  
  # Convert to flextable() format to enhance aesthetics
  table <- base_flextable(tab, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list) %>%
    align(j = 1, align = "left")
  
  return(table)
}


# 17). Create vertical bar plot for enrollment by age grouping (student_age_group). Only for current year.
generate_age_bar_ggplots <- function(df, age_grouping_var, career_var) {
  # Create the bar plot
  p <- ggplot(df, aes(x = !!sym(age_grouping_var), fill = !!sym(career_var))) +
    geom_bar(position = position_dodge2(preserve = "single"), aes(y = after_stat(count)), width = 0.7) +  # Set a fixed width for the bars
    geom_text(
      stat = "count",
      aes(label = after_stat(count), group = !!sym(career_var)),
      position = position_dodge(width = 0.8),  # Adjust position for text
      vjust = -0.5,  # Position text above the bars
      color = "black",
      fontface = "bold",
      size = 2.5
    ) +
    scale_fill_manual(
      values = c("Undergraduate" = NCCU_maroon, "Graduate" = NCCU_gray)
    ) +
    labs(x = "Age group", y = "") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
    ggplot_theme_info()
  
  return(p)
}


# 18). Vertical bar plot for single enrollment trends(Law School, Enrollment Status IPEDS)
generate_law_bar_ggplots <- function(df, time_var) {
  # Summarize the enrollment counts by year and reverse order
  enroll_summary <- df %>%
    group_by(!!sym(time_var)) %>%
    summarise(enrollment_count = n(), .groups = "drop") %>%
    mutate(!!time_var := forcats::fct_rev(factor(!!sym(time_var))))  # Reverse factor levels
  
  # Create the horizontal bar plot
  p <- ggplot(enroll_summary, aes(x = !!sym(time_var), y = enrollment_count)) +
    geom_bar(stat = "identity", fill = NCCU_maroon) +
    geom_text(aes(label = enrollment_count), 
              hjust = -0.1,
              color = "black", 
              fontface = "bold",
              size = 4) +
    coord_flip() +  # Flip to make it horizontal
    labs(x = "Fall semester", y = "") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12), # Center the title
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(face = "bold"),
      legend.margin = margin(c(0, 0, 0, 0)),
      plot.background = element_rect("White"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(color = "black"),
      axis.text.x = element_blank(),
      plot.margin = margin(1,1,1,1)
    )
  return(p)
}


# 19). Top N Horizontal bar plot ggplot (currently for top Departments, Majors, and Concentrations)
topN_horizontal_barplots <- function(df, df_column, topN) {
  
  # Only want Undergraduates for this
  df <- df %>%
    filter(career == "Undergraduate")
  
  if (df_column == "major_1_inst") {
    x_axis_label <- "Major"
  } else if (df_column == "major_1_department") {
    x_axis_label <- "Department"
  } else {
    x_axis_label <- "Unknown"
  }
  
  # Count the number of students in each department
  major_counts <- df %>%
    group_by(func_col = .data[[df_column]]) %>%
    summarize(N = n()) %>%
    arrange(desc(N)) %>%
    head(topN)
  
  
  # Create a horizontal bar plot
  p <- ggplot(data = major_counts, aes(x = reorder(func_col, N), y = N)) +
    geom_bar(stat = "identity", fill = NCCU_maroon) +
    geom_text(aes(label = N, y = N + 0.05 * max(N)),
              color = "black",
              size = 3,
              fontface = "bold",
              position = position_identity()
    ) +
    coord_flip() +  # Flip to make it horizontal
    labs(
      x = x_axis_label,
      y = ""
    ) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
    ggplot_theme_info() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  return(p)
}



# RETENTION AND GRADUATION (DEGREES AWARDED)

# 20). Generate Retention and Graduation summary data frames, which can then be converted to flextable objects in a separate function
retention_graduation_summary_df <- function(df, col_wildcard, new_col_wildcard) {
  # a). Summarize data
  summary <- df %>%
    filter(Student_Type == "First Time Bachelor's") %>%
    group_by(Cohort) %>%
    summarize(
      Headcount = n(),
      across(
        starts_with(col_wildcard), 
        list(
          Count = ~sum(. == 1, na.rm = TRUE), 
          Rate = ~round(mean(. == 1, na.rm = TRUE) * 100, 1)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    arrange(Cohort)
  
  # b). Rename columns for better display
  summary <- summary %>%
    # rename_with(~ gsub(paste0(col_wildcard, "(\\d+)_Count"), sprintf("%s \\1 (#)", new_col_wildcard), .)) %>%
    rename_with(~ gsub(paste0(col_wildcard, "(\\d+)_Count"), paste0(new_col_wildcard, " \\1 (#)"), .)) %>%
    rename_with(~ gsub(paste0(col_wildcard, "(\\d+)_Rate"), paste0(new_col_wildcard, " \\1 (%)"), .))

  # c). Reorder columns so that Count columns come before Rate columns
  count_columns <- grep("#", names(summary), value = TRUE)
  rate_columns <- grep("%", names(summary), value = TRUE)
  other_columns <- setdiff(names(summary), c(count_columns, rate_columns))
  
  # d). Rearrange the columns
  summary <- summary %>%
    select(Cohort, Headcount, all_of(count_columns), all_of(rate_columns), all_of(other_columns))
  
  # e). Add "Total" row at the bottom
  total_row <- summary %>%
    summarise(
      Cohort = "Total", 
      Headcount = sum(as.numeric(Headcount), na.rm = TRUE), # Keep as numeric for sum
      across(
        contains("#"), ~sum(as.numeric(.), na.rm = TRUE),
        .names = "{.col}"
      ),
      across(
        contains("%"), ~round(mean(as.numeric(.), na.rm = TRUE),1),
        .names = "{.col}"
      )
    )
  
  # f). Combine the "Total" row with the original data
  summary <- bind_rows(summary, total_row)
  
  # g). Replace NA and 0 with "--" and apply formatting for counts and rates
  summary <- summary %>%
    mutate(
      across(
        contains(c( "Headcount", "#", "%")), 
        ~ifelse(is.na(.) | . == 0, "--", fmt(as.numeric(.)))
      )
    )
}


# 21). Generate the tables for Retention and Graduation information
generate_ret_grad_tables <- function(df, f2 = NULL) {
  f1 <- "Source: Student Data Mart"
  
  # Information for table style
  numRowCols <- 2
  numTabCols <- ncol(df) - numRowCols
  vert_line_list <- seq(numRowCols, ncol(df))
  
  # Build flextable (I think here is where we would use build_flextable() )
  table <- base_flextable(df, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list, as_flex = FALSE) %>%
    align(j = 1, align = "left") %>%
    hline(i = 1:(nrow(df) - 2), j = seq(numRowCols+1, ncol(df)), border = fp_border(color = black, width = 1)) %>%
    fontsize(size = 8, part = "all") %>%  # Shrink text size for the entire table
    width(width = 1, unit = "in") %>%  # Adjust column width for all other columns
    padding(padding = 2)  # Adjust padding between cells
  
  return (table)
}


# STUDENT FINANCIAL AID

# 22). 
generate_table_from_summary <- function(df, f2 = NULL) {
  
  # 1). Footer information
  f1 <- "Source: Student Data Mart"
  f2 <- f2
  
  # 2). Information for table style
  n_rowLabels <- 2
  vert_line_list <- seq(1, (ncol(df)))
  
  info <- as.matrix(df)[,1]
  rowCol1_hline_indices <- c()
  for (i in 1:length(info)) {
    val <- info[i]
    if (i == 1 || val %in% c("", "Total")) {
      # pass
    } else {
      rowCol1_hline_indices <- c(rowCol1_hline_indices, i+1)
    }
  }
  
  # 3). Convert to flextable object
  table <- base_flextable(df, NCCU_maroon, f1 = f1, f2 = f2, vert_line_list, as_flex = FALSE) %>%
    # Add horizontal borders under each row in the body except for the row cols
    hline(i = 1:nrow(df), j = seq(n_rowLabels, ncol(df)), border = fp_border(color = black, width = 1)) %>%
    # Add horizontal border under the last 2nd rowCol factor for the 1st rowCol
    hline(i = rowCol1_hline_indices - n_rowLabels, j = seq(1, n_rowLabels+1), part = "body", border = fp_border(color = black, width = 1))
  
  return(table)
}


# 23). Plot for Aid Type
generate_line_ggplots_aidType <- function(df, color_mapping, time_var, flag1, flag2, flag3, title) {
  # Group by year and summarize
  summary <- df %>%
    filter(career == "Undergraduate") %>%
    group_by(!!sym(time_var)) %>%
    summarize(
      `Non-Pell grants/scholarships` = n_distinct(student_pidm[!!sym(flag1) == "Y"], na.rm = TRUE),
      `Federal Pell grants` = n_distinct(student_pidm[!!sym(flag2) == "Y"], na.rm = TRUE),
      `Federal loans` = n_distinct(student_pidm[!!sym(flag3) == "Y"], na.rm = TRUE)
    )
  
  # Reshape the data for a proper legend and plot
  summary_long <- summary %>%
    pivot_longer(cols = c(`Non-Pell grants/scholarships`, `Federal Pell grants`, `Federal loans`),
                 names_to = "metric", values_to = "count")
  
  latest_order <- summary_long %>%
    group_by(metric) %>%
    filter(!!sym(time_var) == max(!!sym(time_var))) %>%
    arrange(desc(count)) %>%
    pull(metric)
  
  # Convert metric to factor with correct levels (Fixes legend color issue)
  summary_long$metric <- factor(summary_long$metric, levels = unique(latest_order))
  
  summary_labels <- summary_long %>%
    group_by(metric) %>%
    filter(
      !!sym(time_var) == min(!!sym(time_var)) | !!sym(time_var) == max(!!sym(time_var))
    )
  
  # Create the plot using the LONG format data
  p <- ggplot(summary_long, aes(x = !!sym(time_var), y = count, color = metric, group = metric)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    # geom_point(aes(color = metric), size = 3, shape = 21, fill = "white") +  # Add points at each year
    scale_color_manual(values = color_mapping) +
    # scale_x_continuous(breaks = unique(summary_long[[time_var]])) +  # Show all years
    geom_text(
      data = summary_labels,
      aes(label = fmt(count)),
      hjust = 0.5,
      vjust = -0.7,
      size = 3,
      show.legend = FALSE,
      color = "black",
      fontface = "bold"
    ) +
    labs(
      title = title,
      x = "Academic Year",  # Set x-axis label here
      y = "Headcount"
    ) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    ggplot_theme_info()
}


# 24). Plot for Award Source
generate_line_ggplots_1cat <- function(df, color_mapping, cat_var, time_var, title) {
  
  if (cat_var == "award_source") {
    df <- df %>% filter(career == "Undergraduate")
  }
  
  # Prepare the data
  summary <- df %>%
    filter(!!sym(cat_var) %in% names(color_mapping)) %>%
    group_by(!!sym(time_var), !!sym(cat_var)) %>%
    summarise(unique_students = n_distinct(student_pidm), .groups = 'drop')
  
  latest_order <- summary %>%
    group_by(!!sym(cat_var)) %>%
    filter(!!sym(time_var) == max(!!sym(time_var))) %>%
    arrange(desc(unique_students)) %>%
    pull(!!sym(cat_var)) %>%
    unique()
  
  summary_labels <- summary %>%
    filter(
      !!sym(time_var) == min(!!sym(time_var)) | !!sym(time_var) == max(!!sym(time_var))
    )
  
  # Apply this order to the category variable (affects legend and color)
  summary[[cat_var]] <- factor(summary[[cat_var]], levels = latest_order)
  summary_labels[[cat_var]] <- factor(summary_labels[[cat_var]], levels = latest_order)
  
  # Create the plot
  p <- ggplot(summary, aes(x = !!sym(time_var), y = unique_students, color = !!sym(cat_var), group = !!sym(cat_var))) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    # geom_point(aes(color = !!sym(cat_var)), size = 3, shape = 21) +  # Add points at each year
    geom_text(
      data = summary_labels,
      aes(label = fmt(unique_students)),
      hjust = 0.5,
      vjust = -0.7,
      size = 3,
      show.legend = FALSE,
      color = "black",
      fontface = "bold"
    ) +
    scale_color_manual(values = color_mapping) +  # Map colors to award sources
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = scales::comma) +
    labs(
      x = "Academic Year",
      y = "Headcount",
      title = title
    ) +
    ggplot_theme_info()
  
  return (p)
}


# 25). Provide summary metric table in each section for the General Information section
summarize_metric <- function(df, metric) {
  df %>%
    count(!!sym(metric), name = "N") %>%
    mutate(
      Metric = metric,  
      `%` = round((N / sum(N)) * 100, 1)  
    ) %>%
    rename(Category = !!sym(metric)) %>%
    select(Metric, Category, N, `%`)
}


# 26). Convert all NCCU photo shelter JPEG pictures to HTML format
jpg_to_html_convert <- function(photo_shelter_dir) {
  jpg_files <- list.files(path = photo_shelter_dir, pattern = ".*\\.jpg$", full.names = TRUE)
  
  for (jpg_file in jpg_files) {
    output_html_file <- paste0(gsub("\\.[Jj][Pp][Gg]$", "", jpg_file), ".html")
    if (!file.exists(output_html_file)) {
      save_jpg_to_html(jpg_file, output_html_file)
    }
  }
}


#. 27). Generate Fact book section title HTML pages
generate_section_title_page <- function(section_info, directory, bg_color, section) {
  
  if (section %in% c("Eagle Facts in Brief", "General Information", "University Finances", "End of Document")) {
    html_fragment <- paste0("<div class='section-title-page' style='
      background-color:", bg_color, ";
      color: #862633;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      height: 100vh;
      font-family: Calibri, sans-serif;
      text-align: center;
    '>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
      <h1 style='font-size: 3em;'>", section_info$section, "</h1>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
    </div>
    ")
  } else if (section == "Degrees Awarded") {
    html_fragment <- paste0("
    <div class='section-title-page' style='
      background-color:", bg_color, ";
      color: #862633;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      height: 100vh;
      font-family: Calibri, sans-serif;
      text-align: center;
    '>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
      <h1 style='font-size: 3em;'>", section_info$section, "</h1>
      <p style='font-size: 1.2em; max-width: 80%;'>
        For more in-depth and interactive information,<br>
        please visit our corresponding dashboards:<br>
        <a href='", section_info$dashboard_url, "' target='_blank' style='color: #862633; text-decoration: underline;'><b>Degrees Awarded</b></a> and
        <a href=https://www.nccu.edu/oira/dashboards-institutional-data/retention-dashboard target='_blank' style='color: #862633; text-decoration: underline;'><b>Retention/Graduation</b></a>.
      </p>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
    </div>
    ")
    
  
  } else {
    html_fragment <- paste0("
    <div class='section-title-page' style='
      background-color:", bg_color, ";
      color: #862633;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      height: 100vh;
      font-family: Calibri, sans-serif;
      text-align: center;
    '>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
      <h1 style='font-size: 3em;'>", section_info$section, "</h1>
      <p style='font-size: 1.2em; max-width: 80%;'>
        For more in-depth and interactive information,<br>
        please visit our corresponding 
        <a href='", section_info$dashboard_url, "' target='_blank' style='color: #862633; text-decoration: underline;'><b>dashboard</b></a>.
      </p>
      <hr style='width: 75%; border-top: 5px solid #862633; margin: 30px 0;'>
    </div>
    ")
  }
  
  # Save the HTML fragment (not a full HTML page)
  writeLines(html_fragment, paste0(directory, section_info$section_id, "_", section_info$section, "_0_Title Page.html"))
}


#. 28). Generate Factbook main title HTML page
generate_main_title_page <- function(year, directory, photo_shelter_dir, bg_color) {
  
  output_file <- paste0(directory, "s0_Main Title Page.html")
  
  # Construct title texts
  title_text1 <- paste0(
    "<div><span style='font-size:20px;'><strong>", year, "&ndash;", year + 1, "</strong></span><br><br>",
    "<span style='font-size:60px; color: #862633;'><strong>FACT BOOK</strong></span></div>"
  )
  title_text2 <- paste0("<b>North Carolina Central University</b><br><i>Office of Institutional Research and Analysis</i><br>")
  
  # Read and embed logo content
  logo_html <- readLines(paste0(photo_shelter_dir, "NCCU_logo.html"), warn = FALSE)
  logo_html <- paste(logo_html, collapse = "\n")
  logo_html <- paste0("<div style='background-color:", bg_color, "; padding: 10px;'>", logo_html, "</div>")
  
  # Define styles
  style_block <- paste0(
    "<style>",
    "body { margin: 0; padding: 0; background-color: ", bg_color, "; font-family: Calibri; }",
    # ".page { position: relative; margin-bottom: 0px; min-height: 100vh; height: auto; text-align: center; }",
    ".logo-container { margin-bottom: 40px; }",
    # ".page-number { position: absolute; top: 0px; right: 0px; width: 100%; text-align: right; }",
    "</style>"
  )
  
  # Write to file
  cat("<html>", file = output_file, sep = "\n", append = FALSE)  # Add opening <html> tag
  cat("<head>", file = output_file, sep = "\n", append = TRUE)   # Start <head> section
  cat("<meta charset='UTF-8'>", file = output_file, sep = "\n", append = TRUE)  # Set character encoding
  cat(style_block, file = output_file, sep = "\n", append = TRUE)  # Add style block
  cat("</head>", file = output_file, sep = "\n", append = TRUE)  # Close <head> section
  cat("<body>", file = output_file, sep = "\n", append = TRUE)  # Start <body> section
  cat(paste0("<div class='main-title'>", title_text1, "</div>"), file = output_file, sep = "\n", append = TRUE)
  cat("<div class='logo-container'>", logo_html, "</div>", file = output_file, sep = "\n", append = TRUE)
  cat(paste0("<div class='main-title'>", title_text2, "</div>"), file = output_file, sep = "\n", append = TRUE)
  cat("</div>", file = output_file, sep = "\n", append = TRUE)
  cat("</body>", file = output_file, sep = "\n", append = TRUE)  # Close <body> section
  cat("</html>", file = output_file, sep = "\n", append = TRUE)  # Close <html> section
}

# 29). Generate Table of Contents page
generate_table_of_contents_page <- function(toc_items, output_file, bg_color) {
  toc_html <- paste0("
    <div class='toc-title-page' style='
      background-color:", bg_color, ";
      color: #862633;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      font-family: Calibri, sans-serif;
      text-align: center;
      # padding: 40px;
    '>
      <hr style='width: 80%; border-top: 5px solid #862633; margin: 30px 0;'>
      <h1 style='font-size: 3em;'>Table of Contents</h1>
      <ul style='list-style: none; padding: 0; font-size: 1.2em; max-width: 80%; width: 100%; text-align: left;'>
  ")
  
  # Loop through rows and add each TOC line with dot leaders
  for (i in seq_len(nrow(toc_items))) {
    section <- toc_items$section[i]
    page <- toc_items$page_number[i]
    toc_html <- paste0(toc_html, "
        <li style='display: flex; justify-content: space-between; padding: 4px 0; width: 100%;'>
          <span style='flex: 1; border-bottom: 1px dotted #862633;'>", section, "</span>
          <span style='margin-left: 10px;'>", page, "</span>
        </li>")
  }
  
  toc_html <- paste0(toc_html, "
      </ul>
      <hr style='width: 80%; border-top: 5px solid #862633; margin: 30px 0;'>
    </div>
  ")
  
  # Save as HTML fragment
  writeLines(toc_html, output_file)
}


# 30). Occasionally the html to pdf conversion final step fails due to a timing or loading issue. This function re-runs the code block if this occurs.
retry_chrome_print <- function(html_file, pdf_file, max_attempts = 3, wait_between = 5) {
  attempt <- 1
  success <- FALSE
  
  while (attempt <= max_attempts && !success) {
    message(paste0("Attempt ", attempt, " to print PDF..."))
    
    # Always clean previous servr sessions
    servr::daemon_stop()
    
    # Try to print
    tryCatch({
      pagedown::chrome_print(
        input = normalizePath(html_file),
        output = pdf_file,
        timeout = 200,
        wait = 2,
        verbose = 1
      )
      success <- TRUE
      message("✅ PDF successfully created.")
      servr::daemon_stop()
    }, error = function(e) {
      message(paste0("❌ Attempt ", attempt, " failed: ", e$message))
      servr::daemon_stop()
      attempt <- attempt + 1
      if (attempt <= max_attempts) {
        message(paste0("Waiting ", wait_between, " seconds before retrying..."))
        Sys.sleep(wait_between)
      }
    })
  }
  
  if (!success) {
    stop("Failed to generate PDF after ", max_attempts, " attempts.")
  }
}


