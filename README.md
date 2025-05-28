**Background:**

This repository contains the codebase for automatically computing and generating the yearly FactBook publication for [North Carolina Central University (NCCU)](https://www.nccu.edu/). Within institutional research, a Factbook (sometimes referred to as a *Data Book* or *Institutional Data Profile*) is a comprehensive, annual or periodic report that presents key statistics and high-level summaries about a college or university. It serves as a central reference for internal planning, external reporting, and public transparency.

**Notes:**
1. The R Markdown file (*Factbook_generation_wrapper.Rmd*) acts as a wrapper for generating the Factbook and is the only file that needs to be executed.
2. Raw data files cannot be provided due to the sensitivity of the institutional data and therefore this repository cannot be used; rather, it demonstrates the steps taken for generating the publication.
3. University Financial information (*Factbook_generation_University_Finances.R*) with format `(paste0(university_finances_dir, "NCCU_FY_", year, "-", year+1, ".xlsx")` (e.g. *NCCU_FY_2023-2024.xlsx*).
4. The file *FactBook_function.R* contains an inventory of functions for specific processes during the computation and generation processes. These functions of often called throughout. 


**Usage:**
1. Can only be used by NCCU personnel, who have access to the appropriate institutional databases.
2. Download repository and execute the *Factbook_generation_wrapper.Rmd* file


**Output:**
1. Yearly Factbook publications are publicly avaiable on the Office of Institutuonal Research and Analysis (OIRA) [website](https://www.nccu.edu/oira/institutional-studies/archived-documents#nccu-fact-book).
2. An example of a Factbook generated from this codebase can be found [here](https://drive.google.com/file/d/1FuReAfEYk0ST3zgUdLVOuG4W6EKNn66E/view?usp=sharing).
3. This codebase replaces how Factbooks were previously generated at NCCU, which involved a laborious process of manually designing each page. An example of an old Factbook [here](https://drive.google.com/file/d/1xqC-WP--57FZVgV3SuYNFK36qhx6PW0S/view?usp=sharing).

