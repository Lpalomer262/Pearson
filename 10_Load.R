# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: 10_LOAD_DATA
#' PROJECTS: PEARSON
#' DESCRIPTIONS: SOURCE
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE: 3/9/2020
#' R VERSION: 3.5.0
#' INPUT FILES
#' OUTPUT FILES
#' SECTION


source("./00_Source.R")
source("./01_Functions.R")

catalog <- read_csv("../../Input/raw/ex-catalog.csv", 
                 col_names = TRUE, col_types = cols(.default = 'c')) %>%
  rename_all(~ str_to_lower(.)) %>%
  #' ADDITIONAL COLUMN NAME CLEANING
  rename_all(~ str_replace_all(., "\\_\\_", "_")) %>%
  rename_all(~ str_replace_all(., "\\_$", "")) %>%
  #' CLEAN DATE FIELDS
  mutate_at(.vars = c("start", "end"), ymd) %>%
  #' FORMAT NUMERIC FIELD
  mutate_at(.vars = c("price", "min_effort_hrs_wk", "max_effort_hrs_wk", "length_weeks"), as.numeric) %>%
  #' CLEAN "Data Science: Productivity Tools" COURSE TITLE
  mutate(title = if_else(str_detect(title, "Productivity"), "Data Science: Productivity Tools", title)) %>%
  write_rds("../../Input/rds/10_catalog.rds")
  

logs <- read_csv(file = "../../Input/raw/ex-student-logs.csv", 
                 col_names = TRUE, col_types = cols(.default = 'c'), na = c("NA")) %>% 
  rename_all(~ str_to_lower(.)) %>%
  #' CLEAN DATE FIELDS
  mutate_at(.vars = c("enrollment_created_timestamp", "user_account_creation_timestamp"), ymd_hms) %>%
  mutate_at(.vars = c("last_activity_date", "record_date"), mdy) %>%
  #' FORMAT NUMERIC FIELD
  mutate_at(.vars = c("current_grade"), as.numeric) %>%
  #' CLEAN "Data Science: Productivity Tools" COURSE TITLE
  mutate(course_title = if_else(str_detect(course_title, "Productivity"), "Data Science: Productivity Tools", course_title)) %>%
  write_rds("../../Input/rds/10_logs.rds")



# CATALOG INFORMATION -----------------------------------------------------
count(catalog, title) #' UNIQUE ACROSS ALL PROGRAMS
#' INCLUDES ONE EXTRA CLASS, R BASICS
count(catalog, partner_name)
  #' |partner_name                    |  n|
  #' |:-------------------------------|--:|
  #' |Babson College                  |  2|
  #' |Georgia Institute of Technology |  1|
  #' |Harvard University              |  6|
  #' |University of Maryland System   |  1|
#' POTENTIALL MAKE A BINARY ACROSS UNIVERSITIES

count(catalog, program_type)
  #' |program_type             |  n|
  #' |:------------------------|--:|
  #' |Non-Program              |  1|
  #' |Professional Certificate |  8|
  #' |XSeries                  |  1|


count(catalog, program_name)
  #' |program_name                                    |  n|
  #' |:-----------------------------------------------|--:|
  #' |Agile Project Management                        |  1|
  #' |Business Principles and Entrepreneurial Thought |  1|
  #' |Data Science                                    |  6|
  #' |Introduction to Python Programming              |  1|
  #' |Non-Program                                     |  1|
  

count(catalog, subject)
  #' |subjects                                                |  n|
  #' |:-------------------------------------------------------|--:|
  #' |Business & Management                                   |  2|
  #' |Computer Science                                        |  1|
  #' |Data Analysis & Statistics, Computer Science            |  5|
  #' |Data Analysis & Statistics, Math                        |  1|
  #' |Engineering, Business & Management, Philosophy & Ethics |  1|



# STUDENT LOGS ------------------------------------------------------------
count(logs, course_title)
#' |course_title                                                   |    n|
#' |:--------------------------------------------------------------|----:|
#' |Data Science: Visualization                                    | 7188|
#' |Data Science: Probability                                      | 6136|
#' |Data Science: Inference and Modeling                           | 4431|
#' |Agile Leadership Principles                                    | 4166|
#' |AI for Leaders                                                 | 2912|
#' |Data Science: Productivity Tools                               | 2503|
#' |Marketing Fundamentals: Who Is Your Customer?                  | 2140|
#' |Computing in Python I: Fundamentals and Procedural Programming | 1729|
#' |Data Science: Wrangling                                        | 1374|


summary(logs$current_grade)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 0.00    0.37    0.84    0.67    0.96    1.00   12373 

count(logs, is.na(enrollment_created_timestamp)) #' :)
count(logs, is.na(last_activity_date))
 #' |is.na(last_activity_date) |     n|
 #' |:-------------------------|-----:|
 #' |FALSE                     | 32402|
 #' |TRUE                      |   177| STUDENT WHO OPTED OUT

count(logs, is.na(passed_timestamp))
 #' |is.na(passed_timestamp) |     n|
 #' |:-----------------------|-----:|
 #' |FALSE                   | 13027|
 #' |TRUE                    | 19552|


count(logs, progress_status)
  #' |progress_status |     n|
  #' |:---------------|-----:|
  #' |In Progress     | 16136|
  #' |Passed          | 11745|
  #' |NA              |  4698|

count(logs, user_country_code, sort = TRUE)
  #' |user_country_code |     n|
  #' |:-----------------|-----:|
  #' |IN                | 13678| #' INITAL PROGRAM WAS IN INDIA
  #' |ZA                |  9635| #' SECOND PROGRAM WAS IN SOUTH AFRICA
  #' |AE                |  5418| #' ARAB EMIRATES
  #' |TR                |  1708| #' TURKEY
  #' |LK                |   273| #' SRI LANKA?
  #' |TZ                |   273| #' TANZANIA
  #' |AL                |   182| #' ALBANIA
  #' |IL                |   182| #' ISRIAL
  #' |NA                |   161| ??
  #' |BA                |    91| #' BOSNIA