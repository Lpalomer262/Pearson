# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: 11_LINK_DATA
#' PROJECTS: PEARSON
#' DESCRIPTIONS: SOURCE
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE: 3/10/2020
#' R VERSION: 3.5.0
#' INPUT FILES
#' OUTPUT FILES
#' SECTION

source("./00_Source.R")
source("./01_Functions.R")

catalog <- read_rds("../../Input/rds/10_catalog.rds")
logs <- read_rds("../../Input/rds/10_logs.rds")

course_info <- catalog %>% 
  select(title, partner_name, program_name, program_type, subjects,
         #' QUANTITATIVE
         price, min_effort_hrs_wk, max_effort_hrs_wk, length_weeks) %>%
  full_join(
    logs %>% 
      group_by(course_title) %>%
      summarise(
        n = n(),
        distinct_id = length(unique(id))
      ) %>%
      ungroup(),
    by = c("title" = "course_title")
  ) 
  
#
logs_catalog <- logs %>% 
  left_join(catalog, by = c("course_id" = "key")) %>%
  mutate(ds_flag = str_detect(title, "Data Science\\:")) %>%
  write_rds("../../Input/rds/11_course_info.rds")

#' |title                                                          |    n|
#' |:--------------------------------------------------------------|----:|
#' |Data Science: Inference and Modeling                           | 4431|
#' |Data Science: Probability                                      | 6136|
#' |Data Science: Productivity Tools                               | 2503|
#' |Data Science: Visualization                                    | 7188|
#' |Data Science: Wrangling                                        | 1374|
#' 
#' |Agile Leadership Principles                                    | 4166|
#' |AI for Leaders                                                 | 2912|
#' |Computing in Python I: Fundamentals and Procedural Programming | 1729|
#' |Marketing Fundamentals: Who Is Your Customer?                  | 2140|


ds <- logs_catalog %>%
  dplyr::filter(ds_flag == TRUE) %>% #' REMOVED 10,947
  mutate(course_order = str_extract(course_id, "\\.[0-9]x\\+"),
         course_order = str_extract(course_order, "[0-9]"),
         course_order = factor(course_order),
         #' CREATE A COURSE SECTION
         course_topic = str_trim(str_remove_all(title, "Data Science\\: "), "both")) %>%
  write_rds("../../Input/rds/11_ds_cohort.rds")

#' |course_topic           |course_order |    n|
#' |:----------------------|:------------|----:|
#' |Visualization          |2            | 7188|
#' |Probability            |3            | 6136|
#' |Inference and Modeling |4            | 4431|
#' |Productivity Tools     |5            | 2503|
#' |Wrangling              |6            | 1374|

#' IN THE COURSE ID, YOU'LL FIND ANOTHER INDICATOR WHICH WOULD
#' SUGGEST THIS IS A DATA SCIENCE PROGRAM WITH SEQUENTIAL COURSES
#' THE DATA ALSO ALUDES TO THIS THIS LESS AND LESS STUDENTS TAKE
#' THE HIGHER ORDER CLASSES.

learner_info <- ds %>% 
  group_by(id) %>% #' 98 DISTINCT STUDENTS
  summarise(
    n = n(),
    first_enrollment = min(enrollment_created_timestamp, na.rm = TRUE)
  )

learner_course_info <- ds %>%
  group_by(id, course_title, course_order, unenrollment_end_within_date) %>% #' 98 DISTINCT STUDENTS
  summarise(
    n = n(),
    first_enrollment = min(enrollment_created_timestamp, na.rm = TRUE),
    first_activity = min(last_activity_date, na.rm = TRUE),
    last_activity = max(last_activity_date, na.rm = TRUE)
  )

#' EXAMPLES OF A STUDENT WHOM WE DONT HAVE ACTIVITY DATA FOR
#' MEANING THEY PASSES THE CLASS BEFORE DATA WAS COLLECTED
a <- ds %>% dplyr::filter(id == "421f08f2559b97ea07865112504bc2ec", 
                          course_order == 2)

b <- ds %>% dplyr::filter(id == "421f08f2559b97ea07865112504bc2ec", 
                          course_order == 3)

c <- ds %>% dplyr::filter(id == "421f08f2559b97ea07865112504bc2ec", 
                          course_order == 4)

#' SAME STUDENT, POST COLLECTING DATA
#' SHOWS THEIR DAILY PROGRESSION MEANING THAT WE CAN SHOW HOW MANY PROGRESS DAYS
#' IT TOOK THEM TO HAVE A PASSING GRADE
d <- ds %>% dplyr::filter(id == "421f08f2559b97ea07865112504bc2ec", 
                          course_order == 5) %>%
  arrange((record_date))

e <- ds %>% dplyr::filter(id == "421f08f2559b97ea07865112504bc2ec", 
                          course_order == 6) %>%
  arrange((record_date))



