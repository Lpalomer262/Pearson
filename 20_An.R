# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: 20_ANALYSIS
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

ds <- read_rds("../../Input/rds/11_ds_cohort.rds")


#' distinct(ds, id) %>% nrow() #' 98
#' distinct(ds, id, course_title) %>% nrow() #' 282

ind_course <- ds %>%
  group_by(id, course_title, course_topic, course_order) %>%
  summarise(
    #' REGISTRATION
    min_enroll = as_date(min(enrollment_created_timestamp, na.rm = TRUE)),
    min_grade = min(current_grade, na.rm = TRUE),
    max_grade = max(current_grade, na.rm = TRUE),
    passed_timestamp = as_date(min(passed_timestamp, na.rm = TRUE)),
    min_record_date = min(record_date),
    max_record_date = max(record_date)
  ) %>%
  ungroup() %>%
  mutate_at(.vars = c("min_grade", "max_grade"), 
            .funs = ~ ifelse(is.infinite(.), NA, .)) %>%
  mutate(status_flg = ifelse(is.na(passed_timestamp), "Incomplete", "Complete"),
         complete_status = case_when(
           status_flg == "Incomplete" ~ NA_character_,
           min_enroll >= ymd("2019-09-03") ~ "Complete Record",
           min_enroll < ymd("2019-09-03") ~ "Incomplete Record"
         ),
         incomplete_status =
           case_when(
             status_flg == "Complete" ~ NA_character_,
             status_flg == "Incomplete"  & is.na(min_grade) & is.na(max_grade) ~ "Never Started",
             status_flg == "Incomplete"  & !is.na(min_grade) & !is.na(max_grade) ~ "Incomplete",
             TRUE ~ "ERROR - CHECK ADDITIONAL SCANARIOS"
           )) 


#' SUBCATAGORY FOR VISULIZATION
ind_course %>% 
  mutate(course_status = coalesce(complete_status, incomplete_status)) %>%
  select(id, course_title, course_title, course_order, course_status) %>%
  write_rds("../../Input/rds/20_course_status.rds")
  



#' |complete_status   |incomplete_status |  n|
#' |:-----------------|:-----------------|--:|
#' |Complete Record   |NA                | 44| #' SECTION B
#' |Incomplete Record |NA                | 74| #  SECTION A
#' |NA                |Incomplete        | 69| #' FOCUS OUR ATTENTION
#' |NA                |Never Started     | 95| #' TOUGH ETHICAL CALL



# SECTION A ---------------------------------------------------------------
#' WE CAN STILL LEARN FROM AN IMCOMPLETE RECORD.
#' WE KNOW WHEN THE STUDENT REGISTERED & WHEN THEY COMPLETED THE COURSE

#' [[QUESTION]] HOW MANY DAYS FROM REGISTRATION DOES IT TAKE A STUDENT TO COMPLETE A COURSE
days_to_complete <- ind_course %>%
  dplyr::filter(complete_status %in% c("Complete Record", "Incomplete Record")) %>% #' 118
  mutate(days_to_complete = as.numeric(passed_timestamp - min_enroll)) %>%
  write_rds("../../Input/rds/20_days_to_complete.rds")



# SECTION B ---------------------------------------------------------------


std_comp <- ind_course %>%
  dplyr::filter(complete_status %in% c("Complete Record")) #' 44
  

std_comp_link <- std_comp %>% 
  select(id, course_title, min_enroll, passed_timestamp) %>%
  mutate(passed_timestamp = passed_timestamp + 1) %>%
  #' MANUAL EDIT FOR ID d7daa212d4d606ec52ead047d3538bd1
  mutate(passed_timestamp = if_else(id == "d7daa212d4d606ec52ead047d3538bd1", 
                                     ymd("2019-11-26"), passed_timestamp))


ds_full <- sqldf(
    "
    SELECT A.* 
    FROM ds AS A 
      INNER JOIN std_comp_link AS B
      ON (A.ID = B.ID AND 
          A.COURSE_TITLE = B.COURSE_TITLE AND
          A.RECORD_DATE BETWEEN B.MIN_ENROLL AND B.PASSED_TIMESTAMP)
    ")


stopifnot(
  (distinct(ds_full, id) %>% nrow()) == 
    (distinct(std_comp_link, id) %>% nrow()))

ds_full <- ds_full %>% as_tibble()
  

course_complete_detail <- ds_full %>%
  #' SAME SUMMARY AS ABOVE IN SECTION A
  group_by(id, course_title, course_topic, course_order) %>%
  summarise(
    #' REGISTRATION
    min_enroll = as_date(min(enrollment_created_timestamp, na.rm = TRUE)),
    #' NAs - NUMBER OF DAYS TO START
    n = n(), #' SHOULD MATCH THE DIFFERENCE BETWEEN MIN AND MAX DATE
    na_grades = sum(is.na(current_grade)),
    nan_grades = sum(!is.na(current_grade)),
    #' GRADE PROGRESSION
    min_grade = min(current_grade, na.rm = TRUE),
    max_grade = max(current_grade, na.rm = TRUE),
    passed_timestamp = min(passed_timestamp, na.rm = TRUE),
    min_record_date = min(record_date),
    max_record_date = max(record_date),
    #' NUMBER OF DAYS ENTERED IN THE SYSTEM
    num_activity = length(unique(last_activity_date))
  ) %>%
  ungroup() %>%
  mutate_at(.vars = c("min_grade", "max_grade"), 
            .funs = ~ ifelse(is.infinite(.), NA, .)) %>%
  mutate(date_diff = as.numeric(max_record_date - min_record_date) + 1) %>%
  write_rds("../../Input/rds/20_course_complete_detail.rds")

