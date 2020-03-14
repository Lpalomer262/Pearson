# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: 21_VISULIZATION
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


sankey_theme <- theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        text = element_text(family = "Palatino"),
        legend.position = "bottom",
        plot.title = element_text(size = rel(2.0), hjust=0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = rel(1.0), hjust = 1),
        axis.text.y = element_text(size = rel(1.0)))+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()                                                                                           
  )


basic_theme <- theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        text = element_text(family = "Palatino"),
        legend.position = "bottom",
        plot.title = element_text(size = rel(2.0), hjust=0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = rel(1.0), hjust = 1),
        axis.text.y = element_text(size = rel(1.0)))+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()                                                                                           
  )


#labs(title = "The Distribution of Runners by Age", 
#     subtitle = subtitle, 
#     x = "Age", 
#     y = "Relative Frequencies") 



# LOAD RESULTS ------------------------------------------------------------


catalog <- read_rds("../../Input/rds/10_catalog.rds")
logs <- read_rds("../../Input/rds/10_logs.rds")

course_info <- read_rds("../../Input/rds/11_course_info.rds")
course_status <- read_rds("../../Input/rds/20_course_status.rds")


days_to_complete <- read_rds("../../Input/rds/20_days_to_complete.rds")

ds <- read_rds("../../Input/rds/11_ds_cohort.rds")

course_complete <- read_rds("../../Input/rds/20_course_complete_detail.rds")

#' STUDENT INFO
#' 
#' 57840205a037b79524bd7becb512f902
student_example <- ds %>% filter(id == "57840205a037b79524bd7becb512f902" & course_order == 2) %>%
  arrange(record_date) %>%
  select(current_grade, enrollment_created_timestamp,last_activity_date, passed_timestamp, record_date) %>%
  mutate_at(c("enrollment_created_timestamp","last_activity_date", "passed_timestamp"), as_date) %>%
  write_csv("../../Output/21_student_example.csv")


sankey_info <- course_info %>%
  select(id, course_title, program_name,
         ds_flag) %>%
  left_join(course_status, by = c("id", "course_title"))


ds_data_flow <- sankey_info %>% 
  count(ds_flag, course_title, course_status, program_name) %>%
  #' PROGRAM NAME:  DATA SCIENCE VS OTHERS
  mutate(program_name = 
           if_else(program_name == "Data Science",
                   "Data Science", "Other Programs")) %>%
  mutate(course_status = if_else(program_name == "Data Science", 
                                 course_status, "Not Evaluated")) %>%
  #' CLEAN COURSE TITLE INFORMATION
  mutate(course_title_upd = case_when(
    !(str_detect(course_title, "Data Science")) ~ "Other Programs",
    (str_detect(course_title, "Data Science"))  ~ 
      str_trim(str_remove(course_title, "Data Science\\:"), 'both'),
    TRUE ~ "PLEASE CHECK WORK"
  )) %>%
  mutate(course_title_upd = if_else(program_name == "Data Science", 
                                    course_title_upd, "Other Courses")) %>%
  #' FACTORIZE VARIABLE
  mutate(course_title_upd = 
           factor(course_title_upd, 
                  levels = c(
                    "Visualization",
                    "Probability",
                    "Inference and Modeling",
                    "Productivity Tools",
                    "Wrangling",
                    "Other Courses"
                  ))) %>%
  #' CHANGE STATUS NAME FOR VISULIZATION PURPOSE
  mutate(course_status = case_when(
    course_status == "Complete Record" ~ "Finished & Complete",
    course_status == "Incomplete Record" ~ "Finished & Incomplete",
    course_status == "Incomplete" ~ "Not Finished & Intervene",
    course_status == "Never Started" ~ "Never Started - Intervene",
    course_status == "Not Evaluated" ~ "Not Evaluated",
    TRUE ~ "PLEASE CHECK WORK"
  )) %>% 
  mutate(course_status = 
           factor(course_status, 
                  levels = c(
                    "Not Evaluated",
                    "Finished & Complete",
                    "Finished & Incomplete",
                    "Not Finished & Intervene",
                    "Never Started - Intervene"
                  ))) 
  


# SANKEY FLOW DIAGRAM -----------------------------------------------------
#' https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

#' UNIT TEST
is_alluvia_form(as.data.frame(ds_data_flow), axes = 1:3, silent = TRUE)


#' VISULIZATION 1
 ggplot(as.data.frame(ds_data_flow),
        aes(y = n, axis1 = program_name , axis2 = course_title_upd ,axis3 =course_status )) +
   geom_alluvium(aes(fill = course_title_upd, color = course_title_upd), width = 1/15) +
   geom_stratum(width = 1/12, fill = "black", color = "grey") +
   geom_label(stat = "stratum", infer.label = TRUE) +
   scale_x_discrete(limits = c("Program Type", "Course Title", "Student Status"), expand = c(.05, .05)) +
   labs(title = "EdX Programs, Data Flow", 
        subtitle = "Data Science Program Outweights All Others", 
        # x = "Relative Fields", 
        y = "Number of Observations")  +
   sankey_theme
   
 ggplot(as.data.frame(ds_data_flow %>% dplyr::filter(program_name == "Data Science")),
        aes(y = n, axis1 = program_name , axis2 = course_title_upd ,axis3 =course_status )) +
   geom_alluvium(aes(fill = course_title_upd, color = course_title_upd), width = 1/15) +
   geom_stratum(width = 1/12, fill = "black", color = "grey") +
   geom_label(stat = "stratum", infer.label = TRUE) +
   scale_x_discrete(limits = c("Program Type", "Course Title", "Student Status"), expand = c(.05, .05)) +
   labs(title = "Data Science, Data Flow", 
        subtitle = "Data Science Program Outweights ALl Others", 
        # x = "Relative Fields", 
        y = "Number of Observations")  +
   sankey_theme



# SECTION A ---------------------------------------------------------------
days_to_complete %>%
  summarise(
    days_to_complete_mn = mean(days_to_complete),
    days_to_complete_med = median(days_to_complete)
  ) %>%
  gather()

days_to_complete %>%
  ggplot(aes(days_to_complete)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  expand_limits(x = 0, y = 0) +
  #' ADD VERTICAL LINES AT A FEW MONTHS
  geom_vline(aes(xintercept = median(days_to_complete)),
             color="blue", linetype="solid", size = 1) + 
  labs(title = "Phase I - Median Days to Complete Course", 
       subtitle = "As More Days Pass, Students are Less Likily to Complete Course ", 
       x = "Density", 
       y = "Days to Complete")  +
  basic_theme


days_to_complete %>%
  group_by(course_topic) %>%
  summarise(
    topic_mn = median(days_to_complete, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  #' FACTORIZE VARIABLE
  mutate(course_topic = 
           factor(course_topic, 
                  levels = c(
                    "Visualization",
                    "Probability",
                    "Inference and Modeling",
                    "Productivity Tools",
                    "Wrangling",
                    "Other Courses"
                  ))) %>%
  ggplot(aes(course_topic, topic_mn)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Phase I - Median Days to Complete Course by Course Topic", 
       subtitle = "Tougher Course Yield Less Than Average Days to Complete.", 
       x = "Course Topics", 
       y = "Days to Complete")  +
  basic_theme




# SECTION B ---------------------------------------------------------------

days_to_complete %>%
  dplyr::filter(complete_status == "Complete Record") %>%
  summarise(
    days_to_complete_mn = mean(days_to_complete)
  ) %>%
  gather()

days_to_complete %>%
  dplyr::filter(complete_status == "Complete Record") %>%
  ggplot(aes(days_to_complete)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  expand_limits(x = 0, y = 0) +
  #' ADD VERTICAL LINES AT A FEW MONTHS
  geom_vline(aes(xintercept = median(days_to_complete)),
             color="blue", linetype="dashed", size = 1) +
  labs(title = "Phase II - Median Days to Complete Course for Complete Cases", 
       subtitle = "Average Number of Days is Lower in Phase II than Phase I", 
       x = "Days to Complete", 
       y = "Density")  +
  basic_theme





#' STATISTICS NEEDED FOR RESULTS
course_complete %>%
  summarise(
    n_mn = mean(n), #' STILL LOOKING AT THE TIME 
    date_diff_mn = mean(date_diff),
    activity_mn = mean(num_activity), #' BEST STATISTIC
    days_to_start_med = median(na_grades),
    days_to_start_mn = mean(na_grades),
    days_to_complete_mn = mean(nan_grades),
    
    min_grade_mn = mean(min_grade),
    max_grade_mn = mean(max_grade)
  ) %>%
  gather()


course_complete %>%
  ggplot(aes(na_grades)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  expand_limits(x = 0, y = 0) +
  #' ADD VERTICAL LINES AT A FEW MONTHS
  geom_vline(aes(xintercept = median(na_grades)),
             color="blue", linetype="solid", size = 1) +
  labs(title = "Phase II - Median Days to Start Course", 
       subtitle = "A Student Typically Starts a Course 2.5 After Enrollment", 
       x = "Number of Days to Start the Course", 
       y = "Density")  +
  basic_theme




course_complete %>%
  ggplot(aes(num_activity)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  expand_limits(x = 0, y = 0) +
  #' ADD VERTICAL LINES AT A FEW MONTHS
  geom_vline(aes(xintercept = median(num_activity)),
             color="blue", linetype="solid", size = 1) + 
  labs(title = "Phase II - Median Days to Complete Course", 
       subtitle = "Based on Peaks, Students are Likily to Complete Courses on Weekends", 
       x = "Number of Activiites to Complete Course", 
       y = "Density")  +
  basic_theme

