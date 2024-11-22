library(dplyr) 
library(ggplot2)  
library(janitor)
library(tidyr)
library(stringr)

####ACFR####
sd_raw <- read.csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_4years.csv")

sd_acfr <- sd_raw %>% 
  
  #replace NA with 0
  mutate(net_opeb_liability = replace_na(net_opeb_liability, 0),
         net_pension_liability = replace_na(net_pension_liability,0),
          retire_debt = (net_opeb_liability + net_pension_liability)) %>% 
  
  #only get those with debt > 1
  filter(retire_debt >= 1) %>% 
  filter(revenues >= 0) %>% 
  
  mutate(nces_id = str_squish(ncesID), 
         state_abb = str_squish(state.abb)) %>% 
  select(id, nces_id, state_abb, name, year, retire_debt, revenues, expenses) %>% 
  
  arrange(year) %>% 
  group_by(state_abb, name) %>% 
  
  #create lag values
  mutate(revenues_lag = dplyr::lead(revenues, 1),
         revenues_lag = replace_na(revenues_lag, 0)) %>% 
  ungroup() %>% 
  mutate(retire_debt = replace_na(retire_debt, 0)) %>% 
  filter(revenues_lag != "Inf") %>% 
  drop_na(revenues_lag) %>% 
  filter(retire_debt != "Inf") %>% 
  drop_na(retire_debt)

#####revenues ~ debt#####
sd_acfr %>% filter(retire_debt) %>% View()

m_revenues <- lm(revenues_lag ~ retire_debt, data = sd_acfr)
summary(m_revenues)

# every dollar increase in retire_debt is associated with 48 cents increase in revenues the year after. 
#highly statistically significant.

#TODO: need to check log model too
#m_revenues <- lm(log(revenues_lag) ~ log(retire_debt), data = sd_acfr)
#summary(m_revenues)


#####debts increase over years for all#####
sd_acfr_wider <- sd_acfr %>% select(id, name, year, retire_debt) %>% 
  pivot_wider(names_from = year, values_from = retire_debt) %>% 
  rowwise() %>%
  mutate(
    #`2020_2021` = `2021` > `2020`,
    #`2021_2022` = `2022` > `2021`,
    #`2022_2023` = `2023` > `2022`,
    `2020_2023` = `2023` > `2020`
  ) %>%
  ungroup()

#TODO: need to investigate this. Whose debts keep going over years?
sd_acfr_wider %>% filter(`2020_2023` == TRUE) %>% View()

####NECES####
#download Nov 20, 2024: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=64912

nces <- read.csv("data/ELSI_csv_export_6386771175811766728585.csv", skip = 6) %>%
  pivot_longer(
    cols = -c(1:3, contains("Agency.ID")),
    names_to = c(".value", "year"), # Extract group and year
    names_pattern = "(.*)\\.(\\d{4})" # Matches "Group.Year"
  ) %>% 
  rename(name_nces = 1,
         state.name = 2,
         state.abb = 3,
         nces_id = 4) %>% 
  select(-c(contains("Total.Number"), contains("Lowest.Grade"), 
            contains("Highest.Grade"), contains("State.Name"))) %>% 
  clean_names() %>% 
  mutate(across(4:23, as.numeric)) %>% 
  mutate(nces_id = as.character(nces_id),
         nces_id = str_squish(nces_id), 
         state_abb = str_squish(state_abb)) %>% 
  drop_na() %>% 
  filter(year >= 2020) %>%
  mutate(staff_pupil = total_staff_district/ total_students_all_grades_excludes_ae_district)
  #filter(total_students_all_grades_excludes_ae_district >=85000)

nces %>% View()
####Combined data####
acfr_nces <- sd_acfr %>% 
  left_join(nces, by = c("nces_id", "state_abb", "year")) %>% 
  drop_na() %>% 

# create lag year for all
  arrange(year)  %>% 
  group_by(name) %>% 
  mutate(across(7:29, ~ dplyr::lead(.x, 1), .names = "{.col}_lag")  # Create lagged columns
    ) %>%
    ungroup() %>% 
  
  #clean to make sure log works
  filter(retire_debt > 1) %>% 
  filter(staff_pupil_lag > 0) %>% 
  filter(staff_pupil_lag != "Inf")
  
acfr_nces %>% View()
acfr_nces %>% colnames() %>% sort()
# [1] "elementary_school_counselor_district"                       "elementary_school_counselor_district_lag"                  
# [3] "full_time_equivalent_fte_teachers_district"                 "id"                                                        
# [5] "instructional_coordinators_district"                        "instructional_coordinators_district_lag"                   
# [7] "lea_administrative_support_staff_district"                  "lea_administrative_support_staff_district_lag"             
# [9] "lea_administrators_district"                                "lea_administrators_district_lag"                           
# [11] "librarians_media_specialists_district"                      "librarians_media_specialists_district_lag"                 
# [13] "media_support_staff_district"                               "media_support_staff_district_lag"                          
# [15] "name"                                                       "name_nces"                                                 
# [17] "nces_id"                                                    "other_guidance_counselors_district"                        
# [19] "other_guidance_counselors_district_lag"                     "other_support_services_staff_district"                     
# [21] "other_support_services_staff_district_lag"                  "paraprofessionals_instructional_aides_district"            
# [23] "paraprofessionals_instructional_aides_district_lag"         "pupil_teacher_ratio_district"                              
# [25] "pupil_teacher_ratio_district_lag"                           "retire_debt_1k"                                            
# [27] "school_administrative_support_staff_district"               "school_administrative_support_staff_district_lag"          
# [29] "school_administrators_district"                             "school_administrators_district_lag"                        
# [31] "school_psychologist_district"                               "school_psychologist_district_lag"                          
# [33] "secondary_school_counselor_district"                        "secondary_school_counselor_district_lag"                   
# [35] "state_abb"                                                  "student_support_services_staff_w_o_psychology_district"    
# [37] "student_support_services_staff_w_o_psychology_district_lag" "total_guidance_counselors_district"                        
# [39] "total_guidance_counselors_district_lag"                     "total_staff_district"                                      
# [41] "total_staff_district_lag"                                   "total_students_all_grades_excludes_ae_district"  

####staff per pupil; ~ debt####

#log model
m_staff_pupil <- lm(log(staff_pupil_lag) ~ log(retire_debt), data = acfr_nces)
summary(m_staff_pupil)
# 1 % increase in retirement debt is associated with 2.328% decrease in staff per pupil

m_pupil_teacher <- lm(log(pupil_teacher_ratio_district_lag) ~ log(retire_debt), data = acfr_nces)
summary(m_pupil_teacher)
#NOT the same effect with teacher
