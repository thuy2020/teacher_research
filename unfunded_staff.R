library(dplyr) 
library(ggplot2)  
library(janitor)
library(tidyr)
library(stringr)

####ACFR####
sd_raw <- read.csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_4years.csv")

sd_acfr <- sd_raw %>% 
  mutate(net_opeb_liability = replace_na(net_opeb_liability, 0),
         net_pension_liability = replace_na(net_pension_liability,0),
    retire_debt_1k = (net_opeb_liability + net_pension_liability)/1000) %>% 
  mutate(nces_id = str_squish(ncesID), 
         state_abb = str_squish(state.abb),
         state.name = str_squish(state.name)) %>% 
  select(id, nces_id, state_abb, name, year, retire_debt_1k, revenues, expenses) %>% 
  filter(retire_debt_1k >= 1) %>% 
  arrange(year) %>% 
  group_by(state_abb, name) %>% 
  mutate(revenues_lag = dplyr::lead(revenues, 1)) %>% 
    ungroup()

wider <- sd_acfr %>% select(id, name, year, retire_debt_1k) %>% 
  pivot_wider(names_from = year, values_from = retire_debt_1k) %>% 
  rowwise() %>%
  mutate(
    #`2020_2021` = `2021` > `2020`,
    #`2021_2022` = `2022` > `2021`,
    #`2022_2023` = `2023` > `2022`,
    `2020_2023` = `2023` > `2020`
  ) %>%
  ungroup()

#wider %>% filter(`2020_2023` == FALSE) %>% View()

sapply(sd_acfr, class)
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
  
nces

sapply(nces, class)
####Combined data####
acfr_nces <- sd_acfr %>% 
  left_join(nces, by = c("nces_id", "state_abb", "year")) %>% 
  drop_na() %>% 

# create lag year 
  arrange(year)  %>% 
  group_by(name) %>% 
  mutate(across(7:29, ~ dplyr::lead(.x, 1), .names = "{.col}_lag")  # Create lagged columns
    ) %>%
    ungroup()
  
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

#revenues ~ , retire_debt_1k

m <- lm(revenues_lag ~ retire_debt_1k, data = sd_acfr)
summary(m)


m1 <- lmer(revenues ~ retire_debt_1k  + (1 | name), data = sd_acfr)
summary(m1)

staff_data <- acfr_nces %>% select(staff_pupil_lag, retire_debt_1k) %>% filter(!is.na(staff_pupil_lag)) 

# fix-effect, estimating coef for each district. SD > 85000 --> negative, significant
m0 <- lm(log(staff_pupil_lag) ~ log(retire_debt_1k), data = staff_data)
summary(m0)

# fix-effect, W/O estimating coef for each district
m1 <- lm(school_administrative_support_staff_district_lag ~ retire_debt_1k, 
         data = acfr_nces)
summary(m1)

#linear mixed-effects model: district-specific variability without estimating coefficients for every district

library(lme4)
m1 <- lmer(other_guidance_counselors_district_lag ~ retire_debt_1k  + (1 | name), data = acfr_nces)
summary(m1)


library(plm)
pdata <- pdata.frame(acfr_nces, index = c("name", "year"))

m0 <- plm(instructional_coordinators_district_lag ~ retire_debt_1k, data = pdata, model = "within")
summary(m0)

m0_re <- plm(lea_administrators_district_lag ~ retire_debt_1k, data = pdata, model = "random")
summary(m0_re)

