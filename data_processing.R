library(dplyr) 
library(ggplot2)  
library(janitor)
library(tidyr)
library(stringr)
library(readxl)
#https://nces.ed.gov/programs/digest/d22/tables/dt22_211.60.asp
df_state <- data.frame(state.abb, state.name)

####ACFRs####
sd_acfr <- read.csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_4years.csv") %>% 
  select(-c(X, id))

unfunded <- sd_acfr %>% 
  group_by(state.abb, state.name, year) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate(net_net_PL = (net_pension_liability - net_pension_assets),
         net_net_OPEP = net_opeb_liability - net_opeb_assets) %>% 
  ungroup() %>% 
  select(state.name, state.abb, year, total_liabilities, net_opeb_liability, net_pension_liability,
         net_net_PL, net_net_OPEP) 


####Teacher and Staff count####
#https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=649124


teacher_ratio <- read.csv("data/ELSI_csv_export_638672808259753338916.csv", skip = 6) %>% 
rename(state.name = 1, 
       state.abb = 2) %>% slice(1:51) %>% 
  select(state.name, state.abb, contains("Pupil.Teacher.Ratio..State")) %>% 
  mutate(across(3:ncol(.), as.numeric)) %>% 
  pivot_longer(
    cols = contains("Pupil.Teacher.Ratio..State"),
    names_to = "teacher_ratio", 
    values_to = "teacher_ratio_value") %>% 
  mutate(year = str_extract(teacher_ratio, "(\\d{2})$"),
         year = as.numeric(paste0("20", year)),
         state.name = str_to_title(state.name)) %>% select(-teacher_ratio) 
  

staff <- read.csv("data/ELSI_csv_export_638672808259753338916.csv", skip = 6) %>% 
  rename(state.name = 1, 
         state.abb = 2) %>% 
  slice(1:51) %>% 
  select(state.name, state.abb, contains("Full.Time.Equivalent..FTE..Staff..State")) %>% 
  mutate(across(3:ncol(.), as.numeric)) %>% 
  pivot_longer(
    cols = contains("Full.Time.Equivalent..FTE..Staff..State"),
    names_to = "staff",
    values_to = "staff_value") %>% 
  mutate(year = str_extract(staff, "(\\d{2})$"),
         year = as.numeric(paste0("20", year)),
         state.name = str_to_title(state.name)) %>% select(-staff)

teacher_staff <- staff %>% left_join(teacher_ratio, by = c("year", "state.name", "state.abb")) %>% 
  filter(year >= 2020) %>% 
  mutate(state.abb = str_squish(state.abb), 
         state.name = str_squish(state.name))

teacher_staff_acfr <- teacher_staff %>% left_join(unfunded)
  

m_ <- lm(teacher_ratio_value ~ net_pension_liability + net_opeb_liability, 
         data = teacher_staff_acfr)
summary(m_)

m2 <- lm(staff_value ~ net_pension_liability + net_opeb_liability, 
         data = teacher_staff_acfr)
summary(m2)

####Local tax####
#https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=649124

#many other rev not included
#Local.Rev....from.Intermediate.Agencies..R2...State.Finance..
#Local.Rev....Summer.School.Fees..R1N...State.Finance..1999.00
#Local.Rev....Earnings.on.Investment..R1I...State.Finance..1999.00
#Local.Rev....Tuition.From.Individuals..R1E...State.Finance..1999.00


# revenue included
#Local.Rev....Local.Govt.Non.Property.Tax..R1D...State.Finance..1999.00
#Local.Rev....Local.Govt.Property.Tax..R1C...State.Finance..
#Local.Rev....Non.Property.Tax..R1B...State.Finance..
#Local.Rev....Property.Tax..R1A...State.Finance..
school_local_tax <- read.csv("data/ELSI_csv_export_6386726876775578533334.csv", skip = 6) %>% 
  rename(state.name = State.Name) %>% 
  select(state.name, 
         contains("Local.Rev....Property.Tax"),
         contains("Local.Rev....Non.Property.Tax"), 
         contains("Local.Rev....Local.Govt.Property"),
         contains("Local.Rev....Local.Govt.Non.Property")
         ) %>% 
  mutate(across(2:85, as.numeric)) %>% 
  pivot_longer(cols = 2:85, 
               names_to = "local_tax", 
               values_to = "value") %>% 
  mutate(year = str_extract(local_tax, "(\\d{2})$"),
         year = as.numeric(paste0("20", year)),
         state.name = str_to_title(state.name)) %>% 
  select(-2) %>% 
  group_by(state.name, year) %>% 
  summarise(tot_local_tax = sum(value, na.rm = TRUE)) %>% 
  filter(state.name %in% state.name)

####Teacher salary####

salary <- readxl::read_xls("data/tabn211.60.xls", skip = 2) %>% 
  #get constant dollar
  select(1, 14:17) %>% 
  slice(3:53) %>% 
  pivot_longer(col = 2:5, 
               names_to = "year", values_to = "salary") %>% 
  mutate(year = str_remove(year, "\\...*")) %>% 
  mutate(year = case_when(year == "2009-10" ~ 2010,
                          year == "2019-20" ~ 2020,
                          year == "2020-21" ~ 2021,
                          year == "2021-22" ~ 2022
                          )) %>% 
  rename(state.name = `...1`) %>% 
  left_join(df_state)
  



census <-readRDS("data/census_pop_by_category.RDS") %>% 
  filter(category == "State") %>% 
  select(-category)
####median_income####
median_income <- read_xlsx("data/Unemployment_median income.xlsx", skip = 4) %>% 
  filter(Area_Name %in% state.name) %>% 
  select(State, Area_Name, Median_Household_Income_2021) %>% 
  rename(state.name = Area_Name, 
         state.abb = State)


####Plan contribution####
plan_contribution <- read.csv("data/plan_contribution_data.csv") %>% 
  select(State, Fiscal.Year, Amortization.Rate, Employer.Contribution.Rate, Plan.Name) %>% 
  filter(str_detect(Plan.Name, "(?i)(teacher)|(teachers)|(school)|(TRS)|(Education)")) %>% 
  #filter(!str_detect(Plan.Name, "(?i)city")) %>% 
  #filter(!Plan.Name %in% c("Colorado PERA, School Division Fund", "Missouri Public School Retirement System", 
   #                  "Ohio School Employees Retirement System", 
   #                  "Washington Teachers Plan 2/3")) %>% 
  rename(state.name = State) %>% 
  #filter(Fiscal.Year %in% c(2020, 2021, 2022)) %>% 
  rename(year = Fiscal.Year) %>% 
  mutate(Amortization.Rate = str_remove(Amortization.Rate, "%"),
         Employer.Contribution.Rate = str_remove(Employer.Contribution.Rate, "%")) %>% 
  mutate(Amortization.Rate = as.numeric(Amortization.Rate), 
         Employer.Contribution.Rate = as.numeric(Employer.Contribution.Rate)) %>% 
  select(-Plan.Name)

####Combined data####
dm <- plan_contribution %>% 
  left_join(salary, by = c("state.name", "year"))  %>% 
  left_join(census, by = c("state.abb")) %>% 
  left_join(median_income) %>% 
  left_join(school_local_tax) %>% 
  filter(year >= 2020) %>% 
  mutate(rev_per_cap = tot_local_tax/population, 
         salary_median = salary/Median_Household_Income_2021)

####Models####
#####Salary ~ amor#####
m0_amor <- lm(salary ~ Amortization.Rate, 
         data = dm )
summary(m0_amor)

m1_amor <- lm(pct_salary_medianIncome ~ Amortization.Rate, 
         data = dm )
summary(m1_amor)

# For each 1% increase in Amortization.Rate, the salary is expected to increase by 169 dollars, 
#assuming population and Median_Household_Income_2021 remain constant. 

m1 <- lm(salary ~ Employer.Contribution.Rate, 
         data = dm )
summary(m1)

m2 <- lm(salary ~ Employer.Contribution.Rate + population + Median_Household_Income_2021, 
         data = dm )
summary(m2)

#####Revenue per capita ~ salary as a portion of median#####
m_rev0 <- lm(salary_median ~ rev_per_cap,
  data = dm)

summary(m_rev0)



#higher tax revenues may have more 
#flexibility to absorb pension costs without 
#significantly impacting teacher salaries? 

m_rev_cb <- lm(tot_local_tax ~ Employer.Contribution.Rate,
             data = dm)

summary(m_rev_cb)

#Rising contributions to CalSTRS 
#--> local property tax increases 
#--> and constrained district budgets, 
#affecting teacher salary negotiations.

ca <- dm %>% filter(state.name == "California")
m_ca <- lm(tot_local_tax ~ Employer.Contribution.Rate, d = ca)
summary(m_ca)

####List of teacher plan####
#reason: taking this file https://github.com/ReasonFoundation/Annual-Pension-Report/blob/main/reason_pension_data.xlsx

reason <- read.csv("data/state_contribution_data.csv")
reason %>% colnames() %>% sort()
#TODO: Run this 2: 


#Teacher salary ~ amortization rate (Pension debt leads to lower teacher salaries) +
#cost of living + median income of state
#TODO: find the controlling factors used by other papers.
#Teacher salary ~ total contribution rate  (Generous pensions eat up teacher salaries)

teacher_plans <- reason %>% select(State, Fiscal.Year, Amortization.Rate)


# TODO: which state does not have plan here?

  rename(state.abb = State) %>% 
  left_join(df_state)

#Revenue
revenue <- read_xlsx("data/tabn235.20_year2020-21.xlsx")
revenue <- read_xlsx("data/tabn235.30_year2019-20.xlsx")


# expenditure:
#Table 236.65.
#Current expenditures per pupil in fall enrollment in public elementary and secondary schools, 
#by state or jurisdiction: Selected school years, 1969-70 through 2020-21
epx <- read_xlsx("data/tabn236.65.xlsx")


#Table 236.75. Total and current expenditures per pupil in fall enrollment in public elementary 
#and secondary schools, 
#by function and state or jurisdiction: School year 2020-21		

read_xlsx("data/tabn236.75.xlsx") %>% 
  # TODO: select(suppourt services, student)
  View()




