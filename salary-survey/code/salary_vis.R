# Set-up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(stringdist)

library(Cairo)
library(patchwork)
library(extrafont)
library(ggplot2)
library(ggtext)
library(ggfx)
library(scales)
library(fontawesome)
library(viridis)

# Read in data from TidyTuesday
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
salary_data <- tuesdata$survey

update_geom_defaults("text", list(family = "Overpass"))

# Data cleaning ---------------------------------------

us_salary <- salary_data %>%
  filter(currency=="USD") %>%
  mutate(industry = str_to_title(industry, locale = "en"))

industry_ops <- c("Accounting, Banking & Finance",
                  "Agriculture or Forestry",
                  "Art & Design",
                  "Business or Consulting",
                  "Computing or Tech",
                  "Education (Primary/Secondary)",
                  "Education (Higher Education)",
                  "Engineering or Manufacturing",
                  "Entertainment",
                  "Government and Public Administration",
                  "Health care",
                  "Hospitality & Events",
                  "Insurance",
                  "Law",
                  "Law Enforcement & Security",
                  "Leisure, Sport & Tourism",
                  "Marketing, Advertising & PR",
                  "Media & Digital",
                  "Nonprofits",
                  "Property or Construction",
                  "Recruitment or HR",
                  "Retail",
                  "Sales",
                  "Social Work",
                  "Transport or Logistics",
                  "Utilities & Telecommunications")
industry_ops <- str_to_title(industry_ops, locale = "en")

# Keep the industries that match with existing list

us_list_ind <- us_salary %>%
  filter(us_salary$industry %in% industry_ops) 
us_list_ind %>%
  count(industry) %>%
  arrange(-n)

# 1,763 (8%) of responses were dropped.
# Can use string distances to try to group if needed (stringdist)

us_othr_ind <- us_salary %>%
  filter(!(us_salary$industry %in% industry_ops))

us_othr_ind %>%
  count(industry) %>%
  arrange(-n)

# There are a lot of Librarians. Create new category
# to try to increase data points here

us_othr_ind$industry <- with(us_othr_ind,
                    ifelse(str_detect(us_othr_ind$industry, "Librar"),
                           "Information Services", industry))
us_othr_ind %>%
  count(industry) %>%
  arrange(-n)

us_list_ind <- bind_rows(us_othr_ind %>% filter(industry == "Information Services"),
                         us_list_ind)

sapply(us_list_ind, function(x) sum(is.na(x))) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  rename("varible"="rowname",
         "na_num"=".")

# Early career: <35
# Mid career: 35 to 54
# Late career: 55+

us_list_ind <- us_list_ind %>%
  mutate(career_stage = 
           case_when(
             how_old_are_you %in% c("under 18", "18-24", "25-34") ~ "Early",
             how_old_are_you %in% c("35-44", "45-54") ~ "Mid",
             how_old_are_you %in% c("55-64", "65 or over") ~ "Late",
             TRUE ~ NA_character_)
         ) %>%
  mutate(gender_recode = 
           case_when(
             !(gender %in% c("Man", "Woman")) & !is.na(gender) ~ "Other",
             gender %in% c("Man", "Woman") ~ gender,
             TRUE ~ NA_character_)
         )

# Visualizing ---------------------------------------------------------

# Chart male and female differences in salary 
# by industry and career stage

salary_disp <- us_list_ind %>%
  filter(gender_recode %in% c("Man", "Woman"))

salary_disp$career_stage <- factor(salary_disp$career_stage, 
                                   levels = c("Early", "Mid", "Late"))

# Only keep those industries where there are enough data for both 
# men and women respondents. This will kick out a lot of industries
# because the population is Ask A Manager readers (mostly women).

salary_disp_a <- salary_disp %>%
  group_by(gender_recode, industry, career_stage) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  unite(col="gender_stage", c("gender_recode", "career_stage"), sep="_") %>%
  spread(gender_stage, count) %>%
  filter(Man_Early>=3 & Man_Late>=3 & Man_Mid>=3 &
           Woman_Early>=3 & Woman_Late>=3 & Woman_Mid>=3) %>%
  gather("gender_stage", "count", -industry) %>%
  separate(gender_stage, into=c("gender_recode", "career_stage")) %>%
  group_by(gender_recode, industry, career_stage)

salary_disp_b <- salary_disp %>%
  group_by(gender_recode, industry, career_stage) %>%
  summarise(med_salary=median(annual_salary))

salary_disp_grp <- salary_disp_a %>%
  left_join(salary_disp_b) %>%
  select(-count) %>%
  spread(gender_recode, med_salary) %>%
  rename("med_salary_m"="Man",
         "med_salary_w"="Woman") %>%
  mutate(gender_diff = med_salary_m-med_salary_w)

e <- ggplot(salary_disp_grp %>% filter(career_stage=="Early" & !is.na(gender_diff)), aes(x=reorder(industry, gender_diff), y=gender_diff)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(-11000,120000)) +
  coord_flip()

m <- ggplot(salary_disp_grp %>% filter(career_stage=="Mid" & !is.na(gender_diff)), aes(x=reorder(industry, gender_diff), y=gender_diff)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(-11000,120000)) +
  coord_flip()

l <- ggplot(salary_disp_grp %>% filter(career_stage=="Late" & !is.na(gender_diff)), aes(x=reorder(industry, gender_diff), y=gender_diff)) + 
  geom_bar(stat="identity") +
  scale_y_continuous(limits=c(-11000,120000)) +
  coord_flip()

e + m + l

#https://cran.r-project.org/web/packages/ggtext/vignettes/plotting_text.html 
#https://ggplot2.tidyverse.org/reference/annotate.html
#https://github.com/wilkelab/ggtext/issues/8 