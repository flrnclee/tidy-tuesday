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

# Global variables ------------------------------------
bkgrd_color <- "#fbf3ed"
dot_size <- 4
txt_color <- "#454545"
font_use <- "Overpass"

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

# Keep the industries that match with existing list.

us_list_ind <- us_salary %>%
  filter(us_salary$industry %in% industry_ops) 
us_list_ind %>%
  count(industry) %>%
  arrange(-n)

# 1,763 (8%) of responses were dropped, which isn't too bad.
# Can use string distances to try to group if needed (stringdist).

us_othr_ind <- us_salary %>%
  filter(!(us_salary$industry %in% industry_ops))

us_othr_ind %>%
  count(industry) %>%
  arrange(-n)

# There are a lot of Librarians. Just in case: create new category
# to try to increase data points here.

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

# Data preparation ---------------------------------------------------------

salary_disp <- us_list_ind %>%
  # Restrict analysis to respondents who identified as a man or woman
  filter(gender_recode %in% c("Man", "Woman")) %>%
  mutate(industry = 
           case_when(
             industry == "Education (Higher Education)" ~ "Higher Education",
             TRUE ~ industry
           ))

salary_disp$industry <- str_to_sentence(salary_disp$industry)


#- Calculate gender gap in annual salary by industry and career stage

# Only keep those industries where there are enough data for both 
# men and women respondents. This will kick out a lot of industries
# because the population is Ask A Manager readers (mostly women).

salary_stage_disp_a <- salary_disp %>%
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

salary_stage_disp_b <- salary_disp %>%
  group_by(gender_recode, industry, career_stage) %>%
  summarise(med_salary=median(annual_salary))

salary_stage_disp <- salary_stage_disp_a %>%
  left_join(salary_stage_disp_b) %>%
  select(-count) %>%
  spread(gender_recode, med_salary) %>%
  rename("med_salary_m"="Man",
         "med_salary_w"="Woman") %>%
  mutate(gender_diff = (med_salary_m-med_salary_w)/1000) %>%
  mutate(career_stage = 
           case_when(
             career_stage=="Early" ~ "Early career",
             career_stage=="Mid" ~ "Mid career", 
             career_stage=="Late" ~ "Late career",
             TRUE ~ NA_character_
           ))

salary_stage_disp$career_stage <- factor(salary_stage_disp$career_stage, 
                                   levels = c("Early career", "Mid career", "Late career"))

#- Preview male and female differences in salary by industry and career stage

ggplot(salary_stage_disp, aes(x=gender_diff, y=fct_rev(career_stage))) +
  geom_bar(stat="identity") +
  facet_wrap(~industry)

#- Calculate gender gap in annual salary by industry

# Create a vector of the 12 industries being looked at in 
# this analysis for filtering

us_use_ind <- salary_stage_disp$industry %>% unique()

salary_all_disp_a <- salary_disp %>%
  subset(industry %in% us_use_ind) %>%
  group_by(gender_recode, industry) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  spread(gender_recode, count) %>%
  filter(Man>=3 & Woman>=3) %>%
  gather("gender_recode", "count", -industry) %>%
  group_by(gender_recode, industry)

salary_all_disp_b <- salary_disp %>%
  group_by(gender_recode, industry) %>%
  summarise(med_salary=median(annual_salary))

salary_all_disp <- salary_all_disp_a %>%
  left_join(salary_all_disp_b) %>%
  select(-count) %>%
  spread(gender_recode, med_salary) %>%
  rename("med_salary_m"="Man",
         "med_salary_w"="Woman") %>%
  mutate(gender_diff = (med_salary_m-med_salary_w)/1000) %>%
  arrange(-gender_diff)

# Visualization -----------------------------------------------------

#- Additional aesthetic prep

# Need to make space at the top of the plot to add annotations.
# Temp solution is to add a row in the data so y dimension of annotation 
# can be specified.

label_rowname <- glue::glue("<span style='color: {bkgrd_color};'>Label row</span>")
label_data <- data.frame(industry=label_rowname, gender_diff=1000)

salary_all_disp <- salary_all_disp %>%
  bind_rows(label_data) %>%
  arrange(-gender_diff)

salary_stage_disp <- salary_stage_disp %>%
  bind_rows(label_data) %>%
  arrange(-gender_diff)

# Turn to factor for proper sorting by salary difference overall 

ind_order <- salary_all_disp$industry
salary_all_disp$industry <- factor(salary_all_disp$industry,
                                      levels=ind_order)
salary_stage_disp$industry <- factor(salary_stage_disp$industry,
                                   levels=ind_order)

#- Create dotplot

# There is an issue with running base alone. 
# Troubleshoot later...

base <- ggplot(data=salary_all_disp, aes(x=gender_diff, y=reorder(industry, gender_diff))) +
  scale_x_continuous(limits=c(-20, 110), labels=dollar_format(prefix="$", suffix="K"), breaks=breaks_pretty(8)) +
  geom_hline(yintercept=label_rowname, size=10, colour=bkgrd_color) +
  geom_vline(xintercept = 0, size=0.25, colour="#888888") +
  geom_richtext(x=0, y=label_rowname, label="<span style='font-size:9pt; color:#B1ABA3'>
  <b style='font-family:Overpass'>Higher for men </b><span style='font-family:Wingdings3;'>g</span></span>",
                hjust=0, nudge_x=0.5, fill=NA, label.colour=NA) +
  geom_richtext(x=0, y=label_rowname, label="<span style='font-size:9pt; color:#B1ABA3'>
  <span style='font-family:Wingdings3;'>f</span><b style='font-family:Overpass'> Higher for women</b></span>",
                hjust=1, nudge_x=1, fill=NA, label.colour=NA)

base + 
  geom_point(data=salary_stage_disp, aes(x=gender_diff, y=reorder(industry, gender_diff),
                                         colour=career_stage, fill=career_stage),
             size=dot_size, shape=21, colour=bkgrd_color) +
  scale_fill_manual(values = c("Late career"="#de8971", 
                               "Mid career"="#7b6079", 
                               "Early career"="#a7d0cd"),
                    aesthetics = c("colour", "fill")) +
  geom_point(data=salary_all_disp, aes(x=gender_diff, y=reorder(industry, gender_diff)),
             size=dot_size, shape=0, stroke=1, colour="black") +
  labs(title = "<b>The <span style='font-size:11pt; font-family:Webdings;'>c</span> median annual salary 
  was higher for men than for women across 12 industries, with the highest salary difference observed in Law.</b><br>",
       subtitle="<span style='font-size:10pt'>The gender gap was especially high for <b><span style='color: #de8971; font-family:Webdings;'>n</span><span style='color: #de8971'> late career</span></b>
   professionals compared with <b><span style='color: #a7d0cd; font-family:Webdings;'>n</span><span style='color: #a7d0cd;'> early career</span></b> and 
       <br><b><span style='color: #7b6079; font-family:Webdings;'>n</span><span style='color: #7b6079'> mid career</span></b> professionals.",
       caption = "#TidyTuesday | Week 21 • 5/18/21 | @flrnclee") +
  xlab("Difference in median salary") +
  theme(text = element_text(family=font_use, color=txt_color), 
          panel.background = element_blank(),
          panel.grid=element_blank(),
          axis.ticks=element_blank(),
          axis.text.x=element_markdown(),
          axis.text.y=element_markdown(),
          panel.border=element_blank(),
          panel.grid.major.x = element_line(linetype="dotted", colour = "#DFDFDF"),
          panel.grid.major.y = element_line(linetype="dotted", colour = "#CBCBCB"),
          plot.background = element_rect(fill = bkgrd_color),
          plot.title = element_textbox_simple(
            padding = margin(t=20, r=10, b=0, l=0),
            halign = 0, 
            color = txt_color),
        plot.subtitle = element_textbox_simple(
          padding = margin(t=0, r=10, b=20, l=0),
          halign = 0, 
          color = txt_color),
          axis.title.x = element_textbox_simple(
            padding = margin(t=10),
            halign = 0.5, 
            size = 10),
          axis.title.y = element_blank(),
        plot.caption = element_text(size=7.5, colour="#444444"), 
        legend.position = "none") 

ggsave(
  paste0("salarydisp", format(Sys.time(), "%Y%m%d"), ".png"),
  dpi = 320,
  width = 9,
  height = 5,
  units = "in",
  type = "cairo-png"
)
