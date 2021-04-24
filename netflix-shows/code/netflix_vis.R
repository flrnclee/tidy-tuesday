# Set-up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(Cairo)
library(patchwork)

# Read in data from TidyTuesday
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

nfx_data <- tuesdata$netflix
# create_report(nfx_data)

# Exploration & Preparation ---------------------------------

str(nfx_data)
head(nfx_data)

# duration variable is combination of SEASONS and MINUTES
# ratings have ratings for TV and for MOVIES

nfx_mod <- nfx_data
nfx_mod$date_added <- as.Date(nfx_mod$date_added, format = "%B %d, %Y")
nfx_mod %>% filter(is.na(date_added))

ggplot(data=nfx_mod, aes(x=year_added)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_mod$year_added))+250)) +
  scale_x_discrete(limits=rev) + 
  coord_flip() +
  ggtitle("Titles added by year")

ggplot(data=nfx_mod, aes(x=month_added)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_mod$month_added))+200))+
  scale_x_discrete(limits=rev) + 
  coord_flip()  + 
  ggtitle("Titles added by month")

ggplot(data=nfx_mod, aes(x=rating)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_data$rating)+200)))+
  coord_flip() +
  facet_grid(cols = vars(type))


# Titles over time ------------------------------------------

nfx_mod$year_added <- format(nfx_mod$date_added, "%Y")
nfx_mod$date_added_abbv <- format(nfx_mod$date_added, "%Y-%m")

title_ct_trend <- nfx_mod %>% 
  count(date_added_abbv) %>% 
  arrange(date_added_abbv) %>%
  mutate(date_added_abbv=paste0(date_added_abbv, "-01"))
title_ct_trend$date_added_abbv <- as.Date(title_ct_trend$date_added_abbv)

dts_all <- seq(min(nfx_mod$date_added, na.rm=T), max(nfx_mod$date_added, na.rm=T), by = "month")

dt_range <- dts_all %>% 
  as.data.frame() %>%
  rename("date_added_abbv"=".")

title_ct_trend <- dt_range %>%
  left_join(title_ct_trend)

title_ct_trend[is.na(title_ct_trend)] <- 0

#

ggplot(data=title_ct_trend, aes(x=date_added_abbv, y=n)) +
  geom_bar(stat='identity') +
  ggtitle("Number of Netflix titles added by month")

#




# Several movies have TV ratings
# If we want to use ratings, it might be helpful to group them

####### CHARTS #########
# How have releases changed over time (TV Shows vs. Movies)
# -- Number of releases
# -- Genre of releases
# -- Average length of a feature


