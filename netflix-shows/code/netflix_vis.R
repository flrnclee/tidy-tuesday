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

# Exploration ---------------------------------

str(nfx_data)
head(nfx_data)

# duration variable is combination of SEASONS and MINUTES
# ratings have ratings for TV and for MOVIES

nfx_mod <- nfx_data
nfx_mod$date_added <- as.Date(nfx_mod$date_added, format = "%B %d, %Y")
nfx_mod %>% filter(is.na(date_added))

nfx_mod$year_added <- format(nfx_mod$date_added, "%Y")
nfx_mod$month_added <- format(nfx_mod$date_added, "%m")

p1 <- ggplot(data=nfx_mod, aes(x=year_added)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_mod$year_added))+250)) +
  scale_x_discrete(limits=rev) + 
  coord_flip() +
  ggtitle("Titles added by year")

p2 <- ggplot(data=nfx_mod, aes(x=month_added)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_mod$month_added))+200))+
  scale_x_discrete(limits=rev) + 
  coord_flip()  + 
  ggtitle("Titles added by month")

p1 | p2

p <- ggplot(data=nfx_mod, aes(x=rating)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_data$rating)+200)))+
  coord_flip() 

p + facet_grid(cols = vars(type))

# Several movies have TV ratings
# If we want to use ratings, it might be helpful to group them

####### CHARTS #########
# How have releases changed over time (TV Shows vs. Movies)
# -- Number of releases
# -- Genre of releases
# -- Average length of a feature


