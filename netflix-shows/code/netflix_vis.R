# Set-up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(Cairo)
library(patchwork)
library(splitstackshape)
library(ggplot2)
library(ggtext)
library(ggfx)

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
nfx_mod$date_added <- str_trim(nfx_mod$date_added)
nfx_mod$date_added <- as.Date(nfx_mod$date_added, format = "%B %d, %Y")
nfx_mod <- nfx_mod %>% filter(!is.na(date_added))

nfx_mod$year_added <- format(nfx_mod$date_added, "%Y")

ggplot(data=nfx_mod, aes(x=rating)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_data$rating)+200)))+
  coord_flip() +
  facet_grid(cols = vars(type)) +
  ggtitle("Distribution of ratings by type")

# Several movies have TV ratings
# If we want to use ratings, it might be helpful to group them

# Titles over time ------------------------------------------

# By year
ggplot(data=nfx_mod, aes(x=year_added)) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.1) + 
  scale_y_continuous(expand=c(0,0), limits=c(0, max(table(nfx_mod$year_added))+250)) +
  scale_x_discrete(limits=rev) + 
  coord_flip() +
  ggtitle("Titles added by year")

nfx_mod$date_added_my <- format(nfx_mod$date_added, "%Y-%m")
nfx_mod <- nfx_mod %>% mutate(date_added_my=paste0(date_added_my, "-01"))

title_cts <- nfx_mod %>% 
  count(date_added_my) %>% 
  arrange(date_added_my)
title_cts$date_added_my <- as.Date(title_cts$date_added_my)

dts_mon_all <- seq(min(nfx_mod$date_added, na.rm=T), 
                   max(nfx_mod$date_added, na.rm=T), 
                   by = "month")

title_cts <- title_cts %>% 
  complete(date_added_my = dts_mon_all)

title_cts[is.na(title_cts$n),]$n <- 0

ggplot(data=title_cts, aes(x=date_added_my, y=n)) +
  geom_bar(stat='identity') +
  ggtitle("Number of Netflix titles added by month")


# Horizontal tile plot -------------------------------------------

title_cts <- title_cts %>%
  mutate(date_added_my_pos = as.numeric(date_added_my)) %>%
  select(date_added_my, date_added_my_pos, n) %>%
  rename("n_all"="n")

# Break axis by year based on range of dates

axisbreaks <- title_cts$date_added_my_pos[seq(1, length(title_cts$date_added_my_pos),12)]

# Extract years for label

axislabels <- data.frame(label = title_cts$date_added_my[seq(1, length(title_cts$date_added_my), 12)]) %>%
  mutate(label = str_sub(label, end=4))

# Produce strip plot

stripplt <- ggplot() + geom_tile(data = filter(title_cts, date_added_my >= "2015-01-01"),
                    mapping = aes(x = date_added_my_pos,
                                  y = 1,
                                  fill = n_all), width = 30) +
  scale_fill_gradient(low =  "#190103", high ="#e50914") +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom") #+ 
  #theme_void()

stripplt

# Cleaning up genre data ------------------------------------------

nfx_genre <- nfx_mod

nfx_genre <- nfx_genre %>%
  cSplit("listed_in", sep=",")

clean_genre <- function(col) {
  
  # Function to clean and consolidate similar genres 
  
  col <- sub("TV", "", col)
  col <- sub("Shows", "", col)
  col <- sub("Movies", "", col)
  col <- sub("", "", col)
  col <- str_trim(col)
  
  col <- ifelse(col %in% c("Classic", "Cult"), "Classic & Cult",
                   ifelse(col %in% c("Stand-Up Comedy & Talk", "Stand-Up Comedy"), "Comedies",
                          ifelse(col %in% c("Anime Series", "Anime Features"), "Anime",
                                 ifelse(col=="Kids'", "Children & Family", 
                                        ifelse(col=="Docuseries", "Documentaries",
                                               ifelse(col %in% c("International", "British", "Spanish-Language", "Korean"), "International", col))))))
}

nfx_genre <- nfx_genre %>%
  mutate(across(c("listed_in_1", "listed_in_2", "listed_in_3"), clean_genre))

# Need to deduplicate

nfx_genre %>%
  select(listed_in_1, listed_in_2, listed_in_3) %>%
  t %>%
  c %>%
  unique() %>%
  sort()

nfx_genre_t <- nfx_genre %>%
  gather(key = "cat",
         value = "genre",
         listed_in_1,
         listed_in_2,
         listed_in_3,
         na.rm= TRUE) %>%
  select(-cat) %>%
  # There are 68 titles that had a generic genre (e.g., "Movies", "TV", or "Shows").
  # These were excluded.
  filter(!(genre==""))

# After cleaning genres in listed_in columns, 
# we might have duplicates by show_id and genre

nfx_genre_t <- nfx_genre_t %>%
  distinct(show_id, genre, .keep_all = TRUE)


# Other notes --------------------------------------------

# Multi-season Netflix shows are not listed individually.

# "The Crown", which is currently in its 4th season, is only listed once based
# on latest date_added.

# Look at genre counts over time -------------------------

unique(nfx_genre_t$genre)

# How does distribution of titles change over time by genre?

# If you don't normalize, you will see brighter colors over time
# since there are more added titles.

nfx_genre_cts <- nfx_genre_t %>%
  group_by(genre) %>%
  count(date_added_my)

nfx_genre_cts$date_added_my <- as.Date(nfx_genre_cts$date_added_my)

nfx_genre_cts <- nfx_genre_cts %>%
  group_by(genre) %>%
  complete(genre, date_added_my = dts_mon_all)

nfx_genre_cts[is.na(nfx_genre_cts)] <- 0

nfx_genre_cts <- nfx_genre_cts %>%
  mutate(date_added_my_pos = as.numeric(date_added_my)) %>%
  select(genre, date_added_my, date_added_my_pos, n) %>%
  arrange(genre, date_added_my)

# May want to limit to smaller window to avoid impact of
# increasing num of titles being added over time

ggplot() + geom_tile(data = filter(nfx_genre_cts, date_added_my > "2015-01-01", genre=="Sports"),
                     mapping = aes(x = date_added_my_pos,
                                   y = 1,
                                   fill = n), width = 30) +
  scale_fill_gradient(low =  "#190103", high ="#e50914") +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom") #+ 
#theme_void()

# Normalize by total number of titles released

nfx_genre_pcts <- nfx_genre_cts %>% 
  left_join(title_cts) %>%
  mutate(pct = 100*(n/n_all))

nfx_genre_pcts[is.na(nfx_genre_pcts)] <- 0

ggplot() + geom_tile(data = filter(nfx_genre_pcts, date_added_my > "2015-01-01", genre=="Documentaries"),
                                 mapping = aes(x = date_added_my_pos,
                                               y = 1,
                                               fill = pct), width = 30) +
  scale_fill_gradient(low =  "#190103", high ="#e50914", limits = c(0, 100)) +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom") #+ 
#theme_void()