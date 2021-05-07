# Set-up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(Cairo)
library(patchwork)
library(splitstackshape)
library(extrafont)
loadfonts(device="win")
library(ggplot2)
library(ggtext)
library(ggfx)
library(scales)


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

# Titles over time, by year ------------------------------------------

title_cts_yr <- nfx_mod %>% 
  count(year_added) %>% 
  arrange(year_added) %>%
  rename("n_all"="n")

# Titles over time, by month ------------------------------------------

# Prepare to break up data by month-year 
nfx_mod$date_added_my <- format(nfx_mod$date_added, "%Y-%m")
nfx_mod <- nfx_mod %>% mutate(date_added_my=paste0(date_added_my, "-01"))

title_cts_mo <- nfx_mod %>% 
  count(date_added_my) %>% 
  arrange(date_added_my)
title_cts_mo$date_added_my <- as.Date(title_cts_mo$date_added_my)

# Vector of all dates from first title added to most recent
dts_mon_all <- seq(min(nfx_mod$date_added, na.rm=T), 
                   max(nfx_mod$date_added, na.rm=T), 
                   by = "month")

title_cts_mo <- title_cts_mo %>% 
  complete(date_added_my = dts_mon_all)

title_cts_mo[is.na(title_cts_mo$n),]$n <- 0

title_cts_mo <- title_cts_mo %>%
  mutate(date_added_my_pos = as.numeric(date_added_my)) %>%
  select(date_added_my, date_added_my_pos, n) %>%
  rename("n_all"="n")

# Create horizontal tile plot

# Break axis by year based on range of dates
axisbreaks <- title_cts_mo$date_added_my_pos[seq(1, length(title_cts_mo$date_added_my_pos),12)]

# Extract years for label
axislabels <- data.frame(label = title_cts_mo$date_added_my[seq(1, length(title_cts_mo$date_added_my), 12)]) %>%
  mutate(label = str_sub(label, end=4))

stripplt <- ggplot() + 
  geom_tile(data = title_cts_mo, 
            mapping = aes(x = date_added_my_pos, y = 1, fill = n_all), 
            width = 30) +
  scale_fill_gradient(name="<span style='font-size:8pt'>Titles added per month</span>", 
                      low="#190103", high="#e50914",
                      limits=c(0,300)) +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom") +
  ggtitle("Number of titles added peaked in late 2019") +
  guides(fill = guide_colourbar(title.position = 'top', title.hjust = 0.5, barwidth = unit(20, 'lines'), barheight = unit(0.5, 'lines')))

stripplt + 
  theme(text = element_text(family='Verdana'), 
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_text(colour='#000000'),
        axis.text.y=element_blank(),
        panel.border=element_blank(),
        legend.title=element_markdown(),
        legend.text=element_text(size=8),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position='top')
  

# Cleaning up data by genre ------------------------------------------

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

# Print list of unique genres across all listed_in cols
nfx_genre %>%
  select(listed_in_1, listed_in_2, listed_in_3) %>%
  t %>%
  c %>%
  unique() %>%
  sort()

nfx_genre <- nfx_genre %>%
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

nfx_genre <- nfx_genre %>%
  distinct(show_id, genre, .keep_all = TRUE)

# Other notes --------------------------------------------

# Multi-season Netflix shows are not listed individually.

# "The Crown", which is currently in its 4th season, is only listed once based
# on latest date_added.


# Look at genre counts -------------------------

title_genre_yr <- nfx_genre %>%
  group_by(year_added) %>%
  count(genre) %>%
  arrange(year_added, -n) %>%
  ungroup()

# Normalize by total number of titles released

title_genre_yr <- title_genre_yr %>% 
  left_join(title_cts_yr) %>%
  mutate(pct = 100*(n/n_all))

title_genre_yr[is.na(title_genre_yr)]

# Slope graph to compare change between titles released
# in 2015 and titles released in 2020

title_genre_20 <- title_genre_yr %>%
  filter(year_added=="2020") %>%
  arrange(-pct) %>%
  select(genre, pct) %>%
  rename("pct20"="pct")

order_pct <- order(title_genre_20$pct20, decreasing = T)
title_genre_20$rank20 <- NA 
title_genre_20$rank20[order_pct] <- 1:nrow(title_genre_20)
title_genre_20

title_genre_15 <- title_genre_yr %>%
  filter(year_added=="2015") %>%
  arrange(-pct) %>%
  select(genre, pct) %>%
  rename("pct15"="pct")

order_pct <- order(title_genre_15$pct15, decreasing = T)
title_genre_15$rank15 <- NA 
title_genre_15$rank15[order_pct] <- 1:nrow(title_genre_15)
title_genre_15

title_genre_1520 <- title_genre_20 %>%
  left_join(title_genre_15) %>%
  filter((rank20 >= 1 & rank20 <= 5) | (rank15 >=1 & rank15 <= 5))

dist <- 5
max_pct <- max(title_genre_1520$pct15, title_genre_1520$pct20)


lab15<-paste(title_genre_1520$genre, (paste0(round(title_genre_1520$pct15,0), "% (#", title_genre_1520$rank15, ")")),sep=", ")
lab20<-paste(title_genre_1520$genre, (paste0(round(title_genre_1520$pct20,0), "% (#", title_genre_1520$rank20, ")")),sep=", ")

slopeplt <-ggplot(data = title_genre_1520) + 
  geom_segment(aes(x=0,xend=dist,y=pct15, yend=pct20), 
               colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "#E50914", "#000000"),
               size= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 1.5, 0.7), 
               lineend="round") +
  geom_point(aes(x=0, y=pct15), 
             colour=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "#E50914", "#000000"),
             size=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 4, 3)) + 
  geom_point(aes(x=dist, y=pct20), 
             colour=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "#E50914", "#000000"),
             size=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 4, 3)) +
  xlim(0-1.5, dist+1.5) +
  ylim(0, 1.05*max_pct) +
  geom_text(aes(label=lab20, y=pct20, x=rep.int(dist, nrow(title_genre_1520))),
            hjust=0, 
            nudge_x=0.15,
            nudge_y=0.05,
            size=3, 
            family='Verdana',
            colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "#E50914", "#000000"),
            fontface=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "bold", "plain")) +
  geom_text(aes(label=lab15, y=pct15, x=rep.int(0, nrow(title_genre_1520))),
            hjust=1, 
            nudge_x=-0.15,
            nudge_y=0.05,
            size=3,
            family='Verdana',
            colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "#E50914", "#000000"),
            fontface=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "bold", "plain")) +
  geom_text(label="2015", x=0, y=max_pct+3,
            hjust=1,
            size=4,
            family='Verdana') +
  geom_text(label="2020", x=dist, y=max_pct+3,
            hjust=0,
            size=4,
            family='Verdana') +
  labs(title="<b>The rise of International titles</b><br>
    <span style = 'font-size:10pt;'>Right before Netflix ramped up the number
    of titles they were adding per month, only 19% of titles were International in 2015. By 2020,
       **nearly half** of added titles were International.</span></span>")

# Customize theme for slope plot

slopeplt + theme(text = element_text(family='Verdana'), 
  panel.background = element_blank(),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text=element_blank(),
  panel.border=element_blank(),
  plot.title = element_textbox_simple(
    lineheight = 1.05,
    padding = margin(5.5, 5.5, 5.5, 5.5)
  ),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())

# Create layout -------------------------------------

layout <- c(
  
  area(1, 1, 4, 15),
  area(5, 1, 10, 15),
  area(11, 1, 12, 3),
  area(11, 4, 12, 15),
  area(13, 1, 14, 3),
  area(13, 4, 14, 15),
  area(15, 1, 16, 3),
  area(15, 4, 16, 15),
  area(17, 1, 18, 3),
  area(17, 4, 18, 15),
  area(19, 1, 20, 3),
  area(19, 4, 20, 15)
)

plot(layout)


# Test area -------------------------------------------

df <- data.frame(
  x = 0.1,
  y = 0.8,
  label = "*Lorem ipsum dolor sit amet,* consectetur adipiscing
  elit. Quisque tincidunt eget arcu in pulvinar. Morbi varius leo
  vel consectetur luctus. **Morbi facilisis justo non fringilla.**
  Vivamus sagittis sem felis, vel lobortis risus mattis eget. Nam
  quis imperdiet felis, in convallis elit."
)

p <- ggplot() +
  geom_textbox(
    data = df,
    aes(x, y, label = label),
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1
  ) +
  xlim(0, 1) + ylim(0, 1)

p 

# https://ggplot2-book.org/annotations.html
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/#text 
