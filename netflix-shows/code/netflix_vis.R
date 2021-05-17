# Set-up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(Cairo)
library(patchwork)
library(splitstackshape)
library(extrafont)
library(ggplot2)
library(ggtext)
library(ggfx)
library(scales)

# Read in data from TidyTuesday
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)
nfx_data <- tuesdata$netflix

# Explore and prep ---------------------------------

str(nfx_data)
head(nfx_data)

# Duration variable is combination of SEASONS and MINUTES.
# Ratings have ratings for TV and for MOVIES.

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
nfx_mod <- nfx_mod %>% 
  mutate(date_added_my=paste0(date_added_my, "-01"))

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

# Cleaning up data by genre ------------------------------------------

nfx_genre <- nfx_mod %>%
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


# Look at genre counts -----------------------------------------

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

# Compare change between titles released in 2015 and titles released in 2020

title_genre_20 <- title_genre_yr %>%
  filter(year_added=="2020") %>%
  arrange(-pct) %>%
  select(genre, pct, n) %>%
  rename("pct20"="pct", 
         "n20"="n")

order_pct <- order(title_genre_20$pct20, decreasing = T)
title_genre_20$rank20 <- NA 
title_genre_20$rank20[order_pct] <- 1:nrow(title_genre_20)
title_genre_20

title_genre_15 <- title_genre_yr %>%
  filter(year_added=="2015") %>%
  arrange(-pct) %>%
  select(genre, pct, n) %>%
  rename("pct15"="pct",
         "n15"="n")

order_pct <- order(title_genre_15$pct15, decreasing = T)
title_genre_15$rank15 <- NA 
title_genre_15$rank15[order_pct] <- 1:nrow(title_genre_15)
title_genre_15

title_genre_1520 <- title_genre_20 %>%
  left_join(title_genre_15) %>%
  filter((rank20 >= 1 & rank20 <= 5) | (rank15 >=1 & rank15 <= 5))


# Create title img -----------------------------------------------------------

b_text <- data.frame(
  x = c(0, 0),
  y = c(0, 0),
  label = c("<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/netflix-logo.png', width='160'/>",
          "#tidytuesday <span style='color: #cccccc;'>|</span> Week 17 • 4/20/21 <span style='color: #cccccc;'>|</span> @flrnclee"))

b <- ggplot() +
  geom_textbox(
    data = b_text,
    aes(x, y, label = label),
    box.size = 0,
    family = c("BebasNeueBook", "BebasNeueBook"),
    size = c(30, 7),
    width = unit(1, "npc"),
    fill = NA,
    hjust = 0, 
    vjust = c(0,1),
    halign = 0.5
  ) +
  scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
  scale_y_continuous(limits=c(-1,1.5), expand=c(0,0))

bnr <- b + theme(text = element_text(family="Aktiv Grotesk"), 
          panel.background = element_blank(),
          panel.grid=element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          panel.border=element_blank(),
          plot.title = element_textbox_simple(
            lineheight = 1,
            padding = margin(5.5, 5.5, 5.5, 5.5),
            halign = 0
          ),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

bnr


# Create horizontal tile plot ----------------------------------------

# Break axis by year based on range of dates
axisbreaks <- title_cts_mo$date_added_my_pos[seq(1, length(title_cts_mo$date_added_my_pos),12)]

# Extract years for break labels
axislabels <- data.frame(label = title_cts_mo$date_added_my[seq(1, length(title_cts_mo$date_added_my), 12)]) %>%
  mutate(label = str_sub(label, end=4))

max_added_mo <- title_cts_mo %>% filter(n_all==max(n_all))

strip <- ggplot() + 
  # Start strip plot when titles per month were ramping up
  geom_tile(data = title_cts_mo %>% filter(date_added_my >= "2015-01-01"), 
            mapping = aes(x = date_added_my_pos, y = 1, fill = n_all), 
            width = 30) +
  scale_fill_gradient(low="#190103", high="#B81D24", 
                      expand=c(0,0)) +
  scale_x_continuous(breaks = axisbreaks, 
                     labels = axislabels$label, 
                     position = "bottom",
                     expand=c(0,0)) +
  guides(fill = guide_colourbar(title.position = 'top', title.hjust = 0.5, barwidth = unit(10, 'lines'), barheight = unit(0.5, 'lines'))) +
  labs(title=glue::glue("<span style='font-size:20pt; font-family:BebasNeueBold';>Netflix hit its record number of titles added in a month right before COVID.</span>
       \n<span style='font-size:13pt;'><span style='color:#B81D24'>**{max_added$n_all} titles**</span> were added in {months(max_added$date_added_my)} {format(max_added$date_added_my, '%Y')}.</span>")) +
  geom_tile(data = max_added_mo,
            mapping = aes(x=date_added_my_pos,
                          y = 1), 
            color = "red",
            fill = NA, width = 50, height = 1.1, linetype = "dotted", 
            size = 1)

stripplt <- strip + 
  theme(text = element_text(family="Aktiv Grotesk"), 
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_text(colour="#000000"),
        axis.text.y=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.position="none",
        plot.title = element_textbox_simple(
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          halign = 0
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

stripplt

# Create Top 5 genre pictogram -------------------------------------------

df <- data.frame(
  x = c(0.0, 0.35, 0.7, 1.05, 1.4,
        0.0, 0.35, 0.7, 1.05, 1.4,
        0.0, 0.35, 0.7, 1.05, 1.4),
  y = c(0, 0, 0, 0, 0,
        0.15, 0.15, 0.15, 0.15, 0.15,
        -1.22, -1.22, -1.22, -1.22, -1.22),
  label = c(title_genre_1520$genre[1], title_genre_1520$genre[2], title_genre_1520$genre[3], title_genre_1520$genre[4], title_genre_1520$genre[5],
            "<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/international.png', width='65'/>",
            "<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/drama.png', width='65'/>",
            "<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/comedy.png', width='65'/>",
            "<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/family.png', width='65'/>",
            "<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/netflix-shows/imgs/romance.png', width='65'/>",
            paste0(title_genre_1520$n20[1], " TITLES"), paste0(title_genre_1520$n20[2], " TITLES"), paste0(title_genre_1520$n20[3], " TITLES"),
            paste0(title_genre_1520$n20[4], " TITLES"), paste0(title_genre_1520$n20[5], " TITLES"))
)

covid_added <- title_cts_yr %>% filter(year_added=="2020")
max_genre <- title_genre_1520 %>% filter(rank20==min(rank20))

p <- ggplot() +
  geom_textbox(
    data = df,
    aes(x, y, label = label),
    family = c("BebasNeueBook", "BebasNeueBook", "BebasNeueBook", "BebasNeueBook", "BebasNeueBook", 
               "AktivGrotesk-Regular", "AktivGrotesk-Regular", "AktivGrotesk-Regular", "AktivGrotesk-Regular", "AktivGrotesk-Regular",
               "BebasNeueBook", "BebasNeueBook", "BebasNeueBook", "BebasNeueBook", "BebasNeueBook"),
    size = c(5, 5, 5, 5, 5, 
             30, 30, 30, 30, 30,
             7, 7, 7, 7, 7),
    box.size = NA,
    fill = NA,
    width = unit(0.2, "npc"),
    hjust = 0, 
    vjust = c(0, 0, 0, 0, 0, 
              1, 1, 1, 1, 1, 
              0, 0, 0, 0, 0),
    halign = 0.5
  ) +
  scale_x_continuous(limits=c(0, 1.8), expand=c(0,0)) +
  scale_y_continuous(limits=c(-1.5, 0.5), expand=c(0,0)) +
  labs(title=glue::glue("<span style='font-size:20pt; font-family:BebasNeueBold';>During the first year of COVID, {comma(covid_added$n_all)} titles were released.</span>
       \n<span style='font-size:13pt;'><span style='color:#B81D24'>**{max_genre$genre} titles**</span> led the pack with {max_genre$n20} titles released in 2020.</span>"))

pict <- p + theme(text = element_text(family="Aktiv Grotesk"), 
                  panel.background = element_blank(),
                  panel.grid=element_blank(),
                  axis.ticks=element_blank(),
                  axis.text=element_blank(),
                  panel.border=element_blank(),
                  plot.title = element_textbox_simple(
                    lineheight = 1,
                    padding = margin(5.5, 5.5, 5.5, 5.5),
                    halign = 0
                  ),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()
)

pict

# Creating slope plot -------------------------

dist <- 5
max_pct <- max(title_genre_1520$pct15, title_genre_1520$pct20)

lab15<-paste(title_genre_1520$genre, (paste0(round(title_genre_1520$pct15,0), "%")),sep=", ")
lab20<-paste(title_genre_1520$genre, (paste0(round(title_genre_1520$pct20,0), "%")),sep=", ")

slopeplt <-ggplot(data = title_genre_1520) + 
  geom_segment(aes(x=0,xend=dist,y=pct15, yend=pct20), 
               colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 
                              ifelse(title_genre_1520$genre == "International", "#B81D24", "#000000"), 
                              "#808080"),
               size= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 1.5, 0.7), 
               lineend="round") +
  geom_point(aes(x=0, y=pct15), 
             colour=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 
                           ifelse(title_genre_1520$genre == "International", "#B81D24", "#000000"), 
                                  "#808080"),
             size=3) + 
  geom_point(aes(x=dist, y=pct20), 
             colour=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 
                           ifelse(title_genre_1520$genre == "International", "#B81D24", "#000000"), 
                           "#808080"),
             size=3) +
  scale_x_continuous(limits=c(0-1.5, dist+1.5)) +
  scale_y_continuous(limits=c(0, 1.05*max_pct)) +
  geom_text(aes(label=lab20, y=pct20, x=rep.int(dist, nrow(title_genre_1520))),
            hjust=0, 
            nudge_x=0.15,
            nudge_y=0.05,
            size=3.5, 
            family="Aktiv Grotesk",
            colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 
                           ifelse(title_genre_1520$genre == 'International', "#B81D24", "#000000"), 
                           "#808080"),
            fontface=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "bold", "plain")) +
  geom_text(aes(label=lab15, y=pct15, x=rep.int(0, nrow(title_genre_1520))),
            hjust=1, 
            nudge_x=-0.15,
            nudge_y=0.05,
            size=3.5,
            family="Aktiv Grotesk",
            colour= ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, 
                           ifelse(title_genre_1520$genre == 'International', "#B81D24", "#000000"), 
                           "#808080"),
            fontface=ifelse(title_genre_1520$rank20 < title_genre_1520$rank15, "bold", "plain")) +
  geom_text(label="2015", x=0, y=max_pct+3,
            hjust=1,
            size=3.5,
            colour="#808080",
            family="Aktiv Grotesk") +
  geom_text(label="2020", x=dist, y=max_pct+3,
            hjust=0,
            size=3.5,
            colour="#808080",
            family="Aktiv Grotesk") +
  labs(title=glue::glue("<span style='font-size:20pt; font-family:BebasNeueBold';><b>International titles have been on the rise.</b></span>
    \n<span style = 'font-size:13pt;'>In 2015, only 19% of the added titles on Netflix were International. 
    <span style = 'color:#B81D24;'>**By 2020, nearly half were.**</span></span>"))

# Customize theme for slope plot

slopeplt <- slopeplt + theme(text = element_text(family='Aktiv Grotesk'), 
  panel.background = element_blank(),
  panel.grid=element_blank(),
  axis.ticks=element_blank(),
  axis.text=element_blank(),
  panel.border=element_blank(),
  plot.title = element_textbox_simple(
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    size = 15
  ),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())



slopeplt

# Create layout -------------------------------------

layout <- c(
  
  area(t = 1,
       l = 1,
       b = 4,
       r = 10),
  area(t = 5,
       l = 1, 
       b = 6,
       r = 10),
  area(t = 7,
       l = 1, 
       b = 12,
       r = 10),
  area(t = 13,
       l = 1, 
       b = 25,
       r = 10))

plot(layout)

bnr + 
  stripplt + 
  pict + 
  slopeplt + plot_layout(design=layout) &
  theme(plot.background = element_rect(fill = "#f5f5f1", colour = "#f5f5f1"))

ggsave(
  paste0("netflix", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 320,
  width = 8.5,
  height = 11,
  type = "cairo-png"
)

