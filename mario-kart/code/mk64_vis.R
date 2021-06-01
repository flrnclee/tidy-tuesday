# Setting up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(classInt)

library(Cairo)
library(patchwork)

library(extrafont)

library(ggplot2)
library(ggtext)
library(ggfx)
library(ggforce)
library(ggstar)

library(viridis)
library(RColorBrewer)


# Reading in data from TidyTuesday
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
players <- tuesdata$drivers
records <- tuesdata$records

darken <- function(color, factor=1.4){
        # function to darken colors
        col <- col2rgb(color)
        col <- col/factor
        col <- rgb(t(col), maxColorValue=255)
        col
}

# Global variables ------------------------------------
bkgrd_color <- "#434343"
txt_color <- "#ffffff"
font_use <- "Overpass"

# Mario color palette
m_blue <- "#049CD8"
m_yellow <- "#FBD000"
m_red <- "#E52521"
m_green <-"#43B047"

# Preparing data ----------------------------------------

# In case we need nation
players_unique <- unique(players[c("player", "nation")])

# 5 drivers have missing values on nation
players_unique %>% filter(is.na(nation))

# ---- Look at three lap, no shortcut records over time

three_ns <- records %>% 
        filter(type=="Three Lap" & shortcut=="No") %>%
        left_join(players_unique)

# Create month-year date
three_ns$date_my <- format(three_ns$date, "%Y-%m")
three_ns <- three_ns %>% 
     mutate(date_my=paste0(date_my, "-01"))
three_ns$date_my <- as.Date(three_ns$date_my)

# Create year date
three_ns$year <- format(three_ns$date, "%Y")
three_ns <- three_ns %>% 
        mutate(year=paste0(year, "-01-01"))
three_ns$year <- as.Date(three_ns$year)

# First record for each track
three_ns_first <- three_ns %>%
        group_by(track) %>%
        mutate(first_rec_date = min(date),
               first_rec_time = max(time)) %>%
        filter(date==first_rec_date & time==first_rec_time) %>%
        select(track, player, nation, first_rec_date, first_rec_time)
# Latest record for each track
three_ns_last <- three_ns %>%
        group_by(track) %>%
        mutate(last_rec_date = max(date),
               last_rec_time = min(time)) %>%
        filter(date==last_rec_date & time==last_rec_time) %>%
        select(track, player, nation, last_rec_date, last_rec_time)

# Time improvement overall
three_ns_diff <- three_ns_first %>%
        select(track, first_rec_date, first_rec_time) %>%
        left_join(three_ns_last %>% 
                          select(track, last_rec_date, last_rec_time)) %>%
        mutate(diff_time=last_rec_time-first_rec_time) %>%
        arrange(-diff_time) 

# Order track by "difficulty"
three_ns$track <- factor(three_ns$track, levels=three_ns_diff$track)

# D.K.'s Jungle Parkway, Rainbow Road, Wario Stadium, and Yoshi Valley
# have greatest time improvements since the first WR.

# --- Total world records by country

nation_rec_overall <- three_ns %>%
        group_by(nation) %>%
        summarise(wr_num=n()) %>%
        ungroup() %>%
        arrange(-wr_num)
nation_rec_overall

# --- Total world records by month and country
nation_rec_my <- three_ns %>%
        group_by(date_my, nation) %>%
        summarise(wr_num=n()) %>%
        ungroup()
nation_rec_my$nation <- factor(nation_rec_my$nation, levels=rev(nation_rec_overall$nation))

#Number of world records per month is highly skewed
#Use Jenks or Fisher classification
wr_brks <- classIntervals(nation_rec_my$wr_num, n=5, style="jenks")$brks
wr_brks[1] <- wr_brks[1]-1

nation_rec_my <- nation_rec_my %>%
        mutate(wr_num_cat = cut(wr_num, breaks=wr_brks,
                                labels=c("0-2", "3-5", "6-10", "11-16", "17+"))) %>%
        mutate(wr_num_cat = as.factor(wr_num_cat))

wr_colors <- c(m_green, '#7db93e', '#aac233', '#d3c923', m_yellow)

#- Create dot plot histogram
dot_plt <- ggplot(data=nation_rec_my, aes(x=wr_num)) +
        geom_dotplot(aes(fill=wr_num_cat), 
                     colour=bkgrd_color, 
                     method="histodot", 
                     binwidth=1, 
                     dotsize=1) +
        scale_x_continuous(breaks=seq(0, 40, 5)) +
        scale_fill_manual(values=wr_colors) +
        annotate(
                geom="curve", xend=38, yend=0.03, x=35, y=0.1, 
                curvature=0.3, arrow=arrow(length = unit(2, "mm")), 
                color = txt_color) +
        annotate(
                geom="richtext", x=38, y=0.1, 
                label=glue::glue("In September 1998, Australia <br>broke <b><span style='color: {wr_colors[5]}';>39 world records</span></b>
                                 thanks <br>to one player: <b> Alex Penev</b>."),
                family=font_use, fontface="bold", size=3, lineheight = 1.2, color = txt_color, fill=NA, label.colour=NA,
                hjust="right", vjust="bottom") +
        labs(title = glue::glue("Most countries can break <b><span style='color: {wr_colors[1]}';>
                                1 to 5 world records</span></b> in a competitive month. It's unusual to break <b><span 
                                style='color: {wr_colors[4]}';>more than 10</b>.")) + 
        xlab("Number of world records in a month") +
        theme(text = element_text(
                family=font_use, 
                color=txt_color), 
              # panel settings
              panel.background = element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.grid.major.x = element_line(linetype="solid", colour = "#878787"),
              panel.grid.major.y = element_blank(),
              # axis settings
              axis.ticks=element_blank(),
              axis.text.x=element_markdown(colour="#949494"),
              axis.text.y= element_blank(),
              axis.title.x =  element_textbox_simple(
                      colour = "#949494",
                      padding = margin(t=10),
                      halign = 0.5, 
                      size = 10),
              axis.title.y = element_blank(),
              # plot settings
              plot.background = element_rect(fill = bkgrd_color, color=NA),
              plot.title = element_textbox_simple(
                      padding = margin(t=10, r=5, b=15, l=10),
                      halign = 0,
                      size = 12),
              plot.caption = element_text(size=7.5, 
                                          colour=txt_color,
                                          hjust=0),
              plot.margin = margin(l=30, r=0),
              # legend settings
              legend.position = "none")
              
dot_plt

#--- Look at September 1998 world records in more depth

three_ns$wr <- "Yes"
all_dates <- seq.Date(min(three_ns$date), max(three_ns$date), by="day")

# Keep track of who is holding world record at the end of each day
record_daily <- three_ns %>%
        group_by(track, date) %>%
        slice(which.min(time)) %>%
        arrange(track, date, -time) %>%
        complete(date = all_dates) %>%
        select(track, date, player, time, nation, wr) %>%
        ungroup() %>%
        group_by(track) %>%
        fill(c("track", "date", "player", "time", "nation"), .direction="down") %>%
        ungroup()

record_daily$wr <- with(record_daily, ifelse(is.na(wr), "No", wr))

record_sept <- record_daily %>%
        filter(date>="1998-09-01" & date<="1998-09-30") %>%
        mutate(color = case_when(
                player=="Penev" ~ m_red,
                # generated using chroma.js
                player=="Booth" ~ "#005a91",
                player=="Jonathan" ~ "#007cb5",
                player=="Lachlan B" ~ "#099edb",
                player=="Peter E" ~ "#4fc0fe",
                TRUE ~ NA_character_
        )) %>%
        mutate(nation = case_when(
                nation=="USA" ~ "USA",
                nation=="Austria" ~ "AUT",
                nation=="Canada" ~ "CAN",
                nation=="Australia" ~ "AUS",
                TRUE ~ NA_character_
        )) %>%
        mutate(date_pos = as.numeric(date)) %>%
        # labels for figure
        mutate(player_label = glue::glue("{player} ({nation})"))

record_sept$player_label <- factor(record_sept$player_label, 
                              levels=c("Penev (AUS)", "Booth (USA)", 
                                       "Jonathan (CAN)", "Lachlan B (AUS)", 
                                       "Peter E (AUT)"))

# Break axis by year based on range of dates
axis_brk <- unique(record_sept$date_pos)[seq(1, length(unique(record_sept$date_pos)), 5)]
axis_brk <- c(axis_brk, max(record_sept$date_pos))

# Extract years for break labels
axis_lbl <- data.frame(label = c(unique(record_sept$date)[seq(1, length(unique(record_sept$date_pos)), 5)], 
                                 max(record_sept$date))) %>%
        mutate(label = as.Date(label))

player_lst <- unique(record_sept[c("player_label", "color")])
player_col <- player_lst$color 
names(player_col) <- player_lst$player_label

tile_plt <- ggplot(data=record_sept, aes(x=date_pos, y=track, fill=player_label)) +
        geom_tile(color=bkgrd_color) + 
        scale_x_continuous(breaks = axis_brk, 
                           labels = format(axis_lbl$label, format="%b %d"), 
                           expand=c(0,0)) +
        scale_fill_manual(values=player_col) +
        # stars indicated days where world records were broken
        geom_star(data=record_sept %>% filter(wr=="Yes"), aes(x=date, y=track),
                   colour=darken((record_sept %>% filter(wr=="Yes"))$color),
                   starshape=1, show.legend = FALSE) +
                   labs(title = glue::glue("After breaking 39 world records (<span style='font-family:Wingdings';>\uF0B6</span>) in September 1998, <span style='color:#E52521';><b>Penev</b></span> was the 
                                           world record holder in 11 of the 16 tracks."),
                        caption = "<b>World records:</b> Includes three-lap, non-shortcut world records.<br>
                        #TidyTuesday | Week 22 • 5/25/21 | @flrnclee") +
        theme(text = element_text(
                family=font_use, 
                color=txt_color), 
              # panel settings
              panel.background = element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              #panel.grid.major.x = element_line(linetype="solid", colour = "#878787"),
              panel.grid.major.y = element_blank(),
              # axis settings
              axis.ticks=element_blank(),
              axis.text.x=element_markdown(colour="#949494"),
              axis.text.y= element_markdown(colour=txt_color),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # plot settings
              plot.background = element_rect(fill = bkgrd_color,
                                             color = NA),
              plot.title = element_textbox_simple(
                      padding = margin(t=10, r=10, b=10, l=5),
                      size = 12,
                      halign = 0),
              plot.caption = element_markdown(size=7.5, 
                                          colour=txt_color, 
                                          lineheight=1.5),
              plot.margin = margin(l=0, r=10),
              # legend settings
              legend.position = "top",
              legend.key.size = unit(1,"mm"),
              legend.background = element_rect(fill="transparent"),
              legend.title = element_blank())

tile_plt

# Title graphic ---------------------------------------------

title_txt <- data.frame(
        x = 0,
        y = 0,
        label = c("<img src='https://raw.githubusercontent.com/flrnclee/tidy-tuesday/main/mario-kart/imgs/mario-kart-64-logo.png', width='130'/> WORLD RECORDS"))

title <- ggplot() +
        # dropshadow
        with_shadow(
                geom_textbox(
                        data = title_txt,
                        aes(x, y, label = label),
                        box.size = 0, box.colour=NA,
                        colour = txt_color,
                        family = "Alfa Slab One", size = 13,
                        width = unit(1, "npc"),
                        fill = NA,
                        hjust = 0, vjust = 0.5,
                        halign = 0.5, valign = 1
        ), x_offset=10, y_offset=10, sigma=5) +
        scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
        scale_y_continuous(limits=c(-1,1), expand=c(0,0)) + 
        theme(text = element_text(color=txt_color), 
                 panel.background = element_blank(),
                 panel.grid=element_blank(),
                 panel.border=element_blank(),
                 axis.ticks=element_blank(),
                 axis.text=element_blank(),
                 plot.title = element_textbox_simple(
                         lineheight = 1,
                         padding = margin(5.5, 5.5, 5.5, 5.5),
                         halign = 0
                 ),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank())

title

# Summary stats graphic ---------------------------------------------

player_num <- players_unique$player %>% length()
nation_num <- (three_ns %>% filter(!is.na(nation)))$nation %>% unique() %>% length()
record_num <- three_ns %>% nrow()

summ_txt <- data.frame(
        x = c(0.0, 0.35, 0.7),
        y = c(0, 0, 0),
        label = c(
                glue::glue("<p><span style='font-family:AlfaSlabOne-Regular; font-size:30pt;'>{record_num}</span></p><p><span style='font-family:Overpass; font-size:10pt';><b>WORLD RECORDS</b></span></p>"), 
                glue::glue("<p><span style='font-family:AlfaSlabOne-Regular; font-size:30pt;'>{player_num}</span></p><p><span style='font-family:Overpass; font-size:10pt;'><b>WORLD RECORD HOLDERS</b></span></p>"),
                glue::glue("<p><span style='font-family:AlfaSlabOne-Regular; font-size:30pt;'>{nation_num}</span></p><p><span style='font-family:Overpass; font-size:10pt;'><b>COUNTRIES REPRESENTED</b></span></p>")) 
)

summary <- ggplot() +
        geom_textbox(
                data = summ_txt,
                aes(x, y, label = label),
                family = rep("Alfa Slab One", 3),
                colour = rep(txt_color, 3),
                size = rep(5, 3),
                box.colour = c(m_red, m_yellow, m_blue),
                box.size = 1,
                fill = NA,
                width = unit(0.3, "npc"),
                hjust = 0, 
                vjust = rep(0.5,3),
                halign = 0.5) +
        scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
        scale_y_continuous(limits=c(-0.5, 0.5), expand=c(0,0)) +
        theme(text = element_text(family=font_use), 
              panel.background = element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              axis.ticks=element_blank(),
              axis.text=element_blank(),
              plot.title = element_textbox_simple(
                      lineheight = 1,
                      padding = margin(5.5, 5.5, 5.5, 5.5),
                      halign = 0
                      ),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              )

summary

# Assemble layout ---------------------------------------------

layout <- c(
        area(t = 1,
             l = 1,
             b = 2,
             r = 12),
        area(t = 3,
             l = 1,
             b = 4,
             r = 12),
        area(t = 5,
             l = 1,
             b = 10,
             r = 5),
        area(t = 5,
             l = 6, 
             b = 10,
             r = 12))

title + summary + dot_plt + tile_plt +
        plot_layout(design=layout) &
        theme(
                plot.background = element_rect(fill = bkgrd_color, color=NA),
                plot.margin = margin(t=10, b=5, r=10, l=10))

ggsave(
        paste0("mariokart", format(Sys.time(), "%Y%m%d"), ".png"),
        dpi = 320,
        width = 11,
        height = 7.5,
        units = "in",
        type = "cairo-png"
)
