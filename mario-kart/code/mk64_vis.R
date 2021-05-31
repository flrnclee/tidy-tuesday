# Setting up ---------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(classInt)
library(scales)

library(Cairo)
library(patchwork)

library(ggplot2)
library(ggtext)
library(ggfx)
library(ggforce)

library(extrafont)
library(fontawesome)

library(viridis)
library(RColorBrewer)


# Reading in data from TidyTuesday
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
drivers <- tuesdata$drivers
records <- tuesdata$records

# Global variables ------------------------------------
bkgrd_color <- "#434343"
txt_color <- "#ffffff"
font_use <- "Overpass"

# Preparing data ----------------------------------------

# In case we need nation
drivers_unique <- unique(drivers[c("player", "nation")])

# 5 drivers have missing values on nation
drivers_unique %>% filter(is.na(nation))

# ---- Look at three lap, no shortcut records over time

three_ns <- records %>% 
        filter(type=="Three Lap" & shortcut=="No") %>%
        left_join(drivers_unique)

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

# Highly skewed data - use Jenks, Fisher classification
ggplot(data=nation_rec_my, aes(x=wr_num)) +
        geom_boxplot()

wr_brks <- classIntervals(nation_rec_my$wr_num, n=5, style="jenks")$brks
wr_brks[1] <- wr_brks[1]-1

nation_rec_my <- nation_rec_my %>%
        mutate(wr_num_cat = cut(wr_num, breaks=wr_brks,
                                 labels=c("0-2", "3-5", "6-10", "11-16", "17+"))) %>%
        mutate(wr_num_cat = as.factor(wr_num_cat))

ggplot(data=nation_rec_my %>% filter(!is.na(nation)), aes(x=date_my, y=nation, fill=wr_num_cat)) +
        geom_tile(color=bkgrd_color) + 
        # scales
        scale_y_discrete(expand=c(0,0))+
        scale_fill_manual(values=viridis_pal(option="inferno")(5)) +
        # labels
        labs(title = "<b>Number of world records by month</b>",
             fill = "# of world records") + 
        annotate(
                geom = "curve", x = as.Date("2000-08-01"), y = "Canada", xend = as.Date("1998-09-01"), yend = "Australia", 
                curvature = .3, arrow = arrow(length = unit(2, "mm")), color = txt_color) +
        annotate(
                geom = "richtext", x = as.Date("2000-08-01"), y = "Norway", label = "In September 1998, Australian player<br>Penev broke 
                <b><span style='color: #FCFFA4';>39 world records</span></b>.",
                hjust = "left", vjust="top", family=font_use, size=3, color = txt_color, lineheight = 1.1, fill=NA) + 
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
                        axis.text.y=element_markdown(colour=txt_color),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        # plot settings
                        plot.background = element_rect(fill = bkgrd_color),
                        plot.title = element_textbox_simple(
                                padding = margin(t=5, r=10, b=20, l=0),
                                halign = 0),
                        # plot.subtitle = element_textbox_simple(
                        #         padding = margin(t=0, r=10, b=20, l=0),
                        #         halign = 0),
                        plot.caption = element_text(size=7.5, colour=txt_color),
                        # legend settings
                        legend.position = "top",
                        legend.background = element_rect(fill="transparent"),
                        legend.title = element_text(face="bold"))

