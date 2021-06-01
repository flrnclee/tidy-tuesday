
# keep the fastest time from that month
three_ns_my <- three_ns %>%
     arrange(track, date_my) %>%
     group_by(track, date_my) %>%
     summarise(
          time=min(time)) %>%
     mutate(time_before=lag(time)) %>%
     mutate(time_improve = time-time_before) %>%
     select(-time_before) %>%
     ungroup()

# Clean up handful of invalid world records (longer time than previous WR)
three_ns_my$time_improve <- with(three_ns_my,
                                 ifelse(time_improve>0, 0, time_improve))

time_brks <- classIntervals(three_ns_my$time_improve, n=7, style="jenks")$brks
time_brks[1] <- time_brks[1]-1

three_ns_my <- three_ns_my %>%
     mutate(improve_grp = cut(time_improve, breaks=time_brks,
                              labels=c("10.30+", "4.89-10.30", "3.18-4.89", "1.85-3.18", "0.82-1.85", "0.28-0.82", "0-0.28"))) %>%
     mutate(improve_grp=factor(as.character(improve_grp), levels=rev(levels(improve_grp))))

three_ns_my$track <- factor(three_ns_my$track,
                            levels= three_ns_diff$track) 

ggplot(data=three_ns_my, aes(x=date_my, y=track, fill=improve_grp)) +
     geom_tile(color="gray20") + 
     scale_y_discrete(expand=c(0,0))+
     scale_fill_manual(values=viridis_pal(option="turbo")(7)) +
     theme(text = element_text(), 
           panel.background = element_blank(),
           panel.grid=element_blank(),
           axis.ticks=element_blank(),
           axis.text.x=element_markdown(),
           axis.text.y=element_markdown(),
           panel.border=element_blank(),
           panel.grid.major.x = element_line(linetype="solid", colour = "#555555"),
           panel.grid.major.y = element_blank(),
           plot.background = element_rect(fill = "gray20"),
           plot.title = element_textbox_simple(
                padding = margin(t=20, r=10, b=0, l=0),
                halign = 0),
           plot.subtitle = element_textbox_simple(
                padding = margin(t=0, r=10, b=20, l=0),
                halign = 0),
           axis.title.x = element_textbox_simple(
                padding = margin(t=10),
                halign = 0.5, 
                size = 10),
           axis.title.y = element_blank(),
           plot.caption = element_text(size=7.5, colour="#444444"), 
           legend.position = "right") 




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
