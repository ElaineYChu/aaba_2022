# Color Palette
aaba_colors_2022 <- c('#2F333A','#E4F9F9','#07CB98','#5A90D1','#E6AD1E','#EA6312','#8253A9','#CB274A')

# Figure Defaults
elaine_theme <-  theme_bw() + theme(panel.grid.major=element_blank(),
                                    panel.grid.minor=element_blank(),
                                    legend.background=element_blank(),
                                    legend.box.background=element_rect(color='black'),
                                    legend.key=element_blank(),
                                    legend.title=element_text(face='plain',size=14),
                                    legend.text=element_text(size=12),
                                    axis.title=element_text(size=15, lineheight=.9, vjust=.3),
                                    axis.text=element_text(size=12),
                                    axis.title.x=element_text(vjust=.2),
                                    axis.title.y=element_text(vjust=.3),
                                    plot.title=element_text(size=18, hjust=0.5))