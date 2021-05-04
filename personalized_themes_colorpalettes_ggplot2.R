## Personalized themes and color pallettes ----

scale_fill_manual(values=c("#071d5c", "#008683", "#005583", "#84b082"))+
          theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.1, size = 10),
                axis.text.y = element_text(hjust = 0.5, vjust = 0.1, size = 10, angle = 90),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "grey"),
                panel.border = element_rect(colour = "black", fill=NA, size=1.5),
                axis.title=element_text(size=12,face="bold"))