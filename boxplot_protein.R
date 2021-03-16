## do a ggplot boxplot of one protein or many proteins starting from a wide table  


boxplot_protein <- function(x, protein, title = NULL){
          
          data_long <- pivot_longer(data = x,
                                    cols = matches("^S"),
                                    values_to = "Intensity",
                                    names_to = c("Group-Sample")) %>%
                    separate(col = "Group-Sample", 
                             into = c("Group", "Sample"),
                             sep = "\\-")
          
          if (length(protein) > 1){
                    
                    plot_prot <- ggplot(data_long %>% filter(ID %in% protein), 
                                        aes(x = Group, y = Intensity))+
                              geom_boxplot()+
                              facet_wrap(~ID, ncol = 3, scales = "free")+
                              ggtitle(label = title)+
                              theme_bw()
                    
          } else {
                    
                    plot_prot <- ggplot(data_long %>% filter(ID == protein), 
                                        aes(x = Group, y = Intensity))+
                              geom_boxplot()+
                              facet_wrap(~ID, ncol = 3)+
                              ggtitle(label = title)+
                              theme_bw()
                    
          }
          
          return(plot_prot)
}