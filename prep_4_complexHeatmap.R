### Funtion for preparing data for visualization using the complexHeatmap package  
# Note: this is very specific to the dataset that this function was written for


library(ComplexHeatmap)
### viped summary reformat for heatmap ----
pre_heatmat <- function(data, vipsum){
          group <- vipsum$Group
          IDvip <- vipsum$ID
          
          data_long <- pivot_longer(data = data,
                                    cols = matches("^S"),
                                    values_to = "Intensity",
                                    names_to = c("Group-Sample")) %>%
                    separate(col = "Group-Sample", 
                             into = c("Group", "Sample"),
                             sep = "\\-")
          
          towide <- dplyr::select(vipsum,
                                  ID, mean_Int, Group_mark = Group)%>% 
                    right_join(., data_long, by = "ID") %>% 
                    filter(!is.na(Group_mark)) %>% 
                    filter(ID %in% IDvip) 
          
          wide <- dplyr::mutate(towide,
                                Sample = paste(Group,"-",Sample)) %>% 
                    dplyr::select(ID, Group_mark, Intensity, Sample) %>% 
                    pivot_wider(data = .,
                                names_from = Sample,
                                values_from = Intensity)
          
          group_mark <- wide$Group_mark
          
          tomat <- dplyr::select(wide, -c(Group_mark, ID)) 
          
          row.names(tomat) <- wide$ID
          
          tomat <- as.matrix(tomat) #%>% 
          #scale()
          
          groupcols <- str_split_fixed(colnames(tomat), pattern = "\\ - ", n = 2)[,1]
          
          thelist <- list(matrix = tomat,
                          Group_mark = group_mark,
                          Column_group = groupcols)
          
          return(thelist)
}