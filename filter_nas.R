### Filter dataset by missing values ----

### This is a guidance script/function to filter out those proteins with X
# max missing values in a particular group.

na_filter <- function(data, max_na_per_group, logtrans, minInt_inpute, fraction_Inpute = 5){
          
          data_long <- pivot_longer(data = data,
                                    cols = matches("^S"),
                                    values_to = "Intensity",
                                    names_to = c("Group-Sample")) %>%
                    separate(col = "Group-Sample", 
                             into = c("Group", "Sample"),
                             sep = "\\-")
          
          if (logtrans){
                    data_long <- mutate(data_long,
                                        Intensity = log2(Intensity))
          }
          
          na_count <- group_by(data_long,
                               ID, Group) %>%
                    summarise(na_count = sum(is.na(Intensity)),
                              total = n()) %>% 
                    ungroup() %>% 
                    mutate(NA_fraction = na_count/total)
          
          nafraction <- group_by(na_count,
                                 ID) %>%
                    summarise(max_nafraction = max(NA_fraction),
                              min_nafraction = min(NA_fraction)) %>%
                    ungroup()
          
          included_prots <- dplyr::filter(nafraction,
                                          max_nafraction <= max_na_per_group)
          
          
          
          if (minInt_inpute){
                    
                    min_intensityprot <- data_long %>% 
                              group_by(ID) %>% 
                              mutate(min_int = min(Intensity, na.rm = TRUE)) %>%
                              mutate(th_of_min_int = min_int/fraction_Inpute)
                    
                    data_long_nonas1 <- min_intensityprot %>% 
                              mutate(Intensity = ifelse(is.na(Intensity),
                                                        yes = th_of_min_int,
                                                        no = Intensity),
                                     `Group-Sample` = paste(Group,"-",Sample, sep = "")) %>%
                              dplyr::select(ID, `Group-Sample`, Intensity) %>%
                              dplyr::filter(ID %in% included_prots$ID)
                    
                    data_maxperc_nas <- pivot_wider(data_long_nonas1,
                                                    names_from = `Group-Sample`,
                                                    values_from = Intensity)
          } else {
                    data_long_nonas1 <- filter(data_long,
                                               ID %in% included_prots$ID) %>% 
                              mutate(`Group-Sample` = paste(Group,"-",Sample, sep = "")) %>%
                              dplyr::select(ID, `Group-Sample`, Intensity)
                    
                    data_maxperc_nas <- pivot_wider(data_long_nonas1,
                                                    names_from = `Group-Sample`,
                                                    values_from = Intensity)
          }
          
          
}




