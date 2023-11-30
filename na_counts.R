### Get tabular info of NA counts per protein per sample (without taking into account grouping information) ----

na_counts <- function(data, max_na_per_group){
          
          data_long <- pivot_longer(data,
                                    cols = where(is.numeric),
                                    values_to = "Abundance",
                                    names_to = c("Sample")) %>%
                    separate(col = "Sample", 
                             into = c("Condition", "Run"),
                             sep = "\\_", 
                             remove = FALSE)#
          # count nas here
          na_count <- group_by(data_long,
                               PROTEIN) %>%
                    summarise(na_count = sum(is.na(Abundance)),
                              total = n()) %>% 
                    ungroup() %>% 
                    mutate(NA_fraction = na_count/total)
          
          
          included_prots <- dplyr::filter(na_count,
                                          NA_fraction < max_na_per_group)
          
          inclusion_list <- included_prots %>%
                    pull(PROTEIN)
          
          list_nacounts <- list(na_count = na_count,
                                included_prots = included_prots,
                                proteins_2_include = inclusion_list)
          
          
          return(list_nacounts)
          
}