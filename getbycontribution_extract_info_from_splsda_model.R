## Function to get the proteins by their maximal contribution ----
## This function is used to work with the mixOmics package and extract information from 
## the constructed splsda model.

getbycontribution <- function(data, splsda_model){
          
          n_comps <- splsda_model$ncomp
          
          prots_complist <- list()
          
          for (i in 1:n_comps){
                    prots_complist[[i]] <- selectVar(splsda_model, comp = i)$value %>% 
                              mutate(protein_id = row.names(.), comp = paste0("comp",i))
          }
          
          prots_percomp <- do.call(bind_rows, prots_complist)
          
          data_long <- pivot_longer(data = data,
                                    cols = matches("^S"),
                                    values_to = "Intensity",
                                    names_to = c("Group-Sample"))
          
          int_per_group <- separate(data_long, 
                                    col = `Group-Sample`, 
                                    sep = "-",
                                    into = c("Group", "Sample")) %>% 
                    group_by(ID, Group) %>% 
                    summarise(median_Int = median(Intensity),
                              mean_Int = mean(Intensity)) %>% 
                    filter(ID %in% prots_percomp$protein_id)
          
          proteins_group <- int_per_group %>%
                    ungroup() %>%
                    group_by(ID) %>%
                    filter(mean_Int == max(mean_Int)) %>% 
                    mutate(duplicated = duplicated(ID)) %>%
                    ungroup()
          
          return(proteins_group)
}