### Randomly sample x number of proteins to plot  

sample_proteins <- function(x, # char vector of protein IDs
                            size, # number of proteins to sample
                            seed = 363 # random seed number 
) {
          
          # select/sample N proteins to observe their abundance distribution  
          set.seed(seed)
          
          proteins <- x %>% 
                    unique() %>%
                    str_remove_all(pattern = "Biognosys") # get unique protein IDs
          
          which_prots <- sample(proteins, 
                                size = 50, 
                                replace = FALSE)
          
}