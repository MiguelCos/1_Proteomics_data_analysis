#### Function to prep data for UpSet plot 
### This is very specific to the dataset I was working with at the moment

library(UpSetR)
library(purrr)

make_upset <- function(data, group){
          
          form <- data %>% 
                    filter(Group == group) %>% 
                    dplyr::select(ID, Format)
          
          prots <- data %>% 
                    filter(Group == group) %>% 
                    dplyr::select(ID)
          
          wide_prots <- split(prots, f = form$Format)
          
          vects_prots <- purrr::map(.x = wide_prots, .f = ~discard(flatten_chr(.), . == ""))
          
          
          upset(fromList(vects_prots), nsets = 6)
}