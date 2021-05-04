## Function to extract the list of proteins from a table based on their VIP values after 
# sPLS-DA model.
# The function will select proteins with VIP > 1 as those that have an importance on the
# differentiation between the classifiers.

get_viped_summary <- function(splsda_mod, proteins_grouped){
          vip_splsda <- vip(splsda_mod)
          vip_splsda_big<- vip_splsda[apply(vip_splsda[,-1], 1, function(x) all(abs(x)>1)),]
          
          proteins_vipbig <- row.names(vip_splsda_big)
          
          viped_summary <- proteins_grouped %>% 
                    filter(ID %in% proteins_vipbig) %>% 
                    dplyr::select(-duplicated)
          
          return(viped_summary)
}