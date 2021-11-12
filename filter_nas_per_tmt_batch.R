## Select proteins that are missing in > X TMT mixtures/batches
# long matrix: a table in slim/long format containing at least 3 columns
## Protein (protein ID), Abundance (abundance of the protein), 
## mixture (TMT mixture in which the protein was quantified)

# Tip: you would need to transform your abundance wide matrix into long format
# then use an annotation table to left_join with... Then you will have quant
# info in long format for each protein and TMT batch.


sel_proteins_missing <- function(long_matrix,
                                 threshold) {
          
          na_count <- group_by(long_matrix,
                               Protein, mixture) %>%
                    summarise(na_count = sum(is.na(Abundance)),
                              total = n()) %>% 
                    ungroup() %>% 
                    mutate(NA_fraction = na_count/total)
          
          na_count_perbatch <- na_count %>%
                    group_by(Protein) %>%
                    summarise(na_per_batch = sum(NA_fraction)) %>%
                    ungroup()
          
          proteins2exclude <- na_count_perbatch %>%
                    filter(na_per_batch > threshold) %>%
                    pull(Protein)
}