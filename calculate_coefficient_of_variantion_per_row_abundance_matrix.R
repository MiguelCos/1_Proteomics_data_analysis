## calculate the coefficient of variantion  ----
## Miguel Cosenza v1.0

# required packages ----

library(dplyr)
library(here)
library(janitor)

# required data----

# you need a data frame in wide format (each row = 1 protein, each column = 1 sample) 

# this one is the output from Fragpipe + TMT integrator

df_mat_pre <- read_tsv(file = here::here("data/abundance_protein_MD.tsv")) %>%
  clean_names()

# it would need some preprocessing to eliminate non-interesting columns
# this might be different if you are starting from a different kind of output

df_mat <- df_mat_pre %>%
  dplyr::select(-c(gene, number_psm, max_pep_prob, reference_intensity)) %>%
  column_to_rownames("index") 

# calcualte CV
# based on: https://www.statology.org/coefficient-of-variation-in-r/

coef_var <- function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

df_mat_w_coef_var <- df_mat %>% 
  rowwise() %>% 
  mutate(cpef_var = coef_var(c_across(where(is.numeric))))


         