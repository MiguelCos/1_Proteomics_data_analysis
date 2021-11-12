## prep for clust 

library(tidyverse)

datase <- read.delim("input_limma.txt")


summ <- dplyr::mutate(datase, 
                      ALS = mean(P85c_ALS, P87c_ALS, P89c_ALS, P88c_ALS),
               Baseline = mean(P85c_Baseline, P87c_Baseline, P88c_Baseline, P89c_Baseline),
               CARL =  mean(P85c_CARL, P87c_CARL, P88c_CARL, P89c_CARL))

summ <- mutate(datase, ALS = rowMeans(select(datase, ends_with("ALS")), na.rm = TRUE),
               Baseline = rowMeans(select(datase, ends_with("Baseline")), na.rm = TRUE),
               CARL = rowMeans(select(datase, ends_with("CARL")), na.rm = TRUE) )

summ2 <- select(summ, ID, Baseline, ALS, CARL)

write.table(summ2, "input_clust.txt", row.names = FALSE)
