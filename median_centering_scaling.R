## snippet of code to median-centering scaling

log2_quant <- mutate_all(as.data.frame(a_matrix), 
                         log2)

scaled_data <- scale(log2_quant,
                     scale = F,
                     center = apply(log2_quant, 2, median, na.rm = TRUE) - median(as.matrix(log2_quant), na.rm = TRUE))


