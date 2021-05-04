### Volcano plot with ggplot function 

our_volcano <- function(dataarg, FC_cutoff = 1, pval_cutoff = 0.05, 
                        color_diffex = "red", color_nondifex = "#2a9d8f", 
                        interesting_proteins, vert_line_col = "red", 
                        vert_line_pos = 1,
                        hline_col = "red", hline_pos = 0.05, 
                        linetype = "dashed") {
  volcano <- ggplot(data = dataarg,
                    mapping = aes(x = logFC, y = -log10(adj.P.Val))) +
    geom_point()+
    geom_point(data = dataarg %>% filter(logFC > FC_cutoff,
                                         adj.P.Val < pval_cutoff),
               mapping = aes(x = logFC, y = -log10(adj.P.Val)), color = "red")+
    geom_point(data = dataarg %>% filter(logFC < -FC_cutoff,
                                         adj.P.Val < pval_cutoff),
               mapping = aes(x = logFC, y = -log10(adj.P.Val)), color = "red")+
    geom_point(data = dataarg %>% filter(logFC > FC_cutoff,
                                         adj.P.Val > pval_cutoff),
               mapping = aes(x = logFC, y = -log10(adj.P.Val)), color = "#2a9d8f")+
    geom_point(data = dataarg %>% filter(logFC < -FC_cutoff,
                                         adj.P.Val > pval_cutoff),
               mapping = aes(x = logFC, y = -log10(adj.P.Val)), color = "#2a9d8f")+
    
    ggrepel::geom_text_repel(data = dataarg %>% filter(`Gene names` %in% interesting_proteins),
                             aes(label = `Gene names`))+
    geom_hline(yintercept = -log10(hline_pos),
               color = "red", linetype = "dashed")+
    geom_vline(xintercept = c(-vert_line_pos,vert_line_pos),
               color = "red", linetype = "dashed")+
    theme_bw()
  
}