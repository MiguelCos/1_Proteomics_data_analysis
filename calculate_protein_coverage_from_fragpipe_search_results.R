# function to calculate protein coverage 
# input: psm_tab and prot_tab; corresponding to tables
# psm.tsv and protein.tsv from Fragpipe, respectively

calculate_protein_coverage <- function(psm_tab, prot_tab) {
  # get information about peptide location in protein sequence from psm_tab file
  peptide_location <- psm_tab %>%
    dplyr::select(peptide,
                  protein_id,
                  gene,
                  protein_description,
                  protein_start,
                  protein_end) %>%
    distinct() %>%
    rename(protein_length = length)

  # get information about protein length from prot_tab file
  protein_length <- prot_tab %>%
    dplyr::select(protein_id,
                  length) %>%
    distinct() %>%
    rename(length_prot = length)

  # merge this information into a single table
  protein_coverage_info <- peptide_location %>%
    left_join(., 
              protein_length,
              by = "protein_id") %>%
    distinct()

  # calculate the fraction of contribution for each peptide within each protein
  peptide_contribution <- protein_coverage_info %>%
    mutate(
      peptide_length = protein_end - protein_start + 1,
      peptide_fraction = peptide_length / length_prot * 100
    )

  # calculate the overlap of the peptides within each protein
  peptide_overlap <- peptide_contribution %>%
    group_by(protein_id) %>%
    arrange(protein_start) %>%
    mutate(
      overlap = ifelse(row_number() > 1, pmax(0, pmin(protein_end, lag(protein_end, default = -Inf)) - pmax(protein_start, lag(protein_start, default = -Inf)) + 1), 0)
    )

  # calculate the total coverage per protein
  protein_coverage <- peptide_overlap %>%
    group_by(protein_id) %>%
    summarise(
      total_coverage = sum(peptide_length - overlap) / length_prot[1] * 100
    )

  # merge the peptide contribution and protein coverage tables
  result <- peptide_overlap %>%
    select(peptide, protein_id, gene, protein_description, protein_start, protein_end, peptide_length, peptide_fraction, overlap) %>%
    left_join(., 
              protein_coverage,
              by = "protein_id") %>%
    distinct()

  return(result)
}