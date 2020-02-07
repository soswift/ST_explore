make_phyloseq <- function(cleaned_pipeline) {
  require(phyloseq)
  ### For Testing
  # cleaned_pipeline <- miseq01
  
  abund <- t(cleaned_pipeline[["abundance"]])
  tax   <- cleaned_pipeline[["taxonomy"]]
  meta  <- cleaned_pipeline[["metadata"]]
  
  head(abund)[1:6, 1:10]
  head(tax)[1:6, 1:8]
  head(meta)[1:6, 1:10]
  
  # clean up abundance table
  colnames(abund) <- abund[2, ]
  colnames(abund)
  
  abund <- abund[3:nrow(abund), ]
  
  rows_abund <- row.names(abund)
  cols_abund <- colnames(abund)
  
  abund     <- apply(abund, 2, as.numeric)
  
  colnames(abund) <- cols_abund
  rownames(abund) <- rows_abund
  # clean up taxonomy
  row.names(tax) <- tax$OTU
  tax <- tax[2:ncol(tax)]
  tax <- as.matrix(tax)
  
  # clean up sample table
  row.names(meta) <- meta$id
  meta <- meta[2:ncol(meta)]
  meta$sample_or_control <-
    ifelse(grepl("pcrneg", meta$sample_type, ignore.case = T),
           "Control",
           "Sample")
  # transform into phyloseq
  OTU     <- otu_table(abund, taxa_are_rows = TRUE)
  TAX     <- tax_table(tax)
  samples <- sample_data(meta)
  
  # This is the phyloseq object:
  ps_object <- phyloseq(OTU, TAX, samples)
  return(ps_object)
}