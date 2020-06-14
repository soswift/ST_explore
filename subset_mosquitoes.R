# Quick r script to subset mosquito samples from non-subsampled supertransect data set
setwd("~/Documents/Bioinformatics/Projects/SuperTransect/Analysis/exploration/R_notebook/")
library(data.table)

## read in abundance table
abund <- read.table("../../../Pipeline/16S-pipeline_outputs/Results/raw/all_clustering_100.shared", sep = "\t", header = T)

abund[1:5,1:5]

## read in taxonomy table
tax <- read.table("../../../Pipeline/16S-pipeline_outputs/Results/raw/all_consensusClassification_100.taxonomy", sep = "\t", header = T)
tax[1:5,1:3]

## read in metadata table
meta <- read.table("data/raw/sample/SuperTransect_mapping_file.csv", sep = ",", header = T)

## subset metadata to aedes albopictus
meta <- data.table(meta)
mosq <- meta[grepl("albopictus",metadata)]

## subset abundance
abund <- data.table(abund)
mosq_abund  <- abund[Group %in% mosq$id]

keep_cols <- colSums(mosq_abund) > 0
keep_cols[1] <- TRUE

mosq_abund <- mosq_abund[,keep_cols, with = F]

## subset taxonomy
tax <- data.table(tax)

keep_otus <- colnames(mosq_abund)[4:ncol(mosq_abund)]

mosq_tax <- tax[OTU %in% keep_otus]

## write out all tables

fwrite(abund, "data/processed/mosquito/transect_abund.csv")
fwrite(tax, "data/processed/mosquito/transect_tax.csv")
fwrite(meta, "data/processed/mosquito/transect_meta.csv")
