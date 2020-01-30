

# read and clean 16S ------------------------------------------------------
# setwd("~/Desktop/R/CMAIKI_clean_and_query/bacterial_16S/")
# clean 16S pipeline outputs for R analyses

clean_16S_tables <- function(abundance_file = "Miseq01/abundance_0.03.relabund",
                             taxonomy_file  = "Miseq01/consensus_0.03.taxonomy",
                             metadata_file  = "Miseq01/Miseq1MappingFile.csv",
                             description    = "Miseq01"){
  
  # Requirements
  require(dplyr)
  require(tidyr)
  
  ########## START
  
  #### *** Test Case COMMENT OUT UNLESS TESTING ***
   # abundance_file = "Miseq03/abundance_table_97.shared"
   # taxonomy_file  = "Miseq03/annotations_97.taxonomy"
   # metadata_file  = "Miseq03/Miseq03_MappingFile - Sheet1.csv"
  # description    = "Miseq01_97"
  # 
  # abundance_file = "../../data/interim/all_clustering_97.shared"
  # taxonomy_file = "../../data/interim/all_consensusClassification_97.taxonomy"
  # metadata_file = "../../data/raw/st_miseq04_mapping_file - mapping_new.csv"
  # description = "ST_16S_Miseq04"
  
  # check input files
  if(!file.exists(abundance_file)){
    warning("Abundance file doesn't exist")
  }
  if(!file.exists(taxonomy_file)){
    warning("Taxonomy file doesn't exist")
  }
  if(!file.exists(metadata_file)){
    warning("Metadata file doesn't exist")
  }
  
  
  ## Read in tables
  abund <- read.table( abundance_file, header = T, stringsAsFactors = F )
  tax   <- read.table( taxonomy_file, header = T, stringsAsFactors = F )

  # read in metadata as characters
  fullmeta  <- read.table( metadata_file, header = T, stringsAsFactors = F,
                      colClasses = "character",
                      sep = "," )
  ## Clean tables
  # Clean taxonomy table:
  #Split taxonomic annotations (col = Taxonomy, generates warning because of trailing ";")
  tax <- tax %>% 
            separate( Taxonomy,
                      c( "kingdom", "phylum", "class", "order","family","genus" ),
                      sep = ";")
  
  # Clean abundance table: Cut down id name and delete numOtus
  abund$Group <- sub(".*(1\\d{5}).*","\\1", abund$Group, perl = T)
  abund       <- abund[-3]
  
  # Clean metadata table: Subset to include only samples that appear in the abundance table
  meta        <- fullmeta[fullmeta$id %in% abund$Group,]
  
  
  ## Check if joins work:
  # Check abundance and taxonomy have the same OTUs
  
  if( !all( colnames(abund)[-c(1,2)] %in% tax$OTU)){ # test
          message("*** WARNING Abundance and taxonomy have different OTUs")   # no
  } 
  else {
    message("OKAY Abundance and taxonomy have the same OTUs")                 # yes
    }         
 
   # Check abundance and metadata have same ids
  
  if( !all( abund$Group %in% meta$id)){ # test
    message("*** WARNING Abundance and Metadata have different OTUs")   # no
  } 
  else {
    message("OKAY Abundance and Metadata have the same groups")           # yes
    }    # yes

  # Make all tables into a list
  result <- list( abund, tax, meta, fullmeta)
  names(result) <- c("abundance", "taxonomy", "metadata","full_run_metadata")
  
  # Write out all tables as .csv and return a "result" list of tables
  lapply(1:length(result),
         function(x) write.csv(result[[x]],
                               file = paste(description, names(result[x]), "table.csv", sep = "_"),
                               row.names = F))
  # write message
  message(paste("tables written out as csv files starting with ", description, "...", sep = ""))
  return(result)
  ########## END
}

# miseq1 <- clean_16S_tables(abundance_file = "Miseq01/abundance_0.03.relabund",
#                            taxonomy_file  = "Miseq01/consensus_0.03.taxonomy",
#                            metadata_file  = "Miseq01/Miseq1MappingFile.csv",
#                            description    = "Miseq01_97")
# 
# miseq2 <- clean_16S_tables(abundance_file = "Miseq02/abundance_0.03.relabund",
#                            taxonomy_file  = "Miseq02/consensus_0.03.taxonomy",
#                              metadata_file  = "Miseq02/Miseq02_MappingFile.csv",
#                            description    = "Miseq02_97")

# miseq2 is mostly crow lab data! To subset for just CMAIKI:

# subset abundance based on metadata
# miseq2$abundance <- miseq2$abundance[miseq2$abundance$Group %in% miseq2$metadata$id,]
# 
# # subset taxonomy based on abundance
# miseq2$taxonomy  <- miseq2$taxonomy[miseq2$taxonomy$OTU %in% colnames(miseq2$abundance),]
# 
# lapply(1:length(miseq2),
#        function(x) write.csv(miseq2[[x]],
#                              file = paste("just_CMAIKI_Miseq2", names(miseq2[x]), "table.csv", sep = "_"),
#                              row.names = F))

# check tables

# read.csv("Miseq01_97_abundance_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
# read.csv("Miseq01_97_taxonomy_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
# read.csv("Miseq01_97_metadata_table.csv",header = T, stringsAsFactors = F, colClasses = "character")[1:5,1:5]
# 
# read.csv("Miseq02_97_abundance_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
# read.csv("Miseq02_97_taxonomy_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
# read.csv("Miseq02_97_metadata_table.csv",header = T, stringsAsFactors = F, colClasses = "character")[1:5,1:5]
#
 # read.csv("just_CMAIKI_Miseq2_abundance_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
 # read.csv("just_CMAIKI_Miseq2_taxonomy_table.csv",header = T, stringsAsFactors = F)[1:5,1:5]
 # read.csv("just_CMAIKI_Miseq2_metadata_table.csv",header = T, stringsAsFactors = F, colClasses = "character")[1:5,1:5]
 # 

# summarize 16S sequencing success --------------------------------------------------
pass_fail <- function(clean_tables_list){
            #### for testing!! COMMENT OUT
             #clean_tables_list <- miseq1
            ####
          
            # shorten variable
            run   <- clean_tables_list 
          
            # pull out samples that are in the abundance table, assume these sequence well
            good_samples <- run$full_run_metadata[ run$full_run_metadata$id %in% run$abundance$Group,]
            
            # pull out samples that are not in the abundance table, assume these did not sequence well
            bad_samples  <- run$full_run_metadata[( !(run$full_run_metadata$id %in% run$abundance$Group)
                                                  & run$full_run_metadata$locus == "16S" ),]
            result<-list(good_samples,bad_samples)
            names(result) <- c("good_samples","bad_samples")
            message(paste("for ",
                    deparse(substitute(clean_tables_list)), # run name, ideally
                    ", ",
                    nrow(good_samples), # number of good samples
                    " samples sequenced well and ",
                    nrow(bad_samples), " did not", sep = "")) # number of bad samples
          return(result)

}
# 
# miseq2_16S_quality <- pass_fail(miseq2)
# 
# miseq1_16S_quality <- pass_fail(miseq1)
# 
# write.csv(miseq1_16S_quality$bad_samples, "miseq1_bad_samples.csv", row.names = F)
# 
# write.csv(miseq2_16S_quality$bad_samples, "miseq2_bad_samples.csv", row.names = F)
# 
# 


