
require(tidyverse)
require(vegan)

#16S
setwd("D:/Kerguelen_project/")

##Import OTU data

ASV <- read.csv("./Genomics/16S/resultarchive-Kerguelen/seqtab.nochim_Kerguelen_16S.csv", header=TRUE, sep=",") 

###ASV translation into ASV names instead of whole sequences
translation <- read.csv("./Genomics/16S/resultarchive-Kerguelen/Kerguelen_fasta.txt", header = T, sep = "\t")
ASV$Sequence <- as.character(ASV$X)
translation$Sequence <- as.character(translation$Sequence)
###change the Sequences into ASV names
ASV <- right_join(ASV, translation, by="Sequence")
rownames(ASV) <- ASV$ASV
ASV$X <- NULL
ASV$Sequence <- NULL
ASV$ASV <- NULL

###sequences have weird names so we need to translate into station names
name_trans <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/Sequence_Station_translation_16S.txt", header = T, sep = "\t")
name_trans$sequence_name <- sub("180201_M00758_0705_", "", name_trans$sequence_name)
name_trans <- name_trans%>%filter(sequence_name %in% colnames(ASV))
name_trans$sample_ID <- as.character(name_trans$sample_ID)
ASV <- ASV%>% rename_at(vars(name_trans$sequence_name), ~ name_trans$sample_ID)


##import taxonomic data
taxonomy_16S <- read.csv("./Genomics/16S/resultarchive-Kerguelen/Kerguelen_taxonomy_import.txt",header=TRUE, sep= "\t")

###remove the taxonomy we don't want (Eukaryotes, Chloroplasts)
taxonomy_16S <- taxonomy_16S%>%filter(!genus == "Chloroplast") #257 ASVs matching
taxonomy_16S <- taxonomy_16S%>%filter(!phylum == "Opisthokonta") #2ASVs
taxonomy_16S <- taxonomy_16S%>%filter(!phylum == "Cryptophyceae") #1ASV
taxonomy_16S <- taxonomy_16S%>%filter(!family == "Mitochondria") #68ASVs matching
###remove the taxa which don't have an ASV but only a sequence
taxonomy_16S <- taxonomy_16S%>%filter(str_detect(ASV, ">")) 
###at this point we removed 20% of sequences
###rename ASVs - you need to use gsub not sub here, otherwise he makes random mistakes
taxonomy_16S$ASV <- gsub("\\..*","",taxonomy_16S$ASV)


##reduce ASV table to ASVs in taxonomy table
sequence_list <- as.character(taxonomy_16S$ASV)

ASV$ASV <- rownames(ASV)
ASV_t <- ASV%>%dplyr::filter(ASV %in% sequence_list) #%>% column_to_rownames('ASV')
ASV_t$ASV <- NULL

##reduce meta to ASV table

meta_16S <- meta_X%>%filter(Event %in% colnames(ASV_t))

ASV_16S <- ASV_t%>%dplyr::select(meta_16S$Event)


#clean up!

rm(ASV, ASV_t, name_trans, translation)
