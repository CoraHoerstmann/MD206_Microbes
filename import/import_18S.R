#18S

setwd("D:/Kerguelen_project/")

##load additional packages
require(tidyverse)


##Import ASV data

ASV_18S <- read.csv("./Genomics/ASV_analyses/18S/DADA2_Apr_2020_2/results/seqtab.nochim_Kerguelen_18S_2.csv", row.names = 1, header = T, sep = ",")

#columnnames are very mixed up- so we also have to translate them
name_trans <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/Sequence_Station_translation_18S.txt", header = T, sep = "\t")
name_trans$sequence_name <- as.character(name_trans$sequence_name)
name_trans$station_name <- as.character(name_trans$station_name)
ASV_18S <- ASV_18S%>% rename_at(vars(name_trans$sequence_name), ~ name_trans$station_name)

###import taxonomy data

taxonomy_18S <- read.csv("./Genomics/ASV_analyses/18S/DADA2_Apr_2020_2/results/Kerguelen_Taxonomy_pr2_18S.txt", header = T, sep = "\t")
taxonomy_18S <- cbind(taxonomy_18S, rownames(ASV_18S))
#change sequences into ASV names in ASV table

rownames(ASV_18S) <- taxonomy_18S$Feature.ID


#filter metazoa
taxonomy_18S <- taxonomy_18S%>%filter(!kingdom == "Metazoa") #4.4% of dataset are Metazoa
rownames(taxonomy_18S) <- taxonomy_18S$Feature.ID

#reduce ASV table to what we have in taxonomy and metadata
ASV_18S$ASV <- rownames(ASV_18S)
ASV_18S <- ASV_18S%>%filter(rownames(ASV_18S) %in% rownames(taxonomy_18S))#%>%column_to_rownames('ASV')

meta_18S <- meta_X%>%filter(Event %in% colnames(ASV_18S))
meta_18S$Event <- as.character(meta_18S$Event)
ASV_18S <- ASV_18S%>%dplyr::select(meta_18S$Event)

#change column header of taxonomy for furthere analysis

taxonomy_18S$ASV <- as.character(taxonomy_18S$Feature.ID)
taxonomy_18S$`rownames(ASV_18S)`<- NULL
taxonomy_18S$Feature.ID <- NULL
#clean up!

rm(name_trans)

