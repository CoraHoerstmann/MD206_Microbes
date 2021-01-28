#create relative ASV plots:


suppressPackageStartupMessages(require(magrittr))
packageVersion("magrittr")
suppressPackageStartupMessages(require(phyloseq))
packageVersion("phyloseq")

#16S

###create phyloseq from 16S 

OTU_16S = otu_table(ASV_16S_clr, taxa_are_rows = TRUE)
rownames(taxonomy_16S) <- taxonomy_16S$ASV
tax.matrix_16S<- as.matrix(taxonomy_16S)
TAX_16S = tax_table(tax.matrix_16S)
rownames(meta_16S.z) <- meta_16S.z$Event
map_16S = sample_data(meta_16S.z)
map_16S <- sample_data(map_16S)
phyloseq_merged_16S = phyloseq(OTU_16S, TAX_16S)
all_16S = merge_phyloseq(phyloseq_merged_16S, map_16S) ####merge data into phyloseq
all_16S

#no we created a merged phyloseq which contains all the information 
#together linked via the station names

colnames(tax_table(all_16S)) #check whether you are satisfied with the headers here. e.g. kingdom, phylum,..

#Further, you have to remove mitochindria and chloroplasts if you haven't done it at that point (done, in import file)

##the next step is to analyze te data on a certain taxonomic rank. with colnames you can see the options.

#####be careful that you adjust also colors etc. 

erie_family <- all_16S %>%
  tax_glom(taxrank = "class") %>%   # agglomerate at phylum level
  
  psmelt() %>% # Melt to long format
  filter(Abundance > 0) %>%    #be careful- here we removed everything what is relatively depleted in the sample!
  arrange(phylum) # Sort data frame alphabetically by phylum
# Set colors for plotting


#Bacteroidetes
Bacteroidetes_SO <- erie_family%>%filter(province == "SO" & phylum == "Bacteroidetes")
Bacteroidetes_ISSG <- erie_family%>%filter(province == "ISSG" & phylum == "Bacteroidetes")
print(t.test(Bacteroidetes_SO$Abundance, Bacteroidetes_ISSG$Abundance, var.equal = FALSE))

#Cyanobacteria
Cyano_SO <- erie_family%>%filter(province == "SO" & phylum == "Cyanobacteria")
Cyano_ISSG <- erie_family%>%filter(province == "ISSG" & phylum == "Cyanobacteria")
Cyano_SSTC <- erie_family%>%filter(province == "SSTC" & phylum == "Cyanobacteria")
print(t.test(Cyano_SO$Abundance, Cyano_ISSG$Abundance, var.equal = FALSE))
print(t.test(Cyano_SO$Abundance, Cyano_SSTC$Abundance, var.equal = FALSE))


#clean up!
detach("package:phyloseq", unload=TRUE)
detach("package:magrittr", unload=TRUE)
rm(a,b,c,d,e,f,g,h)