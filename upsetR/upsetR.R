#install.packages("rJava")
#install.packages("UpSetR")
#install.packages("tidyverse")
#install.packages("venneuler")
#install.packages("grid")
#install_github("jokergoo/ComplexHeatmap")

upset_16Splots <- function(abundance){

  #load additional packages
  library(vegan)
  library(UpSetR)
library(rJava)
library(tidyverse)
library(venneuler)
library(grid)
library(devtools)
library(ComplexHeatmap)




## make a binary table out of the abundance table. - presence/absence

OTU_PA <- decostand(abundance, method = "pa")

#for the analysis I choose union mode: 1 means in that set and 0 is not taken into account. 
#When there are multiple 1, the relationship is OR. Then, 1 1 0 means a set of elements in set A or B, 
#and they can also in C or not in C (union(A, B)). Under this mode, the seven combination sets can overlap.

#provinces as input

OTU_ma <-as.data.frame(OTU_PA)

#merge columns into provinces of choice

OTU_ma$SO <- as.integer(OTU_ma$OISO37|OTU_ma$OISO11|OTU_ma$OISOE|OTU_ma$OISO7|OTU_ma$OISO9)
OTU_ma$SSTC <- as.integer(OTU_ma$OISO4|OTU_ma$OISO14|OTU_ma$OISO15)
OTU_ma$ISSG <- as.integer(OTU_ma$OISO3|OTU_ma$OISO2|OTU_ma$OISO18)



set.seed(123)
provinces <- OTU_ma[,c(12:14)]
provinces <- provinces%>% filter_all(any_vars(. != 0)) # filter out rows with 0s

m1= make_comb_mat(provinces)

m1
m3 = make_comb_mat(provinces, mode = "union")
print(UpSet(m1,set_order = c("ISSG", "SSTC", "SO"), comb_order = order(comb_size(m1))))

}


upset_18Splots <- function(abundance){
  
  #load additional packages
  library(vegan)
  library(UpSetR)
  library(rJava)
  library(tidyverse)
  library(venneuler)
  library(grid)
  library(devtools)
  library(ComplexHeatmap)
  
  
  
  
  ## make a binary table out of the abundance table. - presence/absence
  
  OTU_PA <- decostand(abundance, method = "pa")
  
  #for the analysis I choose union mode: 1 means in that set and 0 is not taken into account. 
  #When there are multiple 1, the relationship is OR. Then, 1 1 0 means a set of elements in set A or B, 
  #and they can also in C or not in C (union(A, B)). Under this mode, the seven combination sets can overlap.
  
  #provinces as input
  
  OTU_ma <-as.data.frame(OTU_PA)
  
  #merge columns into provinces of choice
  
  OTU_ma$SO <- as.integer(OTU_ma$OISO37|OTU_ma$OISO11|OTU_ma$OISOE|OTU_ma$OISO7|OTU_ma$OISO9|OTU_ma$OISO6)
  OTU_ma$SSTC <- as.integer(OTU_ma$OISO4|OTU_ma$OISO15)
  OTU_ma$ISSG <- as.integer(OTU_ma$OISO3|OTU_ma$OISO2|OTU_ma$OISO18|OTU_ma$OISO16)
  
  
  
  set.seed(123)
  provinces <- OTU_ma[,c(13:15)]
  provinces <- provinces%>% filter_all(any_vars(. != 0)) # filter out rows with 0s
  
  m1= make_comb_mat(provinces)
  
  m1
  m3 = make_comb_mat(provinces, mode = "union")
  print(UpSet(m1,set_order = c("ISSG", "SSTC", "SO"), comb_order = order(comb_size(m1))))
  
}