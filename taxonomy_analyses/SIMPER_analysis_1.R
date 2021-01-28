#SIMPER ANALYSIS 

SIMPER <-function(abundance.clr, meta, taxonomy){

#load additional packages
suppressPackageStartupMessages(library(vegan))
packageVersion("vegan")
suppressPackageStartupMessages(library(knitr))
packageVersion("knitr")
library(kableExtra)
options(knitr.table.format = "html") 
#SIMPER

starting_df <-data.frame(abundance.clr)
final_df <- as.data.frame(t(starting_df))
#sub_df = final_df[,colSums(final_df) > 50] ##subset only neccessary to shorten the computational time with huge OTU tables.
##now we need the WM in oder to group the samples
#final_df["WM"] <-NA

#final_df$WM <- p
#######watermass/province
water <- meta$province

####
simper(final_df, group = water)
otu.simper <- simper(final_df, group = water)
#summary(otu.simper)
#str(otu.simper)

OTU.simper.10 <- lapply( # apply to each element in simper output
  otu.simper, # simper output
  function(x) {
    data.frame( # create data frame consisting of
      otu = x$species, # OTU names
      contribution = x$average # average contribution
    )[x$ord[1:10], ] # only the 10 OTU which contribute most to the differences, i.e. the first ten when ordered by cumsum
  }
)

#change directory to destination
#setwd("D:/Kerguelen_project/new_analyses_L&O_review/")

##search for the taxonomy in the top 10

SSTC_SO <- OTU.simper.10$SSTC_SO
SSTC_SO$ASV <- as.character(SSTC_SO$otu)
SSTC_SO <- left_join(SSTC_SO, taxonomy, by = "ASV")

a<-SSTC_SO%>%knitr::kable(caption = "**SSTC vs SO**", format = "html")
print(a)


#write.csv(SANT_SSTC, "SIMPER_18S_ASVs_SO_SSTC.csv")

ISSG_SO <- OTU.simper.10$ISSG_SO
ISSG_SO$ASV <- as.character(ISSG_SO$otu)
ISSG_SO <- left_join(ISSG_SO, taxonomy, by = "ASV")

b<-ISSG_SO%>%knitr::kable(caption = "**ISSG vs SO**", format = "html")
print(b)


#write.csv(SANT_ISSG, "SIMPER_18S_ASVs_SO_ISSG.csv")

ISSG_SSTC <- OTU.simper.10$ISSG_SSTC
ISSG_SSTC$ASV <- as.character(ISSG_SSTC$otu)
ISSG_SSTC <- left_join(ISSG_SSTC, taxonomy, by = "ASV")

c<-ISSG_SSTC%>%knitr::kable(caption = "**ISSG vs SSTC**", format = "html")
print(c)

#detach("package:vegan", unload=TRUE)

}
