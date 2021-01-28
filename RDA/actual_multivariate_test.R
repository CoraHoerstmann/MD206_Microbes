
require(ggvegan)
require(vegan)
require(rgr)
require(tidyverse)
require(ggplot2)
require(vegan)
require(phyloseq)
require(zCompositions)
require(adespatial)
require(geodist)
suppressPackageStartupMessages(require("ggords"))


##explore metadata to be able to only include meaningful metadata in multivariate analysis
metadata_exploration <- function(meta_X){
  
  #load additional package
  require(ggbiplot)

  meta_test <- meta_X[,c(1,5,6,8,9,11:15,22,23)] 
  meta_test_data <- meta_test[c(2:10,12)] # select columns with numeric values
  meta_test_groups <- meta_test[,11]
  sites <- meta_test[,1]
  meta_test_a <- meta_test_data - min(meta_test_data) # the problem here was that I had negative values in my data, so what I did is adding everything by the most negative number of the dataset
  meta.pca <- prcomp(meta_test_a, center = TRUE, scale. = TRUE)
  print(meta.pca)
  plot(meta.pca, type = "l")
  summary(meta.pca)  
  

g <- ggbiplot(meta.pca, obs.scale = 2, var.scale = 2, 
              groups = meta_test_groups, ellipse = TRUE, 
              circle = TRUE)+
    scale_color_manual(values = c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D"))+
        geom_text(label = sites, size = 2)+
        theme(axis.title.x = element_text(size=12, vjust = 0.3),
          axis.title.y = element_text(size=12, vjust = 0.3),
          axis.text.y = element_text(size=12, vjust = 0.3),
          axis.text.x = element_text(size=12, vjust = 0.3),
          legend.text = element_text(size=12, vjust = 0.3),
          legend.title = element_text(size=12, vjust = 0.3))

print(g)

#clean up!
detach("package:ggbiplot", unload=TRUE)

}


#Redundancy analysis

RDA_analysis <- function(abundance.clr, abundance.hellinger, meta, meta.sub){

  #load additional packages
  require(vegan)
rownames(meta.sub) <- meta.sub$Event
meta.sub$Event <- NULL
  
abundance.clr.t <- as.data.frame(t(abundance.clr))

abundance.clr.rda <- rda(
  abundance.clr.t ~ .,
  data = meta.sub
)

print(abundance.clr.rda)

invisible(hist(residuals(abundance.clr.rda), main = ""))

abundance.clr.rda.anova <- anova.cca(abundance.clr.rda)
print(abundance.clr.rda.anova)

inertia.rda.tot <- abundance.clr.rda$tot.chi
inertia.rda.tot
inertia.rda.constrained <- abundance.clr.rda$CCA$tot.chi
inertia.rda.constrained
inertia.rda.constrained.prop <- inertia.rda.constrained/inertia.rda.tot
print(inertia.rda.constrained.prop)



#plot

scl <- 2 
sort.df <- with(meta, meta[order(WM),]) ##this step is important, without it just randomly mixes the provinces 
levels(meta$WM) <- c("AZ", "IO", "PFZ", "SAF", "STF")
eco <- meta$WM
bg <- c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D") #change the colors accordingly soon


print(ggrda(abundance.clr.rda, group = eco, spearrow = NULL, farrow = 0.1, fzoom = 5, ellipse = F, scaling = 2, spe = F)+
        scale_color_manual(name = "watermass",values = c("#107734","#F6A316", "#6CB52D", "#0C649A","#73B3E3")))
#beta-dispersion


bray_dist <- vegdist(t(abundance.hellinger), method = "bray", na.rm = FALSE)
beta_dispersion_province <- betadisper(bray_dist, meta$province)
beta_dispersion_WM <- betadisper(bray_dist, meta$WM)

par(mfrow=c(1,2))
plot(beta_dispersion_province)
plot(beta_dispersion_WM)
dev.off()

#PERMANOVA
#PP
abundance.hellinger.t <- t(abundance.hellinger)
print(adonis2(
  formula = abundance.hellinger.t ~ mean_c.fix,
  data = meta,
  method = "bray"
))

#N2
print(adonis2(
  formula = abundance.hellinger.t ~ mean_N2fix,
  data = meta,
  method = "bray"
))
#province
print(adonis2(
  formula = abundance.hellinger.t ~ province,
  data = meta,
  method = "bray"
))
#water mass
print(adonis2(
  formula = abundance.hellinger.t ~ WM,
  data = meta,
  method = "bray"
))


#clean up! 

#detach("package:vegan", unload=TRUE)
}
