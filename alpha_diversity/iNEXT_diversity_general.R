##alpha diversity

diversity_iNEXT<-function(abundance){
  suppressPackageStartupMessages(require("dplyr"))
  suppressPackageStartupMessages(require("vegan"))
  require(iNEXT)
  print(packageVersion("iNEXT"))
  
iNEXT_diversity = iNEXT(abundance, q=0,datatype = "abundance", conf = 0.95,nboot = 10)
iNEXT_diversity = iNEXT_diversity$AsyEst
iNEXT_diversity_observed = iNEXT_diversity[,c(1:3)]
iNEXT_diversity_observed = spread(iNEXT_diversity_observed, Diversity, Observed)

iNEXT_diversity_observed = iNEXT_diversity_observed%>%
  dplyr::rename(Richness = `Species richness`)%>%
  dplyr::rename(Shannon = `Shannon diversity`)%>%
  dplyr::rename(Simpson = `Simpson diversity`)
iNEXT_diversity_observed$Site <- as.character(iNEXT_diversity_observed$Site)

iNEXT_diversity_observed <- as_tibble(iNEXT_diversity_observed)

return(iNEXT_diversity_observed)
#rm(iNEXT_diversity)
detach("package:iNEXT", unload=TRUE)
detach("package:dplyr", unload=TRUE)
}
#unload

diversity_iNEXT_plots <- function(abundance){
  
  require(iNEXT)
  print(packageVersion("iNEXT"))
  
  alpha.rarecurve <- rarecurve(
    as.data.frame(t(abundance)),
    step = 20,
    sample = min(colSums(abundance)),
    col = "blue",
    cex = 0.6
  )
  
  print(alpha.rarecurve)
  
  invisible(alpha.iNEXT.q0 <- iNEXT(abundance, q=0,datatype = "abundance", conf = 0.95,nboot = 100))
  invisible(alpha.iNEXT.q0.p <- ggiNEXT(alpha.iNEXT.q0,type=1, facet.var="none", color.var="site")) 
  invisible(print(alpha.iNEXT.q0.p))
  
  invisible(alpha.iNEXT.q1 <- iNEXT(abundance, q=1,datatype = "abundance", conf = 0.95,nboot = 100))
  invisible(alpha.iNEXT.q1.p <- ggiNEXT(alpha.iNEXT.q1,type=1, facet.var="none", color.var="site")) 
  invisible(print(alpha.iNEXT.q1.p))
  
  invisible(alpha.iNEXT.q2 <- iNEXT(abundance, q=2,datatype = "abundance", conf = 0.95,nboot = 100))
  invisible(alpha.iNEXT.q2.p <- ggiNEXT(alpha.iNEXT.q2,type=1, facet.var="none", color.var="site")) 
  invisible(print(alpha.iNEXT.q2.p))
  
  #rm(iNEXT_diversity)
  detach("package:iNEXT", unload=TRUE)
  
}