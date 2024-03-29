#hellinger and clr transformation

#load additional scripts

hellinger <- function(abundance.tax, count){
suppressPackageStartupMessages(require(vegan))
packageVersion("vegan")
suppressPackageStartupMessages(require(zCompositions))
packageVersion("zCompositions")

d <- as.data.frame(abundance.tax)
sum(d == 0) #filter out low abundance ASVs
#count is the chosen cutoff for rowsums
d.1 <- data.frame(d[which(apply(d, 1, function(x){mean(x)}) > count),], 
                  check.names=F) #centered log ratio transformation
a <- as.matrix(data.matrix(d.1))
abundance.hellinger <- decostand(x = a,
                                 MARGIN = 1,
                                 method = "hellinger")

return(abundance.hellinger)

detach("package:zCompositions", unload=TRUE)
detach("package:vegan", unload=TRUE)
}



#clr transformation

clr <- function(abundance.tax, count){

  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(zCompositions))
  packageVersion("zCompositions")
  
d <- as.data.frame(abundance.tax)
sum(d == 0) #filter out low abundance ASVs
#count is the chosen cutoff for rowsums
d.1 <- data.frame(d[which(apply(d, 1, function(x){mean(x)}) > count),], 
                    check.names=F) #centered log ratio transformation
d.czm <- cmultRepl(t(d.1),  label=0, method="CZM") #CZM is a Bayesian toll for count zeros in compositional datasets. Needs to be done when you want to do a log ratio transformation afterwards cause t cannot deal with 0s.
d.clr <- t(apply(d.czm, 1, function(x){log(x) - mean(log(x))}))
mi <- t(d.clr)
d.clrdata <- data.matrix(mi)
d.clrdata <- as.matrix(d.clrdata)
abundance.clr <- as.data.frame(d.clrdata)

return(abundance.clr)

detach("package:zCompositions", unload=TRUE)
detach("package:vegan", unload=TRUE)

}

