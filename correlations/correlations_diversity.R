#correlations

corr_plots <- function(meta.16S.div, meta.18S.div){
  
  #load additional package
  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(ggplot2))
  packageVersion("ggplot2")
  suppressPackageStartupMessages(require(psych))
  packageVersion("psych")
  
  ##16S diverstity SST
  a <- meta.16S.div%>%
    ggplot(aes(x = Temp, y = Richness, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "SST [°C]", y = "Richness")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Prokaryotic Richness against sea surface temperature")
  print(a)
  
  b <- meta.16S.div%>%
    ggplot(aes(x = Temp, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "SST [°C]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Prokaryotic Inv. Simpson Index against sea surface temperature")
  print(b)
  
  c <- meta.18S.div%>%
    ggplot(aes(x = Temp, y = Richness, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "SST [°C]", y = "Richness")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Eukaryotic Richness against sea surface temperature")
  print(c)
  
  d <- meta.18S.div%>%
    ggplot(aes(x = Temp, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "SST [°C]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Eukaryotic Inv. Simpson Index against sea surface temperature")
  print(d)
  
#against rates
  
  ##PP
  e <- meta.16S.div%>%
    ggplot(aes(x = mean_c.fix, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "primary productivity [umol C L-1 d-1]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Prokaryotic Inv. Simpson Index against primary productivity")
  print(e)
  
  f <- meta.18S.div%>%
    ggplot(aes(x = mean_c.fix, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "primary productivity [umol C L-1 d-1]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Eukaryotic Inv. Simpson Index against primary productivity")
  print(f)
  
  ##n2 fixation
  g <- meta.16S.div%>%
    ggplot(aes(x = mean_N2fix, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "N2 fixation [nmol N L-1 d-1]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Prokaryotic Inv. Simpson Index against N2 fixation")
  print(g)
  
  h <- meta.18S.div%>%
    ggplot(aes(x = mean_N2fix, y = Simpson, col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#F6A316","#73B3E3","#0C649A","#107734", "#6CB52D")) +
    labs(x = "N2 fixation [nmol N L-1 d-1]", y = "Simpson")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Eukaryotic Inv. Simpson Index against N2 fixation")
  print(h)
  
  
 
  #correlation analysis
  
  ##16S
  print(cor.test(meta.16S.div$Temp, meta.16S.div$Richness,  method = "pearson"))
  a <-lm(Temp ~ Richness, data = meta.16S.div)
  plotNormalHistogram(a$residual)
  
  print(cor.test(meta.16S.div$Temp, meta.16S.div$Simpson,  method = "pearson"))
  a <-lm(Temp ~ Simpson, data = meta.16S.div)
  plotNormalHistogram(a$residual)

  print(cor.test(meta.16S.div$mean_c.fix, meta.16S.div$Simpson, method = "pearson"))
  a <-lm(mean_c.fix ~ Simpson, data = meta.16S.div)
  plotNormalHistogram(a$residual)
  
  print(cor.test(meta.16S.div$mean_N2fix, meta.16S.div$Simpson, method = "pearson"))
  a <-lm(mean_N2fix ~ Simpson, data = meta.16S.div)
  plotNormalHistogram(a$residual)
  
  print(corr.test(meta.16S.div[c(8,26,28)], meta.16S.div$Simpson,  method = "pearson", adjust="holm"))
  
  

  ##18S
  print(cor.test(meta.18S.div$Temp,meta.18S.div$Richness, method = "pearson"))
  a <-lm(Temp ~ Richness, data = meta.18S.div)
  plotNormalHistogram(a$residual)
  
  print(cor.test(meta.18S.div$Temp, meta.18S.div$Simpson,  method = "pearson"))
  a <-lm(Temp ~ Simpson, data = meta.18S.div)
  plotNormalHistogram(a$residual)
  
  print(cor.test(meta.18S.div$mean_c.fix, meta.18S.div$Simpson, method = "pearson"))
  a <-lm(mean_c.fix ~ Simpson, data = meta.18S.div)
  plotNormalHistogram(a$residual)
  
  print(cor.test(meta.18S.div$mean_N2fix, meta.18S.div$Simpson, method = "pearson"))
  a <-lm(mean_N2fix ~ Simpson, data = meta.18S.div)
  plotNormalHistogram(a$residual)
  
  print(corr.test(meta.18S.div[c(8,26,28)], meta.18S.div$Simpson,  method = "pearson", adjust="holm"))
  
  #clean up!
  detach("package:ggplot2", unload=TRUE)
  detach("package:vegan", unload=TRUE)
  detach("package:psych", unload=TRUE)
  
}
