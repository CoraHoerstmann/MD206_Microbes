
pigm_plot <-function(pigments, diag_pigm){


  #load additional package
  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(ggplot2))
  packageVersion("ggplot2")
  suppressPackageStartupMessages(require(reshape2))
  packageVersion("reshape2")
  suppressPackageStartupMessages(require(rcompanion))
  packageVersion("rcompanion")
  #sort the data by event
pigments <- with(pigments, pigments[order(Event),])
diag_pigm <- with(diag_pigm, diag_pigm[order(Event),])

pigments_T0 <- left_join(diag_pigm, pigments[c(1:7)], by = c("Event", "Timeslice"))
pigments_T0_all_meta <- left_join(meta_X, pigments_T0, by="Event")

#sort according to Lat and lock via factor
pigments_T0$Event <-as.character(pigments_T0$Event)
pigments_T0$Event <- factor(pigments_T0$Event, levels = pigments_T0$Event[order(pigments_T0$Latitude)])

###normalize the total to 1
f<- vegan::decostand(pigments_T0[c(5:10)],margin=1,"total")

pigments_T0[c(5:10)]<-f

###bring into long format

dp <- melt(pigments_T0, measure.vars = c('Diatoms', 'Dinoflagellates','Green.algae', 'Prokaryotes', 'Prochlorococcus', 'Pico.eukaryotes')
           ,value.name = "abundance", variable.name = "sizeclass")

###plot
p<- ggplot(data=dp, aes(x= Event, y= abundance, fill=sizeclass))+
  geom_bar(aes(fill=factor(sizeclass),y=abundance),stat='identity', width = 0.95) +
  scale_fill_manual(values = c("darkgreen", "red1","green","royalblue2", "midnightblue", "sienna1", "mediumseagreen", "lightslateblue","lightyellow", "orange"),
                    labels = c( "Diatoms", "Dinoflagellates", "Green algae", "Prokaryotes",  "Prochlorococcus", "Pico-eukaryotes"))+
  labs(x = "SST [°C]", y= "Pigment conc. [mg m-3]",fill = "classes")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3, angle = 90),
        legend.text = element_text(size=16, vjust = 0.3),
        legend.title = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(p)

##size classes

sc <-melt(pigments_T0, measure.vars = c('Microplankton', 'Nanoplankton','Picoplankton'),value.name = "classes", variable.name = "bin")


s<- ggplot(sc,aes(x = Temp, y = classes, group = bin, col = bin))+
  geom_point(aes(size = 5, color = factor(bin), shape = factor(bin), alpha = 8/10))+
  scale_color_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                     values = c("palevioletred4","chartreuse3","aquamarine4"))+
  scale_shape_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                     values = c(15:17))+
  scale_fill_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                    values = c("palevioletred4","chartreuse3","aquamarine4"),
                    labels = c("Microplankton", "Nanoplankton", "Picoplankton"),l =40)
s <- s + labs(x = "SST [°C]", y = "sizeclasses [%]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(s)



###other metadata
sc <-melt(pigments_T0_all_meta, measure.vars = c('Microplankton', 'Nanoplankton','Picoplankton'),value.name = "classes", variable.name = "bin")

s<- ggplot(sc,aes(x = mean_c.fix, y = classes, group = bin, col = bin))+
  geom_point(aes(size = 5, color = factor(bin), shape = factor(bin), alpha = 8/10))+
  scale_color_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                     values = c("palevioletred4","chartreuse3","aquamarine4"))+
  scale_shape_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                     values = c(15:17))+
  scale_fill_manual(breaks = c( "Microplankton", "Nanoplankton", "Picoplankton"),
                    values = c("palevioletred4","chartreuse3","aquamarine4"),
                    labels = c("Microplankton", "Nanoplankton", "Picoplankton"),l =40)
s <- s + labs(x = "PP", y = "sizeclasses [%]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(s)

#f <- ggplot(data = pigments_T0_all_meta)+
#  geom_point(aes(x=mean_c.fix, y=Diatoms), color="#30BA1C")+
#  geom_point(aes(x=mean_c.fix, y=Dinoflagellates), color="#C9CE0F")+
#  geom_point(aes(x=mean_c.fix, y=Green.algae), color="#3C6B5A")+
#  geom_point(aes(x=mean_c.fix, y=Prochlorococcus), color="#ACEFA3")+
#  labs(x = "primary productivity", y= "abundance autotroph")+
#  theme(axis.title.x = element_text(size=16, vjust = 0.3),
#        axis.title.y = element_text(size=16, vjust = 0.3),
#        axis.text.y = element_text(size=16, vjust = 0.3),
#        axis.text.x = element_text(size=16, vjust = 0.3),
#        legend.title = element_text(size=12, vjust = 0.3),
#        legend.text = element_text(size=12, vjust = 0.3),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
#print(f)

#calculate significances

all_pig_T0 <- pigments%>%filter(Timeslice == "0")
P_ISSG <- all_pig_T0%>%filter(province == "ISSG")
P_SO <- all_pig_T0%>%filter(province == "SO")
P_SSTC <- all_pig_T0%>%filter(province == "SSTC")

#PERMANOVA hardcode the pigment data in the dataframe

print(adonis2(
  formula = all_pig_T0[7:27] ~ province,
  data = all_pig_T0,
  method = "bray"
))

print(adonis2(
  formula = all_pig_T0[7:27] ~ WM,
  data = all_pig_T0,
  method = "bray"
))

print(all_pig_T0[c(1, 7, 12, 21,24, 28,29)])


#significance of size classes with second polynominal fit

cor.test(pigments_T0$Temp, pigments_T0$Microplankton, method = "spearman") #correlation analysis
model <- lm(pigments_T0$Microplankton ~ poly(pigments_T0$Temp,2))
print(summary(model))
plotNormalHistogram(model$residuals)

model <- lm(pigments_T0$Picoplankton ~ poly(pigments_T0$Temp,2))
print(summary(model))
plotNormalHistogram(model$residuals)

model <- lm(pigments_T0$Nanoplankton ~ poly(pigments_T0$Temp,2))
print(summary(model))
plotNormalHistogram(model$residuals)

pigments_T0 <<- pigments_T0

#unload packages
detach("package:ggplot2", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:reshape2", unload=TRUE)
}