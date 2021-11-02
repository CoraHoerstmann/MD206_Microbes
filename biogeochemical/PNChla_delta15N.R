#PN chla ratio and delta 15N 
#plots and stats

supPN <- function(POCPN_data, N2fix_data){

#load additional packages
suppressPackageStartupMessages(require(vegan))
packageVersion("vegan")
suppressPackageStartupMessages(require(ggplot2))
packageVersion("ggplot2")


##PN.chla
a <- POCPN_data%>%
  ggplot(aes(x = Temperature, y = PN_CHL, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "PN : chl.a ratio")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("PN_chla against sea surface temperature")
print(a)


b <- N2fix_data%>%
  ggplot(aes(x = Temp, y = delta_15N, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "delta 15N")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("delta 15N against sea surface temperature")
print(b)

##PN.chla ratio differences between watermasses


PN <- POCPN_data%>%filter(Timeslice..sampling.time. == "T0")
print(PN[c(1,17:19)])

##delta15N differences between watermasses

dN_SSTC <- N2fix_data%>%filter(province == "SSTC")%>%
  group_by(Event)%>%
  summarise(delta_15N = mean(delta_15N, na.rm = TRUE))

dN_SO <- N2fix_data%>%filter(province == "SO")%>%
  group_by(Event)%>%
  summarise(delta_15N = mean(delta_15N, na.rm = TRUE))

dN_ISSG <- N2fix_data%>%filter(province == "ISSG")%>%
  group_by(Event)%>%
  summarise(delta_15N = mean(delta_15N, na.rm = TRUE))



#clean up!
#detach("package:ggplot2", unload=TRUE)
#detach("package:vegan", unload=TRUE)

}

