
#Nutrient plots

Nutrient_p <- function(Carbonfix, POCPN){
  
  #load additional packages
  suppressPackageStartupMessages(require(vegan))
  packageVersion("vegan")
  suppressPackageStartupMessages(require(ggplot2))
  packageVersion("ggplot2")
  
  
  
  
##Nitrate
a <- Carbonfix%>%
  ggplot(aes(x = Temp, y = NO3umol.l, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "Nitrate conc. [umol L-1]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Nitrate concentration against sea surface temperature")
print(a)

##Phosphate
b <- Carbonfix%>%
  ggplot(aes(x = Temp, y = P_umol.l, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "Phosphate conc. [umol L-1]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Phosphate concentration against sea surface temperature")
print(b)

##Silicate
c <- Carbonfix%>%
  ggplot(aes(x = Temp, y = Si_umol.l, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "Silicate conc. [umol L-1]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Silicate concentration against sea surface temperature")
print(c)


##remove NAs
discard <- apply(Carbonfix, 1, function(x) any(is.na(x)))

Cfix <- Carbonfix[!discard,]

Cfix_SSTC <- Cfix%>%filter(province == "SSTC")%>%
  group_by(Event)%>%
  summarise(NO3umol.l = mean(NO3umol.l, na.rm = TRUE), P_umol.l = mean(P_umol.l, na.rm = TRUE), Si_umol.l = mean(Si_umol.l, na.rm = TRUE))

Cfix_SO <- Cfix%>%filter(province == "SO")%>%
  group_by(Event)%>%
  summarise(NO3umol.l = mean(NO3umol.l, na.rm = TRUE), P_umol.l = mean(P_umol.l, na.rm = TRUE), Si_umol.l = mean(Si_umol.l, na.rm = TRUE))

Cfix_ISSG <- Cfix%>%filter(province == "ISSG")%>%
  group_by(Event)%>%
  summarise(NO3umol.l = mean(NO3umol.l, na.rm = TRUE), P_umol.l = mean(P_umol.l, na.rm = TRUE), Si_umol.l = mean(Si_umol.l, na.rm = TRUE))

print(Cfix%>%group_by(Event)%>%summarise(mean_NO3 = mean(NO3umol.l, na.rm = TRUE), min_NO3 = min(NO3umol.l, na.rm = TRUE), max_NO3 = max(NO3umol.l, na.rm = TRUE), 
                                          mean_P = mean(P_umol.l, na.rm = TRUE), min_P = min(P_umol.l, na.rm = TRUE), max_P = max(P_umol.l, na.rm = TRUE),
                                          mean_Si = mean(Si_umol.l, na.rm = TRUE), min_Si = min(Si_umol.l, na.rm = TRUE), max_Si = max(Si_umol.l, na.rm = TRUE), n = n()))

print(Cfix_SO%>%group_by(Event)%>%summarise(mean_NO3 = mean(NO3umol.l, na.rm = TRUE), min_NO3 = min(NO3umol.l, na.rm = TRUE), max_NO3 = max(NO3umol.l, na.rm = TRUE), 
                                            mean_P = mean(P_umol.l, na.rm = TRUE), min_P = min(P_umol.l, na.rm = TRUE), max_P = max(P_umol.l, na.rm = TRUE),
                                            mean_Si = mean(Si_umol.l, na.rm = TRUE), min_Si = min(Si_umol.l, na.rm = TRUE), max_Si = max(Si_umol.l, na.rm = TRUE), n = n()))




##POC

d <- POCPN%>%
  ggplot(aes(x = Temperature, y = POCPN_ratio, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A", "#73B3E3")) +
  labs(x = "SST [°C]", y = "POCPN molar ratio")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("POCPN ratio against sea surface temperature")
print(d)


### POCPN

POCPN_SSTC <- POCPN%>%filter(province == "SSTC")%>%
  group_by(Event)%>%
  summarise(POCPN_ratio = mean(POCPN_ratio, na.rm = TRUE))

POCPN_SO <- POCPN%>%filter(province == "SO")%>%
  group_by(Event)%>%
  summarise(POCPN_ratio = mean(POCPN_ratio, na.rm = TRUE))

POCPN_ISSG <- POCPN%>%filter(province == "ISSG")%>%
  group_by(Event)%>%
  summarise(POCPN_ratio = mean(POCPN_ratio, na.rm = TRUE))



#unload packages
detach("package:ggplot2", unload=TRUE)
detach("package:vegan", unload=TRUE)
}
