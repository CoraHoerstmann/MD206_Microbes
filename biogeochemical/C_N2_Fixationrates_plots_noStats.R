####Uptake rates
PPplot <-function(Carbonfix){
  
suppressPackageStartupMessages(library("ggplot2"))
packageVersion("ggplot2")
require("rcompanion")
packageVersion("rcompanion")

p <- Carbonfix%>%
  ggplot(aes(x = Temp, y = cfix.day, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A","#73B3E3")) +
  labs(x = "SST [°C]", y = "fixation rate [umol L-1 day-1]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("primary productivity against sea surface temperature")
print(p)

b <- Carbonfix%>%
  ggplot(aes(x = Temp, y = PB, col = WM))+
  geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
  scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A","#73B3E3")) +
  labs(x = "SST [°C]", y = "PB [umol C g chla-1 day-1]")+
  theme(axis.title.x = element_text(size=16, vjust = 0.3),
        axis.title.y = element_text(size=16, vjust = 0.3),
        axis.text.y = element_text(size=16, vjust = 0.3),
        axis.text.x = element_text(size=16, vjust = 0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("PB against sea surface temperature")
print(b)




#remove NAs from C

discard <- apply(Carbonfix, 1, function(x) any(is.na(x)))

Cfix <<- Carbonfix[!discard,]

Cfix_SSTC <- Cfix%>%filter(province == "SSTC")%>%
  group_by(Event)%>%
  summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE), PB = mean(PB, na.rm = TRUE))

Cfix_SO <- Cfix%>%filter(province == "SO")%>%
  group_by(Event)%>%
  summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE), PB = mean(PB, na.rm = TRUE))

Cfix_ISSG <- Cfix%>%filter(province == "ISSG")%>%
  group_by(Event)%>%
  summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE), PB = mean(PB, na.rm = TRUE))

Cfix_all <- Cfix%>%
  group_by(Event)%>%
  summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE), mean_PB = mean(PB, na.rm = TRUE))

print(Cfix%>%
        group_by(Event)%>%
        dplyr::summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE),min_PP = min(cfix.day, na.rm = TRUE), max_PP = max(cfix.day, na.rm = TRUE), n = n()))

print(Cfix%>%
        group_by(Event)%>%
        dplyr::summarise(mean_PB = mean(PB, na.rm = TRUE),min_PB = min(PB, na.rm = TRUE), max_PB = max(PB, na.rm = TRUE), n = n()))

meta_s <- Carbonfix%>%filter(Replicate == "1" & Timeslice..sampling.time. == "T0")
Cfix_all <- inner_join(Cfix_all, meta_s[c(1,6,26,27,28)], by = "Event")

print(cor.test(Cfix_all$MLD..m., Cfix_all$mean_PB, method = "pearson"))
a <-lm(MLD..m.~ mean_PB, data = Cfix_all)

plotNormalHistogram(a$residual, main = "Residual Histogram of correlations between MLD and PB")



}

#N2 fixation

N2plot <-function(N2fix){
  
  suppressPackageStartupMessages(library("ggplot2"))
  packageVersion("ggplot2")
  suppressPackageStartupMessages(require(psych))
  packageVersion("psych")
  
  
  province_MQR <- N2fix%>%group_by(province)%>%summarise(mean_MQR = median(MQR))
  
  N2fix <- left_join(N2fix, province_MQR, by = "province")
  
  p <- N2fix%>%
    ggplot(aes(x = Temp, y = N2.fix..nmol.l.day., col = WM))+
    
    geom_step(aes(x = Temp, y=MQR), color="azure3")+
    
    #annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = MQR,
    #         alpha = 0.2, fill = "azure3")+     ########add mimimum quatifiable rate
  
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A","#73B3E3")) +
    labs(x = "SST [°C]", y = "fixation rate [nmol L-1 day-1]")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  print(p)
  
  
  #remove NAs from C
  
  discard <- apply(N2fix, 1, function(x) any(is.na(x)))
  
  Nfix <- N2fix[!discard,]
  
  Nfix_SSTC <- Nfix%>%filter(province == "SSTC")%>%
    group_by(Event)%>%
    summarise(mean_N2.fix = mean(N2.fix..nmol.l.day., na.rm = TRUE))
  
  Nfix_SO <- Nfix%>%filter(province == "SO")%>%
    group_by(Event)%>%
    summarise(mean_N2.fix = mean(N2.fix..nmol.l.day., na.rm = TRUE))
  
  Nfix_ISSG <- Nfix%>%filter(province == "ISSG")%>%
    group_by(Event)%>%
    summarise(mean_N2.fix = mean(N2.fix..nmol.l.day., na.rm = TRUE))

  #check for individual mean rates   
Nfix_all <-Nfix%>%
    group_by(Event)%>%
    summarise(mean_N2.fix = mean(N2.fix..nmol.l.day., na.rm = TRUE), sd = sd(N2.fix..nmol.l.day.), n = n(), na.rm = TRUE)

print(Nfix%>%
  group_by(Event)%>%
  summarise(MQR_station = mean(MQR, na.rm = TRUE), mean_N2.fix = mean(N2.fix..nmol.l.day., na.rm = TRUE), min = min(N2.fix..nmol.l.day., na.rm = TRUE), max = max(N2.fix..nmol.l.day., na.rm = TRUE), n = n(), na.rm = TRUE))

meta_s <- Nfix%>%dplyr::filter(Replicate == "1" & Incubation == "bin1")
Nfix_all <- inner_join(Nfix_all, Cfix[c(1:15,27,28)], by = "Event")
Cfix$Incubation <- Cfix$Method.comm..incubation.bin.
Nfix_n <- left_join(Nfix, Cfix[c(1:15, 22, 30)], by = c("Event", "Incubation", "Replicate"))
print(Nfix_all%>%
        group_by(WM)%>%
        summarise(mean = mean(mean_N2.fix), min = min(mean_N2.fix), max = max(mean_N2.fix), n = n()))  

  
  p <- Nfix_n%>%
    ggplot(aes(x = NO3umol.l, y = N2.fix..nmol.l.day., col = WM))+
    geom_point(aes(fill = WM), alpha=8/10, shape=21, colour="black", size=5)+
    scale_fill_manual(values=c("#6CB52D","#F6A316","#107734", "#0C649A","#73B3E3")) +
    labs(x = "NO3 umol L-1", y = "fixation rate [nmol L-1 day-1]")+
    theme(axis.title.x = element_text(size=16, vjust = 0.3),
          axis.title.y = element_text(size=16, vjust = 0.3),
          axis.text.y = element_text(size=16, vjust = 0.3),
          axis.text.x = element_text(size=16, vjust = 0.3),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  print(p)
  
  
  
  #unload packages
  
  #detach("package:ggplot2", unload=TRUE)
  
}

