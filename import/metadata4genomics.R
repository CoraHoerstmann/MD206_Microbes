#generate metadata for genomic analysis with means for PP and N2 fix replicates

#only from rate measuremetns are replicates which need a mean from the other metadata we can use Rone replicate instead


meta_s <- Carbonfix%>%filter(Replicate == "1" & Timeslice..sampling.time. == "T0")

meta_C <- Carbonfix%>%
  group_by(Event)%>% 
  summarise(mean_c.fix = mean(cfix.day, na.rm = TRUE), sd_cfix = sd(cfix.day))

meta_PB <- Carbonfix%>%
  group_by(Event)%>% 
  summarise(mean_PB = mean(PB, na.rm = TRUE))

meta_N <- N2fix%>%
  group_by(Event)%>% 
  summarise(mean_N2fix = mean(N2.fix..nmol.l.day., na.rm = TRUE))



meta_P <- POCPN%>%filter(Timeslice..sampling.time. == "T0")

meta_X <- left_join(meta_s[c(1:20,26:29)], meta_P[c(1,14:16)], by = "Event")  
meta_X <- left_join(meta_X, meta_C,  by = "Event")
meta_X <- left_join(meta_X, meta_PB, by = "Event")
meta_X <- left_join(meta_X, meta_N, by = "Event")


#calculate means across provinces
print(meta_X%>%
        group_by(province)%>% 
        #tally()%>%
        summarise(mean(mean_c.fix, na.rm = TRUE), sd(mean_c.fix, na.rm = TRUE)))

print(meta_X%>%
        group_by(province)%>% 
        #tally()%>%
        summarise(mean(mean_PB, na.rm = TRUE), sd(mean_PB, na.rm = TRUE)))


print(meta_X%>%
  group_by(province)%>% 
  #tally()%>%
  summarise(mean(mean_N2fix, na.rm = TRUE), sd(mean_N2fix, na.rm = TRUE)))


#per watermass

print(meta_X%>%
        group_by(WM)%>% 
        #tally()%>%
        summarise(mean(mean_c.fix, na.rm = TRUE), sd(mean_c.fix, na.rm = TRUE)))

print(meta_X%>%
        group_by(WM)%>% 
        #tally()%>%
        summarise(mean(mean_PB, na.rm = TRUE), sd(mean_PB, na.rm = TRUE)))


print(meta_X%>%
        group_by(WM)%>% 
        #tally()%>%
        summarise(mean(mean_N2fix, na.rm = TRUE), sd(mean_N2fix, na.rm = TRUE)))



#clean up!
rm(meta_C, meta_N, meta_P, meta_PB, meta_s)
