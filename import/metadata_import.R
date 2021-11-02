

setwd("D:/Kerguelen_project")

#import Carbon fixation dataset
#download from: https://doi.org/10.1594/PANGAEA.885896
#remove header (performed in excel)
Carbonfix_raw <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/rates_and_metadata.txt", header=T, sep= "\t", fill = T, row.names = 1)
N2fix_raw <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/N2_fixation.txt", header=T, sep= "\t", fill = T, row.names = 1)
pigments_raw <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/pigments.txt", header=T, sep= "\t", fill = T, row.names = 1)

MQR <- read.csv("./Biogeochemistry/C_N_uptake_total/TableS3_MinQuantRate.csv", header=T, sep= ";", fill = T)

#import POC data (local)

POCPN <- read.csv("./analysis_with_R/Rscripts/Submission_analysis/POCPN.txt", header=TRUE, sep= "\t")





#load additional packages

suppressPackageStartupMessages(require("tidyverse"))
packageVersion("tidyverse")

#remove expedition name from events
Carbonfix_raw$Event <- gsub("MD206_","", Carbonfix_raw$Event)
N2fix_raw$Event <- gsub("MD206_","", N2fix_raw$Event)
pigments_raw$Event <- gsub("MD206_","", pigments_raw$Event)
POCPN$Event <- gsub("MD206_","", POCPN$Event)

#change columnnames
print("change column names", quote = F)

Carbonfix_raw <- Carbonfix_raw%>%
  dplyr::rename(Depth_temp = "Depth.water..m...CTD.depth..for.temperature.an....")%>%
  dplyr::rename(Temp = "Temp..Â.C.")%>%
  dplyr::rename(oxygen = "OXYGEN..Âµmol.kg.")%>%
  dplyr::rename(depth = "Depth.water..m...water.sample.depth.")%>%
  dplyr::rename(NO3umol.l = "X.NO3....Âµmol.l.")%>%
  dplyr::rename(NO2_umol.l = "X.NO2....Âµmol.l.")%>%
  dplyr::rename(NH4_umol.l = "X.NH4....Âµmol.l.")%>%
  dplyr::rename(P_umol.l = "X.PO4.3...Âµmol.l.")%>%
  dplyr::rename(Si_umol.l = "Si.OH.4..Âµmol.l.")%>%
  dplyr::rename(chl.a_ug.l = "Chl.a..Âµg.l.")%>%
  dplyr::rename(c.fix.umol.l.h = "C.fix..Âµmol.l.h.")

pigments_raw <- pigments_raw%>%
  dplyr::rename(chl.c3 = "Chl.c3..Âµg.l.")%>%
  dplyr::rename(chl.c1.c2 = "Chl.c1.c2..Âµg.l.")%>%
  dplyr::rename(chlide.a = "Chlide.a..Âµg.l.")%>%
  dplyr::rename(perid = "Perid..Âµg.l.")%>%
  dplyr::rename(Buto_fuco = "But.fuco..Âµg.l.")%>%
  dplyr::rename(fuco = "Fuco..Âµg.l.")%>%
  dplyr::rename(neo = "Neo..Âµg.l.")%>%
  dplyr::rename(hex_fuco = "Hex.fuco..Âµg.l.")%>%
  dplyr::rename(pras = "Pras..Âµg.l.")%>%
  dplyr::rename(viola = "Viola..Âµg.l.")%>%
  dplyr::rename(diadino = "Diadino..Âµg.l.")%>%
  dplyr::rename(anthera = "Anthera..Âµg.l.")%>%
  dplyr::rename(allo = "Allo..Âµg.l.")%>%
  dplyr::rename(diato = "Diato..Âµg.l.")%>%
  dplyr::rename(zea = "Zea..Âµg.l.")%>%
  dplyr::rename(lut = "Lut..Âµg.l.")%>%
  dplyr::rename(chl.b = "Chl.b..Âµg.l.")%>%
  dplyr::rename(dv_chla = "DV.chl.a..Âµg.l.")%>%
  dplyr::rename(chl.a = "Chl.a..Âµg.l.")%>%
  dplyr::rename(a_car = "a.Car..Âµg.l.")%>%
  dplyr::rename(b_car = "b.Car..Âµg.l.")


## chl a concentrations wrong in Carbonfix  / correct in pigments

Carbonfix_raw$chl.a_ug.l <- NULL

T_chla <- pigments_raw%>%filter(Timeslice == "0") 
T_chla <- T_chla[,c("Event", "chl.a")]

Carbon_fix_corrected <- dplyr::left_join(Carbonfix_raw, T_chla, by = "Event")
##


N2fix_raw <- N2fix_raw%>%
  dplyr::rename(delta_15N = "X.15N....air...natural.abundance.")

##add chl.a data to N2 fixation dataset

N2fix_raw <- left_join(N2fix_raw, Carbon_fix_corrected[c("Event","Method.comm..incubation.bin.","Temp","Replicate", "c.fix.umol.l.h", "chl.a")], by = c("Event", "Method.comm..incubation.bin.", "Replicate"))

##add MQR 
###how the MQR was calculated see https://doi.org/10.1594/PANGAEA.885932


N2fix_raw <- N2fix_raw%>%dplyr::rename(Incubation = "Method.comm..incubation.bin.")

MQR$Event <- NULL
MQR$Event <- MQR$Event.label
MQR$Event <- gsub("MD206_","", MQR$Event)

MQR <- MQR%>%dplyr::rename(MQR = "Total.calculated.uncertainty...Minimum.Quantifiable.Rate.")

m <- c("MQR", "Event", "Incubation")

N2fix_raw <- left_join(N2fix_raw, MQR[m], by= c("Event", "Incubation"))

rm(m)

##


Carbon_fix_corrected$cfix.day <- Carbon_fix_corrected$c.fix.umol.l.h*24

##POCPN

POCPN_raw <- inner_join(POCPN, Carbon_fix_corrected[c("Event", "Timeslice..sampling.time.", "Replicate","cfix.day", "chl.a")], by = c("Event", "Timeslice..sampling.time.", "Replicate"))

POCPN_raw$N_umol.L <- POCPN_raw$N.ug.L.*(1/14) #calculate molar concentration
POCPN_raw$C_umol.L <- POCPN_raw$C..ug.L.*(1/12) #calucalte molar concentration

POCPN_raw$POCPN_ratio <- POCPN_raw$C_umol.L/POCPN_raw$N_umol.L

POCPN_raw$CHL_C <- POCPN_raw$chl.a/POCPN_raw$C..ug.L. #chlorophyll to POC ratio
POCPN_raw$PN_CHL <- POCPN_raw$N.ug.L./POCPN_raw$chl.a # PN:chla ratio


##associate provinces and watermasses
print("associate provinces and watermasses", quote = F)

ISSG <- c("OISO2","OISO3","OISO16","OISO18")
Cfix_ISSG_pro <- Carbon_fix_corrected%>%filter(Event %in% ISSG)%>% 
  cbind(province = paste0("ISSG"))

SSTC <- c("OISO4","OISO14","OISO15")
Cfix_SSTC_pro <- Carbon_fix_corrected%>%filter(Event %in% SSTC)%>% 
  cbind(province = paste0("SSTC"))

SO <- c("OISO37","OISOE","OISO11","OISO10", "OISO7", "OISO9", "OISO6")
Cfix_SO_pro <- Carbon_fix_corrected%>%filter(Event %in% SO)%>% 
  cbind(province = paste0("SO"))

Carbonfix_pro <- rbind(Cfix_ISSG_pro, Cfix_SSTC_pro, Cfix_SO_pro)

#water masses

Cwm_ISSG <-  Carbonfix_pro%>%filter(Event %in% ISSG)%>% 
  cbind(WM = paste0("ISSG"))

Cwm_STF <- Carbonfix_pro%>%filter(Event == "OISO4")%>% 
  cbind(WM = paste0("STF"))


SAF <- c("OISO14", "OISO15")
CwmSAF <- Carbonfix_pro%>%filter(Event %in% SAF)%>% 
  cbind(WM = paste0("SAF"))

PFZ <- c("OISO7", "OISO9", "OISO6")
CwmPFZ <- Carbonfix_pro%>%filter(Event %in% PFZ)%>% 
  cbind(WM = paste0("PFZ"))

AZ <- c("OISO37","OISOE","OISO11","OISO10")

CwmAZ <- Carbonfix_pro%>%filter(Event %in% AZ)%>% 
  cbind(WM = paste0("AZ"))

Carbonfix <- rbind(Cwm_ISSG, Cwm_STF, CwmSAF, CwmPFZ, CwmAZ)



##N2 fixation province and wm binding

n2fix_ISSG_pro <- N2fix_raw%>%filter(Event %in% ISSG)%>% 
  cbind(province = paste0("ISSG"))

n2fix_SSTC_pro <- N2fix_raw%>%filter(Event %in% SSTC)%>% 
  cbind(province = paste0("SSTC"))

n2fix_SO_pro <- N2fix_raw%>%filter(Event %in% SO)%>% 
  cbind(province = paste0("SO"))

n2fix_pro <- rbind(n2fix_ISSG_pro, n2fix_SSTC_pro, n2fix_SO_pro)

#water masses

n2wm_ISSG <-  n2fix_pro%>%filter(Event %in% ISSG)%>% 
  cbind(WM = paste0("ISSG"))

n2wm_STF <- n2fix_pro%>%filter(Event == "OISO4")%>% 
  cbind(WM = paste0("STF"))

n2wmSAF <- n2fix_pro%>%filter(Event %in% SAF)%>% 
  cbind(WM = paste0("SAF"))

n2wmPFZ <- n2fix_pro%>%filter(Event %in% PFZ)%>% 
  cbind(WM = paste0("PFZ"))

n2wmAZ <- n2fix_pro%>%filter(Event %in% AZ)%>% 
  cbind(WM = paste0("AZ"))

N2fix <- rbind(n2wm_ISSG, n2wm_STF, n2wmSAF, n2wmPFZ, n2wmAZ)

###pigments province and water mass association

pig_ISSG_pro <- pigments_raw%>%filter(Event %in% ISSG)%>% 
  cbind(province = paste0("ISSG"))

pig_SSTC_pro <- pigments_raw%>%filter(Event %in% SSTC)%>% 
  cbind(province = paste0("SSTC"))

pig_SO_pro <- pigments_raw%>%filter(Event %in% SO)%>% 
  cbind(province = paste0("SO"))

pig_pro <- rbind(pig_ISSG_pro, pig_SSTC_pro, pig_SO_pro)

#water masses

pigwm_ISSG <-  pig_pro%>%filter(Event %in% ISSG)%>% 
  cbind(WM = paste0("ISSG"))

pigwm_STF <- pig_pro%>%filter(Event == "OISO4")%>% 
  cbind(WM = paste0("STF"))

pigwmSAF <- pig_pro%>%filter(Event %in% SAF)%>% 
  cbind(WM = paste0("SAF"))

pigwmPFZ <- pig_pro%>%filter(Event %in% PFZ)%>% 
  cbind(WM = paste0("PFZ"))

pigwmAZ <- pig_pro%>%filter(Event %in% AZ)%>% 
  cbind(WM = paste0("AZ"))

pigments <- rbind(pigwm_ISSG, pigwm_STF, pigwmSAF, pigwmPFZ, pigwmAZ)

##POCPN province and water mass association

###provinces
POC_ISSG_pro <- POCPN_raw%>%filter(Event %in% ISSG)%>% 
  cbind(province = paste0("ISSG"))

POC_SSTC_pro <- POCPN_raw%>%filter(Event %in% SSTC)%>% 
  cbind(province = paste0("SSTC"))

POC_SO_pro <- POCPN_raw%>%filter(Event %in% SO)%>% 
  cbind(province = paste0("SO"))

POC_pro <- rbind(POC_ISSG_pro, POC_SSTC_pro, POC_SO_pro)

###water masses

POCwm_ISSG <-  POC_pro%>%filter(Event %in% ISSG)%>% 
  cbind(WM = paste0("ISSG"))

POCwm_STF <- POC_pro%>%filter(Event == "OISO4")%>% 
  cbind(WM = paste0("STF"))

POCwmSAF <- POC_pro%>%filter(Event %in% SAF)%>% 
  cbind(WM = paste0("SAF"))

POCwmPFZ <- POC_pro%>%filter(Event %in% PFZ)%>% 
  cbind(WM = paste0("PFZ"))

POCwmAZ <- POC_pro%>%filter(Event %in% AZ)%>% 
  cbind(WM = paste0("AZ"))

POCPN <- rbind(POCwm_ISSG, POCwm_STF, POCwmSAF, POCwmPFZ, POCwmAZ)



#clean up

rm(list = ls(pattern = "wm"))
rm(list = ls(pattern = "pro"))
rm(list = ls(pattern = "raw"))
rm(Carbon_fix_corrected)
#unload package
detach("package:tidyverse", unload=TRUE)
