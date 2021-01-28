#DADA2 pipeline followed by tutorial of https://benjjneb.github.io/dada2/tutorial.html
#with modifications of primer trimming (https://benjjneb.github.io/dada2/ITS_workflow.html)

#Cite:
# DADA2- 
#Callahan, B. J., P. J. Mcmurdie, M. J. Rosen, A. W. Han, and A. J. A. 2016. 
#DADA2: High resolution sample inference from Illumina amplicon data. 
#Nat. Methods 13: 581–583. doi:10.1038/nmeth.3869.DADA2

#LOAD LIBS
library(dada2)
packageVersion("dada2")
library(ShortRead)
packageVersion("ShortRead")
library(Biostrings)
packageVersion("Biostrings")
library(stringr)


#CONSTRUCT NEEDED PATHS AND FILE LISTS
setwd("/scratch1/choerstm/ps113at/data/raw/amplicon_raw_sequences")
work_dir <- ("./18S")
raw_dir <- file.path(work_dir,"raw")
preFilt_dir <- file.path(work_dir,"preFilt")
qualFiltTrim_dir <- file.path(work_dir,"qualFiltTrim")

#CONSTRUCT NEEDED FILE LISTS
fnFs.raw <- sort(list.files(raw_dir, pattern = "_L001_R1_001.fastq.gz", full.names = TRUE))
fnRs.raw <- sort(list.files(raw_dir, pattern = "_L001_R2_001.fastq.gz", full.names = TRUE))
fnFs.preFilt <- file.path(preFilt_dir,basename(fnFs.raw))
fnRs.preFilt <- file.path(preFilt_dir,basename(fnRs.raw))
fnFs.qualFiltTrim <- file.path(qualFiltTrim_dir,basename(fnFs.raw))
fnRs.qualFiltTrim <- file.path(qualFiltTrim_dir,basename(fnRs.raw))


#GET SAMPLE NAMES
fnFs.raw
basename(fnFs.raw)
sample.names <- str_remove(basename(fnFs.raw),"_L001_R1_001.fastq.gz")
head(sample.names)

##plot Quality profile

plotQualityProfile(fnFs.raw[1:2])
#PREFILTERING
filterAndTrim(fnFs.raw,fnFs.preFilt,fnRs.raw,fnRs.preFilt,truncQ=2,minQ=2,minLen=50,maxN=0,multithread = 20)

#IDENTIFY PRIMER
FWD_PRIMER="CCAGCASCYGCGGTAATTCC" #Stoeck et al.
REV_PRIMER="ACTTTCGTTCTTGATYRATGA"


allOrients <- function(primer) {
  # Create all orientations of the input sequence
  require(Biostrings)
  dna <- DNAString(primer)  # The Biostrings works w/ DNAString objects rather than character vectors
  orients <- c(Forward = dna, Complement = complement(dna), Reverse = reverse(dna), 
               RevComp = reverseComplement(dna))
  return(sapply(orients, toString))  
}

FWD_PRIMER.orients <- allOrients(FWD_PRIMER)
REV_PRIMER.orients <- allOrients(REV_PRIMER)

FWD_PRIMER.orients 
REV_PRIMER.orients 

primerHits <- function(primer, fn) {
  # Counts number of reads in which the primer is found
  nhits <- vcountPattern(primer, sread(readFastq(fn)), fixed = FALSE)
  return(sum(nhits > 0))
}
#check one sample
rbind(FWD_PRIMER.ForwardReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnFs.preFilt[[1]]), 
      FWD_PRIMER.ReverseReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnRs.preFilt[[1]]), 
      REV_PRIMER.ForwardReads = sapply(REV_PRIMER.orients, primerHits, fn = fnFs.preFilt[[1]]), 
      REV_PRIMER.ReverseReads = sapply(REV_PRIMER.orients, primerHits, fn = fnRs.preFilt[[1]]))

#REMOVE PRIMERS

#cutadapt available?

cutadapt <- "cutadapt" # CHANGE ME to the cutadapt path on your machine
system2(cutadapt, args = "--version") # Run shell commands from R


path.cut <- file.path(work_dir, "cutadapt")
if(!dir.exists(path.cut)) dir.create(path.cut)
fnFs.cut <- file.path(path.cut, basename(fnFs.preFilt))
fnRs.cut <- file.path(path.cut, basename(fnRs.preFilt))

FWD.RC <- dada2:::rc(FWD_PRIMER)
REV.RC <- dada2:::rc(REV_PRIMER)
# Trim FWD and the reverse-complement of REV off of R1 (forward reads)
R1.flags <- paste("-g", FWD_PRIMER, "-a", REV.RC) 
# Trim REV and the reverse-complement of FWD off of R2 (reverse reads)
R2.flags <- paste("-G", REV_PRIMER, "-A", FWD.RC) 

# Run Cutadapt
for(i in seq_along(fnFs.raw)) {
  system2(cutadapt, args = c(R1.flags, R2.flags, "-n", 2, # -n 2 required to remove FWD and REV from reads
                             "-o", fnFs.cut[i], "-p", fnRs.cut[i], # output files
                             fnFs.preFilt[i], fnRs.preFilt[i])) # input files
}

#check one sample - you expect 0 everywhere, because you removed the primers :) 
rbind(FWD.ForwardReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnFs.cut[[1]]), 
      FWD.ReverseReads = sapply(FWD_PRIMER.orients, primerHits, fn = fnRs.cut[[1]]), 
      REV.ForwardReads = sapply(REV_PRIMER.orients, primerHits, fn = fnFs.cut[[1]]), 
      REV.ReverseReads = sapply(REV_PRIMER.orients, primerHits, fn = fnRs.cut[[1]]))
###
#before you do the next trimming you want to check how the sequence lengths are distributed 
#outside R: `cat reads.fastq | awk '{if(NR%4==2) print length($1)}' | sort -n | uniq -c > read_length.txt`
reads<-read.csv(file="./18S/DADA2_Apr_2020/read_length_distribution/read_length_R1.txt", sep="", header=FALSE) 
plot(reads$V2,reads$V1,type="l",xlab="read length",ylab="occurences",col="blue")
##
filterOut <- filterAndTrim(fnFs.cut,fnFs.qualFiltTrim,
                           fnRs.cut,fnRs.qualFiltTrim, 
                           maxN=0,maxEE=c(2,4),
                           #truncLen=c(300,200),
                           verbose = TRUE, rm.phix = TRUE, 
                           compress = TRUE, multithread = 20)
head(filterOut)
summary(filterOut[, 2]/filterOut[, 1])

fnFs.deRep <- derepFastq(fnFs.qualFiltTrim)
fnRs.deRep <- derepFastq(fnRs.qualFiltTrim)
names(fnFs.deRep) <- sample.names
names(fnRs.deRep) <- sample.names


plotQualityProfile(fnFs.raw[1:4])
plotQualityProfile(fnRs.raw[1:4])
plotQualityProfile(fnFs.cut[1:4])
plotQualityProfile(fnRs.cut[1:4])
plotQualityProfile(fnFs.qualFiltTrim[1:4])
plotQualityProfile(fnRs.qualFiltTrim[1:4])


#LEARN DADA2 ERROR RATES
errFWDs <- learnErrors(fnFs.deRep, multithread=10,randomize=TRUE, nbases = 1e8)
errREVs <- learnErrors(fnRs.deRep, multithread=10,randomize=TRUE, nbases = 1e8)


plotErrors(errFWDs, nominalQ=TRUE)
plotErrors(errREVs, nominalQ=TRUE)

#DADA SAMPLE INFERENCE
dadaFWDs <- dada(fnFs.deRep, err=errFWDs, multithread=20, pool="pseudo")
dadaREVs <- dada(fnRs.deRep, err=errREVs, multithread=20, pool="pseudo")

#inspect dada inference objects
dadaFWDs
dadaREVs

#MERGE PAIRED ENDS
mergers <- mergePairs(dadaFWDs, fnFs.deRep, dadaREVs, fnRs.deRep, minOverlap=20,verbose=TRUE)
#Inspect the merger data.frame from the first sample
head(mergers[[1]])

#CONSTRUCT SEQUENCE TABLE
seqtab <- makeSequenceTable(mergers)
#inspect table
dim(seqtab)
table(nchar(getSequences(seqtab)))
saveRDS(seqtab, file = "seqtabKerguelen_18S_2.RDA")
write.csv(t(seqtab), "seqtab_Kerguelen_18S_2.csv", quote=FALSE )


#REMOVE BIMERA AND CONSTRUCT BIMERA-FREE TABLE
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=8, verbose=TRUE)
#inspect table
dim(seqtab.nochim)
table(nchar(getSequences(seqtab.nochim)))
#chimera proportion
sum(seqtab.nochim)/sum(seqtab)
saveRDS(seqtab.nochim, file = "seqtab.nochim.Kerguelen_18S_2.RDA")
write.csv(t(seqtab.nochim), "seqtab.nochim_Kerguelen_18S_2.csv", quote=FALSE )


#TRACK READS
getN <- function(x) sum(getUniques(x))
track <- cbind(filterOut, sapply(dadaFWDs, getN), sapply(dadaREVs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
(track)

#write statistic file

write.csv(track, "Kerguelen_18S_DADA2_stats_2.csv")

##write fasta file for further analysis

####
asv_seqs <- colnames(seqtab.nochim)
asv_headers <- vector(dim(seqtab.nochim)[2], mode="character")
for (i in 1:dim(seqtab.nochim)[2]) {
  asv_headers[i] <- paste(">ASV", i, sep="_")
}
# making and writing out a fasta of our final ASV seqs:
asv_fasta <- c(rbind(asv_headers, asv_seqs))
write(asv_fasta, "Kerguelen_18S_ASVs.fasta")



