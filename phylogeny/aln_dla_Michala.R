source("https://bioconductor.org/biocLite.R")
biocLite("msa")
library("msa")

#For nucleotide sequences
sek=readDNAStringSet("Homo.fasta", format="fasta")

#For amino acid sequences
sek=readAAStringSet("phylogeny/Methanobacterium_arcticum.gb.aa", format="fasta")

names(slot(sek, "ranges")) <- lapply(strsplit(names(slot(sek, "ranges")), "|", fixed = TRUE), 
                                    function(i) paste0(i[1L:4], collapse = "|"))
names(slot(sek, "ranges")) <- gsub("|[|]+", "|", names(slot(sek, "ranges")), fixed = TRUE)


aln=msa(sek, "ClustalOmega")
#Opcje do wyboru: zamiast "ClustalOmega" mo?e by? "ClustalW" i "Muscle"

#system.file("tex", "texshade.sty", package="msa")
#msaPrettyPrint(aln, output="pdf", showNames="left", showLogo="none", askForOverwrite=FALSE, verbose=FALSE)

install.packages("tools")
library("tools")

msaPrettyPrint(aln, output="tex", showNames="left", showLogo="top", shadingModeArg="chemical", consensusThreshold=50, logoColors="chemical", shadingMode="functional")
texi2pdf("aln.tex", clean=TRUE)

#Opcje dla:
#Okienko do wpisywania wartosci: Consensus threshold: consensusThreshold=50
#Logo colors: logoColors=c("chemical", "rasmol", "hydropathy", "structure", "standard area", "accessible area")
#Shading mode: shadingMode=c("identical", "similar", "functional")
#Jezeli wybrane w shadingMode:
#"identical" lub "similar", to pojawia sie okienko shadingModeArg, do wpisywania wartosci od 0 do 100, domyslnie 50
#"functional", to pojawia sie okienko shadingModeArg z mozliwosciami: "charge", "hydropathy", "structure", "chemical", "rasmol", "standard area", and "accessible area" (see documentation of texshade.sty for details).


#Zle sie konwertuja
#seq_nt=msaConvert(aln,type=c("ape::DNAbin"))

seq_aln=msaConvert(aln,type=c("seqinr::alignment"))

install.packages("seqinr")
library("seqinr")

for (i in 1:seq_aln$nb){
write.fasta(sequences=seq_aln$seq[i], names=seq_aln$nam[i], file.out="seq.aln", open= "a")
}
