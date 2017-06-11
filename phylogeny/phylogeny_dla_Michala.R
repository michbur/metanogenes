# install.packages(c("ape", "phangorn", "phytools", "ggplot2"), repos = "https://cloud.r-project.org/")

library(ape)
library(phangorn)
library(phytools)

setwd("./phylogeny/")
#Phylogeny based on nucleotide sequences
#Okineko do wklejania sekwencji i okienko do ladowania pliku z przyrownanymi sekwencjami
seq_nt <- read.dna(file="./phylogeny/homo.fasta.aln", format = "fasta")

#Select a nucleotide substitution model
#Okienko do wyboru opcji
#model:"raw", "N", "TS", "TV", "JC69", "K80", "F81", "K81", "F84", "BH87", "T92", "TN93", "GG95", "logdet", "paralin", "indel", "indelblock".
#Gamma: okienko do wpisywania wartosci domyslnie nic
#Dla pairwise.deletion checkbox, nazwa: Delete sites with at least one missing data for all sequences
chosen_model <- "F84"
dist_nt <- dist.dna(seq_nt, model = chosen_model, gamma = FALSE, pairwise.deletion = FALSE)

# tree_type: lista trzech mozliwości (SN1987, G1997, DG2002)
chosen_tree_type = "SN1987"
tree_funcs <- switch(chosen_tree_type, 
                     SN1987 = list(tree_method = function(x) nj(x)),
                     G1997 = list(tree_method = function(x) bionj(x)),
                     DG2002 = list(tree_method = function(x) fastme.bal(x, 
                                                                        nni = TRUE, 
                                                                        spr = TRUE, 
                                                                        tbr = TRUE)))
#SN1987
#Select a method to reconstruct phylogenetic tree
#the neighbor-joining method of Saitou and Nei (1987)

#G1997
#the BIONJ algorithm of Gascuel (1997)

#DG2002
#the the minimum evolution algorithm of Desper and Gascuel (2002)

tree <- tree_funcs[["tree_method"]](dist_nt)

#Wyb?r miedzy: Rooted tree i Unrooted tree

#Dla Unrooted tree

#Opcja do wyboru: Bootstrap z okienkiem do wpisania wartosci parametru B 
#dla the neighbor-joining method of Saitou and Nei (1987)
fboot = function(x) midpoint.root(nj(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)))
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=FALSE)

#dla the BIONJ algorithm of Gascuel (1997)
fboot = function(x) midpoint.root(bionj(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)))
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=FALSE)

#dla the the minimum evolution algorithm of Desper and Gascuel (2002)
fboot = function(x) midpoint.root(fastme.bal(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)))
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=FALSE)

tree=midpoint.root(tree)


plot(tree, no.margin=FALSE, type="u", cex=0.8)
plot(tree, no.margin=FALSE, type="p", cex=1)
plot(tree, no.margin=FALSE, type="f", cex=1)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)

nodelabels(bp, frame="n", adj = c(1.2, 1.5))

#Dla Rooted tree
#Jezeli bedzie wybrana opcaj Rooted tree, to pojawi sie okienko do wpisania outgroup
outgroup="kapucynka_bialoczelna"

#Opcja do wyboru: Bootstrap z okienkiem do wpisania wartosci parametru B

#dla the neighbor-joining method of Saitou and Nei (1987)
fboot = function(x) root(nj(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)), outgroup)
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=TRUE)

#dla the BIONJ algorithm of Gascuel (1997)
fboot = function(x) root(bionj(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)), outgroup)
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=TRUE)

#dla the the minimum evolution algorithm of Desper and Gascuel (2002)
fboot = function(x) root(fastme.bal(dist.dna(x, model="F84", gamma = FALSE, pairwise.deletion = FALSE)), outgroup)
tb=fboot(seq_nt)
bp = boot.phylo(tb, seq_nt, fboot, B = 100, rooted=TRUE)

tree=root(tree, outgroup, resolve.root = TRUE)

plot(tree, no.margin=FALSE, type="p", cex=1)
plot(tree, no.margin=FALSE, type="f", cex=1)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)

nodelabels(bp, frame="n", adj = c(1.2, 1.5))





#Phylogeny based on aminoacid sequences
#Okineko do wklejania sekwencji i okienko do ladowania pliku z przyrownanymi sekwencjami
seq_aa=read.aa(file="glob.fasta.aln", format = "fasta")

#Select an aminoacid substitution model
#Okienko do wyboru opcji
#model:"JC69", "F81", "WAG", "JTT", "LG", "Dayhoff", "cpREV", "mtmam", "mtArt", "MtZoa", "mtREV24", "VT","RtREV", "HIVw", "HIVb", "FLU", "Blossum62", "Dayhoff_DCMut", "JTT_DCMut".
#Gamma (parametr shape): okienko do wpisywania wartosci domyslnie nic
#Dla exclude checkbox, nazwa: Delete sites with at least one missing data for all sequences
dist_aa=dist.ml(seq_aa, model = "JTT", shape = 1, exclude = "none", k = 1L)

#Select a method to reconstruct phylogenetic tree
#the neighbor-joining method of Saitou and Nei (1987)
tree=nj(dist_aa)

#the BIONJ algorithm of Gascuel (1997)
tree=bionj(dist_aa)

#the the minimum evolution algorithm of Desper and Gascuel (2002)
tree=fastme.bal(dist_aa, nni = TRUE, spr = TRUE, tbr = TRUE)


#Wyb?r miedzy: Rooted tree i Unrooted tree

#Unrooted tree
tree=midpoint.root(tree)

plot(tree, no.margin=FALSE, type="u", cex=1)
plot(tree, no.margin=FALSE, type="p", cex=1)
plot(tree, no.margin=FALSE, type="f", cex=1)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)

#Rooted tree
#Jezeli bedzie wybrana opcaj Rooted tree, to pojawi sie okienko do wpisania outgroup
outgroup="LGB2_LUPLU"
outgroup="HBA_HORSE"

tree=root(tree, outgroup, resolve.root = TRUE)

plot(tree, no.margin=FALSE, type="p", cex=1)
plot(tree, no.margin=FALSE, type="f", cex=1)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)


#Opcja do wyboru: Bootstrap z okienkiem do wpisania wartosci parametru bs
#dla the neighbor-joining method of Saitou and Nei (1987)
Ntrees <- bootstrap.phyDat(seq_aa, FUN=function(x)NJ(dist.ml(x, model = "JTT", shape = 1, exclude = "none", k = 1L)), bs=100)

#dla the BIONJ algorithm of Gascuel (1997)
Ntrees <- bootstrap.phyDat(seq_aa, FUN=function(x)bionj(dist.ml(x, model = "JTT", shape = 1, exclude = "none", k = 1L)), bs=100)

#dla the the minimum evolution algorithm of Desper and Gascuel (2002)
Ntrees <- bootstrap.phyDat(seq_aa, FUN=function(x)fastme.bal(dist.ml(x, model = "JTT", shape = 1, exclude = "none", k = 1L)), bs=100)

treeBP <- plotBS(tree, Ntrees, type = "unrooted", p = 50)
treeBP <- plotBS(tree, Ntrees, type = "phylogram", p = 50)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)






#Laczenie drzewka z innymi danymi z bazy
#Rooted tree ma byc okienko do wpisania outgroup
outgroup="LGB2_LUPLU"

tree=root(tree, outgroup, resolve.root = TRUE)
plot(tree, no.margin=FALSE, type="p", cex=1)

maks=max(tree$edge.length)
zaok=round(maks/8,2)
add.scale.bar(length=zaok)

#wczytanie danych do dolaczenia do drzewka
tab = read.table("dane1.txt", header=TRUE, sep="\t")

install.packages("gdata")
library("gdata")

tab$name <- reorder.factor(tab$name, new.order=tree$tip.label)

install.packages("dplyr")
library("dplyr")

tab = tab %>% arrange(name)

install.packages("adephylo")
library("adephylo")

install.packages("phylobase")
library("phylobase")

obj <- phylo4d(tree, tab)

num=length(tree$tip.label)
table.phylo4d(obj, grid = FALSE, ratio.tree = 5/num, use.edge.length=FALSE, box = FALSE)




