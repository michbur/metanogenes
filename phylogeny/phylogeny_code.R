# install.packages(c("ape", "devtools", phangorn", "phytools", "ggplot2", "ggdendro", "dendextend"), repos = "https://cloud.r-project.org/")
# source("https://bioconductor.org/biocLite.R")
# biocLite("msa", "ggtree")


library(ape)
library(phangorn)
library(phytools)
library(ggtree)


# Phylogeny based on nucleotide sequences
# Okineko do wklejania sekwencji i okienko do ladowania pliku z przyrownanymi sekwencjami
chosen_file <- "./phylogeny/homo.fasta.aln"

#Select a nucleotide substitution model
# model:"raw", "N", "TS", "TV", "JC69", "K80", "F81", "K81", "F84", "BH87", "T92", "TN93", "GG95", "logdet", "paralin", "indel", "indelblock".
chosen_model_DNA <- "F84"

#Select a amino acid substitution model
# model: "JC69", "F81", "WAG", "JTT", "LG", "Dayhoff", "cpREV", "mtmam", "mtArt", "MtZoa", "mtREV24", "VT","RtREV", "HIVw", "HIVb", "FLU", "Blossum62", "Dayhoff_DCMut", "JTT_DCMut"
chosen_model_aa <- "F81"


# Gamma: okienko do wpisywania wartosci domyslnie nic
chosen_gamma <- FALSE

#SN1987
#Select a method to reconstruct phylogenetic tree
#the neighbor-joining method of Saitou and Nei (1987)

#G1997
#the BIONJ algorithm of Gascuel (1997)

#DG2002
#the the minimum evolution algorithm of Desper and Gascuel (2002)

# tree_type: lista trzech mozliwości (SN1987, G1997, DG2002)
chosen_tree_type = "SN1987"




# Okienko do wyboru opcji
# Dla pairwise.deletion checkbox, nazwa: Delete sites with at least one missing data for all sequences
chosen_deletion <- FALSE


# Opcja do wyboru: Bootstrap z okienkiem do wartosci parametru B (możliwe wartości: 100, 200, 300)
chosen_B <- 100


# zmienna wewnętrzna, wartości - DNA dla DNA i AA dla aminokwasów
seq_type <- "aa"



tree_funcs <- switch(chosen_tree_type, 
                     SN1987 = list(tree_method = function(x) nj(x)),
                     G1997 = list(tree_method = function(x) bionj(x)),
                     DG2002 = list(tree_method = function(x) fastme.bal(x, 
                                                                        nni = TRUE, 
                                                                        spr = TRUE, 
                                                                        tbr = TRUE)))

seq_fun <- switch(seq_type,
       DNA = list(read = function(x) read.dna(file = x, format = "fasta"),
                  dist = function(x) dist.dna(seq_nt, model = chosen_model_dna,
                                                   gamma = chosen_gamma,
                                                   pairwise.deletion = chosen_deletion),
                  boot_phyl = function(x) boot.phylo()
       ),
       aa = list(read = function(x) read.aa(file = x, format = "fasta"),
                 dist = function(x) dist.ml(x, model = chosen_model_aa,
                                                 shape = chosen_gamma,
                                                 exclude = ifelse(chosen_deletion, "pairwise",
                                                                  "none"),
                                                 k = 1L)
       )
)

# switch(seq_type, 
#        DNA = list(read_data = function(x) read.dna(file = x, format = "fasta"),
#                   calc_dist = function(x) dist.dna(seq_nt, model = chosen_model_dna, 
#                                                    gamma = chosen_gamma, 
#                                                    pairwise.deletion = chosen_deletion),
#                   boot_phyl = function(x) boot.phylo()
#        ),
#        aa = list(read_data = function(x) read.aa(file = x, format = "fasta"),
#                  calc_dist = function(x) dist.ml(seq_nt, model = chosen_model_aa,
#                                                  shape = chosen_gamma, 
#                                                  exclude = ifelse(chosen_deletion, "pairwise", 
#                                                                   "none"), 
#                                                  k = 1L)
#        )
# )



seq_nt <- try(seq_fun[["read"]](chosen_file), silent = TRUE)
res_tree_cmp <- if(class(seq_nt) != "try-error") {
  try({
    dist_nt <- seq_fun[["dist"]](seq_nt)

    tree <- tree_funcs[["tree_method"]](dist_nt)
    
    fboot <- function(x) tree_funcs[["tree_method"]](dist_nt)
    tb <- fboot(seq_nt)
    bp <- boot.phylo(tb, seq_nt, function(xx) 
      tree_funcs[["tree_method"]]( seq_fun[["dist"]](xx)), B = chosen_B, rooted = FALSE)
    
    bp_tree <- apeBoot(tree, bp)
    list(bp_tree = bp_tree)
  })
} 


seq_aa <- try(seq_fun[["read"]](chosen_file), silent = TRUE)
res_tree_cmp <- if(class(seq_nt) != "try-error") {
  try({
    dist_nt <- seq_fun[["dist"]](seq_aa)
    
    tree <- tree_funcs[["tree_method"]](dist_nt)
    
    
    Ntrees <- bootstrap.phyDat(seq_aa, FUN=function(x) 
      tree_funcs[["tree_method"]](seq_fun[["dist"]](x)), bs = chosen_B)
    
    tree_file <- tempfile()
    phangornBoot(tree, Ntrees) %>% 
      write.nexus(file = tree_file)
 
    read.nexus(tree_file) %>% fortify

    list(bp_tree = bp_tree)
  })
} 




if(class(res_tree_cmp != "try-error")) {
  fort_tree <- fortify(bp_tree)
  fort_tree[["bootstrap"]][fort_tree[["bootstrap"]] < 50] <- NA
  
  svg("tmp_name.svg", width = 7, height = 0.45*sum(!is.na(fort_tree[["label"]])), pointsize = 12)
  ggtree(fort_tree, branch.length = "branch.length") +
    #geom_text(hjust = "outward") +
    geom_tiplab() +
    geom_label(aes(label=bootstrap), size = 3.5, hjust = -0.05, label.size = NA, fill = NA) +
    geom_treescale() +
    ggplot2:::limits(c(0, max(fort_tree[["x"]])*nchar(fort_tree[which.max(fort_tree[["x"]]), "label"])/13), "x") 
  dev.off()
}