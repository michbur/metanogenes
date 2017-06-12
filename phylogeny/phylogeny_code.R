# install.packages(c("ape", "devtools", phangorn", "phytools", "ggplot2", "ggdendro", "dendextend"), repos = "https://cloud.r-project.org/")
source("https://bioconductor.org/biocLite.R")
biocLite("ggtree")


library(ape)
library(phangorn)
library(phytools)


# Phylogeny based on nucleotide sequences
# Okineko do wklejania sekwencji i okienko do ladowania pliku z przyrownanymi sekwencjami
chosen_file <- "./phylogeny/homo.fasta.aln"

#Select a nucleotide substitution model
# model:"raw", "N", "TS", "TV", "JC69", "K80", "F81", "K81", "F84", "BH87", "T92", "TN93", "GG95", "logdet", "paralin", "indel", "indelblock".
chosen_model <- "F84"

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


seq_nt <- try(read.dna(file = chosen_file, format = "fasta"), silent = TRUE)
if(class(seq_nt) != "try-error") {
  dist_nt <- dist.dna(seq_nt, model = chosen_model, 
                      gamma = chosen_gamma, 
                      pairwise.deletion = chosen_deletion)
  
  tree_funcs <- switch(chosen_tree_type, 
                       SN1987 = list(tree_method = function(x) nj(x)),
                       G1997 = list(tree_method = function(x) bionj(x)),
                       DG2002 = list(tree_method = function(x) fastme.bal(x, 
                                                                          nni = TRUE, 
                                                                          spr = TRUE, 
                                                                          tbr = TRUE)))
  
  tree <- compute.brlen(tree_funcs[["tree_method"]](dist_nt))
  
  fboot <- function(x) midpoint.root(tree_funcs[["tree_method"]](dist_nt))
  tb <- fboot(seq_nt)
  bp <- boot.phylo(tb, seq_nt, fboot, B = chosen_B, rooted = FALSE)
} else {
  "Invalid input"
}

fort_tree <- fortify(tree)
  
ggtree(fort_tree, aes(label = label)) +
  geom_text(hjust = "outward") +
  scale_x_continuous(expand = c(0, max(nchar(fort_tree[["label"]]), na.rm = TRUE)/46))

