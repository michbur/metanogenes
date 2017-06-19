phangornBoot <- function (tree, BStrees, type = "unrooted", bs.col = "black", 
                          bs.adj = NULL, p = 50, frame = "none", ...) {
  type <- match.arg(type, c("phylogram", "cladogram", "fan", 
                            "unrooted", "radial"))
  if (type == "phylogram" | type == "cladogram") {
    if (!is.rooted(tree) & !is.null(tree$edge.length)) 
      tree2 = midpoint(tree)
    else tree2 = tree
  }
  if (hasArg(BStrees)) {
    BStrees <- .uncompressTipLabel(BStrees)
    if (any(is.rooted(BStrees))) 
      BStrees <- unroot(BStrees)
    x = prop.clades(tree, BStrees)
    x = round((x/length(BStrees)) * 100)
    tree$node.label = x
  }
  else {
    if (is.null(tree$node.label)) 
      stop("You need to supply BStrees or tree needs \n        needs BS-values as node.label")
    x <- tree$node.label
  }
  
  tree
}


tree_funcs <- switch(chosen_tree_type, 
                     SN1987 = list(tree_method = function(x) nj(x)),
                     G1997 = list(tree_method = function(x) bionj(x)),
                     DG2002 = list(tree_method = function(x) fastme.bal(x, 
                                                                        nni = TRUE, 
                                                                        spr = TRUE, 
                                                                        tbr = TRUE)))



phyl_dna <- function(seq, dist_fun, tree_method) {
  dist_nt <- dist_fun(seq)
  
  tree <- tree_method(dist_nt)
  
  fboot <- function(x) tree_method(dist_nt)
  tb <- fboot(seq)
  bp <- boot.phylo(tb, seq, function(xx) 
    tree_method(dist_fun(xx)), B = chosen_B, rooted = FALSE)
  
  bp_tree <- apeBoot(tree, bp)
  
  bp_tree
}


phyl_aa <- function(seq, dist_fun, tree_method) {
  dist_nt <- dist_fun(seq)
  
  tree <- tree_method(dist_nt)
  
  Ntrees <- bootstrap.phyDat(seq, FUN=function(x) 
    tree_method(dist_fun(x)), bs = chosen_B)
  boot_res <- phangornBoot(tree, Ntrees) 
  bp_tree <- apeBoot(tree, boot_res[["node.label"]])
  
  bp_tree
}


seq_fun <- switch(seq_type,
                  dna = list(
                    read_fasta = function(x) readDNAStringSet(x, format="fasta"),
                    read = function(x) read.dna(file = x, format = "fasta"),
                    dist = function(x) dist.dna(x, model = chosen_model_dna,
                                                gamma = chosen_gamma,
                                                pairwise.deletion = chosen_deletion),
                    phyl = function(x) phyl_dna(x, dist_fun = seq_fun[["dist"]], 
                                                tree_method = tree_funcs[["tree_method"]])
                  ),
                  aa = list(
                    read_fasta = function(x) readAAStringSet(x, format="fasta"),
                    read = function(x) read.aa(file = x, format = "fasta"),
                    dist = function(x) dist.ml(x, model = chosen_model_aa,
                                               shape = chosen_gamma,
                                               exclude = ifelse(chosen_deletion, "pairwise",
                                                                "none"),
                                               k = 1L),
                    phyl = function(x) phyl_aa(x, dist_fun = seq_fun[["dist"]], 
                                               tree_method = tree_funcs[["tree_method"]])
                  )
)

plot_tree <- function(phyl_tree) {
  fort_tree <- fortify(phyl_tree)
  fort_tree[["bootstrap"]][fort_tree[["bootstrap"]] < 50] <- NA
  
  # factor by which x axis should be adjusted to keep names on the plot
  x_adj <- max(fort_tree[["x"]])*nchar(fort_tree[which.max(fort_tree[["x"]]), "label"])/13
  
  ggtree(fort_tree, branch.length = "branch.length") +
    #geom_text(hjust = "outward") +
    geom_tiplab() +
    geom_label(aes(label=bootstrap), size = 3.5, hjust = -0.05, label.size = NA, fill = NA) +
    geom_treescale() +
    ggplot2:::limits(c(0, x_adj), "x") 
}

