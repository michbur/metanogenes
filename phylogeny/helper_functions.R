phangornBoot <- function (tree, BStrees, type = "unrooted", bs.col = "black", 
          bs.adj = NULL, p = 50, frame = "none", ...) 
{
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
