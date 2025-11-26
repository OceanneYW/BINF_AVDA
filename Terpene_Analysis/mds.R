calcMds <- function(df, algo) {
  #reproducibility
  set.seed(55)
  nms <- metaMDS(log1p(df), algo, 2, 100, 1000)
  return(nms)
}

plotMds <- function(p, nms, forPrint = F) {

  nmsPoints <- as.data.frame(nms$points)
  nmsPoints$samp <- row.names(nmsPoints)
  
  #assign to var for return
  p <- p +
    geom_point(data = nmsPoints, aes(x = MDS1, y = MDS2), size = 2) +
    theme_bw()
  return(p)
}