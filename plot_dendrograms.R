library(dendextend)

plot_dendrogram <- function(dend, method, cut_height, max_height) {
  clust = cutree(dend, h=cut_height)
  num_clusts = length(unique(clust))
  dend = as.dendrogram(dend)
  dend = color_branches(dend, k = num_clusts, col = brewer.pal(num_clusts, "Dark2"))
  dend = color_labels(dend, k = num_clusts, col = brewer.pal(num_clusts, "Dark2"))
  labels_cex(dend) = .75
  par(cex.axis = 1, cex.main = 2, cex.sub = 1.5, bg = "#f7f7f7", cex.lab = 1, mar = c(5, 4, 4, 2) + .1)
  # plot(dend, ylim = max_height, hang = -1, xlab = paste("number of clusters", num_clusts))
  return(plot(dend, main = paste(method, "linkage clustering"), xlab = paste("cut height:", round(cut_height, 2), "number of clusters:", num_clusts, "dendrogram max height:", round(max_height, 2))))
  # abline(h=cut_height, lty = 2)
}
