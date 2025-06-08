library(dendextend)

plot_dendrogram <- function(dend, method, max_height) {
  cut_height = get_tallest_branch_height(dend, max_height)
  clust = cutree(dend, h=cut_height)
  num_clusts = length(unique(clust))
  dend = as.dendrogram(dend)
  dend = color_branches(dend, k = num_clusts, col = brewer.pal(num_clusts, "Dark2"))
  dend = color_labels(dend, k = num_clusts, col = brewer.pal(num_clusts, "Dark2"))
  labels(dend) = NA
  par(cex.axis = 1, cex.main = 2, cex.sub = 1.5, bg = "#f7f7f7", cex.lab = 1, mar = c(5, 4, 4, 2) + .1)
  # plot(dend, ylim = max_height, hang = -1, xlab = paste("number of clusters", num_clusts))
  plot(dend, main = paste(method, "linkage clustering"), xlab = paste("cut height:", round(cut_height, 2), "number of clusters:", num_clusts, "dendrogram max height:", round(max_height, 2)), ylim = c(0, max_height), ylab = "Merge Height")
  abline(h=cut_height, lty = 2)
  nodes = get_nodes_xy(dend)
  sorted_nodes = nodes[order(nodes[,2]),]
  if (num_clusts == 1) {
    color = "#1B9E77"
  } else {
    color = "black"
  }
  segments(x0 = sorted_nodes[nrow(nodes), 1], y0 = sorted_nodes[nrow(nodes),2], y1 = max_height, col = color)
}
