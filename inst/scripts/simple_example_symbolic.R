library("surveygraph")
library("igraph")

S <- make_synthetic_data(nrow=400, ncol=9, polarisation=0.3)

e <- make_projection(S, layer="symbolic", threshold_method="raw_similarity", method_value=-1.0)

names <- data.frame(id=c(1:(length(S) - 1)))

g <- graph.data.frame(e, vertices=names, directed=FALSE)

E(g)$width <- 1 * E(g)$weight ** 3
E(g)$label <- E(g)$weight

E(g)$color <- ifelse(E(g)$weight > 1.4, "#af8dc3", "#7fbf7b")

plot(
  g, 
  vertex.size=10,
  layout=layout.circle,
  vertex.label.family="sans",
  edge.label.cex=0.9,
  edge.label.family="sans",
)
