library("surveygraph")
library("igraph")

S <- make_synthetic_data(nrow=500, ncol=15, p=0.14)

names <- data.frame(id=c(1:length(S$group)), group=S$group)

e <- make_projection(S, layer="agent", threshold_method="target_lcc", method_value=0.95)

g <- graph.data.frame(e, vertices=names, directed=FALSE)
g <- delete.vertices(g, which(degree(g)==0))

V(g)$color <- ifelse(V(g)$group == 1, "#0571b0", "#ca0020")

plot(
  g,
  vertex.size=2.5,
  vertex.label=NA,
  edge.width=0.2,
  layout=layout.fruchterman.reingold,
)
