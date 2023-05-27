library("surveygraphr")
library("igraph")

S <- surveygraphr::make_synthetic_data(nrow=400, ncol=15, polarisation=1.25)

names <- data.frame(id=c(1:length(S)))

edgelist <- surveygraphr::make_projection(S, layer="symbolic")

g <- graph.data.frame(edgelist, vertices=names, directed=FALSE)

g <- delete.vertices(g, which(degree(g)==0))

E(g)$label= E(g)$weight

plot(g, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="symbolic layer")
