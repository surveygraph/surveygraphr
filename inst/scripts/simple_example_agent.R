library("surveygraphr")
library("igraph")

S <- make_synthetic_data(nrow=400, ncol=15, polarisation=1.25)

names <- data.frame(id=c(1:length(S$X1)), group=S$X1)

edgelist <- make_projection(S, layer="agent")

g <- graph.data.frame(edgelist, vertices=names, directed=FALSE)

V(g)$color <- ifelse(V(g)$group == 1, "blue", "red")

g <- delete.vertices(g, which(degree(g)==0))

plot(g, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="agent layer")
