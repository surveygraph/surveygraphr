library("surveygraphr")
library("igraph")

S <- surveygraphr::make_synthetic_data(nrow=400, ncol=15, polarisation=1.25)

names1 <- data.frame(id=c(1:length(S$X1)), group=S$X1)
names2 <- data.frame(id=c(1:length(S)))

edgelists <- surveygraphr::make_projection(S)

g1 <- graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
g2 <- graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)

V(g1)$color <- ifelse(V(g1)$group == 1, "blue", "red")

isolated_nodes1 <- which(degree(g1)==0)
isolated_nodes2 <- which(degree(g2)==0)

g1c <- delete.vertices(g1, isolated_nodes1)
g2c <- delete.vertices(g2, isolated_nodes2)

E(g2c)$label= E(g2c)$weight

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="agent layer")
plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="symbolic layer")
