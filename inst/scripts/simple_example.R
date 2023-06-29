library("surveygraph")
library("igraph")

S <- make_synthetic_data(nrow=500, ncol=10, p=0.22, c=0.5)

names1 <- data.frame(id=c(1:length(S$group)), group=S$group)
names2 <- data.frame(id=c(1:length(S)))

#e1 <- make_projection(S, "agent", threshold_method="target_lcc", method_value=0.95)
e1 <- make_projection(S, "agent", threshold_method="target_lcc", method_value=0.99, centre=TRUE)
e2 <- make_projection(S, "symbolic", threshold_method="raw_similarity", method_value=-1, centre=FALSE)

g1 <- graph.data.frame(e1, vertices=names1, directed=FALSE)
g2 <- graph.data.frame(e2, vertices=names2, directed=FALSE)

V(g1)$color <- ifelse(V(g1)$group == 1, "blue", "red")

g1 <- delete.vertices(g1, which(degree(g1)==0))
g2 <- delete.vertices(g2, which(degree(g2)==0))

#E(g2)$label= E(g2)$weight

plot(g1, vertex.size=2.5, vertex.label=NA, edge.width=0.3, layout=layout.fruchterman.reingold, main="agent layer")
#plot(g2, vertex.size=10, edge.width=10 * E(g2)$weight, layout=layout.circle, main="symbolic layer")

#par(mfrow=c(1,2), mar=c(1,1,1,1))
#plot(g1, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="agent layer")
#plot(g2, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="symbolic layer")
