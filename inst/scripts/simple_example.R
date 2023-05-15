library("surveygraphr")
library("igraph")

S <- surveygraphr::generate_survey_polarised(m=500, polarisation=0.00)
#S <- surveygraphr::generate_model_survey(m=500, polarisation=0.00)

edgelist <- surveygraphr::graph_edgelists(S)

g1 <- make_graph(edges=edgelist[[1]], directed=FALSE)
g2 <- make_graph(edges=edgelist[[2]], directed=FALSE)

isolated_nodes1 <- which(degree(g1)==0)
isolated_nodes2 <- which(degree(g2)==0)

g1connected <- delete.vertices(g1, isolated_nodes1)
g2connected <- delete.vertices(g2, isolated_nodes2)

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(g1connected, vertex.size=2, vertex.label=NA, edge.width=0.1, layout=layout.fruchterman.reingold, main="respondent graph")
plot(g2connected, vertex.size=5, vertex.label=NA, edge.width=0.3, layout=layout.fruchterman.reingold, main="item graph")
