library("surveygraphr")
library("igraph")

S <- surveygraphr::generate_survey_polarised(m = 500, polarisation = 0.00)

edgelist <- surveygraphr::graph_edgelists(S)

g <- make_graph(edges = edgelist, directed = FALSE)

isolated_nodes <- which(degree(g) == 0)

gconnected <- delete.vertices(g, isolated_nodes)

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(gconnected, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main = "respondent graph")
plot(gconnected, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main = "item graph")
