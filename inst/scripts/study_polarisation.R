library("surveygraph")
library("igraph")

S1 <- surveygraph::generate_survey_polarised(polarisation = 0.00)
S2 <- surveygraph::generate_survey_polarised(polarisation = 0.75)
S3 <- surveygraph::generate_survey_polarised(polarisation = 1.50)
S4 <- surveygraph::generate_survey_polarised(polarisation = 2.25)

edgelist1 <- surveygraph::graph_edgelists(S1)
edgelist2 <- surveygraph::graph_edgelists(S2)
edgelist3 <- surveygraph::graph_edgelists(S3)
edgelist4 <- surveygraph::graph_edgelists(S4)

g1 <- make_graph(edges = edgelist1, directed = FALSE)
g2 <- make_graph(edges = edgelist2, directed = FALSE)
g3 <- make_graph(edges = edgelist3, directed = FALSE)
g4 <- make_graph(edges = edgelist4, directed = FALSE)

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(g1, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main="polarisation 0")
#plot(g2, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main="polarisation 0.75")
#plot(g3, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main="polarisation 1.5")
#plot(g4, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold, main="polarisation 2.25")
