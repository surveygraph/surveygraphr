library("surveygraphr")
library("igraph")

S <- surveygraphr::generate_survey_polarised(polarisation = 0.00)

edgelist <- surveygraphr::graph_edgelists(S)

g <- make_graph(edges = edgelist, directed = FALSE)

plot(g, vertex.size=2, vertex.label=NA, edge.width = 0.1, layout=layout.fruchterman.reingold)
