library("surveygraphr")
library("igraph")

S <- surveygraphr::gensurvey(200, 25)

edgelist <- surveygraphr::listgraph(S)

g <- make_graph(edges = edgelist, directed = FALSE)
plot(g, vertex.size=5, vertex.label=NA)
