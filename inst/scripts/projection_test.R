suppressMessages(library("surveygraphr"))
suppressMessages(library("igraph"))
suppressMessages(library("ggplot2"))

S <- make_synthetic_data(nrow=200, ncol=5, polarisation=1.25)

names <- data.frame(id=c(1:length(S$X1)), group=S$X1)

e <- make_projection(data=S, layer="agent", threshold_method="target_lcc", method_value=0.95)

#t <- make_threshold_profile(S, layer="agent")
#
#write.table(t, "~/surveygraphr/tmpdata.dat")
#
#p <- ggplot()
#p <- p + geom_point(data = t, aes(x = threshold, y = lcc), color="blue")
#p <- p + xlab(label = 'threshold')
#p <- p + ylab(label = 'lcc')
#p <- p + theme_bw()
#print(p)

g <- graph.data.frame(e, vertices=names, directed=FALSE)

V(g)$color <- ifelse(V(g)$group == 1, "blue", "red")

g <- delete.vertices(g, which(degree(g)==0))

plot(g, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="agent layer")
