library("surveygraphr")
library("ggplot2")

S <- make_synthetic_data(nrow=30, ncol=5, polarisation=0)

t <- make_threshold_profile(S, layer="agent")

p <- ggplot()
p <- p + geom_point(data = t, aes(x = threshold, y = isolated), color="blue")
p <- p + xlab(label = 'threshold')
p <- p + ylab(label = 'graph properties')
p <- p + theme_bw()
print(p)
