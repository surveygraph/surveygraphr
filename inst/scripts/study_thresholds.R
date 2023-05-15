library("surveygraphr")
library("ggplot2")

S <- surveygraphr::generate_survey_polarised(m=500, polarisation=0)

thresholdlist <- surveygraphr::sweep_thresholds(S)

p <- ggplot()
p <- p + geom_point(data = thresholdlist[[1]], aes(x = threshold, y = lcc), color="blue")
p <- p + geom_point(data = thresholdlist[[1]], aes(x = threshold, y = avgdegree), color="red")
p <- p + xlab(label = 'threshold')
p <- p + ylab(label = 'largest component, average degree')
p <- p + theme_bw()
print(p)
