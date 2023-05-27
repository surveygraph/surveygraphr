library("surveygraphr")
library("ggplot2")

S <- surveygraphr::generate_survey_polarised(m=300, n=25, polarisation=0)

thresholdlist <- surveygraphr::sweep_thresholds(S)

p <- ggplot()
p <- p + geom_point(data = thresholdlist[[2]], aes(x = threshold, y = lcc), color="blue")
p <- p + geom_point(data = thresholdlist[[2]], aes(x = threshold, y = avgdegree), color="red")
p <- p + xlab(label = 'threshold')
p <- p + ylab(label = 'largest component, average degree')
p <- p + theme_bw()
print(p)
