library("surveygraphr")
library("ggplot2")

S <- surveygraphr::generate_survey_polarised(m=20, n=4, polarisation=1.25)

thresholdlist <- surveygraphr::sweep_thresholds(S)

#df <- data.frame(radius = thresholdlist[[1]], degree = thresholdlist[[2]], LCC = thresholdlist[[3]])
#
#ggplot(df, aes(x = radius, y = LCC)) + geom_point()
#ggplot(df, aes(x = radius, y = z)) + geom_point()
