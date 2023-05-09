library("surveygraphr")
library("ggplot2")

S <- surveygraphr::gensurvey(200, 25)

results <- surveygraphr::exploregraph(S)

df <- data.frame(radius = unlist(results[1]), degree = unlist(results[2]), LCC = unlist(results[3]))

ggplot(df, aes(x = radius, y = LCC)) + geom_point()
ggplot(df, aes(x = radius, y = z)) + geom_point()
