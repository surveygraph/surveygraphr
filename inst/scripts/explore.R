library("surveygraphr")
library("ggplot2")

S <- surveygraphr::gensurvey(200, 25)

results <- surveygraphr::exploregraphs(S)

df <- data.frame(radius = unlist(results[1]), degree = unlist(results[2]), LCC = unlist(results[3]))

ggplot(df, aes(x = radius, y = LCC)) + labs(title="size of the largest connected component as a function of radius") + geom_point()

ggplot(df, aes(x = radius, y = z)) + labs(title="average degree as a function of radius") + geom_point()
