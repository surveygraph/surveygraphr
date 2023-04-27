# an example R script, load in intereter by doing
# > source("scripts/example.R")

suppressMessages(library("surveygraphr"))
suppressMessages(library("igraph"))

# create a synthetic survey containing 10 respondents and 3 items
survey <- surveygraphr::surveydummy(10, 3)

# generate the respondent and item graphs
elists <- surveygraphr::buildgraphs(survey)

# elists contains the two edge lists
erespondents <- unlist(elists[1])
eitems <- unlist(elists[2])

grespondents <- make_graph(edges = erespondents, n = 10, directed = FALSE)

gitems <- make_graph(edges = eitems, n = 3, directed = FALSE)

#plot(grespondents)
#plot(gitems)
