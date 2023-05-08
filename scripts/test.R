# an example R script, invoke in an intereter by running
# > source("scripts/example.R")

suppressMessages(library("surveygraphr"))
suppressMessages(library("igraph"))

# create a synthetic survey containing 10 respondents and 3 items
S <- surveygraphr::gensurvey(10000, 5)

# generate the respondent and item graphs
elists <- surveygraphr::buildgraphs(S)

## elists contains the two edge lists, first for respondents, second for items
#erespondents <- unlist(elists[1])
#eitems <- unlist(elists[2])
#
#grespondents <- make_graph(edges = erespondents, n = 10, directed = FALSE)
#
#gitems <- make_graph(edges = eitems, n = 3, directed = FALSE)
#
#plot(grespondents)
#plot(gitems)
