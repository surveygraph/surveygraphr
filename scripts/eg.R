library("surveygraph")

df <- data.frame(runif(30), runif(30))
names(df) <- c("a", "b")

ToySurveyArgDF(df)
