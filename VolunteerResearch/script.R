library(ggplot2)
library(statisticalModeling)
library(rpart)

dataset <- read_csv("dataset.csv")
model <- lm(volunteer ~ friends + leadership + resume, data = dataset)

sample <- data.frame(friends = 1, leadership = 1, resume = 0)

r1 <- predict(sample, model)


