library(tidyverse)
library(ggplot2)
library(statisticalModeling)
library(rpart)

dataset <- read.csv("dataset.csv")

list <- sapply(dataset, typeof)
list[1]
sapple(dataset, 2)


col_names <- colnames(dataset)
print(col_names)



for (i in 3:ncol(dataset)) {
  print(i)
  if ()
  cor(dataset[2], dataset[i])
}
