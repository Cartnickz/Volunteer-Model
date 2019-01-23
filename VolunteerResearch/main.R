#devtools::install_github("dtkaplan/statisticalModeling")
#install.packages("stringi")
#install.packages("caTools")
#install.packages("tidyverse")
#install.packages("devtools")
#install.packages("colorspace")
#install.packages("caret")
#install.packages("e1071")
library(colorspace)
library(tidyverse)
library(ggplot2)
library(statisticalModeling)
library(caret)
library(caTools)

set.seed(101)
# import dataset
dataset <- read.csv("dataset.csv")
dataset$school = NULL

# write all column names to a list
col_names <- colnames(dataset)

# run correlation between all columns with type "integer" and volunteer column
name_col <- NULL 
cor_col <- NULL
for (name in col_names) {
  if (class(dataset[, name]) == "integer") {
    name_col <- c(name_col, name)
    cor_col <- c(cor_col, cor(dataset$volunteer, dataset[, name]))
  }
}

# order variables from greatest coorelation to smallest correlation
sort_num <- NULL 
sort_name <- NULL 

sort_name <- name_col
sort_num <- cor_col
n <- length(sort_num)

print(n)

for(k in n:2) {
  i <- 1
  while (i < k) {
    if (abs(sort_num[i]) < abs(sort_num[i + 1])) {
      temp_num <- sort_num[i + 1]
      sort_num[i + 1] <- sort_num[i]
      sort_num[i] <- temp_num
      
      temp_name <- sort_name[i + 1]
      sort_name[i + 1] <- sort_name[i]
      sort_name[i] <- temp_name
    }
    i <- i + 1
  }
}

# shuffle dataset and split 70:30
rows <- sample(nrow(dataset))
dataset <- dataset[rows, ]

split <- round(nrow(dataset) * 0.70)
train <- dataset[1:split, ]
test <- dataset[(split+1):nrow(dataset), ]

train$volunteer <- as.factor(train$volunteer)

# run through all combinations of models with significant variables
form_list <- NULL
result_list <- NULL

sig_name <- sort_name[2:9] # chooses top 8 variables
for (i in 1:length(sig_name)) {
  com <- combn(sig_name, i)
  for (j in 1:ncol(com)) {
    form <- as.formula(paste("volunteer ~", paste(com[,j], collapse = "+")))
    form_list <- c(form_list, form)
    
    model <- train(form, data = train, method = "glm", family = "binomial")
    p <- predict(model, test, type = "prob")

    result <- colAUC(p, test$volunteer, plotROC = FALSE)
    result_list <- c(result_list, result[1])
    
    cat("Model ", j, " of ", ncol(com), " Completed!\n" )
  }
}

# order models from greatest AUC to lowest AUC
sort_form <- form_list
sort_result <- result_list

n <- length(sort_form)
for(k in n:2) {
  i <- 1
  while (i < k) {
    if (sort_result[i] < sort_result[i + 1]) {
      temp_result <- sort_result[i + 1]
      sort_result[i + 1] <- sort_result[i]
      sort_result[i] <- temp_result
      
      temp_form <- sort_form[i + 1]
      sort_form[i + 1] <- sort_form[i]
      sort_form[i] <- temp_form
    }
    i <- i + 1
  }
}

print(sort_result[1:10])
print(sort_form[1:10])

# use model on test data
testset <- read.csv("testset.csv")
summary(testset)

model_final <- train(sort_form[[1]], data = train, method = "glm", family = "binomial")  # create final model

p_final <- predict(model_final, testset, type = "prob")  # create ROC curve
result_ROC <- colAUC(p_final, testset[["volunteer"]], plotROC = TRUE)
print(result_ROC)

p_final <- predict(model_final, testset, type = "raw")  # predict and produce actual predictions

test_con <- ifelse(testset[["volunteer"]] == 1, "1", "0")
test_class <- factor(test_con, levels = c("1","0"))
confusionMatrix(p_final, test_class)




