
setwd("C:/Users/djl/Desktop/ML Project")

train <- read.csv(file = "pml-training.csv", na.strings=c("", "NA", "NULL"))
test<- read.csv(file = "pml-testing.csv", na.strings=c("", "NA", "NULL"))

#The training data has 19622 observations and 160 features, and the distribution of the five measured stances A,B,C,D,E is:
dim(train)
table(train$classe)

#Partitioning the training set
  
#We separate our training data into a training set and a validation set so that we can validate our model.

library(caret)
set.seed(12345)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]

#Feature selection**
  
# Excluding near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

cntlength <- sapply(Training, function(x) {
  sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]

#Model Training: Random forest.
library(randomForest)
rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)

#Model Validation**
ptraining <- predict(rfModel, Training)
print(confusionMatrix(ptraining, Training$classe))
pvalidation <- predict(rfModel, Validation)
print(confusionMatrix(pvalidation, Validation$classe))

#Test Set Prediction**
ptest <- predict(rfModel, test)
ptest
answers <- as.vector(ptest)

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)

library(knitr)
library(markdown)
knit("Machine Learning Project.Rmd")    
markdownToHTML("Machine Learning Project.md", output="Machine Learning Project.html")

