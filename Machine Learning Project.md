---
title: "Practical Machine Learning Course Project"
author: "Godwin Appiah Assumaning"
date: "February 28, 2016"
output: 
  html_document: 
---

https://github.com/godwinass/Machine-Learning-Project_Coursera

**Introduction**

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here.

**Data**

The data for this course project is given as: http://groupware.les.inf.puc-rio.br/har. The training data and the test data are downloaded here.


```r
setwd("C:/Users/djl/Desktop/ML Project")

train <- read.csv(file = "pml-training.csv", na.strings=c("", "NA", "NULL"))
test<- read.csv(file = "pml-testing.csv", na.strings=c("", "NA", "NULL"))
```

The training data has 19622 observations and 160 features, and the distribution of the five measured stances A,B,C,D,E is:


```r
dim(train)
```

```
## [1] 19622   160
```


```r
table(train$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

**Preprocessing**
*Partitioning the training set*

ceating training set and validation set so validate our model.


```r
library(caret)
```


```r
set.seed(12345)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]
```

**Feature selection**

Cleaning up the near zero variance features, columns with missing values and descriptive fields.


```r
# exclude near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

# exclude columns with 40% or more missing values exclude descriptive
# columns like name 
cntlength <- sapply(Training, function(x) {
    sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
    "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]
```

**Model Training**

We apply random forest as our model as implemented in the randomForest package.


```r
library(randomForest)

rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)
```

**Model Validation**

Testing our model performance on the training set and the cross validation set.

*Training set accuracy*


```r
ptraining <- predict(rfModel, Training)
print(confusionMatrix(ptraining, Training$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4464    0    0    0    0
##          B    0 3038    0    0    0
##          C    0    0 2738    0    0
##          D    0    0    0 2573    0
##          E    0    0    0    0 2886
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9998, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

The model performs better against the training set, but we need to cross validate the performance against the hold-out set and check overfitting.

**Validation set accuracy (Out-of-Sample)**


```r
pvalidation <- predict(rfModel, Validation)
print(confusionMatrix(pvalidation, Validation$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1116    4    0    0    0
##          B    0  755    8    0    0
##          C    0    0  676   11    0
##          D    0    0    0  632    1
##          E    0    0    0    0  720
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9939          
##                  95% CI : (0.9909, 0.9961)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9923          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9947   0.9883   0.9829   0.9986
## Specificity            0.9986   0.9975   0.9966   0.9997   1.0000
## Pos Pred Value         0.9964   0.9895   0.9840   0.9984   1.0000
## Neg Pred Value         1.0000   0.9987   0.9975   0.9967   0.9997
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1925   0.1723   0.1611   0.1835
## Detection Prevalence   0.2855   0.1945   0.1751   0.1614   0.1835
## Balanced Accuracy      0.9993   0.9961   0.9925   0.9913   0.9993
```

The cross validation accuracy is 99.5% and the out-of-sample error is therefore 0.5% so our model performs better.

**Test Set Prediction**

The prediction of our algorithm for the test set is:


```r
ptest <- predict(rfModel, test)
ptest
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

We then save the output to files according to instructions.


```r
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
```







