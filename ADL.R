title: "Activities of Daily Living (ADLs)"
author: "Tilen Rupar"
date: "16 6 2021"

#Working directory
setwd("E:/R/ADL")
library(tidyverse)
library(readr)
library(caret)
library(ggplot2)
library(keras)

## Project goals
# I will try to predict human activity based on sensory data with machine learning algorithm.

# The machine learning algorithm is a supervised time series classification model. It uses POSIXct class to classify human activity, based on the time of the day. 
# Data: https://archive.ics.uci.edu/ml/datasets/Activities+of+Daily+Living+(ADLs)+Recognition+Using+Binary+Sensors

###Data import
ADL.uci_file <- download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/00271/UCI ADL Binary Dataset.zip', destfile = "./ADL.zip")
unzip("./ADL.zip")
setwd("E:/R/ADL/UCI ADL Binary Dataset")

# Activities of Daily Living (ADLs) Recognition Using Binary Sensors Data Set (User "A")
ADL <- read_tsv("OrdonezA_ADLs.txt", skip = 2, col_names = F, trim_ws = T, skip_empty_rows = T, )[,c(1,3,5)] 
colnames(ADL) <- colnames(read_tsv("OrdonezA_ADLs.txt")[,1:3])

#Editing the dependent variable as a classification object: Character vector to factor with 9 levels
ADL$Activity <- as.factor(ADL$Activity)
str(ADL)
ggplot(ADL, aes(x=Activity)) +
  geom_bar()

# Data partitioning - 0.8 data goes into training
set.seed(100)
sample_rows <- createDataPartition(ADL$Activity, p=0.8, list = F)
ADL_train <- ADL[sample_rows,]
ADL_test <- ADL[-sample_rows,]

modelFit <- train(Activity ~ ., method="rf", data=ADL_train,
                  ntree=500, tuneGrid=data.frame(.mtry = 3))

predictedADL <- predict(modelFit, ADL_test)
confusionMatrix(ADL_test$Activity, predictedADL)
