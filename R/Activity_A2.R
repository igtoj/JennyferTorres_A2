library(tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)

folder<-dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <-dirname(folder)
data <- read.csv(paste0(parentFolder,"/Dataset/diabetes_012.csv"))

data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)

set.seed(27)
data_ <- data[sample(nrow(data), 3000), ]

table(data_$Sex)
table(data_$Smoker)
table(data_$CholCheck)


pairs.panels(data_ [c("Age", "BMI", "GenHlth", "Fruits")],
             pch = 21,
             bg = c("red", "green3")[unclass(data_$Diabetes_012)])

