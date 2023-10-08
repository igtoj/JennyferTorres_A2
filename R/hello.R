library(tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)

folder<-dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <-dirname(folder)
data <- read.csv(paste0(parentFolder,"/Dataset/diabetes_012.csv"))

data$Diabetes_012 <- ifelse(data$Diabetes_012 == 0, 0, 1)

