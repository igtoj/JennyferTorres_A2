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

# KNN Models and Experiments#

## selection of 1500 samples of each factor of the dataset
set.seed(27)
data_stratified <- data %>% group_by(Diabetes_012) %>%
  sample_n(1500, replace = TRUE) %>% ungroup()

sample.index <- sample(1:nrow(data_stratified)
                ,nrow(data_stratified)*0.7 ,replace = F)

predictors <- c("GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income","HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost")

# data inicial
entrenamiento <- data_stratified[sample.index, c(predictors, "Diabetes_012"), drop = FALSE]
prueba<- data_stratified[-sample.index, c(predictors, "Diabetes_012"), drop = FALSE]


entrenamiento$Diabetes_012 <- factor(entrenamiento$Diabetes_012)
prueba$Diabetes_012 <- factor(prueba$Diabetes_012)
