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

# KNN Models and Experiments Diabetes#

# selection of 1500 samples of each factor of the dataset
set.seed(27)
data_stratified <- data %>% group_by(Diabetes_012) %>%
  sample_n(1500, replace = TRUE) %>% ungroup()

sample.index <- sample(1:nrow(data_stratified)
                ,nrow(data_stratified)*0.75 ,replace = F)

predictors <- c("GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income","HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost")

# data inicial
entrenamiento <- data_stratified[sample.index, c(predictors, "Diabetes_012"), drop = FALSE]
prueba<- data_stratified[-sample.index, c(predictors, "Diabetes_012"), drop = FALSE]

entrenamiento$Diabetes_012 <- factor(entrenamiento$Diabetes_012)
prueba$Diabetes_012 <- factor(prueba$Diabetes_012)

# Entrenamiento k-NN

control <- trainControl(method = "cv", p = 0.75)
knnFit <- train(Diabetes_012 ~ ., data = entrenamiento , method = "knn", trControl = control
        , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 60)
plot(knnFit)

# Crear predicciones- confusion matrix
knnPredict <- predict(knnFit, newdata = prueba)
confusionMatrix(data = knnPredict, reference = prueba$Diabetes_012)

### segundo experimento

predictors_to_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

control <- trainControl(method = "cv", number = 5)
knnFit2 <- train(Diabetes_012 ~ ., data = entrenamiento2, method = "knn", trControl = control
          , preProcess = c("range") # c("center", "scale") for z-score
          , tuneLength = 40)
plot(knnFit2)

# Crear predicciones 2 - confusion matrix 2
knnPredict2 <- predict(knnFit2, newdata = prueba2)
confusionMatrix(data = knnPredict2, reference = prueba2$Diabetes_012)

### Tercer experimento

predictors_to_remove2 <- c("PhysHlth", "Fruits","ChoclCheck", "MentHlth","Veggies")
entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove2)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove2)]

control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(Diabetes_012 ~ ., data = entrenamiento3, method = "knn", trControl = control2
          , preProcess = c("range") # c("center", "scale") for z-score
          , tuneLength = 20)
plot(knnFit3)

# Crear predicciones 3 - confusion matrix
knnPredict3 <- predict(knnFit3, newdata = prueba3)
confusionMatrix(data = knnPredict3, reference = prueba3$Diabetes_012)


#KNN Models HeartDiseaseorAttack#

# selection of 1500 samples of each factor of the dataset
set.seed(27)
data_stratified <- data %>% group_by(HeartDiseaseorAttack) %>%
  sample_n(1500, replace = TRUE) %>% ungroup()
sample.index <- sample(1:nrow(data_stratified)
  ,nrow(data_stratified)*0.75 ,replace = F)

predictors <- c("GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income","HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost")

# data inicial
entrenamiento <- data_stratified[sample.index, c(predictors, "HeartDiseaseorAttack"), drop = FALSE]
prueba<- data_stratified[-sample.index, c(predictors, "HeartDiseaseorAttack"), drop = FALSE]

entrenamiento$HeartDiseaseorAttack <- factor(entrenamiento$HeartDiseaseorAttack)
prueba$HeartDiseaseorAttack <- factor(prueba$HeartDiseaseorAttack)

# Entrenamiento k-NN

control <- trainControl(method = "cv", p = 0.75)
knnFit <- train(HeartDiseaseorAttack ~ ., data = entrenamiento , method = "knn", trControl = control
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 60)
plot(knnFit)

# Crear predicciones- confusion matrix
knnPredict <- predict(knnFit, newdata = prueba)
confusionMatrix(data = knnPredict, reference = prueba$HeartDiseaseorAttack)

### segundo experimento

predictors_to_remove <- c("AnyHealthcare", "NoDocbcCost", "DiffWalk", "Education", "Income")
entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

control <- trainControl(method = "cv", number = 5)
knnFit2 <- train(HeartDiseaseorAttack ~ ., data = entrenamiento2, method = "knn", trControl = control
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 40)
plot(knnFit2)

# Crear predicciones 2 - confusion matrix 2
knnPredict2 <- predict(knnFit2, newdata = prueba2)
confusionMatrix(data = knnPredict2, reference = prueba2$HeartDiseaseorAttack)

### Tercer experimento

predictors_to_remove2 <- c("HvyAlcoholConsump", "Fruits","ChoclCheck", "MentHlth","Veggies")
entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove2)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove2)]

control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(HeartDiseaseorAttack ~ ., data = entrenamiento3, method = "knn", trControl = control2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)
plot(knnFit3)

# Crear predicciones 3 - confusion matrix
knnPredict3 <- predict(knnFit3, newdata = prueba3)
confusionMatrix(data = knnPredict3, reference = prueba3$HeartDiseaseorAttack)


#KNN Models and Experiments to Find Sex #

# selection of 1500 samples of each factor of the dataset
set.seed(27)
data_stratified <- data %>% group_by(Sex) %>%
  sample_n(1500, replace = TRUE) %>% ungroup()
sample.index <- sample(1:nrow(data_stratified)
              ,nrow(data_stratified)*0.75 ,replace = F)

predictors <- c("GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income","HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost")

# data inicial
entrenamiento <- data_stratified[sample.index, c(predictors, "Sex"), drop = FALSE]
prueba<- data_stratified[-sample.index, c(predictors, "Sex"), drop = FALSE]

entrenamiento$Sex <- factor(entrenamiento$Sex)
prueba$Sex <- factor(prueba$Sex)

# Entrenamiento k-NN

control <- trainControl(method = "cv", p = 0.75)
knnFit <- train(Sex ~ ., data = entrenamiento , method = "knn", trControl = control
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 60)
plot(knnFit)

# Crear predicciones- confusion matrix
knnPredict <- predict(knnFit, newdata = prueba)
confusionMatrix(data = knnPredict, reference = prueba$Sex)

### segundo experimento

predictors_to_remove <- c("Age", "PhysActivity", "AnyHealthcare", "NoDocbcCost", "DiffWalk")
entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

control <- trainControl(method = "cv", number = 5)
knnFit2 <- train(Sex~ ., data = entrenamiento2, method = "knn", trControl = control
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 40)
plot(knnFit2)

# Crear predicciones 2 - confusion matrix 2
knnPredict2 <- predict(knnFit2, newdata = prueba2)
confusionMatrix(data = knnPredict2, reference = prueba2$Sex)

### Tercer experimento

predictors_to_remove2 <- c( "Fruits", "Veggies", "ChoclCheck", "MentHlth","HvyAlcoholConsump")
entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove2)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove2)]

control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit3 <- train(Sex ~ ., data = entrenamiento3, method = "knn", trControl = control2
                 , preProcess = c("range") # c("center", "scale") for z-score
                 , tuneLength = 20)
plot(knnFit3)

# Crear predicciones 3 - confusion matrix
knnPredict3 <- predict(knnFit3, newdata = prueba3)
confusionMatrix(data = knnPredict3, reference = prueba3$Sex)

### regresion lineal modelo BMI #

set.seed(27)
data_stratified2 <- data[sample(nrow(data), 3000), ]

predictors <- colnames(data_stratified2)[-5]
sample.index <- sample(1:nrow(data_stratified2),nrow(data_stratified2) * 0.75,
                replace = FALSE)


entrenamiento <- data_stratified2[sample.index, c(predictors, "BMI"), drop = FALSE]
prueba <- data_stratified2[-sample.index, c(predictors, "BMI"), drop = FALSE]

ins_model <- lm(BMI ~ ., data = entrenamiento)
summary(ins_model)
# entrenamiento del modelo
train.control <- trainControl(method = "cv", number = 10 )
model <- train(BMI ~ ., data = entrenamiento, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### second

predictors_to_remove <- c("AnyHealthcare", "CholCheck", "MentHlth", "Education", "Sex")

entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

ins_model <- lm(BMI ~ ., data = entrenamiento2)
summary(ins_model)

# entrenamiento del modelo
entrenamiento <- trainControl(method = "cv", number = 5)
model <- train(BMI ~ ., data = entrenamiento2, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### Tercero
predictors_to_remove <- c("Veggies", "HvyAlcoholConsump","Income", "Stroke", "NoDocbcCost")

entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove)]

ins_model <- lm(BMI ~ ., data = entrenamiento3)
summary(ins_model)

# entrenamiento modelo
entrenamiento <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(BMI ~ ., data = entrenamiento3, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



# Regresión lineal MentHlth #

set.seed(27)
data_stratified2 <- data[sample(nrow(data), 3000), ]

predictors <- colnames(data_stratified2)[-5]
sample.index <- sample(1:nrow(data_stratified2),nrow(data_stratified2) * 0.75,
                       replace = FALSE)

entrenamiento <- data_stratified2[sample.index, c(predictors, "MentHlth"), drop = FALSE]
prueba <- data_stratified2[-sample.index, c(predictors, "MentHlth"), drop = FALSE]

ins_model <- lm(MentHlth ~ ., data = entrenamiento)
summary(ins_model)
# entrenamiento del modelo
train.control <- trainControl(method = "cv", number = 10 )
model <- train(MentHlth ~ ., data = entrenamiento, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### second

predictors_to_remove <- c("BMI", "HeartDiseaseorAttack", "Stroke", "PhysActivity", "CholCheck")

entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

ins_model <- lm(MentHlth ~.,data = entrenamiento2)

summary(ins_model)

# entrenamiento del modelo
entrenamiento <- trainControl(method = "cv", number = 5)
model <- train(MentHlth ~ ., data = entrenamiento2, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### Tercero
predictors_to_remove <- c("Veggies", "Education","Diabetes_012", "HighBP", "HighChol")

entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove)]

ins_model <- lm(MentHlth ~ ., data = entrenamiento3)
summary(ins_model)

# entrenamiento modelo
entrenamiento <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(MentHlth ~ ., data = entrenamiento3, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


# Linear regression model PhysHlth #

set.seed(27)
data_stratified2 <- data[sample(nrow(data), 3000), ]

predictors <- colnames(data_stratified2)[-5]
sample.index <- sample(1:nrow(data_stratified2),nrow(data_stratified2) * 0.75,
                       replace = FALSE)

entrenamiento <- data_stratified2[sample.index, c(predictors, "PhysHlth"), drop = FALSE]
prueba <- data_stratified2[-sample.index, c(predictors, "PhysHlth"), drop = FALSE]

ins_model <- lm(PhysHlth ~ ., data = entrenamiento)
summary(ins_model)
# entrenamiento del modelo
train.control <- trainControl(method = "cv", number = 10 )
model <- train(PhysHlth ~ ., data = entrenamiento, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### second

predictors_to_remove <- c("Sex", "Diabetes_012", "Education", "CholCheck", "Smoker")

entrenamiento2 <- entrenamiento[, !(names(entrenamiento) %in% predictors_to_remove)]
prueba2 <- prueba[, !(names(prueba) %in% predictors_to_remove)]

ins_model <- lm(PhysHlth ~.,data = entrenamiento2)

summary(ins_model)

# entrenamiento del modelo
entrenamiento <- trainControl(method = "cv", number = 5)
model <- train(PhysHlth ~ ., data = entrenamiento2, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

#### Tercero
predictors_to_remove <- c("BMI", "HeartDiseaseorAttack", "PhysActivity", "Veggies", "Stroke")

entrenamiento3 <- entrenamiento2[, !(names(entrenamiento2) %in% predictors_to_remove)]
prueba3 <- prueba2[, !(names(prueba2) %in% predictors_to_remove)]

ins_model <- lm(PhysHlth ~ ., data = entrenamiento3)
summary(ins_model)

# entrenamiento modelo
entrenamiento <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(PhysHlth ~ ., data = entrenamiento3, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

