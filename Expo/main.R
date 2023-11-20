library(naniar)
library(tidyverse)
library(magrittr)
library(mvShapiroTest)
library(caret)
library(MASS)
library(caTools)
library(class)
library(kknn)

#Db
db <- read.csv("Data/heart.csv", header = TRUE)
db$Sex <- as.factor(db$Sex)
db$ChestPainType <- as.factor(db$ChestPainType)
db$RestingECG <- as.factor(db$RestingECG)
db$ExerciseAngina <-as.factor(db$ExerciseAngina)
db$ST_Slope <- as.factor(db$ST_Slope)
db$HeartDisease <- as.factor(db$HeartDisease)
db$FastingBS <- as.factor(db$FastingBS)

db_num <- db %>% select(., Age, RestingBP, Cholesterol, MaxHR, Oldpeak)

str(db)

#NA Analysis
vis_miss(db)

#EDA
sick <- db %>% filter(., HeartDisease == 1)
no_sick <- db %>% filter(., HeartDisease == 0)



ggplot(db, aes(x = Age, y = MaxHR, color = HeartDisease)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión Segmentado por Población", 
       x = "Variable X", y = "Variable Y") +
  theme_minimal()


sick_num <- sick %>% select(., Age, RestingBP, Cholesterol, MaxHR, Oldpeak)
sick_factor <- sick %>% select(!any_of(c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")))

no_sick_num <- no_sick %>% select(., Age, RestingBP, Cholesterol, MaxHR, Oldpeak)
no_sick_factor <- no_sick %>% select(!any_of(c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")))


colMeans(sick_num)
colMeans(no_sick_num)

cov(sick_num)
cov(no_sick_num)

library(mvShapiroTest)
#ho = normal multivariada, se rechaza h0
mvShapiro.Test(as.matrix(db_num))


library(heplots)
#cov2 != cov2 se rechaza h0
heplots::boxM(db_num, db$HeartDisease)

set.seed(123)
indices_train <- createDataPartition(db$HeartDisease, p = 0.7, list = FALSE)
train_data <- db[indices_train, ]
test_data <- db[-indices_train, ]

#LDA
model_lda <- lda(HeartDisease ~ ., data = train_data)
summary(model_lda)
plot(model_lda)


lda_pred <- predict(object=model_lda, newdata=test_data)
table(test_data$HeartDisease, lda_pred$class, dnn=c("Real", "Predicha"))

#Qda
model_qda <- qda(HeartDisease ~ ., data = train_data)
summary(model_qda)
plot(model_qda)


qda_pred <- predict(object=model_qda, newdata=test_data)
table(test_data$HeartDisease, qda_pred$class, dnn=c("Real", "Predicha"))

#logistic
model_logistic <- glm(HeartDisease ~ ., family=binomial, data=train_data)
summary(model_logistic)
logistic_pred <- predict(object=model_logistic, type='response', newdata=test_data)
pred_valid <- ifelse(logistic_pred > 0.5, 1, 0)
table(test_data$HeartDisease, pred_valid, dnn=c("Real", "Predicha"))

#Knn
#pred_knn <- knn(train_data[, 1:11], test_data[, 1:11], cl=train_data$HeartDisease, k = 2, prob = TRUE)
#table(test_data$HeartDisease, pred_knn)
model_knn <- kknn(HeartDisease ~ ., train_data, test_data, k = 2)
knn_pred <- fitted(model_knn)
print(knn_pred)
table(test_data$HeartDisease, knn_pred, dnn=c("Real", "Predicha"))
