library(naniar)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mvShapiroTest)
library(caret)
library(MASS)
library(caTools)
library(class)
library(kknn)
library(ggplot2)
library(GGally)

#Db
db <- read.csv("Data/heart.csv", header = TRUE)
db$Sex <- as.factor(db$Sex)
db$ChestPainType <- as.factor(db$ChestPainType)
db$RestingECG <- as.factor(db$RestingECG)
db$ExerciseAngina <-as.factor(db$ExerciseAngina)
db$ST_Slope <- as.factor(db$ST_Slope)
db$HeartDisease <- as.factor(db$HeartDisease)
db$FastingBS <- as.factor(db$FastingBS)

db_num <- db %>% dplyr::select(., Age, RestingBP, Cholesterol, MaxHR, Oldpeak)

str(db)

#NA Analysis
vis_miss(db)

#EDA
sick <- db %>% filter(., HeartDisease == 1)
no_sick <- db %>% filter(., HeartDisease == 0)


query <- db %>% select(HeartDisease) %>% table %>% as.data.frame()
ggplot(query, aes(x=HeartDisease, y=Freq)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(label = Freq), vjust = -0.5, size = 3)+
  labs(title = "Diagrama de Barras - Insuficiencia cardiaca ", y="Numero de personas")+
  ylim(0, 550) +
  theme_bw()

query <- query %>% mutate(Percent = Freq / sum(Freq) * 100)
ggplot(query, aes(x = HeartDisease, y = round(Percent, 2))) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percent, 2), "%")), vjust = -0.5, size = 3) +
  labs(title = "Diagrama de Barras - Insuficiencia cardiaca", y="Porcentaje de personas")+
  ylim(0, 65) +
  theme_bw()


query <- db %>% select(HeartDisease, Sex) %>% group_by(HeartDisease, Sex) %>% count()
ggplot(query, aes(x = Sex, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=n),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Insuficiencia cardiaca por genero", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  theme_bw()

query <- query %>% mutate(Percent = n / sum(n) * 100)
ggplot(query, aes(x = Sex, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=scales::percent(n/sum(n))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Insuficiencia cardiaca por genero", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  theme_bw()


query <- db %>% select(HeartDisease, RestingECG) %>% group_by(HeartDisease, RestingECG) %>% count()
ggplot(query, aes(x = RestingECG, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=n),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Resultado electrocardiograma en reposo", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  theme_bw()

query <- query %>% mutate(Percent = n / sum(n) * 100)
ggplot(query, aes(x = RestingECG, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=scales::percent(n/sum(n))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Resultado electrocardiograma en reposo", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  theme_bw()


query <- db %>% select(HeartDisease, ChestPainType) %>% group_by(HeartDisease, ChestPainType) %>% count()
names <- c("Asintomático", "Angina atípica", "Dolor no anginoso", "Angina típica")
ggplot(query, aes(x = ChestPainType, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=n),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Tipo de dolor en el pecho", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  scale_x_discrete(labels=names) +
  theme_bw()

query <- query %>% mutate(Percent = n / sum(n) * 100)
ggplot(query, aes(x = ChestPainType, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=scales::percent(n/sum(n))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - Tipo de dolor en el pecho", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  scale_x_discrete(labels=names) +
  theme_bw()



query <- db %>% select(HeartDisease, FastingBS) %>% group_by(HeartDisease, FastingBS) %>% count()
names <- c("<= 120 mg/dl", "> 120 mg/dl")
ggplot(query, aes(x = FastingBS, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=n),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - azúcar en la sangre en ayunas", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  scale_x_discrete(labels=names) +
  theme_bw()

query <- query %>% mutate(Percent = n / sum(n) * 100)
ggplot(query, aes(x = FastingBS, y = n, fill=as.factor(HeartDisease))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=scales::percent(n/sum(n))),position=position_dodge(0.90), vjust=-0.3, size=3)+
  labs(title = "Diagrama de Barras - azúcar en la sangre en ayunas", y="Numero de personas")+
  scale_fill_brewer(name = "Insuficiencia cardiaca", 
                    labels=c("No", "Si"))+
  scale_x_discrete(labels=names) +
  theme_bw()




ggplot2::theme_set(ggplot2::theme_bw())
ggpairs(db_num, legend=c(1,1),mapping=aes(colour=db$HeartDisease,alpha=0.1))+
  theme(legend.position="bottom")


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
plot(model_lda)


lda_pred <- predict(object=model_lda, newdata=test_data)
table(test_data$HeartDisease, lda_pred$class, dnn=c("Real", "Predicha"))

#Qda
model_qda <- qda(HeartDisease ~ ., data = train_data)
plot(model_qda)

summary(model_qda)

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

#The best K
train.kknn(train_data$HeartDisease ~ ., train_data, kmax = 20)


model_knn <- kknn(HeartDisease ~ ., train_data, test_data, k = 20)

knn_pred <- fitted(model_knn)
table(test_data$HeartDisease, knn_pred, dnn=c("Real", "Predicha"))




