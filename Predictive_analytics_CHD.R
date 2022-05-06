#Installing Packages and calling library functions

install.packages("corrplot")
library(haven)
library(car)
library(corrplot)
library(psych)
library(ggplot2)
library(car)
library(gmodels)
library(caTools)
library(caret)
library(xlsx)
library(ROSE)
install.packages("randomForest")
library(randomForest)
set.seed(123)
#--------------------------------------------------------------------
#loading Data
chd_data<-read.csv("E:/UIC MASTERS BUSINESS ANALYTICS/UIC 3RD SEM/Hiranya/framingham.csv", header = TRUE,sep = ",")
head(chd_data)
View(chd_data)
nrow(chd_data)
ncol(chd_data)
sum(is.na.data.frame(chd_data))
#checking data types of all data and making it a data frame
colSums(is.na(chd_data))
data_type<-sapply(chd_data, class)

data_type <- data.frame(data_type)
#only 489 rows has missing records, since it is healthcare data we are removing these obs to make the dataset as pure as possible
chd_data_1<-chd_data[complete.cases(chd_data), ]
colSums(is.na(chd_data_1))
nrow(chd_data_1)
summary(chd_data)
-------------------------------------------------------------------------
#Converting the variables to to the required data types
View(data_type)
chd_data_1$male<-as.factor(chd_data_1$male)
chd_data_1$currentSmoker<-as.factor(chd_data_1$currentSmoker)
chd_data_1$BPMeds<- as.factor(chd_data_1$BPMeds)
chd_data_1$prevalentStroke<- as.factor(chd_data_1$prevalentStroke)
chd_data_1$prevalentHyp<- as.factor(chd_data_1$prevalentHyp)
chd_data_1$diabetes<- as.factor(chd_data_1$diabetes)
chd_data_1$TenYearCHD<- as.factor(chd_data_1$TenYearCHD)

data_type_1<-sapply(chd_data_1, class)

data_type_1 <- data.frame(data_type_1)
View(data_type_1)
#Exploratory Data Analysis ---------------------------------------------------

#correlation
colnames(chd_data_1)
correlation_table1 <- chd_data_1[c(11,12,13)]
corr_result <- cor(correlation_table1,use = "complete.obs")
corrplot(corr_result,method="number")
View(corr_result)

#Data manipulation
summary(chd_data_1$age)
chisq.test(chd_data_1$age,chd_data_1$TenYearCHD, correct = FALSE)
table(chd_data_1$TenYearCHD)
table(chd_data_1$male)
table(chd_data_1$currentSmoker)
table(chd_data_1$prevalentStroke)
table(chd_data_1$prevalentHyp)
table(chd_data_1$BPMeds)
chisq.test(chd_data_1$age,chd_data_1$TenYearCHD, correct = FALSE)
chisq.test(chd_data_1$BPMeds, chd_data_1$TenYearCHD, correct = FALSE)
chisq.test(chd_data_1$currentSmoker,chd_data_1$TenYearCHD, correct = FALSE)
chisq.test(chd_data_1$prevalentHyp, chd_data_1$TenYearCHD, correct = FALSE)
chisq.test(chd_data_1$male, chd_data_1$TenYearCHD, correct = FALSE)

#creating new variables

chd_data_1$high_chol <- NA
chd_data_1$high_chol[chd_data_1$totChol >= 240 ] <- 1
chd_data_1$high_chol[chd_data_1$totChol < 240 ] <- 0
View(chd_data_1)

#-------------

chd_data_1$Obese <- NA
chd_data_1$Obese[chd_data_1$BMI >= 30 ] <- 1
chd_data_1$Obese[chd_data_1$BMI < 30 ] <- 0
View(chd_data_1)

#------------------------

View(chd_data_1)
#Chi SQuare Test
chd_data_1$Obese <- as.factor(chd_data_1$Obsese)
chd_data_1$high_chol <- as.factor(chd_data_1$high_chol)
chisq.test(chd_data_1$high_chol, chd_data_1$TenYearCHD, correct = FALSE)
chisq.test(chd_data_1$Obese, chd_data_1$TenYearCHD, correct = FALSE)
#cross table analysis
CrossTable(chd_data_1$male,chd_data_1$TenYearCHD)

#Data modelling with the same data
#splitting the data into training and testing
sample = sample.split(chd_data_1, SplitRatio = 0.85)
data_train = subset(chd_data_1, sample == TRUE)
data_test  = subset(chd_data_1, sample == FALSE)
nrow(data_train)
nrow(data_test)
table(data_train$TenYearCHD)

#logistic regression model
colnames(chd_data_1)
attach(chd_data_1)
logistic_model <- glm(TenYearCHD ~ male+age+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+sysBP+heartRate+high_chol+Obese,family=binomial(link='logit'),data=data_train)
summary(logistic_model)

vif(logistic_model)
Odds_ratio<-exp(coef(logistic_model))

Odds_ratio <- data.frame(Odds_ratio)
View(Odds_ratio)
#---Predicttion-----------------
predict_result <- predict(logistic_model,data_test,type = 'response')
View(predict_result)
fitted.results <- ifelse(predict_result > 0.45,1,0)
data_test$TenYearCHD_1<- fitted.results
View(data_test)
table(data_test$TenYearCHD_1)
misClasificError <- mean(fitted.results != chd_data_1$TenYearCHD)
print(paste('Accuracy',1-misClasificError))

data_test$TenYearCHD_1 <- as.factor(data_test$TenYearCHD_1)
confusionMatrix(data_test$TenYearCHD_1, data_test$TenYearCHD)
View(fitted.results)



#-------------------------------------------------------------------------
  #Using ROSE FUNCTION:

training_rose<-ovun.sample(TenYearCHD ~., data = data_train, method="both", N=3047)$data
nrow(training_rose)
table(training_rose$TenYearCHD)
logistic_model_rose <- glm(TenYearCHD ~ male+age+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+sysBP+heartRate+high_chol+Obese,family=binomial(link='logit'),data=training_rose)
summary(logistic_model_rose)

vif(logistic_model_rose)
exp(coef(logistic_model_rose))
#-----------------------------------------------------------------------
#Prediction
predict_result_rose <- predict(logistic_model_rose,data_test,type = 'response')
View(predict_result_rose)
fitted.results_rose <- ifelse(predict_result_rose > 0.45,1,0)
data_test$TenYearCHD_1<- fitted.results_rose
View(data_test)
table(data_test$TenYearCHD_1)
misClasificError <- mean(fitted.results != chd_data_1$TenYearCHD)
print(paste('Accuracy',1-misClasificError))
data_test$TenYearCHD_1 <- as.factor(data_test$TenYearCHD_1)
confusionMatrix(data_test$TenYearCHD_1, data_test$TenYearCHD)
data_test$TenYearCHD_1 <- as.factor(data_test$TenYearCHD_1)
View(fitted.results)

#-----------------------------------------------------------------------------------------------
#RandomForest

random_forest_model <- randomForest(data_train$TenYearCHD ~ male+age+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+sysBP+heartRate+high_chol+Obese, data = data_train, ntree = 800, mtry = 2, importance = TRUE)
random_forest_model

#prediction
# Predicting on Validation set
predValid <- predict(random_forest_model, data_test, type = "class")
# Checking classification accuracy
mean(predValid == data_test$TenYearCHD)
importance(random_forest_model)

