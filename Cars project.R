#################################Load Data, libraries #############################
#load libraries
library(ggplot2)
library(ggpubr)
library(caret)
#install.packages("mlr")
library(mlr)
#install.packages("fastmatch")
library(fastmatch)
library(DataExplorer)
#install.packages("esquisse")
library(esquisse)
library(caTools)
library(corrplot)
library(vif)
library(car)  
#install.packages("FSelector",dependencies = TRUE)
#install.packages("rJava")
library(rattle)
library(rpart.plot)

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_251')

library(rJava)
library(FSelector)
library(nFactors)
library(psych)
library(scales)
library(DMwR) #for SMOTE
library(tidyverse)
library(readxl)
library(class) #classification- KNN
#install.packages("DataExplorer")
library(DataExplorer) 
#install.packages("reshape2")
library(reshape2)
library(corrplot)
#install.packages("mctest")
library(mctest)
#install.packages("caTools") #splitting
library(caTools)
library(rms)
library(caret) #classification and regression traininig package
library(MASS) #LDA- modern applied statistics with s+
#install.packages("klaR")
library(klaR) #naive bayes
library(pROC)
library(ineq)
library(ROCR)
library(ipred)
library(rpart)
library(gbm)
#install.packages("modelStudio")
#library(modelStudio)

#rm(list=ls()) #remove all objects

#load data
setwd("/Users/Vicky/Desktop/PGP-BABI-vikas/Machine Learning")
getwd()
data=read.csv("cars.csv",na.strings = c(" ","NA","?",""),stringsAsFactors = TRUE)
head(data)
str(data)
sapply(data,function(x)length(unique(x)))
data$Engineer=as.factor(data$Engineer)
data$MBA=as.factor(data$MBA)
data$license=as.factor(data$license)
str(data)
summary(data)
which(is.na(data),arr.ind = TRUE)
#########################################EDA#######################
summary(data$Gender)
table(data$Transport)
(table(data$Transport)/nrow(data))*100
boxplot(data$Salary~data$Transport)
par(mar=c(3,2,2,1))
boxplot(data$Age~data$Transport)
boxplot(data$Work.Exp~data$Transport,col="cyan")
boxplot(data$Distance~data$Transport,col="cyan")
table(data$Gender,data$Transport)
prop.table(table(data$Transport,data$Gender),2)*100
table(data$Transport,data$Engineer)


summarizeColumns(data)
class(data)
#missing value imputation using KNN
data1 = knnImputation(data)

summarizeColumns(data1)

create_report(data1)

# missing value imputation using mean
data2=data
data2$Age[c(21,22,23)]=NA
data2$Age
summarizeColumns(data2)
meandata=round(mean(data2$Age,na.rm=TRUE),0)
data2$Age[which(is.na(data2$Age))]=meandata

pairs(~Age+Salary, data=data)
scatterplot(data$Age,data$Salary,data=data)

# plotting data.- Intutively using esquisse

esquisser(
  data = data1[-10],
  coerce_vars = getOption(x = "esquisse.coerceVars", default = TRUE),
  disable_filters = getOption(x = "esquisse.disable_filters", default = FALSE),
  viewer = getOption(x = "esquisse.viewer", default = "dialog")
)

#ggplot2- Plot = data + Aesthetics + Geometry
ggplot(data1) +
 aes(x = Transport, y = Salary, fill = Gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

ggplot(data1) +
 aes(x = Transport, y = Age, fill = Gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

ggplot(data1) +
 aes(x = Transport, y = Salary, fill = Gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

ggplot(data1) +
 aes(x = Transport, y = Distance, fill = Gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

ggplot(data1) +
 aes(x = Transport, y = Age, fill = license) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

ggdensity(data1, x = "Age",
          add = "mean", rug = TRUE,             # Add mean line and marginal rugs
          color = "Transport", fill = "Transport",  # Color by groups
          palette = "jco")                      # use jco journal color palette

qplot(x=Transport,y=Distance,data = data1,geom="boxplot")


#################outliers treatment###################

detect_outliers <- function(x) {
  print(summary(x))
  outlier_values <- boxplot.stats(x)$out  # outlier values
  return(outlier_values)
}
detect_outliers(data1$Age)
detect_outliers(data1$Work.Exp)
detect_outliers(data1$Salary)
##Outlier Treatment
attach(data1)
outlier_norm <- function(x){
  qntile <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- caps[1]
  x[x > (qntile[2] + H)] <- caps[2]
  return(x)
}
data1$Age=outlier_norm(data1$Age)
data1$Work.Exp=outlier_norm(data1$Work.Exp)
data1$Salary=outlier_norm(data1$Salary)
data1$Distance=outlier_norm(data1$Distance)

boxplot(data1$Age)
boxplot(data1$Work.Exp)
boxplot(data1$Salary)
summarizeColumns(data1)

boxplot(data1$Age, data1$Work.Exp, data1$Distance, data1$Salary,
           main = "Checking for Outliers",col="cyan")

################# outliers methods -udemy #####################

###Capping and flooring

quantile(data1$Age,0.95)
uv = quantile(data1$Age,0.95)
data1$Age[data1$Age>uv]=uv
boxplot(data1$Age)$out     


################### correlation treatment #############
cordata=cor(data1[,c(1,5,6,7)])
corrplot(cordata)
data.treated=data1[,-c(5,6)]
cor(data.treated[,c(1,5)])
create_report(data.treated)


############# split data #############
#Converting 3 levels in target variable to 2 levels 1- car; 2- 2wheeler+public transport
data.treated$Transport = ifelse(data.treated$Transport=="Car",1,0)
table(data.treated$Transport)
61/(383+61)*100
#13.73 % of minority class- car in target variable
#0   1 
#383  61 
set.seed(123)
sample=sample.split(data.treated$Transport,0.75) #stratified sampling
train.data=subset(data.treated,sample==T)
test.data=subset(data.treated,sample==F)
train.data$Transport=as.factor(train.data$Transport)
test.data$Transport=as.factor(test.data$Transport)

table(train.data$Transport)
46/(287+46)*100 #13.81%
table(test.data$Transport)
15/(15+96)*100 #13.51%
#data is propotionally split among train and test sets

####################################SMOTE####################################

str(train.data)
bal.train.data <- SMOTE(Transport ~., train.data, perc.over = 1000, k = 5, perc.under = 150)
table(bal.train.data$Transport)
506/(690+506)*100 #42% of minority class done by SMOTE

#perc.over= (1000/100=10) (46*10=460)+ 46 =506 cases
#perc.under = (150/100=1.5) 460*1.5=690


######################### Logistic regression ##########################

logistic = glm(Transport ~., data = bal.train.data, family = binomial)
summary(logistic)
plot(logistic)

#stepwise logistic regression
set.seed(123)
Logistic.stepwise = train(bal.train.data$Transport~.,bal.train.data,method = "glmStepAIC",
                          trControl = trainControl(method = "cv", number = 10),
                          tuneLength = 20)
summary(Logistic.stepwise)

vif(logistic)

# Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2


## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
rsquare=(ll.null - ll.proposed) / ll.null
rsquare
## The p-value for the R^2
p= 1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

# now we can plot the data
predicted.data <- data.frame(
  probability=logistic$fitted.values,
  Cartransport=bal.train.data$Transport)

predicted.data <- predicted.data[
  order(predicted.data$probability, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

plot(predicted.data)
ggplot(data=predicted.data, aes(x=rank, y=probability)) +
  geom_point(aes(color=Cartransport), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of car as transport")

predictedY <- predict(logistic, test.data, type="response") 
predictedY=as.data.frame(predictedY)

#Logistic regression model performance

plot(bal.train.data$Transport,logistic$fitted.values,main="Car Vs Probabilities")

#Roc curve and AUC
roccurve=roc(bal.train.data$Transport,logistic$fitted.values)
plot.roc(roccurve)
roccurve

#KS
pred.train=prediction(logistic$fitted.values,bal.train.data$Transport)
perf.train=performance(pred.train,"tpr","fpr")
KS.train = max(perf.train@y.values[[1]]-perf.train@x.values[[1]])
round(KS.train,2)
summary(KS.train)
plot(perf.train@y.values[[1]]-perf.train@x.values[[1]],ylab="probability",main="KS-Train")
#GINI
gini.train=ineq(logistic$fitted.values,type="Gini")
round(gini.train,2)

#calibrating probability cut-offs
status.predicted=ifelse(logistic$fitted.values<0.8,"0","1")
status.predicted=as.factor(status.predicted)
ct.log=table(bal.train.data$Transport,status.predicted)
class(bal.train.data$Transport)

##confusion matrix
ct.log
confusionMatrix(ct.log)


#### Logistic regression - Test data

test.data.lr=test.data
test.data.lr$response=predict(logistic,newdata=test.data.lr,type="response")



logistic.test=glm(test.data$Transport~.,data=test.data,family="binomial")
logistic.test
summary(logistic.test)
#Roc curve and AUC -test
roccurve.test=roc(test.data$Transport,logistic.test$fitted.values)
plot.roc(roccurve.test,main="ROC-test-logistic regression")
roccurve.test

#KS
pred.test=prediction(logistic.test$fitted.values,test.data$Transport)
perf.test=performance(pred.test,"tpr","fpr")
KS.test = max(perf.test@y.values[[1]]-perf.test@x.values[[1]])

round(KS.test,2)
summary(KS.test)
plot(perf.train@y.values[[1]]-perf.train@x.values[[1]],ylab="probability",main="KS-Test")

#GINI
gini.test=ineq(logistic.test$fitted.values,type="Gini")
round(gini.test,2)

#calibrating probability cut-offs
status.predicted.test=ifelse(logistic.test$fitted.values<0.8,"0","1")
status.predicted.test=as.factor(status.predicted.test)
ct.log.test=table(test.data$Transport,status.predicted.test)

##confusion matrix
ct.log.test
confusionMatrix(ct.log.test)

################################### K N N ####################################
#KNN-preprocessing data ; knn needs normalising as it depends on distance measures
norm.train.data=scale(bal.train.data[,-c(2,3,4,6,7)]) #removing factor/character variables
norm.test.data=scale(test.data[,-c(2,3,4,6,7)])
norm.train.data=cbind(norm.train.data,bal.train.data[,c(2,3,4,6,7)])#readding the removed variables to scaled data
norm.test.data=cbind(norm.test.data,test.data[,c(2,3,4,6,7)])
summary(norm.train.data)
summary(norm.test.data)
#converting all variables to numeric-- whenever algorithms which use distance based methods are involved we need to convert all factor variables to numeric.
norm.train.data$Gender=as.numeric(norm.train.data$Gender)
norm.train.data$Engineer=as.numeric(norm.train.data$Engineer)
norm.train.data$MBA=as.numeric(norm.train.data$MBA)
norm.train.data$license=as.numeric(norm.train.data$license)
norm.train.data$Transport=as.numeric(norm.train.data$Transport)

norm.test.data$Gender=as.numeric(norm.test.data$Gender)
norm.test.data$Engineer=as.numeric(norm.test.data$Engineer)
norm.test.data$MBA=as.numeric(norm.test.data$MBA)
norm.test.data$license=as.numeric(norm.test.data$license)
norm.test.data$Transport=as.numeric(norm.test.data$Transport)



set.seed(123)
knnstatus=knn(norm.train.data,norm.test.data,norm.train.data$Transport,k=5)# test data is classified based on k N N in train data
knnstatus
summary(knnstatus)
plot(knnstatus)
table(norm.test.data$Transport)
ct=table(norm.test.data$Transport,knnstatus) #actual test value is compared against the Predicted value
ct
confusionMatrix(ct)
#knn - tuning K value

set.seed(123)
library(caret)
# train function used to evaluate, using resampling, the effect of model tuning parameters on performance
knnmodel = caret::train(Transport ~., data= norm.train.data, method = "knn",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneLength = 20)
knnmodel
plot(knnmodel)
k=knnmodel$bestTune
k
#KNN model performance

#confusion matrix
knn.table=table(norm.test.data$Transport,knnstatus)
knn.table
confusionMatrix(as.factor(norm.test.data$Transport),knnstatus)


############################## Naive Bayes ################################
names(bal.train.data)
###train data
nbmodel=NaiveBayes(Transport~.,data=bal.train.data)
nbmodel$apriori #prior probabilities
nbmodel$tables # conditional probabilities
# Make predictions
predictionsnb <- nbmodel %>% predict(bal.train.data)
predictionsnb

# Model accuracy
tablenb=table(predictionsnb$class,bal.train.data$Transport)
confusionMatrix(tablenb)

#or
mean(predictionsnb$class==bal.train.data$Transport)

###test data
nbmodel.test=NaiveBayes(Transport~.,data=test.data)
nbmodel.test$apriori
nbmodel.test$tables
# Make predictions
predictionsnb.test <- nbmodel.test %>% predict(test.data)
predictionsnb.test

# Model accuracy
tablenb.test=table(predictionsnb.test$class,test.data$Transport)
confusionMatrix(tablenb.test)

#or
mean(predictionsnb.test$class==test.data$Transport)



##########################  Bagging  ##########################
#bagging- target variable is factor
bal.train.data2=bal.train.data# copy of dataset created for usage in bagging

library(rpart)
carbagging = bagging(Transport ~.,
           data=bal.train.data2,
           control=rpart.control(maxdepth=5, minsplit=4))


bal.train.data2$pred.class <- predict(carbagging, bal.train.data2)
ct.bag=table(bal.train.data2$Transport,bal.train.data2$pred.class)
table(bal.train.data2$pred.class)
confusionMatrix(ct.bag)

#bagging-Test data
test.data1=test.data
test.data1$pred.class = predict(carbagging,test.data1)
cmbagtest=table(test.data1$pred.class,test.data1$Transport)
confusionMatrix(cmbagtest)



#########################  boosting ###########################
bal.train.data3= bal.train.data
test.data3=test.data
class(bal.train.data3$Transport)
bal.train.data3$Transport=as.numeric(bal.train.data3$Transport)
test.data3$Transport=as.numeric(test.data3$Transport)
###caret - Gradient boosting

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), #complexity of the tree
                        n.trees = (1:30)*50, # number of iterations, i.e. trees,
                        shrinkage = 0.1,#learning rate: how quickly the algorithm adapts, called shrinkage
                        n.minobsinnode = 20) #the minimum number of training set samples in a node to commence splitting

BOOST.fit = caret::train(Transport~., data = bal.train.data3, method = "gbm", verbose=FALSE,tuneGrid = gbmGrid) 
BOOST.fit
plot(BOOST.fit)
bal.train.data3$Predict = predict(BOOST.fit, bal.train.data3)

cmboost=table(bal.train.data3$Predict, bal.train.data3$Transport)
confusionMatrix(cmboost)

#boost -test data
test.data3$Predict=predict(BOOST.fit,test.data3)
cmboost.test=table(test.data3$Predict,test.data3$Transport)
confusionMatrix(cmboost.test)

