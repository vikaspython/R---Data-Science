#########importing data############
setwd("/Users/Vicky/Desktop/PGP-BABI-vikas/FRA")
data.raw=read_excel("raw-data.xlsx")
data.val=read_excel("validation_data.xlsx")
summary(data.val)
summary(data$`Networth Next Year`)


#networth next year is converted into default column.
data.raw$`Networth Next Year`=ifelse(data.raw$`Networth Next Year`>0,0,1)
library(tidyverse)

colnames(data.raw)[colnames(data.raw) == "Networth Next Year"] <- "Default - 1" # Rename column

############# combining original data with validation data.############
data=rbind(data.raw,data.val)

colnames(data)[colnames(data) == "Default - 1"] <- "Default" # Rename column


summary(as.factor(data$`Default`))
prop.table(summary(as.factor(data$`Default`)))*100



sum(is.na(data))
summary(data)
names(data)
View(data)
library(mlr)
summarizeColumns(data)

data$`Creditors turnover`=as.numeric(data$`Creditors turnover`)
data$`Debtors turnover`=as.numeric(data$`Debtors turnover`)
data$`Finished goods turnover`=as.numeric(data$`Finished goods turnover`)
data$`WIP turnover`=as.numeric(data$`WIP turnover`)
data$`Raw material turnover`=as.numeric(data$`Raw material turnover`)
data$`Shares outstanding`=as.numeric(data$`Shares outstanding`)
data$`Equity face value`=as.numeric(data$`Equity face value`)
data$`PE on BSE`=as.numeric(data$`PE on BSE`)

summary(data$`Deposits (accepted by commercial banks)`)
data=data[,-22]


####################missing values treatment using KnnImputation################
library(DMwR)
#install.packages("DMwR")
class(data)
data=as.matrix(data)
data=knnImputation(data,k=3,scale=T,meth="median",distData = NULL)
data=as.data.frame(data)
summarizeColumns(data)

boxplot(data$`Default`~.,data=data)
library(DataExplorer)
#create_report(data)


###############outlier treatment##################
boxplot(data$`Total assets`)
uv = quantile(data$`Total assets`,0.99)
data$`Total assets`[data$`Total assets`>uv]=uv

boxplot(data$`Net worth`)
uv = quantile(data$`Net worth`,0.99)
data$`Net worth`[data$`Net worth`>uv]=uv

boxplot(data$`Total income`)
uv = quantile(data$`Total income`,0.99)
data$`Total income`[data$`Total income`>uv]=uv

boxplot(data$`Change in stock`)
uv = quantile(data$`Change in stock`,0.99)
data$`Change in stock`[data$`Change in stock`>uv]=uv

lv = quantile(data$`Change in stock`,0.01)
data$`Change in stock`[data$`Change in stock`<lv]=lv

boxplot(data$`Total expenses`)
uv = quantile(data$`Total expenses`,0.99)
data$`Total expenses`[data$`Total expenses`>uv]=uv


boxplot(data$`Profit after tax`)
uv = quantile(data$`Profit after tax`,0.99)
data$`Profit after tax`[data$`Profit after tax`>uv]=uv

boxplot(data$PBDITA)
uv = quantile(data$PBDITA,0.99)
data$PBDITA[data$PBDITA>uv]=uv

boxplot(data$PBT)
uv = quantile(data$PBT,0.99)
data$PBT[data$PBT>uv]=uv

boxplot(data$`Cash profit`)
uv = quantile(data$`Cash profit`,0.99)
data$`Cash profit`[data$`Cash profit`>uv]=uv

boxplot(data$`PBDITA as % of total income`)
lv = quantile(data$`PBDITA as % of total income`,0.01)
data$`PBDITA as % of total income`[data$`PBDITA as % of total income`<lv]=lv

boxplot(data$`PBT as % of total income`)
lv = quantile(data$`PBT as % of total income`,0.01)
data$`PBT as % of total income`[data$`PBT as % of total income`<lv]=lv

boxplot(data$`PAT as % of total income`)
lv = quantile(data$`PAT as % of total income`,0.01)
data$`PAT as % of total income`[data$`PAT as % of total income`<lv]=lv

boxplot(data$`Cash profit as % of total income`)
lv = quantile(data$`Cash profit as % of total income`,0.01)
data$`Cash profit as % of total income`[data$`Cash profit as % of total income`<lv]=lv

boxplot(data$Sales)
uv = quantile(data$Sales,0.99)
data$Sales[data$Sales>uv]=uv

boxplot(data$`Income from financial services`)
uv = quantile(data$`Income from financial services`,0.99)
data$`Income from financial services`[data$`Income from financial services`>uv]=uv

boxplot(data$`Other income`)
uv = quantile(data$`Other income`,0.99)
data$`Other income`[data$`Other income`>uv]=uv

boxplot(data$`Total capital`)
uv = quantile(data$`Total capital`,0.99)
data$`Total capital`[data$`Total capital`>uv]=uv

boxplot(data$`Reserves and funds`)
uv = quantile(data$`Reserves and funds`,0.99)
data$`Reserves and funds`[data$`Reserves and funds`>uv]=uv

boxplot(data$Borrowings)
uv = quantile(data$Borrowings,0.99)
data$Borrowings[data$Borrowings>uv]=uv

boxplot(data$`Current liabilities & provisions`)
uv = quantile(data$`Current liabilities & provisions`,0.99)
data$`Current liabilities & provisions`[data$`Current liabilities & provisions`>uv]=uv

boxplot(data$`Deferred tax liability`)
uv = quantile(data$`Deferred tax liability`,0.99)
data$`Deferred tax liability`[data$`Deferred tax liability`>uv]=uv

boxplot(data$`Shareholders funds`)
uv = quantile(data$`Shareholders funds`,0.99)
data$`Shareholders funds`[data$`Shareholders funds`>uv]=uv

boxplot(data$`Cumulative retained profits`)
uv = quantile(data$`Cumulative retained profits`,0.99)
data$`Cumulative retained profits`[data$`Cumulative retained profits`>uv]=uv

boxplot(data$`Capital employed`)
uv = quantile(data$`Capital employed`,0.99)
data$`Capital employed`[data$`Capital employed`>uv]=uv

boxplot(data$`Contingent liabilities / Net worth (%)`)
uv = quantile(data$`Contingent liabilities / Net worth (%)`,0.99)
data$`Contingent liabilities / Net worth (%)`[data$`Contingent liabilities / Net worth (%)`>uv]=uv

boxplot(data$`Contingent liabilities`)
uv = quantile(data$`Contingent liabilities`,0.99)
data$`Contingent liabilities`[data$`Contingent liabilities`>uv]=uv

boxplot(data$`Net fixed assets`)
uv = quantile(data$`Net fixed assets`,0.99)
data$`Net fixed assets`[data$`Net fixed assets`>uv]=uv

boxplot(data$Investments)
uv = quantile(data$Investments,0.99)
data$Investments[data$Investments>uv]=uv

boxplot(data$`Current assets`)
uv = quantile(data$`Current assets`,0.99)
data$`Current assets`[data$`Current assets`>uv]=uv

boxplot(data$`Net working capital`)
uv = quantile(data$`Net working capital`,0.99)
data$`Net working capital`[data$`Net working capital`>uv]=uv
lv = quantile(data$`Net working capital`,0.01)
data$`Net working capital`[data$`Net working capital`<lv]=lv

boxplot(data$`Quick ratio (times)`)
uv = quantile(data$`Quick ratio (times)`,0.99)
data$`Quick ratio (times)`[data$`Quick ratio (times)`>uv]=uv

boxplot(data$`Current ratio (times)`)
uv = quantile(data$`Current ratio (times)`,0.99)
data$`Current ratio (times)`[data$`Current ratio (times)`>uv]=uv

boxplot(data$`Debt to equity ratio (times)`)
uv = quantile(data$`Debt to equity ratio (times)`,0.99)
data$`Debt to equity ratio (times)`[data$`Debt to equity ratio (times)`>uv]=uv

boxplot(data$`Cash to current liabilities (times)`)
uv = quantile(data$`Cash to current liabilities (times)`,0.99)
data$`Cash to current liabilities (times)`[data$`Cash to current liabilities (times)`>uv]=uv

boxplot(data$`Cash to average cost of sales per day`)
uv = quantile(data$`Cash to average cost of sales per day`,0.99)
data$`Cash to average cost of sales per day`[data$`Cash to average cost of sales per day`>uv]=uv

boxplot(data$`Creditors turnover`)
uv = quantile(data$`Creditors turnover`,0.99)
data$`Creditors turnover`[data$`Creditors turnover`>uv]=uv

boxplot(data$`Debtors turnover`)
uv = quantile(data$`Debtors turnover`,0.99)
data$`Debtors turnover`[data$`Debtors turnover`>uv]=uv

boxplot(data$`Finished goods turnover`)
uv = quantile(data$`Finished goods turnover`,0.99)
data$`Finished goods turnover`[data$`Finished goods turnover`>uv]=uv

boxplot(data$`WIP turnover`)
uv = quantile(data$`WIP turnover`,0.99)
data$`WIP turnover`[data$`WIP turnover`>uv]=uv

boxplot(data$`Raw material turnover`)
uv = quantile(data$`Raw material turnover`,0.99)
data$`Raw material turnover`[data$`Raw material turnover`>uv]=uv

boxplot(data$`Shares outstanding`)
uv = quantile(data$`Shares outstanding`,0.99)
data$`Shares outstanding`[data$`Shares outstanding`>uv]=uv
lv = quantile(data$`Shares outstanding`,0.01)
data$`Shares outstanding`[data$`Shares outstanding`<lv]=lv

boxplot(data$`Equity face value`)
lv = quantile(data$`Equity face value`,0.01)
data$`Equity face value`[data$`Equity face value`<lv]=lv
uv = quantile(data$`Equity face value`,0.99)
data$`Equity face value`[data$`Equity face value`>uv]=uv

boxplot(data$EPS)
lv = quantile(data$EPS,0.01)
data$EPS[data$EPS<lv]=lv
uv = quantile(data$EPS,0.99)
data$EPS[data$EPS>uv]=uv

boxplot(data$`Adjusted EPS`)
lv = quantile(data$`Adjusted EPS`,0.01)
data$`Adjusted EPS`[data$`Adjusted EPS`<lv]=lv
uv = quantile(data$`Adjusted EPS`,0.99)
data$`Adjusted EPS`[data$`Adjusted EPS`>uv]=uv

boxplot(data$`Total liabilities`)
uv = quantile(data$`Total liabilities`,0.99)
data$`Total liabilities`[data$`Total liabilities`>uv]=uv

boxplot(data$`PE on BSE`)
uv = quantile(data$`PE on BSE`,0.99)
data$`PE on BSE`[data$`PE on BSE`>uv]=uv
lv = quantile(data$`PE on BSE`,0.01)
data$`PE on BSE`[data$`PE on BSE`<lv]=lv

names(data)


#create_report(data)

####################new variable creation########################
data$current.ratio=data$`Current assets`/data$`Current liabilities & provisions`

data$debt.ratio=data$`Total liabilities`/data$`Total assets`

data$gross.margin.ratio=data$`Profit after tax`/Sales

data$company.size=data$`Net worth`/data$`Total assets`
names(data)


##########multi collinearity ############
library(corrplot)
corrplot(cor(data))
cor.data=as.matrix(cor(data))
write.csv(cor.data,"/Users/Vicky/Desktop/PGP-BABI-vikas/FRA/cor-data.csv", row.names = FALSE)

######### removing highly correalted variables###
data1=data
data1=data1[,-c(1,3,4,7,8,10,11,14,16,17,23,34,50,21,27,49)]
cor.data1=as.matrix(cor(data1))
write.csv(cor.data1,"/Users/Vicky/Desktop/PGP-BABI-vikas/FRA/cor-data1.csv", row.names = FALSE)



#####creating default indicator###########

plot(data1$`Total income`,data1$`Networth Next Year`)
Default=as.factor(Default)


########variable importance#######

library(rpart)
library(rpart.plot)
part.model1= rpart(Default~.,data=data1)
rpart.plot(part.model1)
var.imp=part.model1$variable.importance
var.imp



attach(data1)

#########split the data into train and validation###############

data1.tr=data1[c(1:3541),]
data1.va=data1[c(3542:4256),]


##########Bivariate analysis############
qplot(x=Default,y=PBITA,data = data1,geom="boxplot")
plot(as.factor(data1$Default),data1$`PBDITA as % of total income`,main="default vs `PBDITA as % of total income`")
plot(as.factor(data1$Default),data1$`PE on BSE`,main="default vs PE on BSE")
plot(as.factor(data1$Default),data1$company.size,main="default vs company.size")
plot(as.factor(data1$Default),data1$`Debt to equity ratio (times)`,main="default vs Debt to equity ratio (times)")
plot(as.factor(data1$Default),data1$`Cash profit as % of total income`,main="default vs `Cash profit as % of total income`")
plot(as.factor(data1$Default),data1$`Cash to current liabilities (times)`,main="default vs `Cash to current liabilities (times)`")




#######logistic regression###########

log.model1= glm(data1.tr$Default ~ `PBT as % of total income`
                +`Cash profit as % of total income`
                +company.size + gross.margin.ratio + PBDITA +`Debt to equity ratio (times)`
                  + data1.tr$`PE on BSE` +data1.tr$EPS
                ,family=binomial,data=data1.tr)

log.model1
summary(log.model1)
summary(log.model1$fitted.values)  #default probabilities of a particular company
#fitted probabilities can be used to discriminate between good and bad companies


plot(as.factor(log.model1$y),log.model1$fitted.values)
varImp(log.model1,scale=FALSE)

#model using all variables
log.model2= glm(Default~.,family=binomial,data=data1.tr)
log.model2
summary(log.model2)

plot(as.factor(log.model2$y),log.model2$fitted.values)
summary(log.model1$fitted.values)
default.pred=ifelse(log.model1$fitted.values>0.6,1,0)
table(as.factor(log.model2$y),default.pred)
varImp(log.model2,scale=FALSE)

###Roc curve and AUC
library(ROCR)
library(pROC)
roccurve=roc(data1.tr$Default,log.model2$fitted.values) 
plot.roc(roccurve)
roccurve
#KS
pred.train=prediction(log.model2$fitted.values,data1.tr$Default) 
perf.train=performance(pred.train,"tpr","fpr")
KS.train = max(perf.train@y.values[[1]]-perf.train@x.values[[1]]) 
round(KS.train,2)
plot(perf.train@y.values[[1]]- perf.train@x.values[[1]],ylab="probability",main="KS-Train")
#GINI
library(ineq)
gini.train=ineq(log.model2$fitted.values,type="Gini")
round(gini.train,2)
#calibrating probability cut-offs
status.predicted=ifelse(log.model2$fitted.values<0.7,0,1)
status.predicted=as.factor(status.predicted) 
ct.log=table(data1.tr$Default,status.predicted)
##confusion matrix
ct.log 
confusionMatrix(ct.log)

####VALIDATION DATA#######

predictedY <- predict(log.model2,newdata=data1.va, type="response") 
pred.classY<- as.factor(ifelse(predictedY > 0.7,1,0))
confusionMatrix(pred.classY,as.factor(data1.va$Default))

roccurve.va=roc(data1.va$Default,predictedY) 
plot.roc(roccurve.va)
roccurve.va
#KS
pred.train.va=prediction(predictedY,data1.va$Default) 
perf.train.va=performance(pred.train.va,"tpr","fpr")
KS.train.va = max(perf.train.va@y.values[[1]]-perf.train.va@x.values[[1]]) 
round(KS.train.va,2)
plot(perf.train.va@y.values[[1]]- perf.train.va@x.values[[1]],ylab="probability",main="KS-Validation")
#GINI
library(ineq)
gini.train.va=ineq(predictedY,type="Gini")
round(gini.train.va,2)

#calibrating probability cut-offs
status.predicted=ifelse(log.model2$fitted.values<0.7,0,1)
status.predicted=as.factor(status.predicted) 
ct.log=table(data1.tr$Default,status.predicted)

exp(coef(log.model2))



##rank ordering #######



# predict the probability- train data
data2.tr=data1.tr
data2.tr$pred = predict(log.model2, data=data2.tr, type="response")

# create the ranks

# cut_p returns the cut internal for each observation
cut_p = with(data2.tr,
             cut(pred, breaks = quantile(pred, prob=seq(0,1,0.1)), include.lowest = T))

levels(cut_p)

# convert cut internal into easy-to-read ranks
data2.tr$rank = factor(cut_p, labels = 1:10)

plot(data2.tr$rank,data2.tr$pred,main="deciles in train data")

# predict the probability- validation data
data2.va=data1.va
data2.va$pred = predict(log.model2, newdata=data2.va, type="response")

# create the ranks

# cut_p returns the cut internal for each observation
cut_p.va = with(data2.va,
             cut(pred, breaks = quantile(pred, prob=seq(0,1,0.1)), include.lowest = T))

levels(cut_p.va)

# convert cut internal into easy-to-read ranks
data2.va$rank = factor(cut_p.va, labels = 1:10)

plot(data2.va$rank,data2.va$pred,main="deciles in validation data")

