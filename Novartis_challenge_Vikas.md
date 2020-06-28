Novartis Machine Learning Challenge - Vikas Pericherla
================

### Objective:

Building a predictive model which can identify if the Hack will occur.

### Hypothesis:

Null Hypothesis – None of the independent variables is able to predict
the dependant variable(MULTIPLE\_OFFENCE)

Alternate Hypothesis – At least one of the independent variable is able
to predict the dependant variable(MULTIPLE\_OFFENCE)

### Importing Train and Test data provided by Novartis Data Science Hiring challenge.

``` r
setwd("/Users/Vicky/Desktop/PGP-BABI-vikas/Dataset novartis")
getwd()
```

    ## [1] "/Users/Vicky/Desktop/PGP-BABI-vikas/Dataset novartis"

``` r
train=read.csv("Train.csv",na.strings=c(" ", "NA","?","unknown",""))
test=read.csv("Test.csv",na.strings=c("", "NA","?","unknown"))
```

## Checking summary of train data.

    ## Loading required package: ParamHelpers

    ## 'mlr' is in maintenance mode since July 2019. Future development
    ## efforts will go into its successor 'mlr3' (<https://mlr3.mlr-org.com>).

    ## 
    ## Attaching package: 'mlr'

    ## The following object is masked from 'package:caret':
    ## 
    ##     train

    ##                name      type  na        mean       disp median     mad min max
    ## 1       INCIDENT_ID character   0          NA  0.9999581     NA      NA   1   1
    ## 2              DATE character   0          NA  0.9990778     NA      NA   1  22
    ## 3               X_1   integer   0   0.4837777  1.4397379      0  0.0000   0   7
    ## 4               X_2   integer   0  24.7912056 15.2402310     24 19.2738   0  52
    ## 5               X_3   integer   0  24.6374497 15.1350925     24 19.2738   0  52
    ## 6               X_4   integer   0   4.2767438  2.9446721      4  2.9652   0  10
    ## 7               X_5   integer   0   2.4556087  1.9630947      3  2.9652   0   5
    ## 8               X_6   integer   0   6.1541751  4.4717560      5  4.4478   1  19
    ## 9               X_7   integer   0   4.8765091  3.8819307      4  4.4478   0  18
    ## 10              X_8   integer   0   0.9724598  1.4531445      1  1.4826   0  99
    ## 11              X_9   integer   0   4.9241281  1.3626246      5  1.4826   0   6
    ## 12             X_10   integer   0   1.2448021  1.1193007      1  0.0000   1  90
    ## 13             X_11   integer   0 206.9545188 93.0333480    249 99.3342   0 332
    ## 14             X_12   numeric 182   0.9740644  1.1677251      1  0.0000   0  90
    ## 15             X_13   integer   0  85.2373826 27.5972264     98 16.3086   0 116
    ## 16             X_14   integer   0  72.6742958 43.2973203     62 48.9258   0 142
    ## 17             X_15   integer   0  33.4647468  8.3868337     34  0.0000   0  50
    ## 18 MULTIPLE_OFFENSE   integer   0   0.9552314  0.2067998      1  0.0000   0   1
    ##    nlevs
    ## 1  23856
    ## 2   9121
    ## 3      0
    ## 4      0
    ## 5      0
    ## 6      0
    ## 7      0
    ## 8      0
    ## 9      0
    ## 10     0
    ## 11     0
    ## 12     0
    ## 13     0
    ## 14     0
    ## 15     0
    ## 16     0
    ## 17     0
    ## 18     0

Observations:

1.  All the variables except date and Incident ID are numeric.
2.  Missing values in X\_12 variable, which needs to be imputed.
3.  Skewness can be observed in most variables from the difference in
    mean and median.

## Lets check the head of the Data.

    ##   INCIDENT_ID      DATE X_1 X_2 X_3 X_4 X_5 X_6 X_7 X_8 X_9 X_10 X_11 X_12 X_13
    ## 1   CR_102659 04-JUL-04   0  36  34   2   1   5   6   1   6    1  174    1   92
    ## 2   CR_189752 18-JUL-17   1  37  37   0   0  11  17   1   6    1  236    1  103
    ## 3   CR_184637 15-MAR-17   0   3   2   3   5   1   0   2   3    1  174    1  110
    ## 4   CR_139071 13-FEB-09   0  33  32   2   1   7   1   1   6    1  249    1   72
    ## 5   CR_109335 13-APR-05   0  33  32   2   1   8   3   0   5    1  174    0  112
    ## 6    CR_96263 07-APR-03   0  45  45  10   3   1   0   1   6    1  303    1   72
    ##   X_14 X_15 MULTIPLE_OFFENSE
    ## 1   29   36                0
    ## 2  142   34                1
    ## 3   93   34                1
    ## 4   29   34                1
    ## 5   29   43                1
    ## 6   62   34                1

### Combine Data

1.  ID and date columns are removed as it is not essential to
    EDA.(Exploratory data analysis). These objects are suffixed with 1.
2.  Train and test data are combined for further EDA using rbind
    function and stored in object trainc.
3.  Summary of combined dataset is checked using summarizeColumns
    function.

<!-- end list -->

``` r
train1=train[,-c(1,2,18)]
test1=test[,-c(1,2)]
trainc=rbind(train1,test1)
summarizeColumns(trainc)
```

    ##    name    type  na        mean      disp median     mad min max nlevs
    ## 1   X_1 integer   0   0.4775019  1.428755      0  0.0000   0   7     0
    ## 2   X_2 integer   0  24.7637768 15.235522     24 19.2738   0  52     0
    ## 3   X_3 integer   0  24.6124903 15.131877     24 19.2738   0  52     0
    ## 4   X_4 integer   0   4.2797354  2.956638      4  2.9652   0  10     0
    ## 5   X_5 integer   0   2.4527528  1.963184      3  2.9652   0   5     0
    ## 6   X_6 integer   0   6.1264619  4.463585      5  4.4478   1  19     0
    ## 7   X_7 integer   0   4.8709475  3.870959      4  4.4478   0  18     0
    ## 8   X_8 integer   0   0.9781685  1.460421      1  1.4826   0  99     0
    ## 9   X_9 integer   0   4.9179808  1.367462      5  1.4826   0   6     0
    ## 10 X_10 integer   0   1.2433663  1.017419      1  0.0000   1  90     0
    ## 11 X_11 integer   0 206.9543500 93.061957    249 99.3342   0 332     0
    ## 12 X_12 numeric 309   0.9733333  1.060945      1  0.0000   0  90     0
    ## 13 X_13 integer   0  85.2188687 27.555325     98 16.3086   0 117     0
    ## 14 X_14 integer   0  72.4920144 43.353765     62 48.9258   0 142     0
    ## 15 X_15 integer   0  33.4478986  8.357811     34  0.0000   0  50     0

### Missing value imputation

1.  Missing values are imputed using knnimputation function.
2.  Imputed file is stored in trainc2 object.

<!-- end list -->

    ## Loading required package: grid

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ##    name    type na        mean      disp median     mad min max nlevs
    ## 1   X_1 integer  0   0.4775019  1.428755      0  0.0000   0   7     0
    ## 2   X_2 integer  0  24.7637768 15.235522     24 19.2738   0  52     0
    ## 3   X_3 integer  0  24.6124903 15.131877     24 19.2738   0  52     0
    ## 4   X_4 integer  0   4.2797354  2.956638      4  2.9652   0  10     0
    ## 5   X_5 integer  0   2.4527528  1.963184      3  2.9652   0   5     0
    ## 6   X_6 integer  0   6.1264619  4.463585      5  4.4478   1  19     0
    ## 7   X_7 integer  0   4.8709475  3.870959      4  4.4478   0  18     0
    ## 8   X_8 integer  0   0.9781685  1.460421      1  1.4826   0  99     0
    ## 9   X_9 integer  0   4.9179808  1.367462      5  1.4826   0   6     0
    ## 10 X_10 integer  0   1.2433663  1.017419      1  0.0000   1  90     0
    ## 11 X_11 integer  0 206.9543500 93.061957    249 99.3342   0 332     0
    ## 12 X_12 numeric  0   0.9664478  1.059966      1  0.0000   0  90     0
    ## 13 X_13 integer  0  85.2188687 27.555325     98 16.3086   0 117     0
    ## 14 X_14 integer  0  72.4920144 43.353765     62 48.9258   0 142     0
    ## 15 X_15 integer  0  33.4478986  8.357811     34  0.0000   0  50     0

### Correlation check

``` r
trainc2.cor=cor(trainc2)
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
corrplot(trainc2.cor)
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
Plot Observations: 1. variables x\_2 and x\_3 are highly correalted. 2.
variables x\_10 and x\_12 are highly correalted. 3. variables x\_6 and
x\_7 are highly correalted.

we need to treat for multicollenearity.

Variables 3,5,6,7,12 are removed as due to their high correlation. The
new dataset is named as trainc6 and its correlation plot is checked.

``` r
trainc6=trainc2[,-c(3,5,6,7,12)]
trainc6.cor=cor(trainc6)
corrplot(trainc6.cor)
```

![](Novartis_challenge_Vikas_files/figure-gfm/multicollinearity%20treatment-1.png)<!-- -->

Observations: No significant multicollinearity effect among the
variables. Hence we can proceed for furthur analysis.

We will split the data into train and test - named as train2 and test2
attaching the target variable which was removed earlier, to the train2
object.

``` r
train2=trainc2[1:23856,]
test2=trainc2[23857:39759,]

train2$MULTIPLE_OFFENSE=train$MULTIPLE_OFFENSE
```

### EDA - checking individual variables with the Target variable

``` r
boxplot(train2$X_1~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
boxplot(train2$X_2~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
boxplot(train2$X_3~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
boxplot(train2$X_4~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
boxplot(train2$X_5~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
boxplot(train2$X_6~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
boxplot(train2$X_7~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
boxplot(train2$X_8~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
boxplot(train2$X_9~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
boxplot(train2$X_10~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
boxplot(train2$X_11~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
This Plot indicates that the variable X\_11 have good predictive power
on target variable, as binary classification is separated by this
variable.

``` r
boxplot(train2$X_12~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
boxplot(train2$X_13~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
boxplot(train2$X_14~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
boxplot(train2$X_15~train2$MULTIPLE_OFFENSE, boxwex=0.4, col = "red", border="blue")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Balancing the dataset

``` r
table(train2$MULTIPLE_OFFENSE)
```

    ## 
    ##     0     1 
    ##  1068 22788

``` r
print(1068/(1068+22788)*100 )
```

    ## [1] 4.476861

Minority class is 4.48 percent of total which is highly imbalanced.
Hence we will use SMOTE technique to balance the dataset.

``` r
table(train2$MULTIPLE_OFFENSE)
```

    ## 
    ##     0     1 
    ##  1068 22788

``` r
train2$MULTIPLE_OFFENSE=as.factor(train2$MULTIPLE_OFFENSE)
train2s <- SMOTE(MULTIPLE_OFFENSE ~., train2, perc.over = 700, k = 5, perc.under = 300)
table(train2s$MULTIPLE_OFFENSE)
```

    ## 
    ##     0     1 
    ##  8544 22428

``` r
8544/(8544+22428)*100
```

    ## [1] 27.58621

``` r
train2s=train2s[,-c(3,5,6,7,12)]
test2=test2[,-c(3,5,6,7,12)]
```

We improved the minority class from 4.48% to 27.59%.

### Random Forest

Random forest is a tree-based algorithm which involves building several
trees (decision trees), then combining their output to improve
generalization ability of the model.

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
RF_Model <- randomForest(MULTIPLE_OFFENSE~ ., data = train2s, ntree= 201, 
                         mtry=3, nodesize = 50,replace = TRUE,
                         importance = TRUE)
RF_Model
```

    ## 
    ## Call:
    ##  randomForest(formula = MULTIPLE_OFFENSE ~ ., data = train2s,      ntree = 201, mtry = 3, nodesize = 50, replace = TRUE, importance = TRUE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 201
    ## No. of variables tried at each split: 3
    ## 
    ##         OOB estimate of  error rate: 0.57%
    ## Confusion matrix:
    ##      0     1 class.error
    ## 0 8501    43 0.005032772
    ## 1  135 22293 0.006019262

``` r
plot(RF_Model, main = "")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")
```

![](Novartis_challenge_Vikas_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
Prob.Train_RF = predict(RF_Model, train2[1:15], type="class")
confusionMatrix(Prob.Train_RF,train2$MULTIPLE_OFFENSE)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0  1052   163
    ##          1    16 22625
    ##                                           
    ##                Accuracy : 0.9925          
    ##                  95% CI : (0.9913, 0.9936)
    ##     No Information Rate : 0.9552          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9177          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.98502         
    ##             Specificity : 0.99285         
    ##          Pos Pred Value : 0.86584         
    ##          Neg Pred Value : 0.99929         
    ##              Prevalence : 0.04477         
    ##          Detection Rate : 0.04410         
    ##    Detection Prevalence : 0.05093         
    ##       Balanced Accuracy : 0.98893         
    ##                                           
    ##        'Positive' Class : 0               
    ## 

Random Forest gives an accuracy of 99.45% . we will explore other models
and run the best model on the test data.

### Logistic Regression

Since it is a binary classification model we will run a logistic
regression model.

Logistic regression is the simplest non-linear classifier with a linear
combination of parameters and nonlinear function (sigmoid) for binary
classification.

``` r
log.model=glm(formula = train2s$MULTIPLE_OFFENSE ~ ., family ="binomial", data = train2s)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(log.model) 
```

    ## 
    ## Call:
    ## glm(formula = train2s$MULTIPLE_OFFENSE ~ ., family = "binomial", 
    ##     data = train2s)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6865  -0.2328   0.3850   0.5206   8.4904  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.2463081  0.1321616  24.563  < 2e-16 ***
    ## X_1          0.0504091  0.0129562   3.891 9.99e-05 ***
    ## X_2         -0.0163393  0.0010737 -15.218  < 2e-16 ***
    ## X_4          0.0469533  0.0057512   8.164 3.24e-16 ***
    ## X_8          0.2469510  0.0154579  15.976  < 2e-16 ***
    ## X_9         -0.0138081  0.0137263  -1.006    0.314    
    ## X_10        -2.1718065  0.0283276 -76.667  < 2e-16 ***
    ## X_11         0.0050149  0.0001882  26.642  < 2e-16 ***
    ## X_13        -0.0014147  0.0006130  -2.308    0.021 *  
    ## X_14        -0.0030523  0.0004069  -7.501 6.32e-14 ***
    ## X_15         0.0173089  0.0017779   9.736  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 36485  on 30971  degrees of freedom
    ## Residual deviance: 23278  on 30961  degrees of freedom
    ## AIC: 23300
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
predictedY <- predict(log.model,train2s, type="response") 
pred.classY<- as.factor(ifelse(predictedY > 0.5, 1,0))
confusionMatrix(pred.classY,train2s$MULTIPLE_OFFENSE)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0  5545  1307
    ##          1  2999 21121
    ##                                           
    ##                Accuracy : 0.861           
    ##                  95% CI : (0.8571, 0.8648)
    ##     No Information Rate : 0.7241          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6293          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.6490          
    ##             Specificity : 0.9417          
    ##          Pos Pred Value : 0.8093          
    ##          Neg Pred Value : 0.8757          
    ##              Prevalence : 0.2759          
    ##          Detection Rate : 0.1790          
    ##    Detection Prevalence : 0.2212          
    ##       Balanced Accuracy : 0.7954          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

Logistic regression gives an accuracy of 86.55% . we will explore other
models and run the best model on the test data.

We can proceed for XGBoost:

### XGBoost

XGBoost belongs to a family of boosting algorithms that convert weak
learners into strong learners.

Preparing data for XGBoost model.

``` r
library(xgboost)
  seed <- 248
  set.seed(seed)
  Target <- train2s[,11]
  Train_XGB <- train2s
  features_train_XGB<-as.matrix(Train_XGB[,1:10])
  label_train_XGB<-as.matrix(Train_XGB[,11])
  
  Test_XGB <- test2
  features_test_XGB<-as.matrix(Test_XGB[,1:10])
```

Build the XGBoost model:

### Parameters for Tree Booster

1.  **nrounds\[default=100\]**
      - It controls the maximum number of iterations. For
        classification, it is similar to the number of trees to grow.
2.  **eta\[default=0.3\]\[range: (0,1)\]**
      - It controls the learning rate, i.e., the rate at which our model
        learns patterns in data. After every round, it shrinks the
        feature weights to reach the best optimum.
      - Lower eta leads to slower computation. It must be supported by
        increase in `nrounds`.
      - Typically, it lies between 0.01 - 0.3
3.  **gamma\[default=0\]\[range: (0,Inf)\]**
      - It controls regularization (or prevents overfitting).
4.  **max\_depth\[default=6\]\[range: (0,Inf)\]**
      - It controls the depth of the tree.
      - Larger the depth, more complex the model; higher chances of
        overfitting.
5.  **min\_child\_weight\[default=1\]\[range:(0,Inf)\]**
      - it blocks the potential feature interactions to prevent
        overfitting.
6.  **subsample\[default=1\]\[range: (0,1)\]**
      - It controls the number of samples (observations) supplied to a
        tree.
7.  **colsample\_bytree\[default=1\]\[range: (0,1)\]**
      - It control the number of features (variables) supplied to a tree

<!-- end list -->

``` r
xgb.fit <- xgboost(
    data = features_train_XGB,label = label_train_XGB,eta = 0.86,
    max_depth = 3,
    min_child_weight = 1,
    nrounds = 1000,nfold = 10,
    objective = "binary:logistic",verbose = 0,early_stopping_rounds = 10,gamma=0.01,colsample_bytree=0.90)
```

The XGBoost model is run through the train model and checked for model
performance.

``` r
 library(caret)
  xgb.pred.class_train <- predict(xgb.fit, features_train_XGB)
  pred.class.xgb.train<- as.factor(ifelse(xgb.pred.class_train > 0.5, 1, 0))
  ct.train=table(label_train_XGB,pred.class.xgb.train)
  confusionMatrix(ct.train)
```

    ## Confusion Matrix and Statistics
    ## 
    ##                pred.class.xgb.train
    ## label_train_XGB     0     1
    ##               0  8543     1
    ##               1     1 22427
    ##                                      
    ##                Accuracy : 0.9999     
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.7241     
    ##     P-Value [Acc > NIR] : <2e-16     
    ##                                      
    ##                   Kappa : 0.9998     
    ##                                      
    ##  Mcnemar's Test P-Value : 1          
    ##                                      
    ##             Sensitivity : 0.9999     
    ##             Specificity : 1.0000     
    ##          Pos Pred Value : 0.9999     
    ##          Neg Pred Value : 1.0000     
    ##              Prevalence : 0.2759     
    ##          Detection Rate : 0.2758     
    ##    Detection Prevalence : 0.2759     
    ##       Balanced Accuracy : 0.9999     
    ##                                      
    ##        'Positive' Class : 0          
    ## 

Train data gives perfect 100% accuracy, sensitivity and specificity.

We will run this model through test data and check the results.

``` r
  xgb.pred.class_test <- predict(xgb.fit, features_test_XGB)
  pred.class.xgb<- as.factor(ifelse(xgb.pred.class_test > 0.5, 1, 0))
  table(pred.class.xgb)
```

    ## pred.class.xgb
    ##     0     1 
    ##   741 15162

\#\#Checking variable importance

``` r
 mat <- xgb.importance (feature_names = colnames(train2s),model = xgb.fit)
  xgb.plot.importance (importance_matrix = mat[1:10])
```

![](Novartis_challenge_Vikas_files/figure-gfm/variable%20importance-1.png)<!-- -->
Variables X\_10, X\_11 and X\_15 are the most important for the
predictions.

### Final Submission file - XGBoost

Test ID column is attached with predictions from XGBoost to make the
final submsssion file

``` r
MULTIPLE_OFFENSE=pred.class.xgb
  INCIDENT_ID=test[,1]
  submission.XG=data.frame(INCIDENT_ID,MULTIPLE_OFFENSE)
  write.csv(submission.XG,"/Users/Vicky/Desktop/PGP-BABI-vikas/Dataset novartis/submissionxgb.csv", row.names = FALSE)
```
