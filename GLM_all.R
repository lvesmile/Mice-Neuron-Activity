library(tidyverse)
library(R.matlab)
library(e1071)
set.seed(2022)
library(caret)

###409 
bb<- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

### 412

bb<- readMat("DATA/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))




### 414 
bb<- readMat("DATA/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))


### 416

bb<- readMat("DATA/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_zscore.mat") %>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))



### 417

bb<- readMat("DATA/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))


#### 418

bb<- readMat("DATA/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm<- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
names(zm_drop)[1]<- "closed_arm"
zm_drop$closed_arm <- as.factor(zm_drop$closed_arm)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(closed_arm ~ ., data=train_close, family="binomial")
summary(glm_fit)

glm_pred <- round(predict(glm_fit, type="response", test_close))

confusionMatrix(as.factor(glm_pred), test_close$closed_arm)
library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
