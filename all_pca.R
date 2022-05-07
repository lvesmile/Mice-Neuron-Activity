library(tidyverse)
library(R.matlab)
library(e1071)
library(caret)
#install.packages("facetoextra")
library(factoextra)
library(ggbiplot)
library(psych)
library(devtools)
library(pROC)


###409
set.seed(2022)

bb409<- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs409 <- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm_409 <- cbind.data.frame(bb409,zs409)
zm_drop<- zm_409 %>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]

# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 24, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:24])

set.seed(2022)
zm_part<- zm_409 %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))



######412
set.seed(2022)

bb412<- readMat("DATA/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs412 <- readMat("DATA/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm_412 <- cbind.data.frame(bb412,zs412)
zm_drop<- zm_412 %>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]
 
# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 27, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:27])

set.seed(2022)
zm_part<- zm_412 %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))


### 414

set.seed(2022)

bb<- readMat("DATA/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm <- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]

# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 11, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:11])

set.seed(2022)
zm_part<- zm %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))



### 416

set.seed(2022)

bb<- readMat("DATA/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm <- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]

# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 11, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:11])

set.seed(2022)
zm_part<- zm %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))




### 417

set.seed(2022)

bb<- readMat("DATA/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm <- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]

# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 11, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:24])

set.seed(2022)
zm_part<- zm %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))



### 418

set.seed(2022)

bb<- readMat("DATA/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zs<- readMat("DATA/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm <- cbind.data.frame(bb,zs)
zm_drop<- zm%>% filter(V1 != 0 | V2 !=0) # drop solo time
zm_drop$V1 <- NULL
zm_drop$V2<- NULL #drop open arm

df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.7))
train<-zm_drop[df,]
test<- zm_drop[-df,]

# bartlett.test(c) 
KMO(train) 

fa.parallel(train, fa = "both", n.iter = 1, show.legend = T)
df.pc <- principal(train, nfactors = 24, score = T)
###
pca <- prcomp(zm_drop, scale=TRUE)

pcs <- as.data.frame(pca$x[,1:24])

set.seed(2022)
zm_part<- zm %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.7))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

pred_glm <- round(predict(glm_fit, test_close, type="response"))

confusionMatrix(as.factor(pred_glm), test_close$closed_arm)

pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))










