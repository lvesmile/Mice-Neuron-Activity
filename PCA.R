library(tidyverse)
library(R.matlab)
library(e1071)
library(caret)
#install.packages("facetoextra")
library(factoextra)


set.seed(2022)

zm_bb409 <- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zm_zs409 <- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm_409 <- cbind.data.frame(zm_bb409,zm_zs409)
zm_drop<- zm_409 %>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
zm_drop$V1 <- NULL

pca <- prcomp(zm_drop, scale=TRUE)

fviz_eig(pca, ncp = 21,addlabels = TRUE)


pcs <- as.data.frame(pca$x[,1:21])

pr.var <- pca$sdev^2
pve <- pr.var / sum(pr.var)
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = "b")


set.seed(2022)
zm_part<- zm_409 %>% filter(V1 != 0 | V2 !=0)
zm_part$V2 <- NULL
#names(zm_part)[1]<- "closed_arm"
dat <- cbind(zm_part$V1, pcs) %>% as.data.frame()
names(dat)[1]<- "closed_arm"
dat$closed_arm <- as.factor(dat$closed_arm)
df<-sort(sample(1:nrow(dat), nrow(dat)*.5))
train_close <-dat[df,]
test_close <- dat[-df,]

glm_fit = glm(closed_arm~., data=train_close, family = binomial("logit") )

par(mfrow=c(2,2))
plot(glm_fit)

pred_glm <- round(predict(glm_fit, test_close, type="response"))

# 81.24% accuracy
confusionMatrix(as.factor(pred_glm), test_close$closed_arm)


df <- rbind(data.frame(predictor = predict(glm_fit, train_close),
                       known.truth = train_close$closed_arm,
                       model = "train"),
            data.frame(predictor = predict(glm_fit, test_close),
                       known.truth = test_close$closed_arm,
                       model = "test"))

# the aesthetic names are not the most intuitive
# `d` (disease) holds the known truth
# `m` (marker) holds the predictor values 
library(ROCR)
#install.packages("plotROC")
library(plotROC)
library(ggplot2)
ggplot(df, aes(d = known.truth, m = predictor, color = model)) + 
  geom_roc(n.cuts = 0)

# library(ROCR)
# #install.packages("Metrics")
# library(Metrics)
# pr <- prediction(pred_glm, test_close$closed_arm)
# perf <- performance(pr, "tpr", "fpr") 
# plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
#      ylab="Sensitivity")    
# abline(0, 1)
# auc(test_close$closed_arm, pred_glm)

library(pROC)
pred_glm <- predict(glm_fit, test_close, type="response")
rocobj <- roc(test_close$closed_arm, pred_glm)

auc <- round(auc(test_close$closed_arm, pred_glm),4)
ggroc(rocobj, colour = 'blue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
