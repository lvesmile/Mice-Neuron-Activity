library(dplyr)
library(R.matlab)
library(e1071)
set.seed(2022)
zm_bb409 <- readMat("Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zm_zs409 <- readMat("Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm_409 <- cbind.data.frame(zm_bb409,zm_zs409)
zm_drop<- zm_409 %>% filter(V1 != 0 | V2 !=0) # drop 409 solo time
zm_drop$V2<- NULL #drop open arm
zm_drop$V1 <- as.factor(zm_drop$V1)
df<-sort(sample(1:nrow(zm_drop), nrow(zm_drop)*.5))
train_close <-zm_drop[df,]
test_close <- zm_drop[-df,]

glm_fit <- glm(V1 ~ ., data=train_close, family="binomial")
summary(glm_fit)

# library(performance)
# library(see)
# check_model(glm_fit)
par(mfrow=c(2,2))
plot(glm_fit)

library(caret)

glm_pred <- round(predict(glm_fit, type="response", test_close))
#glm_pred
# library(tidyverse)
# complete_factor_levels <-c(train_close$V1 %>% factor %>% levels, 
#                            test_close$V1 %>% factor %>% levels) %>% unique
# train_close <- train_close %>% 
#   mutate(factor(train_close$V1, levels = complete_factor_levels))
# test_close <- test_close %>% 
#   mutate(factor(test_close$V1, levels = complete_factor_levels))
confusionMatrix(as.factor(glm_pred), test_close$V1)
