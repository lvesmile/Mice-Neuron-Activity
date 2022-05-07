library(tidyverse)
library(R.matlab)
library(e1071)
library(caret)
library(tensorflow)
library(keras)
set.seed(2022)

zm_bb409 <- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()

zm_zs409 <- readMat("DATA/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

set.seed(2022)
zm_409 <- cbind.data.frame(zm_bb409,zm_zs409)
zm_part<- zm_409 %>% filter(V1 != 0 | V2 !=0)
names(zm_part)[1]<- "closed_arm"

names(zm_part)[2]<- "open_arm"

zm<- as.matrix(zm_part[, 1:60])
batch_size <- 20
total_epochs <- 15

set.seed(2022)

ind <- sample(2, nrow(zm),
              replace = TRUE, prob = c(0.7, 0.3))

x.train = zm[ind == 1, 3:60]
y.train = zm[ind == 1, -(3:60)]
x.test = zm[ind == 2, 3:60]
y.test = zm[ind == 2, -(3:60)]

# Initialize a sequential model
model <- keras_model_sequential()

# model %>%
#   layer_dense(units = 2, activation = 'sigmoid',
#               input_shape = ncol(x.train))
# summary(model)
# Add layers to model
set.seed(2022)
model %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(x.train)) %>% layer_dropout(rate=0.1) %>%
  layer_dense(units = 64, activation = 'relu') %>%layer_dropout(rate=0.1) %>%
  layer_dense(units = ncol(y.train), activation = 'softmax')

summary(model)
model %>% compile(loss = 'categorical_crossentropy', 
                  optimizer = 'rmsprop', 
                  metrics = c('accuracy'))
# Actually train our model! This step will take a while

trained_model <- model %>% fit(
  x = x.train, # sequence we're using for prediction 
  y = y.train, # sequence we're predicting
  batch_size = batch_size, # how many samples to pass to our model at a time
  epochs = total_epochs, # how many times we'll look @ the whole dataset
  validation_split = 0.2) # how much data to hold out for testing as we go along

# how well did our trained model do?
trained_model

# plot how our model preformance changed during training 
plot(trained_model)

# Predict the classes for the test data
classes <- model %>% predict(x.test, batch_size = batch_size)%>% round()
# Confusion matrix
# table(y_test, as.factor(classes))

confusionMatrix(as.factor(classes), as.factor(y.test))

score <- model %>% evaluate(x.test, y.test)
