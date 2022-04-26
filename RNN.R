source("PCA.R")

library(tensorflow)
library(keras)


max_len <- 10
batch_size <- 20
total_epochs <- 15

set.seed(2022)

close<- zm_part$closed_arm

start_indexes<- seq(1,length(close)- (max_len + 1), by=3)

zm_matrix<- matrix(nrow = length(start_indexes), ncol = max_len + 1)

for (i in 1:length(start_indexes)){
  zm_matrix[i,] <- close[start_indexes[i]:(start_indexes[i] + max_len)]
}

x <- zm_matrix[, -ncol(zm_matrix)]
y <- zm_matrix[, ncol(zm_matrix)]

training_index <- createDataPartition(y, p = .7, 
                                      list = FALSE, 
                                      times = 1)
# training data
X_train <- array(x[training_index,], dim = c(length(training_index), max_len, 1))
y_train <- y[training_index]

# testing data
X_test <- array(x[-training_index,], dim = c(length(y) - length(training_index), max_len, 1))
y_test <- y[-training_index]

# initialize our model
model <- keras_model_sequential()

# dimensions of our input data
dim(X_train)

model %>%
  layer_dense(input_shape = dim(X_train)[2:3], units = max_len)

model %>% 
  layer_simple_rnn(units = 6)

model %>%
  layer_dense(units = 1, activation = 'sigmoid') # output

summary(model)

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'RMSprop', 
                  metrics = c('accuracy'))

# Actually train our model! This step will take a while
trained_model <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = batch_size, # how many samples to pass to our model at a time
  epochs = total_epochs, # how many times we'll look @ the whole dataset
  validation_split = 0.1) # how much data to hold out for testing as we go along

# how well did our trained model do?
trained_model

# plot how our model preformance changed during training 
plot(trained_model)

# Predict the classes for the test data
classes <- model %>% predict(X_test, batch_size = batch_size)%>% round()
# Confusion matrix
# table(y_test, as.factor(classes))

confusionMatrix(as.factor(classes), as.factor(y_test))
