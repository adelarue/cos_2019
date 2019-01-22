#### Data ####

# Get the MNIST data for this section
library(keras)

mnist = dataset_mnist()

# Get the train-test split that has been defined for us
X_train = mnist$train$x
y_train = mnist$train$y
X_test = mnist$test$x
y_test = mnist$test$y

# For the sake of time we're going to down-sample the data from 60000 to 10000
# training samples

# Get the indices for our down-sampled data (each label is equally 
# represented in the data so we do not need to proportionally down-sample)
set.seed(17)
idx = sample(1:dim(X_train)[1], size = 10000)
X_train = X_train[idx, , ]
y_train = y_train[idx]

#### Row vs Column Major Data ####

# Native R, column-major format
array_reshape(1:4, dim = c(2, 2), order = "F")

# Python, row-major format
array_reshape(1:4, dim = c(2, 2), order = "C")

# Briefly compare how knowing this fact affects performance
copy_col = function(x) {
  n = length(x)
  arr = matrix(data = 0., nrow = n, ncol = n)
  for (i in 1:n) {
    arr[, i] = x
  }
  return(NULL)
}

copy_row = function(x) {
  n = length(x)
  arr = matrix(data = 0., nrow = n, ncol = n)
  for (i in 1:n) {
    arr[i, ] = x
  }
  return(NULL)
}

# Let's quickly benchmark the performance difference between these functions
set.seed(17)
x = rnorm(n = 10000)

start_time = Sys.time()
copy_col(x)
print(paste("copy_col time:", Sys.time() - start_time))

start_time = Sys.time()
copy_row(x)
print(paste("copy_row time:", Sys.time() - start_time))

#### Data Pre-Processing ####

## Exercise 1 ##

# Convert our training matrices into a row-major format so they're ready to be
# used by Keras
X_train = array_reshape(X_train, dim=c(dim(X_train), 1), order="C")

## Exercise 2 ##

# Normalize the data using standard techniques

# HINT: it is valid with images to simply use the global mean and standard deviation
mu = mean(X_train)
sigma = sd(X_train)

X_train = (X_train - mu) / sigma
X_test = (X_test - mu) / sigma

# Finally we're going to one-hot encode our target vectors
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)

#### Convolutional Networks ####

## Exercise 3 ##

# For this exercise, I want you to use your knowledge of the Keras API to 
# train a convolutional network with the following properties

# One convolutional layer with 32 filters, (3, 3) kernel size and padding = "same"
# One global pooling layer with default settings
# One dense layer with 64 nodes
# Standard SGD optimizer
# ReLU activation function for each layer
# Run the model for 3 epochs with a 25% validation split

height = dim(X_train)[2]
width = dim(X_train)[3]
channels = dim(X_train)[4]

# Finally we're you're done, evaluate the model on the test set
model = keras_model_sequential() %>% 
  layer_conv_2d(filters=32, kernel_size=7, activation = "relu",
                input_shape = c(NULL, height, width, channels),
                padding="same") %>% 
  layer_global_average_pooling_2d() %>% 
  layer_dense(units=64, activation = "relu") %>% 
  layer_dense(units=dim(y_train)[2], activation = "softmax")
  

# Compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(),
  metrics = c("accuracy")
)

# Fit to the data
res = model %>% fit(
  x = X_train, y = y_train, epochs = 2, verbose = 1
)

#### Kernel Size ####

## Exercise 4 ##

# Using the kernel size that was assigned to your group, implement a neural
# network with the same parameters as before except with the specified 
# kernel size. Make sure that you are typing out the full model and not just
# copying-and-pasting what you have done previously; report the performance 
# on the test set and compare this to the previous model we trained

#### Additional Convolutional Layers ####

## Exercise 5 ##

# For the final exercise of this section, I want you to add one more
# convolutional layer (w/ 32 filters) and a default max_pooling layer between it
# Use a kernel size of 7 and keep everything else the same; remember to type out
# the full model so that you're getting used to the API and repor the model's 
# final performance on the test set; compare this to how we did with a single
# layer convolutional model
model = keras_model_sequential() %>% 
  layer_conv_2d(filters=32, kernel_size = 7, activation = "relu",
                input_shape = c(NULL, height, width, channels),
                padding = "same") %>% 
  layer_max_pooling_2d() %>% 
  layer_conv_2d(filters=32, kernel_size = 7, activation = "relu", padding="same") %>% 
  layer_global_average_pooling_2d() %>% 
  layer_dense(units=64, activation = "relu") %>% 
  layer_dense(units=dim(y_train)[2], activation = "softmax")

#### Transfer Learning ####

# This is not an exercise, I just want to make you aware of how you can employ
# this technique because it's a common practice used for real-world problems
# Get the initial VGG16 weights
vgg <- application_vgg16(include_top = FALSE,
                         input_shape = c(128, 128, 3),
                         pooling = 'max')

# Freeze the VGG16 weights
freeze_weights(vgg)

# Now we can define our model
model <- keras_model_sequential() %>% 
  vgg %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dense(units = 1, activation = 'sigmoid')

# Let's take a look at our model
summary(model)
