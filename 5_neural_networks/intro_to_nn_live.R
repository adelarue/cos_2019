# This is the script used for the live coding session. Some of the 
# portions have already been filled for you, but others you will have to do
# yourself as exercises. These will be clearly marked

#### Data ####
# We need to load the Keras library and get the Boston data
library(keras)
boston = dataset_boston_housing()

# Get the training and test sets
X_train = boston$train$x
y_train = boston$train$y
X_test = boston$test$x
y_test = boston$test$y


#### Exploratory Analysis ####

# Before we start working with neural networks, to both gain some intuition 
# about what's going on with this data as well as give a good warm-up to R,
# let's start with some exercises; I have provided the column names for the
# Boston data
library(tidyverse)
col_names = c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
              'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')

train_df = as_tibble(X_train)
colnames(train_df) = col_names
train_df$PRICE = y_train

## Warm-up Exercise 1 ##

# One of the features in our model CHAS is binary -- it indicates whether 
# the property is "bounded" by the Charles River or not; for our first warm-up
# exercise, I would like you to tell me what how being by the Charles River
# affects the median selling price of a home, additionally, do this using
# a "tidy" methodology; once you have done this, think of why you are seeing
# this result; we will briefly discuss this as a class

## Warm-up Exercise 2 ##

# For our final warm-up, I also want to remind us how to use gglot as well as
# combining this with dplyr commands. One of the features in the data is RAD
# this indicates the "index of accessibility" to radial highways. One of these
# indices is 24. Additionally there is a feature called AGE which defines the
# proportion of houses built before 1940. For this exercise, I want you to 
# focus on instances where the RAD is 24 and then plot their relation of the
# home AGE to its price. Tell me what you see.

#### Data Pre-Processing ####

# A standard practice in ML is to normalize the data so that each column
# has zero mean and a variance of 1

# Get the mean and sd for each column
mu_vect = apply(X_train, 2, mean)
sigma_vect = apply(X_train, 2, sd)

# Normalize the training and testing data
X_train = sweep(X_train, 2, mu_vect, "-", check.margin = F)
X_train = sweep(X_train, 2, sigma_vect, "/", check.margin = F)

X_test = sweep(X_test, 2, mu_vect, "-", check.margin = F)
X_test = sweep(X_test, 2, sigma_vect, "/", check.margin = F)

#### Keras API ####
# Now that we've done some simple data preparation, we're ready to introduce
# the Keras API. It has three major components. I will provide a simple 
# example of how to use it and then we will run some exercises so you have
# a chance to practice with the API as well as gaining some intution about 
# neural networks
model = keras_model_sequential()

# The first component of the Keras API is defining a model. This can be done
# by typing
model %>% 
  layer_dense(units = 32, activation = "relu",  
              input_shape = dim(X_train)[2]) %>% 
  layer_dense(units = 1, activation = "linear")

# After we have defined our model, we can look at what we have instantiated by
# typing
summary(model)

# The next thing a Keras model needs is a compiler -- specifically it needs
# to know how the model should be optimized
model %>% compile(
  loss = 'mse',
  optimizer = optimizer_sgd()
)

# Finally we need to tell the API how we want to train the model. This can 
# be done by typing
model %>% fit(
  x = X_train, y = y_train,
  epochs = 10, verbose = 1,
  validation_split = 0.25,
  batch_size = 128
)

model %>% evaluate(X_test, y_test)

# And that's all there is to defining and training a neural network in Keras.
# Now let's do some exercises that give you a chance to work with the API
# as well as gaining some intuition about key hyper-parameters

#### Learning Rate ####

## Exercise 1 ##

# The first hyper-parameter we will focus on is the learning rate. This defines
# how much we update the inferred parameters in our model at each iteration. 
# Using the learning rate that was specified for your group, I want you to 
# train the exact same neural network as we did before. As a hint, 
# type ?optimizer_sgd; this might also lead to some other questions. Also make
# sure to TYPE out the code, do not just copy from what we did previously and
# try to do it from memory; this will force you to try to understand what each
# part of the model is doing and how it all flows with another. When you're
# finished, we will discuss the results that we're seeing

#### Multi-Layered Neural Networks ####

## Exercise 2 ##

# Another key hyper-parameter is the number of layers to use in your model
# So, for our next exercise, add one more layer to the model and report the 
# results. Namely, plot the loss profile, determine the final validation loss, 
# and compare the results to the model with only one layer. When adding layers,
# keep the number of units the same and use a learning rate of 1e-3.

# HINT: To plot the loss profile, you can save the training history from the
# fit command to a variable

#### Adding More Units ####

## Exercise 3 ##

#Another hyper-parameter that can be changed is the number of nodes or units 
#to have for a particular layer of a neural network. For this exercise, using 
#the single-layer architecture, fit a layer with 512 nodes. Evaluate this 
#model both in and out-of-sample

#### Dropout ####

## Exercise 4 ##

# Using your knowledge of the Keras API as well as the architecture for the 
# model that we created for Exercise 3, add a layer_dropout with a rate of 0.7
# to the model to regularize it
