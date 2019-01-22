# Load the packages we'll need for this session
library(keras)
library(tidyverse)

#### Data ####
imdb <- dataset_imdb()

# Get the train-test split
X_train <- imdb$train$x
y_train <- imdb$train$y
X_test <- imdb$test$x
y_test <- imdb$test$y

#### Exploratory Analysis ####

# The data is in a somewhat unusual format; let's take a look at how it's 
# currently formated and how we'll need to adjust it to be used by Keras
X_train[[1]]

## Exercise 1 ##

# Typically, natural language processing problems are very skewed -- namely, a
# small number of words cover most of the uses in the data. If this is true, 
# then this typically implies we can shrink the vocabulary without paying a 
# big price in terms of model performance while significantly speeding up 
# computation time (similar to PCA). To check this hypothesis, our first 
# exercise for this project will be to look at the distribution of words in the
# data. Specifically, I would like you to create a histogram displaying the word
# usage distribution in each of the reviews. To do this, you will need to 
# represent the training data as a DataFrame and use this DataFrame to make a 
# histogram.

# Fill in the create_word_df function

# An example of a valid output for this function would look like

# sample_num_vect | words
# -----------------------
# 1               | 15
# 1               | 27
# 1               | 3
# ...

create_word_df = function(word_vect, sample_num) {
  # Repeat the sample_num an appropriate number of times
  sample_num_vect = rep(sample_num, length(word_vect))
  
  # Return a DataFrame with two columns: the sample_num_vect and the words
  return(tibble(sample_num_vect = sample_num_vect, words = word_vect))
}

# Apply the user-created function to each element of the X_train list
# hence we will need purrr's map (please think about what this code is doing
# you will be using it quite a bit in the next session)
word_df = map2_df(.x = X_train, .y = 1:length(X_train), 
                  .f = ~ create_word_df(.x, .y))

# Using the DataFrame we just created to plot the distribution of words in 
# the data
word_df %>% 
  ggplot(aes(x=words)) + 
  geom_histogram()

## Exercise 2 ##

# Before we can use a word embedding model, each of the word vectors need to
# have the same size; therefore, we need to see the distribution of review
# lengths in the data. Please generate a boxplot using ggplot and dplyr
# techniques

# HINT: n() is a function which tells you how many rows are in a group

# HINT: When calling boxplots in ggplot with only one group, it is not
# necessary to provide an x variable
word_df %>% 
  group_by(sample_num_vect) %>% 
  summarize(review_len = n()) %>% 
  ggplot(aes(x="", y = review_len)) + 
  geom_boxplot()

#### Data Pre-Processing ####

# Grab our data again with the constraints described above
imdb <- dataset_imdb(num_words = 5000, maxlen = 500, seed = 17)
X_train <- imdb$train$x
y_train <- imdb$train$y
X_test <- imdb$test$x
y_test <- imdb$test$y

# Pad our sequences if they're less than 500
X_train <- pad_sequences(sequences = X_train, maxlen = 500)
X_test <- pad_sequences(sequences = X_test, maxlen = 500)

# Let's check to make sure our data looks correct
X_train[1, ]

#### Word Embeddings ####

## Exercise 3 ##

# For this exercise, using the Keras API and the following layers:
# layer_embedding and layer_global_average_pooling_1d() generate a neural network
# with the following hyper-parameters

# Embedding layer with 32 dimensional word vectors
# Default layer_global_average_pooling_1d()
# One dense layer with 64 units
# Standard SGD optimizer
# Binary cross-entropy loss function
# Train for five epochs

# When this is done, evaluate the model out-of-sample

# HINT: when you have a binary output, the final activation function is "sigmoid"
# and only has one unit
model = keras_model_sequential() %>% 
  layer_embedding(input_dim = 5000, output_dim = 32, input_length = 500) %>% 
  layer_global_average_pooling_1d() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer=optimizer_sgd()
)

res = model %>% fit(
  x = X_train, y = y_train,
  epochs = 5, batch_size = 128
)

#### Optimizers ####

## Exercise 4 ##

# Using the optimization algorithm that was assigned to your group, define and
# train an embedding model that has the same specifications as before; plot
# the history and report the final test error. Remember to type out the
# neural network and do not just copy-paste

#### Vector Representation ####

## Exercise 5 ##

# In our previous model, we just arbitrarily chose the words to be represented
# by 32-dimensional vectors; let's see how sensitive our model is to that
# choice; using the values assigned to your groups and using the Adam
# optimizer, train a sentiment analysis model

