This directory holds course materials for Session 5, Deep Learning. The main focus of this lesson is to introduce neural networks 
and how to implement them programmatically using TensorFlow/Keras. 

# Pre-Assignment

## Project Submission

Please submit a `flexdashboard` in `.html` format on Stellar. Your dashboard should be suitable for a presentation of no longer than 5 minutes. This time limit will be rigorously enforced. Your dashboard should include the name of both of your partners, each of whom should turn in a copy. 

Please note that your dashboard is due Monday evening at 10pm, PRIOR to Session 5. No exceptions will be made, and no late submissions will be accepted. 

## Technical Preparation

If you do not already have the Anaconda distribution of python, you will also have to download it. You can get the distribution at https://www.anaconda.com/download/. You will need to do this before installing the Keras package because the R version of Keras calls python functions. 

Afterwards please download the following two packages in R using the commands below:

```{r}
install.packages("keras")
```

Next we need to get the core packages that the Keras API relies upon installed on your machine. To do this, execute the commands
```{r}
library(keras)
install_keras()
```

Finally, to ensure that Keras has been properly installed and that it’s ready to be used for the lesson, please execute the following commands. 

```{r}
library(keras)

# Generate some toy data 
x <- runif(n = 100)
y <- (0.1 * x) + 0.3

# Define our computational graph
model <- keras_model_sequential() %>% 
  layer_dense(units = 1, input_shape = c(NULL, 1))

# Define the model compiler
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_sgd()
)

# Fit the model to the data
model %>% fit(
  x = x, y = y, epochs = 10, verbose = 1
)
```

If there are no errors, take a screenshot of the output in your console and post the result to Stellar. 
You should see ten progress bars and a loss next to them. If you do have any trouble with getting 
the packages to work, please email me at zblanks@mit.edu and we'll figure out a solution.

# Possible Errors
Sometimes installation issues arise with using Keras in RStudio. If you would prefer, I have provided Jupyter notebooks
that can be used to follow along with the lecture in Python. To install Keras on your machine you will need to execute
the following command:

`conda install keras`

This will cause Anaconda to download the appropriate files which can then be used to follow the lecture in Python.
