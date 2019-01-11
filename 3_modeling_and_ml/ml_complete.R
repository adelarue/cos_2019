## Statistical Modelling and Machine Learning in R ##

#### Introduction ####
# Within the world of supervised learning, we can divide tasks
# into two parts. In settings where the response variable is
# continuous we call the modelling *regression*, and when the
# response is categorical we call it *classification*. We will
# begin with regression to understand what factors influence
# price in the AirBnB data set.	

# Let's start by loading the data (after first setting the
# correct working directory). We'll use the 'listings.csv' file
# for now. Since we'll be wrangling and visualizing, we'll
# also load the `tidyverse` package. (Instead of `tidyverse`,
# it also works to load `tidyr`, `dplyr`, and `ggplot2` as
# we saw last session.)	
library(tidyverse)
listingsOrig <- read.csv("data/listings.csv", stringsAsFactors = FALSE)
# Note that when we do a lot of data wrangling, sometimes it's nice
# to keep a copy of the original data set so we don't have to read it in again.

#### Data Wrangling ####
# We're going to prepare the data set a bit so that we can build
# models to predict the price of an Airbnb in Boston. 
# As a review, we need to change the price column
# to a numeric form.
listings <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price)))
summary(listings$price) # Check to make sure things worked

# Now, which variables may be predictive of price? We can
# use `names(listings)` to get a look at all the variable names.
names(listings)

# Let's begin by looking at the relationship between
# `listings$accommodates` and `listings$price`. As a first look
# (using our ggplot tools from last time):
listings %>% 
  ggplot(aes(x = accommodates, y = price)) + 
  geom_point()

# Looks like there are some outliers on both axes. There are fancier 
# ways to deal with this statistically, but for today let's just get
# rid of the outliers and fit a model on the cleaner data:	
listings <- listings %>%	
  filter(accommodates <= 10, price <= 1000)

# Where are these Airbnbs, and what type are they? This information
# is included in the `property_type` and `neighbourhood_cleansed`
# variables.
sort(table(listings$property_type))
sort(table(listings$neighbourhood_cleansed))
listings <- listings %>%
  filter(property_type %in% c("Apartment", "House", "Bed & Breakfast",
                              "Condominium", "Loft", "Townhouse"),	
         !(neighbourhood_cleansed %in%
             c("Leather District","Longwood Medical Area")))

# Next, let's check if any of variables in our data set have
# missing values.  We can use the following handy command:
sort(colSums(is.na(listings)))

# Note that some of the variables are missing almost all of their values,
# so we can filter these out.  Let's filter out all variables with
# >90% missing values using the `select_if` function:
?select_if
listings <- listings %>%
  select_if(function(x) sum(is.na(x)) < 0.9 * nrow(listings))

# It seems like we still have a fair amount of missing values
# for the review_scores variables, so the best solution would be
# to do missing data imputation here.  There are many good R packages
# for this, for example `mice` for multiple imputation or `missForest`
# for random forest imputation.  However, two quick and fast solutions
# are:
# 1) Complete-case analysis: Drop all rows with missing values, using
# the `na.omit()` command
# 2) Mean/median impute: Impute the mean (or median) for each missing value
# using the `coalesce(., median(., na.rm = TRUE))` command
# Let's try (2):
listings <- listings %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(function(x) coalesce(x, median(x, na.rm = TRUE)))
sum((is.na(listings))) # Check that there are no more missing values

#### Exercise 1 ####

# 1. Working with dates
# The `mutate_at` command is very similar to `mutate_if` and
# `mutate_all` commands, but you can specify the variables that
# you want to change directly, for example, we can say:
# listingsOrig %>%
#   mutate_at(vars(city, market), as.factor)
# Using the `mutate_at` and `as.Date()` commands,
# convert the variables first_review and last_review to
# dates, and fill in the missing values with the median dates. ->
head(listingsOrig$first_review)
head(listingsOrig$last_review)
?as.Date
# Both are in the same format, so we can use mutate_at command
# with the same conversion: function(x) as.Date(x, "%Y-%m-%d")
listingsNew <- listingsOrig %>%
  mutate_at(vars(first_review, last_review),
            function(x) as.Date(x, "%Y-%m-%d")) %>%
  mutate_at(vars(first_review, last_review),
            function(x) coalesce(x, median(x, na.rm = TRUE)))
summary(listingsNew$first_review)
summary(listingsNew$last_review)
?mutate_at

# 2. Converting to NA's
# Sometimes the data set can be tricky, and we need to specify
# which values are NA's for ourselves.  For instance, look at
# the `host_response_rate` column.  Convert the appropriate 
# values to NA's using the `na_if()` command and impute the
# missing values with the median host_response_rate. ->
table(listingsOrig$host_response_rate)
?na_if
listingsNew <- listingsOrig %>%
  mutate(host_response_rate = na_if(host_response_rate, "N/A")) %>%
  mutate(host_response_rate = as.numeric(gsub("%", "", host_response_rate))) %>%
  mutate(host_response_rate =
           coalesce(host_response_rate, median(host_response_rate, na.rm = TRUE)))
table(listingsNew$host_response_rate, useNA = "always")

# In dplyr, another way of writing functions instead of
# "function(x) <code here with x as a variable>" is
# "~ <code here with . as a variable".  In summary, this is
# our data pipeline from the beginning using this new notation:
listings <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price))) %>%
  filter(accommodates <= 10, price <= 1000) %>%
  filter(property_type %in% c("Apartment", "House", "Bed & Breakfast",
                              "Condominium", "Loft", "Townhouse"),	
         !(neighbourhood_cleansed %in%
             c("Leather District","Longwood Medical Area"))) %>%
  select_if(~sum(is.na(.)) < 0.9 * nrow(listingsOrig)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_all(~coalesce(., median(., na.rm = TRUE)))
# The great thing is, we can just run the previous command and be caught
# up to speed with all of the data wrangling stuff.  Woohoo dplyr!

## Regression
#### Linear Regression ####
# Before we build a linear model to predict price, we'll reserve
# a portion of our data to be a test set. There are lots of ways
# to do this. We'll use the `caTools` package.
# install.packages("caTools")
library(caTools)
set.seed(123)
spl <- sample.split(listings$price, SplitRatio = 0.7)
listingsTrain <- subset(listings, spl == TRUE)
listingsTest <- subset(listings, spl == FALSE)

# We now have two data frames which contain the training and
# testing data, respectively.  The command `set.seed(123)`
# ensures that our random splits will be the same.  (There is a random
# number generator in R initialized with the value 123.) 

# In R, we specify a model structure and then use the corresponding
# function to tell R to optimize for the best-fitting model.
# For linear regression, the function is `lm()`:	
lm1 <- lm(price ~ accommodates, data = listingsTrain)
# We'll talk more about the '~' notation soon	

# Let's check out the lm1 object. It is a list of a bunch of relevant
# information generated by the `lm()` function call, and we can use
# the `$` to view different elements. 
names(lm1)
lm1$coefficients

# The function `summary()` is overloaded for many different objects
# and often gives a useful snapshot of the model as well.
summary(lm1)

# There we go, this is more useful! First, let's look at the section
# under "Coefficients". Notice that R automatically adds an intercept
# term unless you tell it not to (we'll see how to do this later).
# In the "estimate" column, we see that the point estimates for the
# linear model here say that the price is \$55.20 plus \$37.79 for
# every person accommodated. Notice the '***' symbols at the end of
# the "(Intercept)" and "accommodates" rows. These indicate that
# according to a statistical t-test, both coefficients are extremely
# significantly different than zero, so things are okay from an
# inference perspective.	

# As another check on inference quality, let's plot the fitted line.
# There are some nifty functions in the `modelr` package that make
# interacting with models easy in the `tidyr` and `dplyr` setting.
# We'll use `modelr::add_predictions()` here.	
# install.packages("modelr")
library(modelr)
listingsTrain %>%	
  add_predictions(lm1) %>%	
  ggplot(aes(x = accommodates)) +	
  geom_point(aes(y = price)) +	
  geom_line(aes(y = pred), color = 'red')

# Nice. We can also remove the linear trend and check the residual
# uncertainty, which we'll do here using `modelr::add_residuals()`.
# This is helpful to make sure that the residual uncertainty looks
# like random noise rather than an unidentified trend.	
listingsTrain %>%	
  add_residuals(lm1, var = "resid") %>%	
  ggplot(aes(x = accommodates, y = resid)) + 
  geom_point()

# Since we have finitely many values, maybe box plots tell a better story:	
listingsTrain %>%	
  add_residuals(lm1, var = "resid") %>%	
  group_by(as.factor(accommodates)) %>%	
  ggplot(aes(x = as.factor(accommodates), y = resid)) + 
  geom_boxplot()

# Things are pretty centered around zero, with the exception of
# 9- & 10-person accommodations. Maybe the model doesn't apply
# so well here, why might that be?	

# Let's now take a look at out-of-sample performance. We'll plot the
# predictions versus the actuals as we did before, but this time
# for the test data.	
listingsTest %>%
  add_predictions(lm1) %>%	
  ggplot(aes(x = accommodates)) +	
  geom_point(aes(y = price)) +	
  geom_line(aes(y = pred), color = 'red')	

# Now, what if we wanted to *quantify* how well the model predicts
# these out-of-sample values? We'll look at the out-of-sample R^2 (OSR^2),
# also known as the "coefficient of determination":

# OSR^2 = 1 - SSE / SST,
# where:
# SSE = $\sum_{i=1}^n (\hat{y}_i - y_i)^2$  ("Sum of Squares Error")
# SST = $\sum_{i=1}^n (\bar{y} - y_i)^2$  ("Sum of Squares Total")

# In these equations, $\hat{y}_i$ is the predicted value for test
# observation $i$, $y_i$ is the actual value, $n$ is the size of
# the test set, and $\bar{y}$ is the mean of $y_i$ in the training set.
# Let's code this up.
pred_test <- predict(lm1, newdata = listingsTest)
OSR2 <- 1 - sum((pred_test - listingsTest$price) ^ 2) /
  sum((mean(listingsTrain$price) - listingsTest$price) ^ 2)

# The in-sample R^2 value should agree with the "Multiple R-squared"
# value returned by summary().  Are we overfitting?
summary(lm1)
OSR2

# Let's save this into a data.frame to be accessed later
results <- data.frame(model = "original", R2 = summary(lm1)$r.squared,
                      OSR2 = OSR2, stringsAsFactors = F)

#### Exercise 2 ####
# 1. Building a simple model
# Regress price on review_scores_rating. Plot the regression
# line and the actual training points, and find the in-sample
# R^2. (Read below for more details if you need them.)

# DETAILS:
# -Use `lm()` to learn the linear relationship
# -In-sample R^2 is one of the outputs of `summary()`
# -Use `add_predictions()` and ggplot tools for the plotting ->
mod <- lm(price ~ review_scores_rating, data = listingsTrain)
summary(mod)$r.squared

listingsTrain %>%
  add_predictions(mod) %>%
  ggplot(aes(x = review_scores_rating)) + 
  geom_line(aes(y = pred)) +
  geom_point(aes(y = price), color = "dark blue")

# 2. Adding more varibles
# Try to beat the out-of-sample performance of the
# price ~ accommodates model by adding other variables. You can use
# `names(listings)` to explore potential predictors.
# If you start getting  errors or unexpected behavior, make sure
# the predictors are in the format you think they are.
# You can check this using the `summary()` and `str()` functions
# on listings$<variable of interest>. ->
better_lm <- lm(price ~ accommodates + neighbourhood_cleansed,
                data = listingsTrain)
summary(better_lm)
pred_test <- predict(better_lm, newdata = listingsTest)
OSR2_new <- 1 - sum((pred_test - listingsTest$price) ^ 2) /
  sum((mean(listingsTrain$price) - listingsTest$price) ^ 2)
OSR2_new
results # Original model R^2 and OSR^2

# 3. Median Regression
# # Since we're dealing with data on price,
# we expect that there will be high outliers. While least-squares
# regression is reliable in many settings, it has the property 
# that the estimates it generates depend quite a bit on the outliers.
# One alternative, median regression, minimizes *absolute* error
# rather than squared error. This has the effect of regressing
# on the median rather than the mean, and is more robust to outliers.
# In R, it can be implemented using the `quantreg` package.

# For this exercise, install the quantreg package, and compare
# the behavior of the median regression fit (using the `rq()`)
# function) to the least squares fit from `lm()` on the original
# listings data set given below which includes all the price outliers.
data <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price)))
# Hint: Enter ?rq for info on the rq function.

# DETAILS:
# -Split into training/testing set
# -Fit the median and linear regression models
# -Plot the two lines together using `gather_predictions`,
# which is very similar to the `add_predictions` function
# that we saw in class. 
# -Add "color = model" as a geom_line aesthetic
# to differentiate the two models in the plot. ->

set.seed(123)
split <- sample.split(data$price, SplitRatio = 0.7)
dataTrain <- subset(data, split == TRUE)
dataTest <- subset(data, split == FALSE)

# install.packages("quantreg")
library(quantreg)
mr_model <- rq(price ~ accommodates, data = dataTrain)
lm_model <- lm(price ~ accommodates, data = dataTrain)
summary(mr_model)
summary(lm_model)

dataTest %>%
  gather_predictions(mr_model, lm_model) %>%
  ggplot(aes(x = accommodates)) +	
  geom_point(aes(y = price)) +	
  geom_line(aes(y = pred, color = model))

## Summary and More about Formulas	
# First, let's review the pattern, because it can be generalized to
# a whole bunch of more complex models. We asked the questions:
# How does listing price depend on the number of people it
# accommodates? How well does accommodation size predict price?
# Since we were interested in prediction, we reserved part of our
# data as a test set. We then chose to use a linear model to
# answer these questions, and found the corresponding function `lm()`.
# This function, and modelling functions in general, takes as arguments:

# * Data on the response and predictor variables,
# usually through a `formula` object	
# * Model parameters (in the case of `lm()`, we used all
# the default values)	

# R then automatically found the "best" linear model by computing
# the least squares estimate, and returned a `lm` object, which
# was a list including information about:

# * Fitted coefficients	
# * Residuals	
# * Statistical significance	
# * And more...	

# We interacted with the model to evaluate goodness-of-fit and
# out-of-sample performance. In our case, we used the `caTools`
# and `dplyr` framework to do this cleanly.	

# We didn't say too much about the `price ~ accommodates` syntax.
# Many modelling functions in R take `formula`s as arguments,
# together with a `data` argument. The `data` argument specifies
# the data frame, and the `formula` argument tells the model
# which are the responses and which are the predictors. We'll
# play around with formulas in the exercises, but here are a
# few helpful pointers:	

# * The `~` separates the response (on the left) from the
# predictors (on the right)	
# * Predictors are separated with a `+`	
# * Use `.` on the right-hand side to include all predictors
# in a given data frame	
# * You can also use `.-x` to include all predictors except `x`	
# * To include interactions between variables, use the `*` symbol.
# For example: `y ~ x + z + x*z` expresses the form:
# "regress `y` on `x`, `z`, and on `x` interacted with `z`	
# * To exclude the intercept term, include `-1` or `+0` on
# the right-hand side	
# For more detailed info, see
# <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html>.

#### Model Selection and Tuning	####
# Let's work a bit harder on predicting price, this time using more
# than one predictor. In fact, we'll add a bunch of predictors to
# the model and see what happens.	

# As one set of predictors, the column listings$amenities looks interesting:
listings$amenities[1:2]

# This could be good predictive information if we can separate out which
# listing has which amenity. Our goal here is to turn the amenities
# column into many columns, one for each amenity, and with logical
# values indicating whether each listing has each amenity. This is
# just a bit tricky, so I've written a function called `expand_amenities`
# that will do this for us. We need to `source()` the file that has this
# function in it, and then we'll call it on the `listings` data frame.	
source("expand_amenities.R")
listingsBig <- expand_amenities(listings)

# In total, we'll use all of these predictors:
# * accommodates	
# * property_type	
# * review_scores_rating	
# * neighbourhood_cleansed	
# * accommodates*room_type	
# * property_type*neighbourhood_cleansed 	
# * review_scores_rating*neighbourhood_cleansed 	
# * accommodates*review_scores_rating	
# * All columns created from the amenities column	

# Note that whenever we include a non-numeric (or categorical)
# variable, R is going to create one indicator variable for all
# but one unique value of the variable. We'll see this in the
# output of `lm()`.	

# First, let's separate our new data set `listingsBig`
# into training and test sets:
set.seed(123)
spl <- sample.split(listingsBig$price, SplitRatio = 0.7)
listingsBigTrain <- subset(listingsBig, spl == TRUE)
listingsBigTest <- subset(listingsBig, spl == FALSE)

# To get R to learn the model, we need to pass it a formula.
# We don't want to write down all those amenity variables by hand.
# Luckily, we can use the `paste()` function to string all the
# variable names together, and then the `as.formula()` function
# to translate a string into a formula.	
amenities_string <- listingsBigTrain %>%
  select(starts_with("amenity")) %>%
  names() %>%
  paste(collapse = " + ")
amenities_string # Taking a look to make sure things worked	

big_formula <- as.formula(paste("price ~ accommodates + accommodates*room_type", 
                                "property_type + neighbourhood_cleansed", 
                                "property_type*neighbourhood_cleansed",
                                "review_scores_rating*neighbourhood_cleansed", 
                                "accommodates*review_scores_rating", 
                                amenities_string, sep = " + "))
big_formula

# Now we can use the `lm()` function:
lm2 <- lm(big_formula, data = listingsBigTrain)	
summary(lm2)
# What happens when we compare in-sample and
# out-of-sample prediction performance?
R2_2 <- summary(lm2)$r.squared
pred_test <- predict(lm2, newdata = listingsBigTest)
OSR2_2 <- 1 - sum((pred_test - listingsBigTest$price) ^ 2) /
  sum((mean(listingsBigTrain$price) - listingsBigTest$price) ^ 2)

# Let's add these values as a new row to our results data.frame
# that we created earlier
results <- results %>%
  rbind(list(model = "big", R2 = R2_2, OSR2 = OSR2_2))
results

# This is way better than our initial model, but we've got an overfitting
# problem here, meaning that the training error is smaller than the
# test error. The model is too powerful for the amount of data we have.
# Note that R recognizes this by giving warnings about a "rank-deficient fit."	


#### Regularized/Penalized Regression	####
# But is there still a way to use the info from all these
# variables without overfitting? Yes! One way to do this is by
# regularized, or penalized, regression.	

# Mathematically, we add a term to the optimization problem that
# we're solving when fitting a model, a term which penalizes models
# that get too fancy without enough data. If we call $\beta$ the
# coefficient vector that we'd like to learn about for linear
# regression, then the regular regression we've worked with
# looks like	
# $$	
# \min_\beta \sum_{i=1}^n (y_i-x_i^T\beta)^2,	
# $$	
# but penalized regression looks like	
# $$	
# \min_\beta \sum_{i=1}^n (y_i-x_i^T\beta)^2 + \lambda ||\beta||.	
# $$	

# There are two types of flexibility within this framework:
# * Choice of norm, a structural decision, and	
# * Choice of $\lambda$, a parametric decision.	

# Two natural choices of norm are the Euclidean 1- and 2-norms.
# When we use the 2-norm, it's often called "ridge regression."
# We'll focus today on the 1-norm, or "LASSO regression". On a
# very simple level, both types of regression shrink all the
# elements of the unconstrained $\beta$ vector towards zero,
# some more than others in a special way. LASSO shrinks the coefficients
# so that some are equal to zero. This feature is nice because it helps
# us interpret the model by getting rid of the effects of many
# of the variables.	

# To do LASSO, we'll use the `glmnet` package. Of note, this package
# doesn't work very elegantly with the `tidyverse` since it uses
# matrix representations of the data rather than data frame
# representations. However, it does what it does quite well, and
# will give us a chance to see some base R code. Let's load the
# package and check out the function `glmnet()`. We can see the
# documentation from the command line using `?glmnet`.	
library(glmnet)
?glmnet

# Notice that `glmnet()` doesn't communicate with the data via
# formulas. Instead, it wants a matrix of predictor variables and
# a vector of values for the variable we're trying to predict,
# including all the categorical variables that R automatically
# expanded into indicator variables. Fortunately, R has a
# `model.matrix()` function which takes a data frame and gets it
# into the right form for `glmnet()` and other functions with this
# type of input.	

# First, we need to convert the data to a matrix for X and a vector for Y
# We'll use the big_formula that we made previously for our linear
# regression, but with the dependent variable removed.
model_formula <- as.formula(gsub("price", "", paste(big_formula)))
x <- model.matrix(model_formula, data = listingsBig)
y <- as.vector(listingsBig$price)

# Next, split into training/testing sets
set.seed(123)
spl <- sample.split(y, SplitRatio = 0.7)
xTrain <- x[spl, ]
xTest <- x[!spl, ]
yTrain <- y[spl]
yTest <- y[!spl]

# Finally, let's fit a our first LASSO model. There's a way to
# specify lambda manually, but let's just accept the default
# for now and see what we get.
lasso_price <- glmnet(xTrain, yTrain)

# This time the `summary()` function isn't quite as useful:
summary(lasso_price)

# It does give us some info, though. Notice that "lambda" is a
# vector of length 86. The `glmnet()` function has defined 86
# different values of lambda and found the corresponding optimal
# beta vector for each one! We have 86 different models here.
# Let's look at some of the coefficients for the different models.
# We'll start with one where lambda is really high:	
lasso_price$lambda[1]
nnzero(lasso_price$beta[, 1]) # How many coefficients are nonzero?	

# Here the penalty on the size of the coefficients is so high
# that R sets them all to zero. Moving to some smaller lambdas:	
lasso_price$lambda[10]	
lasso_price$beta[which(lasso_price$beta[, 10] != 0), 10]	

lasso_price$lambda[20]	
lasso_price$beta[which(lasso_price$beta[, 20] != 0), 20]

# And, to see the whole path of lambdas:	
plot.glmnet(lasso_price, xvar = "lambda")

# Here, each line is one variable. The plot is quite messy with
# so many variables, but it gives us the idea. As lambda shrinks,
# the model adds more and more nonzero coefficients.	

## Cross-Validation
# How do we choose which of the 86 models to use? Or in other words,
# how do we "tune" the $\lambda$ parameter? We'll use a similar idea
# to the training-test set split called cross-validation.	

# The idea behind cross-validation is this: what if we trained our
# family of models (in this case 86) on only some of the training data
# and left out some other data points? Then we could use those other
# data points to figure out which of the lambdas works best
# out-of-sample. So we'd have a training set for training all the
# models, a validation set for choosing the best one, and a test set
# to evaluate performance once we've settled on a model.	

# There's just one other trick: since taking more samples
# reduces noise, could we somehow take more validation set
# samples? Here's where *cross*-validation comes in.
# We divide the training data into groups called *folds*,
# and for each fold repeat the train-validate procedure on
# the remaining training data and use the current fold as a
# validation set. We then average the performance of each model
# on each fold and pick the best one.	

# This is a very common *resampling* method that applies in lots and
# lots of settings. Lucky for us the glmnet package has a very handy
# function called `cv.glmnet()` which does the entire process
# automatically. Let's look at the function arguments using `?cv.glmnet`.	

# The relevant arguments for us right now are	
# * x, the matrix of predictors	
# * y, the response variable	
# * nfolds, the number of ways to split the training set (defaults to 10)
# * type.measure, the metric of prediction quality. It defaults to
# mean squared error, the square of RMSE, for linear regression	

# Let's do the cross-validation:
lasso_price_cv <- cv.glmnet(xTrain, yTrain)
summary(lasso_price_cv) # What does the model object look like?	

# Notice the "lambda.min". This is the best lambda as determined by
# the cross validation. "lambda.1se" is the largest lambda such that
# the "error is within 1 standard error of the minimum."	

# There's another automatic plotting function for `cv.glmnet()`
# which shows the error for each model:	
plot.cv.glmnet(lasso_price_cv)

# The first vertical dotted line shows `lambda.min`, and the
# second is `lambda.1se`. The figure illustrates that we cross-validate
# to find the "sweet spot" where there's not too much bias (high lambda)
# and not too much noise (low lambda). The left-hand side of this graph
# is flatter than we'd sometimes see, meaning that the unpenalized model
# may not be too bad. However, increasing lambda increases
# interpretability at close to no loss in prediction accuracy!	

# Let's again compare training and test error. Because we are using
# glmnet we need to use the specialized `predict.cv.glmnet()` function:
?predict.cv.glmnet
pred_train <- predict.cv.glmnet(lasso_price_cv, newx = xTrain, s = "lambda.min")
R2_lasso <- 1 - sum((pred_train - yTrain) ^ 2) /
  sum((mean(yTrain) - yTrain) ^ 2)
pred_test <- predict.cv.glmnet(lasso_price_cv, newx = xTest, s = "lambda.min")
OSR2_lasso <- 1 - sum((pred_test - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)

# Let's add these as a row to our results data.frame
results <- results %>%
  rbind(list(model = "lasso", R2 = R2_lasso, OSR2 = OSR2_lasso))
results

# The overfitting problem has gotten better, but hasn't yet gone
# away completely. I added a bunch variables for dramatic effect
# that we could probably screen out before running the LASSO if we
# really wanted a good model.	

# One more note on cross-validation: the `glmnet` package has built-in
# functionality for cross-validation. In situations where that's not
# the case, `modelr::crossv_kfold()` will prepare data for
# cross-validation in a nice way.

#### Exercise 3 ####
# 1. The glmnet package is actually more versatile than just
# LASSO regression. It also does ridge regression (with the l2 norm),
# and any mixture of LASSO and ridge. The mixture is controlled
# by the parameter alpha: alpha=1 is the default and corresponds
# to LASSO, alpha=0 is ridge, and values in between are mixtures
# between the two (check out the formula using ?glmnet).
# One could use cross validation to choose this
# parameter as well. For now, try just a few different values of
# alpha on the model we built for LASSO using `cv.glmnet()`
# (which does not cross-validate for alpha automatically).
# How do the new models do on out-of-sample R^2? ->
mod_alpha_0 <- cv.glmnet(xTrain, yTrain, alpha = 0)
mod_alpha_0.5 <- cv.glmnet(xTrain, yTrain, alpha = 0.5)

pred_test_0 <- predict.cv.glmnet(mod_alpha_0, newx = xTest, s = "lambda.min")
pred_test_0.5 <- predict.cv.glmnet(mod_alpha_0.5, newx = xTest, s = "lambda.min")
OSR2_alpha_0 <- 1 - sum((pred_test_0 - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)
OSR2_alpha_0.5 <- 1 - sum((pred_test_0.5 - yTest) ^ 2) /
  sum((mean(yTrain) - yTest) ^ 2)
OSR2_alpha_0        
OSR2_alpha_0.5
results # Original model R^2 and OSR^2

## Classification	
# So far we've looked at models which predict a continuous response
# variable. There are many related models which predict categorical
# outcomes, such as whether an email is spam or not, or which digit
# a handwritten number is. We'll take a brief look at two of these:
# logistic regression and classification trees.	

#### Logistic Regression ####
# Logistic regression is part of the class of generalized linear
# models (GLMs), which build directly on top of linear regression.
# These models take the linear fit and map it through a non-linear
# function. For logistic regression this function is the logistic
# function, $f(x) = 1/(1+\exp(-x))$, which looks like this:	
xs <- seq(-10, 10, 0.25)
ys <- exp(xs) / (1 + exp(xs))
plot(xs, ys)

# Since the function stays between zero and one, it can be interpreted
# as a mapping from predictor values to a probability of being in one
# of two classes.	

# Let's apply this model to the `listings` data. Let's try to predict
# which listings have elevators in the building by price. To make sure
# we're asking a sensible question, we'll only consider apartments
# priced at $500 or less.
listingsGLM <- listingsBig %>%
  filter(property_type == "Apartment", price <= 500)
set.seed(123)
spl <- sample.split(listingsGLM$amenity_Elevator_in_Building, SplitRatio = 0.7)
listingsGLMTrain <- subset(listingsGLM, spl == TRUE)
listingsGLMTest <- subset(listingsGLM, spl == FALSE)

# One nice thing about using `sample.split` for classification is that
# it preserves the ratio of class labels in the training and testing sets.

# Instead of the `lm()` function, we'll now use `glm()`, but the
# syntax is almost exactly the same:	
l.glm <- glm(amenity_Elevator_in_Building ~ price,
             family = "binomial", data = listingsGLMTrain)
summary(l.glm)

# Again, we can add predictions to the data frame and plot
# these along with the actuals, although the result doesn't
# look nearly as clean:	
listingsGLMTest %>%
  mutate(pred = predict(l.glm, newdata = listingsGLMTest, type = "response")) %>%
  ggplot(aes(x = price)) + 
  geom_line(aes(y = pred)) + 
  geom_point(aes(y = amenity_Elevator_in_Building + 0))

# One way to get a more informative plot is by using the
# `logi.hist.plot()` function in the `popbio` package.	

# In the meantime, we can explore out-of-sample performance. 
# Ultimately, we want to predict whether or not a listing has an
# elevator. However, logistic regression gives us something a bit
# different: a probability that each listing has an elevator. This
# gives us flexibility in the way we predict. The most natural
# thing would be to predict that any listing with predicted
# probability above 0.5 *has* an elevator, and any listing with
# predicted probability below 0.5 *does not have* an elevator. But
# what if I use a wheelchair and I want to be really confident that
# there's going to be an elevator? I may want to use a cutoff value
# of 0.9 rather than 0.5. In fact, we could choose any cutoff value
# and have a corresponding prediction model.	

# There's a really nice metric that measures the quality of all
# cutoffs simultaneously: *AUC*, for "Area Under the receiver
# operating characteristic Curve." That's a mouthful, but the idea
# is simpler: For every cutoff, we'll plot the *false positive rate*
# against the *true positive rate* and then take the area under this
# curve. (A *positive* in our case is a listing that has an elevator.
# So a *true positive* is a listing that we predict has an elevator
# and really does have an elevator, while a *false positive* is a
# listing that we predict has an elevator and does *not* actually
# have an elevator.)	

# As the cutoff shrinks down from 1 to 0, the rate of total positives
# will increase. If the rate of true positives increases faster than
# the rate of false positives, this is one indication that the model
# is good. This is what AUC measures.	

# The `ROCR` package is one implementation that allows us to plot
# ROC curves and calculate AUC. Here's an example:	
# install.packages("ROCR")
library(ROCR)
pred_test <- predict(l.glm, newdata = listingsGLMTest, type = "response")	
pred_obj <- prediction(pred_test, listingsGLMTest$amenity_Elevator_in_Building)
# Creating a prediction object for ROCR	
perf <- performance(pred_obj, 'tpr', 'fpr')	
plot(perf, colorize = T) # ROC curve
performance(pred_obj, 'auc')@y.values # AUC - a scalar measure of performance	

# As you can see, the `performance()` function in the `ROCR` package
# is versatile and allows you to calculate and plot a bunch of
# different performance metrics.	

# In our case, this model gives an AUC of 0.7. The worst possible
# is 0.5 - random guessing. We're definitely better than random here,
# and could likely improve by adding more predictors.	

# We've covered basic logistic regression, but just as with linear
# regression there are many, many extensions. For example, we could
# do regularized logistic regression if we wanted to use many predictors,
# using the `glmnet` package.	

#### Classification Trees ####
# We will briefly explore classification trees (often referred to as
# CART, for Classification And Regression Trees), and then in the
# second half of the session we'll take a deeper dive.	

# A (binary) classification tree makes predictions by grouping similar
# observations and then assigning a probability to each group using the
# proportion of observations within that group that belong to the
# positive class. Groups can be thought of as nodes on a tree, and
# tree branches correspond to logical criteria on the predictor
# variables. There's a lot of neat math that goes into building the
# trees, but we won't get into that today. For now let's get
# familiarized by looking at a simple example. We need the
# `rpart` library.	
library(rpart)

# The model construction step follows the same established pattern.
# We use the modelling function `rpart()`, which takes a formula
# and a data frame (and optional parameters) as arguments.	
l.rpart <- rpart(amenity_Elevator_in_Building ~ price + 
                   neighbourhood_cleansed,
                 data = listingsGLMTrain)	
summary(l.rpart)	

# This is another case when the `summary()` function is less
# helpful. We can plot the resulting tree:	
library(rpart.plot)
prp(l.rpart)

# To evaluate the prediction accuracy of our classification tree,
# we count up the number of times each of the following occurs:
# Y = 1, prediction = 1 (True Positive)
# Y = 0, prediction = 1 (False Positive)
# Y = 1, prediction = 0 (False Negative)
# Y = 0, prediction = 0 (True Negative)
# A table that holds these values is called a "confusion matrix".
# Then, accuracy = (# True Positives + # True Negatives) / (Total # of observations)
# Let's calculate the training and testing accuracy for our model
pred_train <- predict(l.rpart)
confusionMatrixTrain <- table(listingsGLMTrain$amenity_Elevator_in_Building,
                              ifelse(pred_train > 0.5, "pred = 1", "pred = 0"))
accTrain <- sum(diag(confusionMatrixTrain)) / nrow(listingsGLMTrain)
pred_test <- predict(l.rpart, newdata = listingsGLMTest)
confusionMatrixTest <- table(listingsGLMTest$amenity_Elevator_in_Building,
                             ifelse(pred_test > 0.5, "pred = 1", "pred = 0"))
accTest <- sum(diag(confusionMatrixTest)) / nrow(listingsGLMTest)

# What is the baseline out-of-sample accuracy?
# This is just the frequency of the most common class in the training set.
table(listingsGLMTest$amenity_Elevator_in_Building)[1] /
  nrow(listingsGLMTest)

#### Tuning the CART model ####
# If we want to construct high accuracy decision tree models,
# then we need to tune the parameters.  CART has many parameters that 
# specify how the decision tree is constructed, but one of the
# most important is cp, the complexity parameter. 
# cp is a non-negative parameter which typically takes values
# like 0.1, 0.1, 0.01, 0.001, etc. (default = 0.01).
# It is the minimum complexity threshold that the CART algorithm uses
# to decide whether or not to make a split.  So:

# - If cp is low => low splitting threshold => big tree
# - If cp is high => high splitting threshold => small tree

# Similar to lambda in the LASSO model, cp controls the
# "complexity" of the model, so it important to tune it to avoid
# over-fitting or under-fitting. You can think of it like this:

# - If the tree is too big => too many splits => we have an over-fitting problem.
# - If the tree is too small => too few splits => we have an under-fitting problem.

# We want to find a tree in the middle, that is "just right".  
# The rpart package makes it easy to perform tune cp via cross-validation.
# Basically, we start out with a big tree, then "prune" it
# down to get the right sized tree.

# Let's begin by constructing a tree with a small cp parameter,
# which will vary depending upon the problem.  Here, let's do 0.001.  
treeBig <- rpart(amenity_Elevator_in_Building ~ price + neighbourhood_cleansed,
                 data = listingsGLMTrain,
                 cp = 0.001)
prp(treeBig)

# We can use the `printcp()` command to see the
# cross-validated error for different values of cp,
# where:
# "nsplit"    = number of splits in tree
# "rel error" = scaled training error
# "xerror"    = scaled cross-validation error
# "xstd"      = standard deviation of xerror
printcp(treeBig)

# Cool, so rpart automatically computes all of the cross-validated
# errors for trees with cp = 0.001 and up! We can also see these results
# visually using the `plotcp` command.  In this plot:
#  - size of tree = (number of splits in tree) + 1
#  - the dotted line occurs at 1 std. dev. above the minimum xerror
plotcp(treeBig)

# A rule of thumb is to select the cp value which first
# goes below the dotted line, and then prune the tree using
# this value.
treeFinal <- prune(treeBig, cp = 0.011)
prp(treeFinal)

# In this case, because the best cp value = 0.011 is very close to the 
# default cp value of 0.01, this tree is identical to the initial tree
# that we constructed.  This occurs because the best tree in this case
# is relatively simple.

# Note that we could have also computed the confusion matrices, training/testing
# accuracy, and baseline accuracy for our logistic regression model.

# We can also try looking at a more detailed graph of the tree
rpart.plot(treeFinal)
# Let's save the plot as a pdf
pdf("finalTree.pdf", width = 5, height = 2)
rpart.plot(treeFinal)
dev.off()

#### Exercise 4 ####
# 1. Add more variables to Logistic Regression
# Try to beat the out-of-sample performance for logistic
# regression of elevators on price by adding new variables.
# Compute the out-of-sample AUC of the final model,
# and plot the ROC curve.  ->
l.glm_2 <- glm(amenity_Elevator_in_Building ~
                 price + neighbourhood_cleansed,
               family = "binomial", data = listingsGLMTrain)
summary(l.glm_2)

pred_test <- predict(l.glm_2, newdata = listingsGLMTest, type = "response")
pred_obj <- prediction(pred_test, listingsGLMTest$amenity_Elevator_in_Building)
perf <- performance(pred_obj, 'tpr', 'fpr')
performance(pred_obj, 'auc')@y.values
plot(perf, colorize = TRUE)
confusionMatrixTest_2 <- table(listingsGLMTest$amenity_Elevator_in_Building,
                               ifelse(pred_test > 0.5, "pred = 1", "pred = 0"))
accTest_2 <- sum(diag(confusionMatrixTest_2)) / nrow(listingsGLMTest)
confusionMatrixTest # CART model
accTest
confusionMatrixTest_2 # Logistic Regression Model
accTest_2

# 2. Tuning a CART model
# Let's try building a more complicated CART model and
# tuning the parameters. Using the below formula, build a
# CART model to predict `neighbourhood_cleansed` based on price
# and all of the amenities, tuning the cp parameter.
tree_formula <- as.formula(paste("neighbourhood_cleansed ~ price", 
                                 amenities_string, sep = " +  "))
# Plot the final tree with the option "varlen = 0"
treeBig2 <- rpart(tree_formula,
                  data = listingsGLMTrain,
                  cp = 0.001)
plotcp(treeBig2)
treeFinal2 <- prune(treeBig2, 0.0024)
prp(treeFinal2, varlen =  0)

#### Natural Language Processing ####
# Before we begin after the break, let's refresh our session.
# We have a lot of variables currently floating around in memory,
# so at this point a restart would be helpful.
# The "broom" icon in the Environment clears our workspace,
# and "Session > Restart R" clears any packages that we may have loaded.
# After we do this, remember to set the working directory again.
# (These steps are equivalent to closing and reopening RStudio.)
# Let's load in the following packages:
library(tidyverse)
library(modelr)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)
library(ROCR)

# So far, we've only used the numeric variables in our predictive models.
# But there can be a lot of useful information in free-text data too.
# In the next few sections, we'll explore how we can engineer
# useful features from free-text data for our predictive models.
# Here, we're going to switch our goal from predicting "price" to predicting
# "review_scores_rating" for an Airbnb in Boston.  

#### Data Preparation for Natural Language Processing ####
# First let's read in listings data set, and convert price column to numeric
listingsOrig <- read.csv("data/listings.csv")
listings <- listingsOrig %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price)))

# Now let's look at another data set.  Read in the reviews data set,
# making sure to set stringsAsFactors=FALSE
reviews <- read.csv("data/reviews.csv", stringsAsFactors = FALSE)

# View the data from `reviews.csv`. What does each row represent?
head(reviews, 3)

# Display the top 10 most reviewed Airbnb listings using
# the `reviews` data frame.
# Are the counts consistent with the data in the `listings` data frame?
sort(listings$number_of_reviews, decreasing = TRUE)[1:10]
sort(table(reviews$listing_id), decreasing = TRUE)[1:10]

# Later on, we will want to merge the two data frames: `listings` and `reviews`.
# The ID variables that we will use for this merge operation are
# `listings$id` and `reviews$listing_id`. Let's confirm that these
# columns will match up.  
head(sort(listings$id))
head(sort(unique(reviews$listing_id)))

# We will take `review_scores_rating` as the dependent variable that we
# are trying to predict.  This is the average customer rating of the Airbnb listing,
# on a scale of 0-100. Plot a simple histogram of `review_scores_rating`,
# and count the number of values != NA.
ggplot(data = listings) +
  geom_bar(aes(x = review_scores_rating))
sum(!is.na(listings$review_scores_rating))

# Next, create a new data frame with just the review scores data from `listings.csv`.
# Filter out rows with `review_scores_rating`=NA.
review_scores <- listings %>% 
  filter(number_of_reviews > 0) %>%
  select("LISTING_ID" = id, "RATING" = review_scores_rating) %>%
  filter(!is.na(RATING))
str(review_scores)

# Next, let's write a function to convert the listing rating from
# a scale of 0-100 to ("Terrible","Low","Mid","High","Perfect").
# The syntax for writing the function f(x) = x^2 is
# f <- function(x){
#   return(x*x)
# }
# Given an integer input rating from 0-100, our function should output:
# "Perfect"   if rating = 100
# "High"      if 95 <= rating < 99
# "Mid"       if 90 <= rating < 94
# "Low"       if 80 <= rating < 89
# "Terrible"  if rating <= 79
convert_rating <- function(rating) {
  if (rating == 100) {
    return("Perfect")
  } else if (rating >= 95) {
    return("High")
  } else if (rating >= 90) {
    return("Mid")
  } else if (rating >= 80) {
    return("Low")
  } else {
    return("Terrible")
  }
}
# Test a few values to make sure that the function works
convert_rating(100)
convert_rating(98)
convert_rating(90)
convert_rating(82)
convert_rating(46)

# To apply the `convert_rating()` function to each element in an array,
# we need to "vectorize" the function first.  Avoid using for-loops
# in R whenever possible because those are slow.
v_convert_rating <- Vectorize(convert_rating, c("rating"))
# Test a few values to make sure that the function works.
v_convert_rating(c(100, 32, 87))

# Compute the new column using a mutate call.
review_scores <- review_scores %>%
  mutate(RATING_TEXT = v_convert_rating(RATING))

# Take a look
table(review_scores$RATING_TEXT)

# These groupings look relatively well-balanced, which is desirable.
# For the NLP task, we will try to predict this rating category
# based upon the text data from `reviews.csv`.

# Let's go back to `reviews` data frame.
str(reviews)

# Currently, we have a data frame with 68275 rows.
# We would like to have a data frame with 2829 rows - one per each listing.
# We can use the `group_by()` and `summarize()` functions to transform
# the data frame in this way.
reviews_by_listing <- reviews %>%
  select(listing_id, comments) %>%
  group_by(listing_id) %>%
  summarize(all_comments = paste(comments, collapse = " "))

# Check out the updated data frame - 2829 rows.
str(reviews_by_listing)

# View the first listing's comments.
reviews_by_listing$all_comments[1]

# Observations? What are some problems that we might
# run into with bag-of-words?

#### Constructing the Bag-of-Words ####
# Now, we are ready to construct the Bag-of-Words
# with Airbnb reviews for the prediction task.
# The following step-by-step procedure for building
# the Bag-of-Words is adapted from MIT EdX - Analytics Edge, Lecture 8.

## **Step 0:** Install and load two packages for pre-processing:
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)

## **Step 1:** Convert reviewer comments to a corpus,
##         automatically processing escape characters like "\n".
## **Step 2:** Change all the text to lower case.
## **Step 3:** Remove all punctuation.
## **Step 4:** Remove stop words (this step may take a minute).
## **Step 5:** Stem our document.
corpus <- Corpus(VectorSource(reviews_by_listing$all_comments)) %>%
  tm_map(tolower) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stemDocument)

# Take a look
strwrap(corpus[[1]])[1:3]

# Take a look at tm's stopwords:
stopwords("english")[1:100]

## **Step 6:** Create a word count matrix (rows are reviews, columns are words).
frequencies <- DocumentTermMatrix(corpus)
# Take a look
frequencies

## **Step 7:** Account for sparsity.
# Use findFreqTerms to get a feeling for which words appear the most.
# Words that appear at least 10000 times:
findFreqTerms(frequencies, lowfreq = 10000)

# All 45645 terms will not be useful to us. We might as well get rid
# of some of them - why? Let's only keep terms that appear in
# 5% or more of the reviews (142 or more).
sparse <- removeSparseTerms(frequencies, 0.95)

# How many did we keep? (1136 terms, compared to 45645 previously)
sparse
colnames(sparse)

## **Step 8:** Create data frame.
commentsTM <- as.data.frame(as.matrix(sparse))

# View data frame (rows are reviews, columns are words)
str(commentsTM, list.len = 10)

# Drop columns that include numbers
commentsTM <- commentsTM[, !grepl("[0-9]", names(commentsTM))]

# We have finished building the term frequency data frame `commentsTM`.
# Next, we need to merge the two data frames `commentsTM` (features) and
# `review_scores` (labels) before we can run our machine learning
# algorithms on this data.  This will be a inner_join by `LISTING_ID`,
# which means that we will only include observations present
# in both data sets (e.g. Airbnb locations with >= 1 rating/review_score
# and >= 1 comment).
commentsTM$LISTING_ID <- reviews_by_listing$listing_id
commentsTM <- inner_join(review_scores, commentsTM)

# View the first few data frame columns
# Note: Column names corresponding to word frequencies are lowercase,
# while all other column names are uppercase.
str(commentsTM, list.len = 10)

# Up to here, we have just pre-processed and prepared our data.
# Now, we are ready to build models.

#### Exercise 5 ####
# 1. Your own Bag-of-Words
# Build a Bag-of-Words using the listings$description text data.
# Add price as the dependent variable (named "PRICE")
# and remove all rows where this value is NA. ->

## **Steps 1-5:** Convert listings description to a corpus,
## perform operations and stem document.  
corpus_2 <- Corpus(VectorSource(listings$description)) %>%
  tm_map(tolower) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stemDocument)
strwrap(corpus_2[[1]])[1:3]
## **Step 6:** Create a word count matrix (rows are descriptions, columns are words).
frequencies_2 <- DocumentTermMatrix(corpus_2)
## **Step 7:** Account for sparsity.
sparse_2 <- removeSparseTerms(frequencies_2, 0.95)
## **Step 8:** Create data frame.
listingsTM <- as.data.frame(as.matrix(sparse_2))
listingsTM <- listingsTM[, !grepl("[0-9]", names(listingsTM))]
listingsTM <- listingsTM %>%
  mutate(PRICE = listings$price) %>%
  select(PRICE, everything())
str(listingsTM, list.len = 5)

# 2. Bag-of-Words + LASSO
# Using the Bag-of-Words constructed in the previous exercise,
# build a LASSO model to predict price based upon listing descriptions.
# What are the non-zero coefficients? Do these make sense? 
# (Hint: Use the `coef(., s = "lambda.min")` command) ->

xBagOfWords <- model.matrix(~ . - PRICE, data = listingsTM)
set.seed(123)
spl <- sample.split(listingsTM$PRICE, SplitRatio = 0.7)
xTrain <- xBagOfWords[spl, ]
xTest <- xBagOfWords[!spl, ]
yTrain <- listingsTM$PRICE[spl]
yTest <- listingsTM$PRICE[!spl]
BagOfWords_LASSO_cv <- cv.glmnet(xTrain, yTrain)

#### Unsupervised Learning ####
# Thus far, our machine learning task has been to predict labels,
# which were either continuous-valued (for regression) or
# discrete-valued (for classification).  To do this, we input
# to the ML algorithms some known (feature, label) examples
# (the training set), and the ML algorithm outputs a function
# which enables us to make predictions for some unknown (feature, ?)
# examples (the testing set).  This problem setup is
# known as **Supervised Learning**.

# Next, we consider **Unsupervised Learning**, where we are
# not given labelled examples, and we simply run ML algorithms
# on (feature) data, with the purpose of finding interesting
# structure and patterns in the data.  Let's run one of the
# widely-used unsupervised learning algorithms,
# **k-means clustering**, on the `listings` data frame
# to explore the Airbnb data set.

# First, let's look at help page for the function `k-means()`:
help(kmeans)

# Let's create a new data.frame `listings_numeric` which
# has the subset of columns that we wish to cluster on.  For the
# `k-means()` function, all of these columns must be numeric.
listings_numeric <- listingsOrig %>%
  select(id, latitude, longitude, accommodates, bathrooms, 
         bedrooms, review_scores_rating, price) %>%
  mutate(price = as.numeric(gsub("\\$|,", "", price))) %>%
  na.omit()
str(listings_numeric)

# Next, run the **k-means** algorithm on the numeric data.frame,
# with `k = 5` cluster centroids:
set.seed(1234)
kmeans_clust <- kmeans(listings_numeric[,-1:-3],
                       5, iter.max = 1000, nstart = 100)
kmeans_clust

# Look at the average values within the clusters.  
# What are the characteristics of these 5 groups?  
# How many listings are in each cluster?
kmeans_clust$centers
table(kmeans_clust$cluster)

# Finally let's take a look at where the clusters are located; to do this we 
# will use a package called `leaflet`; additionally to help us get a good color
# scheme we will use `RColorBrewer`
library(RColorBrewer)
library(leaflet)

# To look at color scheme options we can simply type
display.brewer.all()

# Let's visualize the distribution of clusters from the houses; first
# we need to add our cluster labels to the data and then we will 
# define as color palette that leaflet can use to help us
listings_numeric = listings_numeric %>% 
  mutate(clust_label = as.factor(kmeans_clust$cluster))

# Now we need to define a color palette that to distinguish the clusters; since
# we have five cluters we will need five distinct colors
pal = colorFactor(palette = "Set1", domain = listings_numeric$clust_label)

# Now let's plot the houses by cluster
leaflet(listings_numeric) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, color = ~pal(clust_label))

# Can you see where the clusters are?  Also, what is the proper
# number of clusters? We will revisit this in the next session,
# because it requires some more advanced tidyverse tools.  Stay tuned!

# In this module, we have covered examples of machine learning methods
# for linear regression, LASSO, CART, and k-means.  This is just the
# tip of the iceberg.  There are tons more machine learning methods
# which can be easily implemented in R.  We provide some bonus R code
# for random forest, regularized logistic regression, and SVM applied
# to the Airbnb data set in the file **bonus.R**