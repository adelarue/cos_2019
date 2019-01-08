# IAP 2018
# 15.S60 Computing in Optimization and Statistics
# Lecture 1: Introduction to R

# Script file intro.R
# In this script file, we cover the basics of using R.

###################################################
## RUNNING R AT THE COMMAND LINE, SCRIPTING, AND ##
## SETTING THE WORKING DIRECTORY                 ##
###################################################

# Using the R console (command line):
# - You can type directly into the R console (at '>') and 
#   execute by pressing Enter
# - Previous lines can be accessed using the up and down arrows
# - Tabs can be used for auto-completion
# - Incomplete commands will be further prompted by '+'

# Using R scripts in conjunction with the console:
# - We are currently in a script ("intro.R")
# - Individual lines (or multiple) in this script can be executed 
#   by placing the cursor on the line (or selecting) and typing 
#   Ctrl + r on PC or Cmd + Enter on Mac

# Current working directory
# - We are in a current working directory
# - getwd() tells us the current working directory
getwd()
# - We can move to a different directory with setwd()

################################################
## BASICS: CALCULATIONS, FUNCTIONS, VARIABLES ##
################################################

# You can use R as a calculator.  E.g.:
3^(6-4)
22/7
16^(1/4)

6*9 == 
  
  # What happened with that last one? Check the R console!
  # Let's see if it's equal to 42...
  
  # Use the arrow keys to recall the command and check to see
  # if 54 will give you the answer you expect.
  
  # Other useful functions:

sqrt(2)
abs(-2)

sin(pi/2)
cos(0)

exp(-1)
(1 - 1/100)^100

log(exp(1))

# The help function can explain certain functions
# What if we forgot if log was base 10 or natural log?
help(log)
?log

# You can save values, calculations, or function outputs to variables
# with either <- or = 
x <- 2^3
y = 6

# Use just the variable name to display the output
x
y

# Note! If you run a script using source(""), output will be 
# suppressed, unless you use the print function
print(x)
print(y)

# Rules for variable names 
# - Can include letters, numbers
# - Can have periods, underscores
# - CANNOT begin with a number
# - Case-sensitive
# - CANNOT use spaces

# Use the ls() function to see what variables are available
ls()

##############################
## WORKING WITH DATA FRAMES ##
##############################

# Let's grab a data set. First, make sure that your working directory 
# is set to the location of this script file. You can do this by choosing 
# Session -> Set Working Directory -> Source File Location

# for the read_csv function
# install.packages('tidyverse')
library(tidyverse)

# Our data is from AirBnB 2016-2017 listings in Boston. We're going to read 
# in the listings.csv data set, which contains basic information about 
# each residence on offer. 

data <- read_csv('../data/listings.csv')

# The environment now has a `data` object, and tells you how many rows 
# (observations) and columns (variables) are contained in that object. 

# Let's first take an interactive look at the data to see what kind 
# of object we are dealing with.  

View(data)

# So, we are dealing with a standard "rectangular" data set.  
# How does R represent this object? 

class(data)

# The base class is data.frame; tbl_df is a wrapper with a few
# convenient extra tricks. We'll refer to data frames from here 
# on out, and not distinguish between the two. 

# The function str tells us about the structure of the columns
# of our data set.
str(data)

# glimpse gives the same information, including the data types 
# of the columns: 
glimpse(data)

# What if we just need to know how many rows and columns there are?
nrow(data)
ncol(data)




# Finally, you can just type in the name of your data into the 
# console to get a view as well. Note that this may be hard to 
# read if you have lots of columns. 
data

# Use data.frame$col to extract the column col from a data frame.

data$host_response_time
data$bedrooms

# Many operations in R are vectorized; that is, you can apply 
# a single expression to an entire vector at once, without 
# using a loop.

data$review_scores_rating / 100

data$bathrooms + data$bedrooms

data$property_type == "Apartment"

# The subset function can be used to extract rows of
# interest from a data frame (first argument is the
# data frame, second argument is the criterion on which
# to select)

data.high_review = subset(data, review_scores_rating >= 90)
glimpse(data.high_review)

####################################################
## BASIC STATISTICS, PLOTTING, AND SUMMARY TABLES ##
####################################################

# Basic summary statistics
mean(data$bedrooms)

# whoops! need to account for NA (missing values)

mean(data$bedrooms, na.rm = TRUE)
sd(data$bedrooms, na.rm = TRUE)

# Get a statistical overview with summary()

summary(data$bedrooms)

# A convenient way to compute a proportion: 

mean(data$property_type == "Apartment", na.rm = TRUE)

# Basic Plotting
# 
# Relationship between bedrooms and bathrooms? 
plot(data$bedrooms, data$bathrooms)

# Plot with a title, x- and y-axis labels
plot(data$bedrooms, data$bathrooms, main="Bathrooms vs. Bedrooms", xlab = "Number of Bedrooms", ylab = "Number of Bathrooms")

# We can also create a table to look at counts 
table(data$bedrooms, data$bathrooms)


################################
## SAVING YOUR PROGRESS ##
################################

# Sometimes you need to share a processed data set with a collaborator, 
# or use it in another task. 
# To save the data to a file, we use write_csv().

# Example: construct a version of the data set including only listings 
# with a known number of bedrooms. 

is.na(data$bedrooms)
!is.na(data$bedrooms)

data_without_missing_entries = subset(data, !is.na(data$bedrooms))
write_csv(data_without_missing_entries,"data_without_missing_entries.csv")

#################
## ASSIGNMENTS ##
#################

##
# 1a) Try out a few other basic statistics and graphing functions

min(data$square_feet, na.rm = TRUE)
median(data$square_feet, na.rm = TRUE)
max(data$square_feet, na.rm = TRUE)

sum(data$square_feet, na.rm = TRUE)

hist(data$square_feet)
boxplot(data$square_feet)

#  b) Edit the histogram plot above to ensure that it has a title
#     and that the x-axis is labeled properly
