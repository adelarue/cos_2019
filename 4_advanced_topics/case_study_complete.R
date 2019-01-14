#' -------------------------------------------------------------------------
#' CASE STUDY: Advanced Topics in Data Science
#' By Phil Chodrow
#' January 16th, 2019
#' -------------------------------------------------------------------------

#' This is the script for following along with our case-study analysis of trends 
#' in per-person rental prices. Following along with the case study is highly 
#' recommended. 

#' Prior to running any code, you probably want to go to 
#' Session > Set Working Directory > To Source File Location

#' Load libraries ----------------------------------------------------------

library(tidyverse) #' includes dplyr, tidyr, ggplot2, purrr
library(broom)     #' for retrieving model predictions
library(lubridate) #' for manipulating dates and times
library(leaflet)   #' for geospatial visualization

#' Today we are going to continue with that AirBnB data set we used earlier
#' in our time together. We are going to use two distinct data sets. 
#' The first is the set og 
#' For the analysis we are going to do today, we need two data sets: 
#' - The `listings` data set contains detailed information about each space.
#' - The `prices` data set contains, for each listing, the price-per-person 
#'   on each data listed in the data set. 
#' Unfortunately, we have a problem: navigate to `/data/listings` and 
#' `/data/prices/`. What do you see?

#' EXERCISE 1: Working with your partner, write a for-loop to read in the `prices``
#' data set and assemble a single data frame from the result. You may find it useful 
#' to see what `list.files('data/prices', full.names = T)` does. 
#' The syntax for for-loops in `R` is: 
#' `for(thing in things){do_something_to(thing)}`

names <- list.files('data/prices', full.names = T)

prices <- data_frame()
for(name in names){
  df <- read_csv(name)
  prices <- rbind(df, prices)
}
prices

#' Well, that worked, but we defined a `names` vector that we don't care about
#' and wrote five lines to achieve our aim. Let's explore a better approach. 

#'---------------------------------------------------------------------------- 
#' 
#'----------------------------------------------------------------------------

#' Let's try this: 

prices <- list.files('data/prices', full.names = T) %>% 
  map(read_csv) %>% 
  reduce(rbind)

#' What do `map()` and `reduce()` do?

#' EXERCISE: Working with your partner, inspect the data in the `data/listings` 
#' directory. Write a similar command to read in this data. 

listings <- list.files('data/listings', full.names = T) %>% 
  map(read_csv) %>% 
  reduce(rbind)

#' OPTIONAL EXERCISE: Generalize the last two code chunks by writing a function that 
#' reads in all files in a given directory and returns a single data frame. 
#' You may assume that all given files are in .csv format and have the same columns. 

read_all_csv <- function(directory){
  list.files(directory, full.names = T) %>% 
    map(read_csv) %>% 
    reduce(rbind)
}
prices   <- read_all_csv('data/prices')
listings <- read_all_csv('data/listings')

#' Take a moment to inspect the data by typing the name of each data frame 
#' (prices and listings)  in the terminal. Hopefully some of the column names 
#' look pretty familiar. The `price_per` column was computed for you by dividing
#' the price of the listing on the given date by the number of people that listing
#' accomodates. You can see the file `prep_data.R` for the full data preparation 
#' process. 

#' Preliminary Exploration -------------------------------------------------

#' EXERCISE: Time to take a look to see what we have. The first version of 
#' our analysis question is: 
#' 
#' > How do AirBnB prices vary over time? 
#' 
#' For a first pass at this question, work with your partner to construct a 
#' visualization of the first 2000 rows of the prices data frame using ggplot2. 
#' 	- The `date`` should be on the x-axis
#' 	- The `price_per`` should be on the y-axis
#' 	- The `group` `aes`thetic should be the `listing_id``
#' 	- `geom_line()` is probably the way to go. 
#' 	- You might want to distinguish the series by either using 
#' 	  `aes(color = listing_id)`` or `facet_wrap(~listing_id)`
#' 	- You can use `head(nrows)`` function to extract a data frame with just the
#' 	  first `nrows`` of data. 

prices %>%
	head(2000) %>% 
	ggplot() + 
	aes(x = date, 
		y = price_per, 
		group = listing_id,
		color = listing_id) +
	geom_line() 

#' EXERCISE: It might be easier to get a big-picture view by plotting the average
#' over time. Working with the person next to you, construct a visualization of 
#' the mean over time. 
#' 	- Use `group_by(date) %>% summarise()`` to create a data frame holding the mean
#' 	- You probably want `geom_line()`` again
#'  - We are going to come back to this plot, so name it `p`

p <- prices %>% 
	group_by(date) %>% 
	summarise(mean_pp = mean(price_per, na.rm = T)) %>% 
	ggplot() +
	aes(x = date, y = mean_pp) + 
	geom_line()
p

#' Three interesting things are happening here...what are they? 

#' Modeling ----------------------------------------------------------------
#' Now that we know how (in)complete our data is, let's move on to modeling. 
#' We want to peel out the seasonal variation present in the data. 
#' The seasonal variation should be a smooth curve that varies slowly. 
#' LOESS (**LO**cally **E**stimated **S**catterplot **S**moothing) is a simple 
#' method for fitting such models. LOESS is easy to use; in fact, it's the 
#' default option for `geom_smooth`:
#' 

p + geom_smooth()

#' Ok, so that's helpful, but we've seen that the seasonal variation is different
#' between listings. Eventually, we want to fit a *different* model to *each*
#' listing. For now, let's fit a single one. The span is a hyperparameter, a bit
#' like lambda in LASSO. 

model_data <- prices %>%       # extract a single listing's 
	filter(listing_id == 5506)   # worth of data

single_model <-  loess(price_per ~ as.numeric(date),  
					   data = model_data, span = .2)

single_model 
single_model %>% summary()

#' We can get the smoothing curve as the predicted value from the model. 
#' The most convenient way to do this is using the `augment()` function from the 
#' `broom` package. The output is a data frame containing all the original data, 
#' plus the fitted model values, standard errors, and residuals. 

model_preds <- broom::augment(single_model, model_data) 
model_preds %>% head()

#' Note that the `augment()` function returns fitted values, residuals, and 
#' standard errors, in addition to the original columns. 

#' EXERCISE: Working with your partner, plot both the  `price_per` column and 
#' the `.fitted` column against the date. 

model_preds %>%
	ggplot(aes(x = date)) + 
	geom_line(aes(y = price_per)) + 
	geom_line(aes(y = .fitted), color = 'red')

#' Now, how can we do this many times, for each listing? 
#' If you said `map()`, you are absolutely on point. However, we need a strange 
#' trick to get there...

#'---------------------------------------------------------------------------- 
#'
#'----------------------------------------------------------------------------

#' Nesting -------------------------------------------------------------------

prices_nested <- prices %>% 
	nest(-listing_id)

#' What just happened? Maybe we should inspect things a bit: 

prices_nested$data[1] # hey look, it's a data frame! 

#' Now we define our LOESS modeling function. Its first argument is a
#' data frame, and its second argument is the span hyperparameter. What kind 
#' of object does it return? 

my_loess <- function(data, span){
  loess(price_per ~ as.numeric(date),
        data = data, 
        span = span)
}

#' Our next step is to use `map()` to model each of the data frames in the 
#' data column of prices_nested. Note that we are fitting 
#' `nrow(prices_nest) = 1,705` distinct models simultaneously with this command. 

prices_with_models <- prices_nested %>% 
	mutate(model = map(data, my_loess, span = .25))

#' Just like there are data frames in the data column, there are statistical 
#' models in the model column. Let's inspect that column to make sure it has 
#' in it what we'd expect. 

prices_with_models$model[[1]] %>% summary()

#' Once you're comfortable with that, it's time to extract predictions from 
#' the models. We'll use purrr::map2 and broom::augment to do this. map2 is just
#' like map, but it iterates over two lists simultaneously. We do this because
#' the augment function requires both the model and the original data. 
#' This call might take a little while.  

prices_with_preds <- prices_with_models %>%
	mutate(preds = map2(model,data, augment))

#' Hey look, another list column of data frames! You may want to inspect this 
#' column too, for example: 
#' 	prices_with_preds$preds[[1]] %>% head()
#' Now we're ready to get out of bizarro land. 

prices_modeled <- prices_with_preds %>% 
	unnest(preds) 

#' The first three columns are exactly where we started, but the last three are 
#' new: they give the model predictions (and prediction uncertainty) that we've
#' generated. Let's rename the .fitted column "trend" instead:

prices_modeled <- prices_modeled %>% 
	rename(trend = .fitted) 

#' EXERCISE: Now, working with a partner, please visualize the model predictions
#' against the actual data for the first 2000 rows. Use geom_line() for both. 
#' You'll need to use geom_line() twice, with a different y aesthetic in each,
#' and you should consider using facet_wrap to show each listing and its model
#' in a separate plot. 

prices_modeled %>% 
	head(2000) %>% 
	ggplot(aes(x = date, group = listing_id)) +
	geom_line(aes(y = price_per)) +
	geom_line(aes(y = trend), color = 'red') +
	facet_wrap(~listing_id, ncol = 1, scales = 'free_y') + 
	theme(strip.text.x = element_blank())

#'---------------------------------------------------------------------------- 
#'
#'----------------------------------------------------------------------------

#' Isolating the Signal ----------------------------------------------------

#' Our next step is to begin isolating the April signal. We can think of the 
#' LOESS models we've fit as expressing the signal of long-term, seasonal 
#' variation. Next, we should capture the short-term, periodic signal. While
#' there are packages that can do this in a systematic way, we don't actually
#' need to use them, because we know the period of the signal -- the 7-day week. 
#' Our strategy is simple: we'll compute the average residual associated with 
#' each weekday. This is easy with a little help from the lubridate package. 
#' Note that .resid is "what's left" after we've accounted for the seasonal 
#' variation, so it's what we should be working with. 

prices_modeled <- prices_modeled %>% 
	mutate(weekday = wday(date, label = TRUE)) %>% #' compute the weekday
	group_by(listing_id, weekday) %>% 
	mutate(periodic = mean(.resid)) %>% 
	ungroup()

#' Now we can construct a new column for the part of the signal that's not 
#' captured by either the long-term trend or the periodic oscillation:  

prices_modeled <- prices_modeled %>% 
	mutate(remainder = price_per - trend - periodic)

#' EXERCISE: Working with your partner, plot all four columns 
#' (price_per, trend, periodic, and remainder) as facets on the same visualization. 
#' Start by figuring out what the following code does: 

prices_modeled %>% 
	head(1500) %>% 
	select(-.se.fit, -.resid, -weekday) %>% 
	tidyr::gather(metric, value, -listing_id, -date) 

#' Now build from there

prices_modeled %>% 
	head(1500) %>% 
	select(-.se.fit, -.resid, -weekday) %>% 
	gather(metric, value, -listing_id, -date) %>% 
	mutate(metric = factor(metric, c('price_per',
									 'trend',
									 'periodic',
									 'remainder'))) %>%
	ggplot() + 
	aes(x = date, 
		y = value, 
		group = listing_id, 
		color = as.character(listing_id)) + 
	geom_line() + 
	facet_grid(metric~., scales = 'free_y') +
	guides(color = FALSE)

#' EXERCISE: Now we can also take a look at the overall trend in what part of the
#' signal we've failed to capture. Working with your partner, construct a simple 
#' visualization of the mean of the remainder column over time. What do you see?

prices_modeled %>% 
	group_by(date) %>% 
	summarise(mean_remainder = mean(remainder, na.rm = T)) %>% 
	ggplot() + 
	aes(x = date, y = mean_remainder) + 
	geom_line()

#' We haven't done a perfect modeling job, but we have made considerable progress
#' toward isolating the signal in April. If we like, we can compare this picture to 
#' the original signal p from before: 

p

#' Now to K-Means ---------------------------------------------------------

#' Our motivating problem is: while the signal is apparent on average, not every 
#' individual raises their prices in April. How can we identify individual 
#' listings who did? There are plenty of ways to approach this, but we are going 
#' to practice our tidy modeling skills by using k-means. The idea is to separate
#' listings that raised their prices from listings that didn't into two "clusters." 

#' Our first step is to prepare the data. Since we know the phenomenon we are
#' interested in is in April, we'll focus only on the data in that month. We'll
#' also just pick the columns we'll need. 

april_prices <- prices_modeled %>% 
	filter(month(date, label = T) == 'Apr') %>% 
	select(listing_id, date, remainder)

#' Now we'll construct the matrix of data to use for K-means. 
#' Note that we are including only listings that have data for the entirety
#' of April. `complete()` fills in any missing `NA` values, and then we 
#' use a `group_by() %>% filter()` to exclude listings that have any `NA`s.   

prices_to_cluster<- april_prices %>% 
  complete(listing_id, date) %>% 
  group_by(listing_id) %>% 
  filter(sum(is.na(remainder)) == 0) %>% 
  ungroup() %>% 
  spread(key = date, value = remainder) 

prices_matrix <- prices_to_cluster %>% 
	select(-listing_id) %>% 
	as.matrix()

#' We don't necessarily know that there are two valid clusters in the data. 
#' Now we'll fit 10 models for each of k = 1...10, for a total of 100 models.
#' As before, `map`` makes this easy. Note that an equivalent and more concise
#' approach would be 
#' 
#' cluster_models <- data_frame(k = rep(1:10, 10)) %>% 
#' 	mutate(kclust = map(k, ~ kmeans(prices_for_clustering, .)))

set.seed(1234)
my_kmeans <- function(k){
	kmeans(prices_matrix, k)
}

cluster_models <- data_frame(k = rep(1:10, 10)) %>%
  mutate(kclust = map(k, my_kmeans))

#' Model Evaluation ------------------------------------------------------------

#' How do we know how many clusters to use? One way is to extract a summary of 
#' each model using `broom::glance()`. Here's how that looks for a single model: 

cluster_models$kclust[[1]] %>% glance()

#' EXERCISE: Extract the model summary using glance for each model, and then 
#' convert the result into a "well-behaved" data frame called cluster_performance 
#' with no nesting. 

cluster_performance <- cluster_models %>% 
	mutate(summary = map(kclust, glance)) %>% 
	unnest(summary)

#' EXERCISE: Compute the mean tot.withinss for each value of k and plot the
#' results, with k on the x-axis and the mean tot.withinss on the y-axis. What
#' value of k should we use? 

cluster_performance %>% 
	group_by(k) %>%
	summarise(withinss = mean(tot.withinss)) %>%
	ggplot() + 
	aes(x = k, y = withinss) +
	geom_point() 

#' We really only need one cluster, so let's extract the first one with our 
#' chosen value of k. 

chosen_model <- cluster_models$kclust[cluster_models$k == 2][[1]]

#' Let's inspect the clustered series. To do this, we need to add the 
#' predictions to prices_for_clustering and use `gather()`: 

prices_clustered <- prices_to_cluster %>% 
	mutate(cluster = chosen_model$cluster) %>% 
	gather(key = date, value = price, -cluster, -listing_id) %>% 
	mutate(date = as.Date(date))

#' Moment of Truth!

prices_clustered %>% 
	ggplot() + 
	aes(x = date, y = price, group = listing_id) + 
	geom_line() + 
	facet_wrap(~cluster)

#' Geospatial Visualization ----------------------------------------------------

#' We've isolated the signal: users in one group have large spikes at the 
#' specific week in April; users in the other don't. Now we're ready to visualize
#' where these listings are located in geographic space. 
#' To do so, we need to combine our cluster labels with the geographic information
#' in the `listings` data frame. We'll use `left_join()` for this, but first
#' we need to remove all the duplicates in `prices_clustered`.  

cluster_lookup <- prices_clustered %>% 
	filter(!duplicated(listing_id)) %>% 
	select(listing_id, cluster)

locations_to_plot <- listings %>% 
	left_join(cluster_lookup, by = c('id' = 'listing_id')) %>% 
	filter(!is.na(cluster)) %>% 
  select(cluster, latitude, longitude)

locations_to_plot

#' Now we'll plot using leaflet, an interative geospatial 
#' visualization library. 
library(leaflet)

pal <- colorFactor(c("navy", "orange"), domain = c("1", "2"))

leaflet(data = locations_to_plot)%>% 
	addProviderTiles(providers$CartoDB.Positron) %>% 
	addCircles(~longitude, ~latitude, radius = 50, color = ~pal(cluster))
	
#' Does this map support or testify against our hypothesis?