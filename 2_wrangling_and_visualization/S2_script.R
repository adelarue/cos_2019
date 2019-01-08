#' ---
#' title: "Session 2: Data Wrangling and Visualization in R"

#' 
#' # Agenda
# 1. Base R basics
# 2. tidyr and dplyr basics
# 3. ggplot basics
# 4. Deeper with gather/spread and joins

#' # Introducing the Tidyverse: `dplyr`
#' 
#' [(back to top)](#agenda)
#' 
#' Hadley Wickham, a statistician and computer scientist, introduced a suite of packages
#'  to give an elegant, unified approach to handling data in R (check out [the paper]
#'  (http://vita.had.co.nz/papers/tidy-data.html)!).  These data analysis tools, 
#'  and the philosophy of data handling that goes with them, have become standard practice
#'   when using R. 	

#' The motivating observation is that data *tidying* and *preparation* consumes a majority 
#' of the data scientist's time; the underlying concept is then to envision data wrangling 
#' in an idiomatic way.  	

#' ## Loading the libraries
#' 
#' If you did the homework, you already have the libraries we need 
#' installed, but if not, install them with: `install.packages('tidyr')` 
#' and `install.packages('dplyr')`.
#' 
#' Now that we have the libraries installed, we'll load them into our current R session by calling:
#' 
## ----message=FALSE-------------------------------------------------------
library(tidyr)
library(dplyr)

#' 
#' ## Basic exploration and cleaning
#' 
#' Remember to *set the working directory* to the folder with the data folder
#' in it (one easy to do this is in the Files tab using the "More" drop-down menu).  Then, in a fresh 
#' script (or following along in the class script), type and execute:
#' 
## Let's load up the AirBnB data.  
raw_listings <- read.csv('../data/listings.csv')
#' We can look in Environment and see there's actually 3,585 rows of 95 variables.
#' 
#' 
#' Let's get a clearer idea of what the data encompass.
#' The 'str' command displays the structure of an object:
str(raw_listings)  

#' Notice here that the price entries are not factors. Factors are a vector of integer values with
#' a corresponding set of character values to use when the factor is displayed.

#' The 'summary' command gives summary statistics	
summary(raw_listings)   
#' The 'colnames' command displays just column names	
colnames(raw_listings)  

#' Now that we have a better idea of the structure of our data, we can `select` a specific column, 
#' and looking at the first few rows of that column:
#' The `head` command prints out the first parts of a vector, matrix, table, etc.
## 
head(select(raw_listings, price))

#' 
#' Instead of nesting our code like that, we can use a nifty operator included with tidyr 
#' called the **chaining operator** which looks like `%>%` and 
#' serves like a pipeline from one function to another. Specifically: `x %>% f == f(x)`. 
#' 
#' The chaining operator feeds in the object on its left as the first argument 
#' into the function on its right.
#' 
raw_listings %>% select(price) %>% head()

#' A useful command is `table`, which quickly cross-tabulate counts of different variables.  
#' So if we want to see the count of how many listings are listed under each room type: 	
#' 
table(raw_listings$room_type)	

#' We can even make one of the arguments a "conditional," meaning a statement that can be
#' answered by "true" or "false", like the count of rooms by type that accommodate >= 4 people:	
table(raw_listings$room_type, raw_listings$accommodates >= 4)	

#' Now if we want to see the distribution of prices? We want to run something like 'hist(listings$price)'
#' However, this wouldn't work because the prices are factors.
#'  so we'll need to convert them to integers. 

#' The 'function()' command allows us to create a function as an R object
#' 
#' 'gsub' replaces all occurences of a pattern with a new pattern
#' 
#' The blackslash \ helps us escape special behavior
#' 
#' Now we replace all dollar signs in the price with space:
#' 
clean_price <- function(price) as.numeric(gsub('\\$|,', '', price))

#' The `mutate` command allows us to add new variables (and thus new columns) to data, often by referring to existing ones.
#' 
raw_listings %>%
  mutate(nprice = clean_price(price)) %>%
  select(name, price, nprice) %>% 
  head()

#' We can also `arrange` this info (sort it) to see the lowest- or highest-priced listings:
#' 
## ------------------------------------------------------------------------
raw_listings %>%
  mutate(nprice = clean_price(price)) %>%
  select(name, price, nprice) %>%
  arrange(nprice) %>%
  head()

#' Alternative, we can 'arrange' to see the highest-lowest priced listings:

raw_listings %>% 
  mutate(nprice=clean_price(price))%>%
  select(name, price, nprice) %>%
  arrange(desc(nprice)) %>%
  head()
#' 
#' 
#' Note that the tidyverse packages generally do not change the dataframe objects they act on. 
#' For example, the code above doesn't change `raw_listings`, but instead returns a new dataframe 
#' that has the same data as `raw_listings`, plus an extra column.
#' 
# Let's make a new variable that will have the numeric version of price in it:	
raw_listings$numericprice = as.numeric(gsub('\\$|,', '', raw_listings$price))	
#' Now let's try again to see the distribution of prices:
hist(raw_listings$numericprice)	
#' 
# The plot is not ideal, but at least it worked. Let's try drawing a scatter plot of price vs. reviews:	
plot(raw_listings$review_scores_rating, raw_listings$nprice)	
#' 
#' Now, let's learn some more verbs. Let's say we're interested in understanding the relationship
#'  between bedrooms and price. But some of the listings don't have data on bathrooms; 
#'  The `is.na()` function returns "True" if something is `NA`, so `!is.na()` (read: "*not* is NA") 
#'  returns the opposite.
#'  The 'count' command helps us count how many NA there are in a column.
#' 
## ------------------------------------------------------------------------
raw_listings %>% count(is.na(bathrooms))

#' 
#' Let's filter these out:
#' 
## ------------------------------------------------------------------------
raw_listings %>% filter(!is.na(bathrooms)) %>% head()

#' 
#' Finally, let's combine some of these to make a clean dataset to work with. We want to make sure 
#' our data has a correct price column and no missing bedroom or bathroom columns. We'll assign it 
#' to a new dataframe named `listings`. 

## ------------------------------------------------------------------------

listings <- raw_listings %>%
  filter(!is.na(bedrooms), !is.na(bathrooms)) %>%
  mutate(nprice = clean_price(price),
         weekly_price = clean_price(weekly_price),
         monthly_price = clean_price(monthly_price))

#' 
#' 
#' ## Aggregation
#' Now, let's see some summary statistics, like the average price for a listing? 
#' Let's take our clean dataset and `summarize` it to find out.
#' 
## ------------------------------------------------------------------------
listings %>% summarise(avg.price = mean(nprice))

#' 
#' If we want to look at mean price by neighborhood_cleansed: we can do that with `group_by`:
#' 
## ------------------------------------------------------------------------
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg.price = mean(nprice))

#' 
#' Maybe we're a little worried these averages are skewed by a few outlier listings. 
#' The `n()` function gives a count of how many rows we have in each group. 

## ------------------------------------------------------------------------
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg.price = mean(nprice),
            med.price = median(nprice),
            num = n())

#' We do notice some red flags to our "mean" approach.
#' 
#' - First, if there are a very small number of listings in a neighborhood compared to the rest of 
#' the dataset, we may worry we don't have a representative sample, or that this data point should be
#'  discredited somehow (on the other hand, maybe it's just a small neighborhood, like Bay Village, and 
#'  it's actually outperforming expectation).
#' 
#' - Second, if the *median* is very different than the *mean* for a particular neighborhood, it indicates 
#' that we likely have *outliers* skewing the average.  Because of those outliers, as a rule of thumb, 
#' means tend to be a misleading statistic to use with things like rent prices or incomes.
#' 
#' One thing we can do is just filter out any neighborhood below a threshold count:
## ------------------------------------------------------------------------
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg.price = mean(nprice),
            med.price = median(nprice),
            num = n()) %>%
  filter(num > 200)

#' 
#'   We can also pick a few neighborhoods to look at by using the `%in%` keyword in a `filter`
#'    command with a list of the neighborhoods we want:
#'    We use 'c()' function to combine its arguments
## ------------------------------------------------------------------------
listings %>%
  filter(neighbourhood_cleansed %in% c('Downtown', 'Back Bay', 'Chinatown')) %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg.price = mean(nprice),
            med.price = median(nprice),
            num = n()) %>%
  arrange(med.price)


#' We have now seen: `select`, `filter`, `count`, `summarize`, `mutate`, `group_by`, and `arrange`.  
#' This is the majority of the dplyr "verbs" for operating on a single data table (although 
#' [there are many more](https://cran.r-project.org/web/packages/dplyr/dplyr.pdf)), but as you can see,
#'  learning new verbs is pretty intuitive. What we have already gives us enough tools to accomplish a 
#'  large swath of data analysis tasks.
#' 
#' But ... we'd really like to visualize some of this data, not just scan summary tables. 
#' Next up, ggplot.
#' 
#' 
#' #################
## Plotting Graphs with ggplot
#################
#'
#' # Introducing the Grammar of Graphics
#' 
#' [(back to top)](#agenda)
#' 
#' `ggplot` provides a unifying approach to graphics, similar to what we've begun to see with tidyr.
#'  ggplot was created by Leland Wilkinson with his book 
#'  [The Grammar of Graphics](https://www.cs.uic.edu/~wilkinson/TheGrammarOfGraphics/GOG.html) 
#'  (which is the gg in ggplot), and put into code by Hadley Wickham.  
#'  We'll see it not only provides a clean way of approaching data visualization,
#'   but also nests with the tidyr universe like a hand in a glove.
#' 
#' ## Philosophy
#' 
#' What does **grammar of graphics** mean?  A grammar is a set of guidelines for how to combine 
#' components (ingredients) to create new things.  One example is the grammar of language: 
#' in English, you can combine a noun (like "the dog") and a verb (like "runs") to create a 
#' sentence ("the dog runs").
#' 
#' Let's translate this idea to visualization. Every ggplot consists of three main elements:
#' 
#' - **Data**: The dataframe we want to plot.
#' - **Aes**thetics: The dimensions we want to plot, e.g. x, y, color, size, shape.
#' - **Geom**etry:  The specific visualization shape. Line plot, scatter plot, bar plot, etc.
#' 
#' ## Example
#' 
#' First, make sure you've got `ggplot2` installed (with `install.packages('ggplot2')`) 
#' and then load it into your session:
#' 
## ----message=FALSE-------------------------------------------------------
library(ggplot2)

#' 
#' Next, let's use our tidyverse tools to make the dataset we're interested in working with.
#' Let's try grouping listings together if they have the same review score, and take the median
#'  within the group. Oh, and just filter out those NAs.
#' 
## ------------------------------------------------------------------------
by.rating.bedroom <- listings %>%
  filter(!is.na(review_scores_rating)) %>%
  group_by(review_scores_rating, bedrooms) %>%
  summarize(med.price = median(nprice), listings = n())

by.rating.bedroom
#' 
#' Now, we chain this into the `ggplot` function. The 'geom_point()' command creates scatterplots
## ------------------------------------------------------------------------
by.rating.bedroom %>%
  ggplot(aes(x=review_scores_rating, y=med.price)) +
  geom_point()

#' 
#' Behold: we specify our Data (`listings`), our Aesthetic mapping (`x` 
#' and `y` to columns of the data), and our desired Geometry (`geom_point`).  
#' We are gluing each new element together with `+` signs.   Clean, intuitive, and
#'  already a little prettier than the Base R version.  But most importantly, this is
#'   much more extensible.  Let's see how.
#' 
#' **Adding aesthetics:** Suppose we want to see these points broken out by the number of bathrooms.
#'  One way to get that extra dimension is to color these points by the number of bedrooms. 
#' 
## ------------------------------------------------------------------------
by.bedroom.rating %>%
  ggplot(aes(x=review_scores_rating, y=med.price, color=factor(bedrooms))) +
  geom_point()

#' 
#' Note that `factor` essentially tells ggplot to treat `bedrooms` as categorical rather than numeric.
#' 
#' **Adding geoms:** We can also keep adding additional geoms to the plot to visualize
#' the same data in different ways. 
#' 
#' In the following example, we throw in a linear best-fit line for each bedroom class. 
#' Note that the same x, y, and color aesthetics propagate through all the geoms. 
#' 
#' The 'geom_smooth' command creates a line that represents smoothed conditional means
#' 
## ------------------------------------------------------------------------
by.bedroom.rating %>%
  ggplot(aes(x=review_scores_rating, y=med.price, color=factor(bedrooms))) +
  geom_point() +
  geom_smooth(method = lm)

#' lm stands for linear smooths. We can also choose other methods, i.e. loess for locally smooths.
#' 
#' 

#' ===========================================================================
#'Exercise:
#'Let's try an exercise to integrate what we've learned so far, and to learn a couple new tricks. 
#'Can we draw a scatterplot of the listings such that
#'
#'the entries are grouped by review_scores_rating,
#'the bubbles are pink and 50% transparent
#'the size of the bubbles indicate how many reviews received by that group
#'the background is white
#'the plot is price against score
#'
#' *ANSWER:*
#' 

#' =================================================================================


### Saving a plot	

#' By the way, you can flip back through all the plots you've created in RStudio using the 
#' navigation arrows, and it's also always a good idea to "Zoom" in on plots.  	

#' When you finally get a plot you like, you can "Export" it to a PDF (recommended), image, 
#' or just to the clipboard.  
#' Another way to save a plot is to use `ggsave()`, which saves the last plot by default, 
#' for example: 
ggsave('price_vs_score.pdf') 


#' ## Other geometries: Line plots, Box plots, and Bars
#' We will now quickly run through a few of the other geometry options available, 
#' but truly, we don't need to spend a lot of time here since each follows the same grammar as before 
#' Note that geoms are stackable -- we can just keep adding them on top of each other.

#' 
#' 
#' First let's group the listings by neighbourhood_cleansed and save the summary information
#'  we want to plot into its own object:	
by.neighbor = listings %>%	
  group_by(neighbourhood_cleansed) %>%	
  summarize(med.price = median(nprice))
#' 
#' 
#' 
#' ### Line Plots
#' We have already seen smooth line plots in some previous example, let's now see a standard line plot
#' by running 'geom_line':

by.neighbor %>%	
  ggplot(aes(x=neighbourhood_cleansed, y=med.price)) + 	
  geom_point() +	
  geom_line(group=1)	

#' Because our `x` is not a continuous variable, but a list of neighborhoods, 
#' `geom_line` thinks the neighborhoods are categories that each need their own line --- 
#' so we had to specify `group=1` to group everything into one line.  	

#' This is misleading, since it falsely implies continuity of price between neighborhoods 
#' based on the arbitrary alphabetical ordering.  
#' So we switch to bar plots

#' ### Bar Plots

#' We use `geom_bar` to draw bar charts.  
#' Let's rotate the labels on the x-axis so we can read them:
#' 
by.neighbor %>%
  ggplot(aes(x=neighbourhood_cleansed, y=med.price)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=60, hjust=1))

#' 
#' Notice we added an argument to `geom_bar`: `stat='identity'`.  
#' This tells `geom_bar` that we want the height of the bar to be equal to the `y` value
#'  (identity in math means "same as" or "multiplied by one").  We could have instead told it 
#'  to set the height of the bar based on an aggregate count of different x values, or by binning
#'   similar values together --- we'll cover this idea of binning more in the next subsection.

#' Again, notice we are separating thematic (non-content) adjustments like 
#' text rotation, from geometry, from aesthetic mappings.  (Try playing around with the settings!)
#' 
#' Let's clean up this plot a bit:
#' 
## ------------------------------------------------------------------------
by.neighbor %>%
  ggplot(aes(x=reorder(neighbourhood_cleansed, -med.price), y=med.price)) +
  geom_bar(fill='dark red', stat='identity') +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(x='', y='Median price', title='Median daily price by neighborhood')

#' 
#' Only new tool here is the `reorder` function we used in the `x` aesthetic, 
#' which simply reorders the first argument in order by the last argument, which in our case 
#' was the (negative) median price (so we get a descending order).
#' 
#' Again we have an interesting visualization because it raises a couple questions:
#' 
#' - What explains the steep dropoff from "North End" to "Jamaica Plain"?  
#' (Is this a central-Boston vs. peripheral-Boston effect? What would be some good ways to 
#' visualize that?)
#' 
#' - Does this ordering vary over time, or is the Leather District always the most high end?
#' 
#' - What is the distribution of prices in some of these neighborhoods? -- we have tried to take 
#' care of outliers by using the median, but we still have a hard time getting a feel for a 
#' neighborhood with just a single number.
#' 
#' 
#' ### Plotting Distributions
#' 
#' For now, let's pick out a few of these high end neighborhoods and plot a more detailed view 
#' of the distribution of price using `geom_boxplot`.  We also need to pipe in the full dataset 
#' now so we have the full price information, not just the summary info.
## ------------------------------------------------------------------------
listings %>%
  filter(neighbourhood_cleansed %in% c('South Boston Waterfront', 'Bay Village',
                                       'Leather District', 'Back Bay', 'Downtown')) %>%
  ggplot(aes(x=neighbourhood_cleansed, y=nprice)) +
  geom_boxplot()

#' 
#' A boxplot shows the 25th and 75th percentiles (top and bottom of the box), the 50th percentile or 
#' median (thick middle line), the max/min values (top/bottom vertical lines), and outliers (dots).  
#' By simply changing our geometry command, we now see that although the medians were very similar, 
#' the distributions were quite different (with Bay Village especially having a "heavy tail" of 
#' expensive properties), and there are many extreme outliers ($3000 *a night*?!).
#' 
#' 
# Another slick way to visualize this is with a "violin plot".  We can just change our geometry to `geom_violin` and voila!	
listings %>%	
  filter(neighbourhood_cleansed %in% c('South Boston Waterfront', 'Bay Village', 	
                                       'Leather District', 'Back Bay', 'Downtown')) %>%	
  ggplot(aes(x=neighbourhood_cleansed, y=nprice)) +	
  geom_violin()	

#' 
#' `stat_ecdf` is another useful tool for visualizing distributions (Naming note: "stats" are 
#' essentially geoms that bake in some statistical transformations. There aren't that many of them 
#' so we won't worry about that for now). `stat_ecdf` plots the cumulative distribution of 
#' (i.e. percentiles vs. values) of vectors, which gives you a lot more information about the 
#' distribution at the expense of being a bit harder to read.
#' 
#' Let's plot the distribution of price by number of bedrooms, and use `coord_cartesian` to limit
#'  the x-axis of the plot.
#' 
## ------------------------------------------------------------------------
listings %>%
  ggplot(aes(nprice, color=factor(bedrooms))) +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 1000))

#' 
#' Couple of interesting findings here:
#' 
#' - Prices cluster around multiples of $100 (look for the vertical lines). 
#' Maybe people should be differentiating on price more!
#' - Low-end zero-bedroom units are cheaper than low-end one-bedroom units, but 
#' one-bedroom units are cheaper at the high end. Maybe there are different types of zero-bedroom units?
#' 
#' ###################
#' ## Tips and tricks
#' #####################
#' 
#' ### Facetting
#' Need a couple extra dimensions in your plot? Try facetting, i.e. breaking out your plot 
#' into subplots by some variable(s). It's a simple addition to the end of our `ggplot` chain.  
#' For example, using the price by room type example, let's plot each histogram in its own facet:
## ------------------------------------------------------------------------

# We would also like to "normalize" the values --- divide each bar height by the total sum of all
#the bars --- so that each bar represents the fraction of listings with that price range, instead 
#of the count.  We can do that with a special mapping in `aes` to make `y` a "density", like so:	

listings %>%
  filter(nprice < 500) %>%
  ggplot(aes(x=nprice, y=..density.., fill=room_type)) +
  geom_histogram(binwidth=50, center=25, position='dodge', color='black') +
  labs(x='Price', y='Frac. of Listings', fill='Room type') +
  facet_grid(.~room_type)

#' Dodging preserves the vertical position of an geom while adjusting the horizontal position. 

#' 
#' If we interpret the facet layout as an x-y axis,the `.~room_type` formula means layout 
#' nothing (`.`) on the y-axis, against `room_type` on the x-axis.  Sometimes we have too many 
#' facets to fit on one line, and we want to let ggplot do the work of wrapping them in a nice way.  
#' For this we can use `facet_wrap()`.  Try plotting the distribution of price, faceted by how many
#'  the listing accommodates, using `facet_wrap()`.  Note that now we can't have anything on the 
#'  y-axis (since we are just wrapping a long line of x-axis facets), so we drop the period from 
#'  the `~` syntax.
#' 
## ------------------------------------------------------------------------
listings %>%
  filter(nprice < 500) %>%
  ggplot(aes(x=nprice, y=..density..)) +
  geom_histogram(binwidth=50, center=25, position='dodge', color='black') +
  labs(x='Price', y='Frac. of Listings') +
  facet_wrap(~accommodates)

#' 


#' # ggplot Exercises
#' 
#' [(back to top)](#agenda)
#' 
#' 
#' **Exercise 1. `geom_tile`**  
#' A useful geometry for displaying heatmaps in `ggplot` is `geom_tile`.  
#' This is typically used when we have data grouped by two different variables, 
#' and so we need visualize in 2d.  For example, try using `geom_tile` to visualize median price 
#' grouped by # bedrooms and bathrooms.
#' 
#' *ANSWER:*
## ----exercise-------------------------------------------------------





#' 
#' BONUS: We can enforce that the color scale runs between two colors by adjusting 
#' a `scale_fill_gradient` theme, like this:
## ----exercise-------------------------------------------------------







#' 
#' 
#' # Going Wider and Deeper
#' 
#' [(back to top)](#agenda)
#' 
#' We will now go a little deeper with what tidyr/dplyr and ggplot can do.  We hope to gain an understanding of the intent and philosophy behind the tidy R approach, and in doing, gain a more powerful set of analysis and visualization tools.
#' 
#' ## Philosophy
#' 
#' The unifying philosophy of the Tidyverse is:
#' 
#' >- ***Each row is an observation***
#' >- ***Each column is a variable***
#' >- ***Each table is an observational unit***
#' 
#' Simple, right?  Yet a lot of data isn't formed that way.  Consider the following table
#' 
#' |Company  | Qtr.1  |  Qtr.2  |  Qtr.3  |  Qtr.4  |
#' |---------|--------|---------|---------|---------|
#' |ABC      |$134.01 |$256.77  |$1788.23 |$444.37  |
#' |XYZ      |$2727.11|$567.23  |$321.01  |$4578.99 |
#' |GGG      |$34.31  |$459.01  |$123.81  |$5767.01 |
#' 
#' This looks completely acceptable, and is a compact way of representing the information. 
#'However, if we are treating "quarterly earnings" as the observed value, then this format 
#'doesn't really follow the tidy philosophy: notice that there are multiple prices (observations)
#' on a row, and there seems to redundancy in the column headers...
#' 
#' 
#' In the tidyverse, we'd rather have the table represent "quarterly earnings," with each row 
#' giving a single observation of a single quarter for a single company, and columns representing 
#' the company, quarter, and earning.  Something like this:
#' 
#' |Company  | Quarter |  Earnings  |
#' |---------|---------|------------|
#' |ABC      |Qtr.1    |$134.01     |
#' |ABC      |Qtr.2    |$256.77     |
#' |ABC      |Qtr.3    |$1788.23    |
#' |...      |...      |...         |
#' 
#' This is also called the **wide** vs. the **long** format. To see why this is important in the 
#' Tidyverse: suppose we wanted to compare each company's earnings across each quarter in a bar plot.
#' 
#' How would we do that in the first case? It'd be an ugly and manual process. 
#' In the second case, simply run: 
#' `ggplot(quarterly_earnings, aes(Quarter, Earnings, fill = Company)) + geom_bar(position = 'dodge')`
#' 
#' 
#' ## Changing data between wide and long
#' 
#' Think about our `listings` dataset.  Earlier, we plotted the distribution of daily prices for 
#' different room types.  This was easy because this particular slice of the data happened to be 
#' tidy: each row was an observation of price for a particular listing, and each column was a single 
#' variable, either the room type or the price.
#' 
#' But what if we want to compare the distributions of daily, weekly, and monthly prices?  
#' Now we have a similar situation to the quarterly earnings example from before: now we want 
#' each row to have single price, and have one of the columns specify which kind of price we're 
#' talking about.
#' 
#' To gather up **wide** data into a **long** format, we can use the `gather` function.  
#' This needs us to specify the desired new columns in standardized form, and the input columns 
#' to create those new ones:
#' 
## ------------------------------------------------------------------------
long.price <- listings %>%
  select(id, name, nprice, weekly_price, monthly_price) %>%
  gather(price_type, price_value, nprice, weekly_price, monthly_price) %>%
  filter(!is.na(price_value))

long.price %>% head()  # take a peek

#' 
#' Let's break down what gather does:
#' - The first two arguments are names of columns we want to create in the new, long dataframe.
#'   - `price_type` is the "key": the name of each wide column we're collapsing.
#'   - `price_value` is the "value": the values of those wide columns.
#' - The following arguments `price`, `weekly_price`, and `monthly_price` are the wide columns 
#' we want to collapse into long format.
#' - Any columns we don't pass to gather (here, `id` and `name`) are copied across all the long 
#' rows. Typically, you can use them to identify which wide row each long row corresponds to.
#' 
#' 
#' **Quick exercise:** What's the gather command for the quarterly earnings table above?
#' *Answer:* 
#' 
#' 
#' To spread it back out into the original wide format, we can use `spread`. 
#' We tell spread the "key" and "value" columns, and it turns long data into wide data.
## ------------------------------------------------------------------------
long.price %>%
  spread(price_type, price_value) %>%
  head()

#' 
#' 
#' ## Visualizing long data
#' 
#' Now what was the point of all that, you may ask?  One reason is to allow us to cleanly map 
#' our data to a visualization.  Let's say we want the distributions of daily, weekly, and monthly 
#' price, with the color of the line showing which type of price it is.  Before we were able to do 
#' this with room type, because each listing had only one room type.  But with price, we would need 
#' to do some brute force thing like ... `y1=price, y2=weekly_price, y3=monthly_price`? 
#' And `color=` ... ?  This looks like a mess, and it's not valid ggplot commands anyway.
#' 
#' But with the long format data, we can simply specify the color of our line with the `freq` 
#' column, which gives which type of observation it is.
#' 
## ------------------------------------------------------------------------
long.price %>%
  filter(price_value < 1000) %>%
  ggplot(aes(x=price_value, color=price_type)) +
  stat_ecdf()

#' 
#' There are lots of times we need this little "trick," so you should get comfortable with it
#'  --- sometimes it might even be easiest to just chain it in. 
#' Let's plot a bar chart showing the counts of listings with different numbers of bedrooms 
#' and bathrooms (we'll filter out half-rooms just to help clean up the plot):
#' 
## ------------------------------------------------------------------------
listings %>%
  select('Bedrooms'=bedrooms, 'Bathrooms'=bathrooms) %>%
  gather(type, number, Bedrooms, Bathrooms) %>%
  filter(!is.na(number), number %% 1 == 0) %>%
  ggplot(aes(x=number, fill=type)) +
  geom_bar(stat='count', position='dodge', color='black') +
  labs(x='# Rooms', y='# Listings', fill='Room type')

#' 
#' 
#' ## Joining datasets
#' 
#' Our last topic will be how to **join** two data frames together.  
#' We'll introduce the concept with two toy data frames, then apply it to our AirBnB data.
#' 
#' ### Join together, right now, over me...
#' 
#' (The following example adapted from [here](https://rpubs.com/bradleyboehmke/data_wrangling).)  
#' Let's say `table1` is
## ------------------------------------------------------------------------
table1 = data.frame(name=c('Paul', 'John', 'George', 'Ringo'),
                    instrument=c('Bass', 'Guitar', 'Guitar', 'Drums'),
                    stringsAsFactors=F)
table1  # take a look

#' 
#' and `table2` is
## ------------------------------------------------------------------------
table2 = data.frame(name=c('John', 'George', 'Jimi', 'Ringo', 'Sting'),
                    member=c('yes', 'yes', 'no', 'yes', 'no'),
                    stringsAsFactors=F)
table2

#' 
#' then we might want to join these datasets so that we have a `name`, `instrument`, 
#' and `member` column, and the correct information filled in from both datasets (with NAs
#'  wherever we're missing the info).  This operation is called a `full_join` and would give us
#'   this:
#' 
## ------------------------------------------------------------------------
full_join(table1, table2, by='name')

#' 
#' Notice we have to specify a **key** column to join `by`, in this case `name`.
#' 
#' We might also want to make sure we keep all the rows from the first table 
#' (the "left" table) but only add rows from the second ("right") table if they match 
#' existing ones from the first.  This called a `left_join` and gives us
## ------------------------------------------------------------------------
left_join(table1, table2, by='name')

#' 
#' since "Jimi" and "Sting" don't appear in the `name` column of `table1`.
#' 
#' Left and full joins are both called "outer joins" (you might think of merging two 
#' circles of a Venn diagram, and keeping all the non-intersecting "outer" parts).  
#' However, we might want to use only rows whose key values occur in both tables (the intersecting 
#' "inner" parts) --- this is called an `inner_join` and gives us
## ------------------------------------------------------------------------
inner_join(table1, table2, by='name')

#' 
#' There is also `semi_join`, `anti_join`, ways to handle coercion, ways to handle different 
#' column names ... we don't have time to cover all the variations here, but let's try using 
#' some basic concepts on our AirBnB data.
#' 
#' ### Applying joins
#' 
#' Let's say we have a tidy table of the number of bathrooms and bedrooms for each listing, 
#' which we get by doing
## ------------------------------------------------------------------------
rooms <- listings %>%
  select(name, bathrooms, bedrooms) %>%
  gather(room.type, number, bathrooms, bedrooms)

#' 
#' But we may also want to look at the distribution of daily prices, which we can store as
## ------------------------------------------------------------------------
prices <- listings %>%
  select(name, nprice) %>%
  mutate(price = as.numeric(gsub('\\$|,', '', nprice)))

#' 
#' Now, we can do a full join to add a `price` column.
## ------------------------------------------------------------------------
rooms.prices <- full_join(rooms, prices, by='name')

#' 
#' This gives us a table with the number of bed/bathrooms separated out in a tidy format 
#' (so it is amenable to ggplot), but also prices tacked on each row (so we can incorporate 
#' that into the visualization).  Let's try a boxplot of price, by number of rooms, and use 
#' facets to separate out the two different types of room.  (We will also filter out half-rooms 
#' just to help clean up the plot.)
## ------------------------------------------------------------------------
rooms.prices %>%
  filter(!is.na(number), number %% 1 == 0) %>%
  mutate(number = as.factor(number)) %>%
  ggplot(aes(x=number, y=price, fill=room.type)) +
  geom_boxplot() +
  facet_grid(~room.type) +
  labs(x='# of Rooms', y='Daily price', fill='Room type')

#' 
#' This allows us to easily use the `room.type` column (created in the gather before) to set our 
#' fill color and facet layout, but still have access to all the price information from the original
#'  dataset.  This visualization shows us that there is a trend of increasing price with increasing 
#'  number of bathrooms and bedrooms, but it is not a strict one, and seems to taper off at 
#'  around 2 bedrooms for example.
#' 
#' In the next sessions, we will need data from the `listings.csv` file and the other datasets 
#' `calendar.csv` and `reviews.csv`, so we will use these joins again.
#' 
#' # `tidyr` Exercises
#' 
#' Make a bar chart showing the mean review score for each neighborhood and for each type of 
#' review: `review_scores_cleanliness`, `review_scores_location`, `review_scores_value`.
#' 
#' *ANSWER:*
## ------------------------------------------------------------------------

#'
#' # Wrapping Up
#' 
#' In this session, we introduced some basics of data wrangling and visualization in R.  
#' Specifically, we introduced the powerful framework of the "Tidyverse," its accompanying 
#' visualization suite `ggplot`, discussed some of the elegant data philosophy behind these 
#' libraries, briefly covered some more involved operations like gather/spread and dataset joins, 
#' and hinted at deeper applications such as predictive analytics and time series analysis that 
#' we will cover in the next two sessions.
#' 
#' ## Further reading
#' 
#' Some of the infinitude of subjects we did not cover are: heatmaps and 2D histograms, statistical
#'  functions, plot insets, ...  And even within the Tidyverse, don't feel you need to limit 
#'  yourself to `ggplot`.  Here's a good overview of some [2d histogram techniques]
#'  (http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html), 
#'  a discussion on [overlaying a normal curve over a histogram]
#'  (http://stackoverflow.com/questions/5688082/ggplot2-overlay-histogram-with-density-curve), 
#'  a workaround to fit multiple plots in [one giant chart]
#'  (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/).  
#'  In general, if you can dream it up, someone else has too and there's a solution/explanation 
#'  on the internet.
#' 
#' The best way to learn R, and data analysis in general, is not to read blogs and papers, 
#' but to *get out there and do it*.  There are always intriguing competitions on data hosting 
#' websites like [Kaggle](http://www.kaggle.com), and there many areas like [sports analytics]
#' (http://www.footballoutsiders.com), [political forecasting]
#' (http://www.electoral-vote.com/evp2016/Info/data.html), [historical analysis]
#' (https://t.co/3WCaDxGnJR), and countless others that have [clean]
#' (http://http://www.pro-football-reference.com/), [open]
#' (http://www.kdnuggets.com/datasets/index.html), and interesting data just waiting for you 
#' to `read.csv`.  You don't need proprietary data [to make headlines]
#' (http://fivethirtyeight.com/features/a-plagiarism-scandal-is-unfolding-in-the-crossword-world/), 
#' and some data that seems like it would be hard to get is actually [out there in the wild]
#' (https://www.kaggle.com/kaggle/hillary-clinton-emails).
#' 
#' These are hobbyist applications, but we also hope this session has sparked your interest in 
#' applying analytics to your own research.  Nearly every modern research field has been touched 
#' by the power and pervasiveness of data, and having the tools to take advantage of this in your 
#' field is a skill with increasing value.
#' 
#' And plus, it's pretty fun.
