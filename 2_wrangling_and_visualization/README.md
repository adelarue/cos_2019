# Data Wrangling and Visualization with R

In the first part of the session, we will introduce basic techniques in data wrangling and visualization in R.  Specifically, we will cover some basic tools using out-of-the-box R commands, then introduce the powerful framework of the "tidyverse" (both in wrangling and visualizing data), and finally gain some understanding of the philosophy of this framework to set up deeper exploration of our data.  Throughout, we will be using a publicly available dataset of AirBnB listings. 

In the second part of the session, we will introduce RMarkdown.

## Preassignment 0: Project Prep

By Thursday, we expect you to: 

1. Choose a project group. 
2. Choose a data set that interests you. It can be from your research or from an online venue like [Kaggle](http://www.kaggle.com). 

In addition to the below, we therefore expect you to turn in on Stellar the names of your project group and a brief description of your data set, including how you will access it and what makes it interesting. 

If you would like a partner but are having trouble finding one, please email the instructors and we will do our best to pair you up. 

### Example Preassignment 0: 

The following text gives an example of an acceptable submission for the project preparation. 

*Team members: Andreea G.*

*I will be looking at the Kaggle dataset on ski resorts, which is available to download at https://www.kaggle.com/beaubellamy/ski-resort. The dataset lists different ski resorts along with attributes such as country / region, altitude, size, difficulty of slopes, trails, snow reliability, facilities and equipment, average ticket price per age group, other services available. Here are two examples of questions I could investigate: 1. Pricing depending on the ski attributes of the resort, and 2. Number of resorts depending on the attributes of the region. One limitation of this dataset is the missing information of other touristic features of the given regions.*


## Pre-assignment 1: Download `R` and RStudio

We will be learning the `R` programming language for statistical computation. To interact with `R`, we will use RStudio to write and execute `R` commands. 

* **Install `R`**: Navigate to https://cran.cnr.berkeley.edu/ and follow the instructions for your operating system. 
* **Download RStudio**: Navigate to https://www.rstudio.com/products/rstudio/download/ and download RStudio Desktop with an Open Source License. 
* **Test Your Installation**: Open RStudio and type 1+2 into the Console window, and type "Enter."
* Later in the class we will require the most recent version of R (>= 3.3.1). If you already had R installed on your computer, you may have an earlier version. To check and update the R version, you can either follow the **Install `R`** instructions above or follow the steps in pre-lecture-assignment-1.R.


## Pre-assignment 2: Keeping current

To ensure that you have the most current versions of all files, please fire up a terminal, navigate to the directory into which you cloned the full set of materials for the course, and run `git pull`.  (Refer back to Session 1 if you're having trouble here.)

Before class, it is recommended to skim through the [online session notes](https://philchodrow.github.io/cos_2017/2_wrangling_and_viz/S2_master.html).

We recommend you follow along in class using the `S2_script.R` and `S2_exercises.R` files, which will allow you to live-code along with the session leader and work through un-solved exercises.  

It may be helpful, however, to also keep handy the `S2_script_full.R` and `S2_exercises_solved.R` files which have all code and exercise answers filled in.

(The `S2_master.Rmd` and `S2_master.html` files creating the [online session notes](https://philchodrow.github.io/cos_2017/2_wrangling_and_viz/S2_master.html) can be ignored.)


## Pre-assignment 3: Installing libraries

We will use three libraries for this session: `tidyr`, `dplyr`, and `ggplot2`. These three packages (any many more) can all be conveniently loaded using the metalibrary `tidyverse`. You should have previously installed `tidyverse` prior to Session 1. In case you haven't, do so now: 

```
install.packages('tidyverse')
```

You should test that the libraries will load by then running
```
library(tidyverse)
```

Then test that dplyr/tidyr work by executing the command:
```
data.frame(name=c('Ann', 'Bob'), number=c(3.141, 2.718)) %>% gather(type, favorite, -name)
```
which should output something like this
```
      name   type favorite
    1  Ann number    3.141
    2  Bob number    2.718
```

Finally, test that ggplot works by executing the command
```
data.frame(x=rnorm(1000), y=rnorm(1000)) %>% ggplot(aes(x,y)) + geom_point()
```
which should produce a cloud of points centered around the origin.

**Please upload a screenshot of these two outputs to Stellar (the table and the scatter plot).**


## Pre-assignment 4: Reproducible Reporting with RMarkdown

[R Markdown](https://rmarkdown.rstudio.com/) is a framework for easily producing interactive reports, presentations, and dashboards in `R`. 

To prepare RStudio to work with R Markdown, run the following code in your RStudio console. 

```{r}
install.packages(c('knitr', 'rmarkdown', 'flexdashboard', 'leaflet'))
```

To test that you have installed these packages successfully, please: 

1. Pull to ensure that you have the latest version of this repository. 
2. Open the file `preassignment.rmd` in RStudio. 
3. Click the "Knit" button at the top of the source editor, or press `cmd + shift + k` (`ctrl + shift + k` on Windows). The "Knit" button is the one circled in [this image](http://cinf401.artifice.cc/images/workflow-25.png).
4. After a few moments, RStudio should pop up with a new window containing a dashboard that looks like [this](https://philchodrow.github.io/mban_orientation/data_science_intro/preassignment/preassignment1.html). If your dashboard matches the example, submit it on Stellar. If not, please email the instructors with the error message you received. 

## Additional Resources

`dplyr` and `tidyr` are well-established packages within the `R` community, and there are many resources to use for reference and further learning. Some of our favorites are below. 

- Tutorials by Hadley Wickham for `dplyr` [basics](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html), [advanced grouped operations](https://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html), and [database interface](https://cran.r-project.org/web/packages/dplyr/vignettes/databases.html).
- Third-party [tutorial](http://www.dataschool.io/dplyr-tutorial-for-faster-data-manipulation-in-r/) (including docs and a video) for using `dplyr`
- [Principles](http://vita.had.co.nz/papers/tidy-data.pdf) and [practice](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) of tidy data using `tidyr`
- (Detailed) [cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf?version=0.99.687&mode=desktop) for `dplyr` and `tidyr` 
- A useful [cheatsheet](https://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html) for `dplyr` joins
- [Comparative discussion](http://stackoverflow.com/questions/21435339/data-table-vs-dplyr-can-one-do-something-well-the-other-cant-or-does-poorly) of `dplyr` and `data.table`, an alternative package with higher performance but more challenging syntax.  

Some of the infinitude of visualization subjects we did not cover are: heatmaps and 2D histograms, statistical functions, plot insets, ...  And even within the Tidyverse, don't feel you need to limit yourself to `ggplot`.  Here's a good overview of some [2d histogram techniques](http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html), a discussion on [overlaying a normal curve over a histogram](http://stackoverflow.com/questions/5688082/ggplot2-overlay-histogram-with-density-curve), a workaround to fit multiple plots in [one giant chart](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/). 

For other datasets and applications, one place to start is data hosting and competition websites like [Kaggle](http://www.kaggle.com), and there many areas like [sports analytics](http://www.footballoutsiders.com), [political forecasting](http://www.electoral-vote.com/evp2016/Info/data.html), [historical analysis](https://t.co/3WCaDxGnJR), and countless others that have [clean](http://http://www.pro-football-reference.com/), [open](http://www.kdnuggets.com/datasets/index.html), and [interesting](https://www.kaggle.com/kaggle/hillary-clinton-emails) data just waiting for you to `read.csv`. 


