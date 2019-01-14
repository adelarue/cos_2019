
# Welcome 

This directory holds course materials for Session 4, Advanced Topics in Data Science. The core of this session is learning ideas and coding techniques that will help you efficiently navigate the Circle of Data Science: 

![data science](https://ismayc.github.io/moderndiver-book/images/tidy1.png)

Some ideas we'll cover include functional programming, nesting, and never, ever writing for-loops. 

# Before the Session

## Project Preparation

Working with your partner, create and submit a short report, written in R Markdown, in which you perform a short machine learning analysis on your data. Your report should answer the following questions: 

1. What is the question you are aiming to answer with your analysis? What is its scientific or operational significance? 
2. What algorithm are you using? Why did you choose that one? 
3. What are the features (independent variables)? Is there a dependent variable? 
4. What was the outcome of your analysis? Is the model "good," and according to which metrics? 

Additionally, your report should include at least one plot. 

Again, both partners should submit a version of the report. While we will not be policing code for copying and pasting, it is highly recommended for your learning that each partner type the code chunks themselves. 

## Technical Preparation

The `tidyverse` packages should already be installed from Session 3. You will also need the `ggmap` and the `knitr` package. The following code should be sufficient: 

```r
install.packages('tidyverse')
install.packages('ggmap')
install.packages('knitr')
```

To test your installation, paste the following code into your `R` console, run it, and turn in a screencap of the result on Stellar. 

```{r}
library(tidyverse)
library(knitr)
1:10 %>% 
	purrr::map(~.^2) %>% 
	unlist() %>% 
	as.data.frame() %>% 
	kable()
```

