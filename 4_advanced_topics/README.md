
# Welcome 

This directory holds course materials for Session 4, Advanced Topics in Data Science. The core of this session is learning ideas and coding techniques that will help you efficiently navigate the Circle of Data Science: 

![data science](https://ismayc.github.io/moderndiver-book/images/tidy1.png)

Some ideas we'll cover include functional programming, nesting, and never, ever writing for-loops. 

# Before the Session

The `tidyverse` packages should already be installed from Session 3. ## Preassignment

Prior to the session, you must complete the preassignment below. This is a short set of instructions to install necessary software and ensure its proper functioning. 

### Install Packages

You should already have installed the `tidyverse` package. If not, 

```{r}
install.packages('tidyverse')
```

Now we need to install some packages that you may not have used before. 

```{r}
install.packages('knitr')
install.packages('leaflet')
```

### Test packages

Type or paste the following code into your console and hit "enter." 

```{r}
library(tidyverse)
list('To', 'boldly', 'go', 'where', 'no', 'man', 'has', 'gone', 'before') %>% 
    map(nchar) %>% 
    reduce(`*`)
```

Your console should print out `51840`. If you like, take a moment to think about what this code does. 

Next, type or paste the following code into your console and hit "enter." You will need an internet connection for this one. 

```{r}
    library(tidyverse)
    library(leaflet)

    m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
    m %>% addTiles()
```   

A map of the greater Boston area should appear in your viewer pane. Take a screencap of the map and upload it to Stellar. 

Please email the instructor if either of these tests do not produce the expected result. 
