# Install the packages we need for the session
install.packages(c("tidyverse", "modelr", "ROCR", "tm", "SnowballC",
                   "caTools", "rpart", "rpart.plot", "leaflet",
                   "RColorBrewer", "glmnet"))

# Load the packages we need to validate the installation
library(glmnet)
library(tm)
library(tidyverse)

e_net <- glmnet(matrix(c(1,2,3,4,3,4,5,6), nrow = 4), c(2,4,6,8))
e_net$beta[1,]
# The output should match up with "output-1.png"

bag_of_words_ex <- "Twelve astronauts have walked on the moon, and over five 
                    hundred people have been in outer space.  Currently, two 
                    astronauts from the USA are aboard the International 
                    Space Station."

small_corpus <- Corpus(VectorSource(bag_of_words_ex)) %>%
  tm_map(tolower) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument)
ex_frequencies <- DocumentTermMatrix(small_corpus)
inspect(ex_frequencies)
# The output should match up with "output-2.png"
