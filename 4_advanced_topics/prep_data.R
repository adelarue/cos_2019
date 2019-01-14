library(tidyverse)

#' read in the data
prices <- read_csv('data/raw/calendar.csv')
listings <- read_csv('data/raw/listings.csv')

#' clean the prices data frame, including computation of price_per
prices <- prices %>%
	left_join(listings, by = c('listing_id' = 'id' )) %>% 
	select(listing_id, date, price = price.x, accommodates) %>%
	mutate(price = as.numeric(gsub('\\$|,', '', price)),
		   price_per = price / accommodates) %>% 
	select(listing_id, date, price_per) %>% 
	group_by(listing_id) %>% 
	filter(sum(!is.na(price_per)) >= 200) %>% 
	na.omit() %>% 
	ungroup()

#' split out all the data. 
#' 
prices %>% 
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE),
         month = as.character(month)) %>% 
  nest(-month) %>% 
  mutate(dummy = walk2(month, 
                       data, 
                       ~write_csv(.y, paste0('data/prices/',.x, '.csv'))))

listings %>% 
  mutate(neighbourhood = ifelse(neighbourhood == "Fenway/Kenmore", "Fenway", neighbourhood)) %>% 
  mutate(neighbourhood2 = neighbourhood) %>% 
  nest(-neighbourhood2) %>% 
  mutate(dummy = walk2(neighbourhood2, 
                       data, 
                       ~write_csv(.y, paste0('data/listings/',.x, '.csv'))))

