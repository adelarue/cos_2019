expand_amenities <- function(listings_df) {
  listings_df <- listings_df %>%
    mutate(amenities = gsub("[{}]|\"|[()]|-", "", amenities)) %>%
    mutate(amenities = gsub(" |/", "_", amenities)) %>%
    mutate(amenities = gsub("24", "x24", amenities)) %>%
    mutate(amenities = gsub("translation_missing:_en.hosting_amenity",
                            "other", amenities))
  
  # Then, split the strings by amenity and create new columns
  splitting <- strsplit(listings_df$amenities, ",")
  all_amenities <- Reduce(union, splitting)
  for (i in all_amenities) {
    listings_df[paste0("amenity_", i)] <- 
      grepl(i, listings_df$amenities)
  }
  return(listings_df)
}
