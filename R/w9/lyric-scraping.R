library(tidyverse)
library(rvest)

##########
# ALBUMS #
##########

# Set artist name
artist <- 'The Menzingers'

# Set artist albums url, modifying the artist name to account for spaces. You
# may need to modify further depending on special characters, or just hardcode it.
artist_discography_url <- paste0(
  'https://genius.com/artists/',
  gsub(' ', '-', artist),
  '/albums'
)

# Get artist albums page html
page_discography <- read_html(artist_discography_url)

# Get album list items
discography_nodes <- page_discography %>%
  html_nodes("ul") %>%
  .[2] %>%
  html_nodes('li')

# Get album titles
album_titles <- discography_nodes %>%
  html_nodes('h3') %>%
  html_text(trim = TRUE)

# Get album urls
album_urls <- discography_nodes %>%
  html_nodes('a') %>%
  html_attr('href')

# Build df
artist_albums <- tibble(artist, album_title = album_titles, album_url = album_urls)

# NOTE
# Web scraping is often finding the right trade-off between manual and automation.
# Looking at `artist_albums`, we see that there might be albums we don't want to
# include, such as deluxe versions or live albums. Whether it's easier to manually
# remove these or come up with an automated filtering solution depends on time and
# effort. My rule of thumb is if I'm going to be running this process again in the
# future, it is worth taking the time to come up with a programmatic solution.

################
# ALBUM TRACKS #
###############

artist_tracks <- NULL

for (i in 1:nrow(artist_albums)) {
  row <- artist_albums[i, ]
  
  # Set album title
  album_title <- row$album_title
  # Set album url
  album_url <- row$album_url
  
  # Log loop status
  message("Getting data for ", album_title, " by ", artist)
  
  # Read album page
  page_album <- read_html(album_url)
  
  # Get list of tracks
  track_nodes <- page_album %>%
    html_nodes('.chart_row-content')
  
  # Get track titles
  track_titles <- track_nodes %>%
    html_nodes('h3') %>%
    html_text(trim = TRUE) %>%
    # If you ran the code above, you'd notice it includes "\n              Lyrics"
    # after each tile. This is because there is a span tag within our h3 tag. We can
    # use sub() to replace part of a string based on a pattern. Here we use a regular
    # expression to match whitespace followed by a newline character, and then
    # everything after it.
    sub("\\s*\\n.*", "", .)
  
  # Get track urls
  track_urls <- track_nodes %>%
    html_nodes('a') %>%
    html_attr('href')
  
  # Build df
  artist_tracks<- bind_rows(artist_tracks,
            tibble(artist,
                   album_title,
                   track_title = track_titles,
                   track_url = track_urls)
            )
  
}

# NOTE 
# When you are building out a scraping loop, it is almost always better to
# write the process for a single page and then scale up for the entire loop.
# For example, here is the same code as above, but we just use the first item
# in artist_albums. Breaking your problems into smaller bits is always helpful!


# page_album <- read_html(artist_albums[1, ]$album_url)
# 
# track_nodes <- page_album %>%
#   html_nodes('.chart_row-content')
# 
# track_titles <- track_nodes %>%
#   html_nodes('h3') %>%
#   html_text(trim = TRUE) %>%
#   sub("\\s*\\n.*", "", .)
# 
# track_urls <- track_nodes %>%
#   html_nodes('a') %>%
#   html_attr('href')
# 
# 
# bind_rows(artist_tracks,
#           tibble(artist, album_title, track_title = track_titles, ))



############
# HOMEWORK #
############

# Try going one step further and creating a dataframe of song lyrics. Start with
# one song and then build out your loop.
# Hint, try accessing divs with [data-lyrics-container="true"] 

