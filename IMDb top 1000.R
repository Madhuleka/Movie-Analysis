
# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Scrapped Data from IMDb top 1000
movie_reviews <- read_html("https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating") %>%
  html_nodes(".lister-item-header") %>%
  html_text()
movie_reviews
  
#Scrapped Runtime of the movies
movie_runtime <- read_html("https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating")%>%
  html_nodes(".runtime") %>%
  html_text()
movie_runtime
x <- parse_number(movie_runtime)
x

#Scrapped Rating of the movies
movie_rating <- read_html("https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating") %>%
  html_nodes(".rating-rating") %>%
  html_text()
movie_rating

#Scrapped Certification of the movies
movie_certificate <- read_html("https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating")%>%
  html_nodes(".certificate") %>%
  html_text()
movie_certificate


#Scrapped Genre of the movies
movie_genre <- read_html("https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating")%>%
  html_nodes(".genre") %>%
  html_text()
movie_genre 

#Put the data in a dataframe
Movie_Dataframe<-data.frame(Name = movie_reviews, Run_Time = c(x),
                            Rating = c(movie_rating), Genre = c(movie_genre))

Movie_Dataframe

movie_csv <- write.csv(Movie_Dataframe, "./movie_data.csv", row.names = FALSE)


