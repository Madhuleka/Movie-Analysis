
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

C_A = 0
C_U = 0
C_R = 0
C_UA = 0
C_UA13=0
C_12 = 0
C_PG = 0

for(element in movie_certificate) {
  if(element == "A" || element == "18") { 
    C_A = C_A + 1}
  if(element == "U") { 
    C_U = C_U + 1}
  if(element == "R") { 
    C_R = C_R + 1}
  if(element == "UA" || element == "7" ) { 
    C_UA = C_UA + 1}
  if(element == "UA13+" || element == "13") { 
    C_UA13 = C_UA13 + 1}
  if(element == "12+") { 
      C_12 = C_12 + 1}
  if(element == "PG" || element == "G") { 
    C_PG = C_PG + 1}
}

#Put the data in a dataframe
Movie_Dataframe<-data.frame(Name = movie_reviews, Run_Time = c(x),
                            Rating = c(movie_rating))

Movie_Dataframe


Certificate <- c("A", "U", "R", "UA", "UA13", "12", "PG")
Frequency <- c(C_A, C_U, C_R, C_UA, C_UA13, C_12, C_PG)

barplot(Frequency, names.arg = Certificate, xlab = "Ratings", ylab = "Frequency")
ggplot(Movie_Dataframe, mapping = aes(x=Run_Time, y=Rating)) +  geom_point()

