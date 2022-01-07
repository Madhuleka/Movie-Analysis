
# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


url <- "https://www.imdb.com/search/title/?count=100&groups=top_1000&sort=user_rating"
doc <- read_html(url)
Movie_Dataframe = data.frame(Name = doc %>% html_nodes(".lister-item-header") %>% html_text(),
           Rating=doc %>% html_nodes(".rating-rating") %>% html_text(),
           Runtime=parse_number(doc %>% html_nodes(".runtime") %>% html_text()), 
           Genre=doc %>% html_nodes(".genre") %>% html_text(),
           Certificate=c(doc %>% html_nodes(".certificate") %>% html_text(),rep(NA, 6)))
  
#Put the data in a dataframe

movie_csv <- write.csv(Movie_Dataframe, "./movie_data.csv", row.names = FALSE)


