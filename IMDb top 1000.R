#######
#This R program scraps data from the IMDb website and saves it in a the form
#of a csv file. 
########

# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE)

Movie_Dataframe = data.frame()

for(i in seq(from = 1, to = 1000, by = 250))
{
  url <- paste0("https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating,desc&count=250&start=", i, "&ref_=adv_nxt")
  doc <- read_html(url)

  Movie_Dataframe = rbind(Movie_Dataframe, data.frame(Name = doc %>% html_nodes(".lister-item-content h3 a") %>% html_text(),
                               Rating=doc %>% html_nodes(".ratings-imdb-rating strong") %>% html_text(),
                               Runtime=parse_number(doc %>% html_nodes(".runtime") %>% html_text()), 
                               Genre=gsub("[\n]", "", doc %>% html_nodes(".genre") %>% html_text()), 
                               Certificate=c(doc %>% html_nodes(".certificate") %>% html_text(),rep(NA, 250-length(doc %>% html_nodes(".certificate"))))))
}

movie_csv <- write.csv(Movie_Dataframe, "./movie_data.csv", row.names = FALSE)


