
# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

movie_data <- read.csv("movie_data.csv")

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

Certificate <- c("A", "U", "R", "UA", "UA13", "12", "PG")
Frequency <- c(C_A, C_U, C_R, C_UA, C_UA13, C_12, C_PG)

barplot(Frequency, names.arg = Certificate, xlab = "Ratings", ylab = "Frequency")
ggplot(Movie_Dataframe, mapping = aes(x=Run_Time, y=Rating)) +  geom_point()


