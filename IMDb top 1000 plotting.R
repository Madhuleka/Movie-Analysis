
# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2", "sjmisc")

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

for(element in movie_data$Certificate) {
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

barplot(Frequency, names.arg = Certificate, xlab = "Certificate", ylab = "Frequency")
ggplot(movie_data, mapping = aes(x=Runtime, y=Rating)) +  geom_point()
ggplot(movie_data, mapping = aes(x=Rating, y=Genre)) +  geom_point()

Count_1 = 0
Count_2 = 0
Count_3 = 0
Count_4 = 0
Count_5 = 0
Count_6 = 0
Count_7 = 0
Count_8 = 0
Count_9 = 0
Count_10 = 0
Count_11 = 0

for(element in movie_data$Genre)
{ 
  y = unlist(strsplit(element,","))
  if(str_contains(y, c("Action","War","Adventure"),ignore.case = TRUE,logic = "or"))
    Count_1=Count_1+1
  if(str_contains(y, c("Drama","Family"),ignore.case = TRUE,logic = "or"))
    Count_2 = Count_2 + 1
  if(str_contains(y, c("Fantasy","Sci-Fi"),ignore.case = TRUE,logic = "or"))
    Count_3 = Count_3 + 1
  if(str_contains(y, c("Crime","Mystery","Thriller"),ignore.case = TRUE,logic = "or"))
    Count_4 = Count_4 + 1
  if(str_contains(y, "Biography",ignore.case = TRUE,logic = "or"))
    Count_5 = Count_5 + 1
  if(str_contains(y, c("Comedy","Romance"),ignore.case = TRUE,logic = "or"))
    Count_6 = Count_6 + 1
  if(str_contains(y, "Horror",ignore.case = TRUE,logic = "or"))
    Count_7 = Count_7 + 1
}
genre_names<-c("Adventure,War,Action","Drama,Family","Fantasy,Sci-fi","Crime,Mystery,Thriller","Biography","Comedy,Romance","Horror")
genre_count<-c(Count_1,Count_2,Count_3,Count_4,Count_5,Count_6,Count_7)

barplot(genre_count,names.arg=genre_names,xlab="Genre",ylab="Count",col="blue",
        main="Genre chart")
pie(genre_count, genre_names, radius = 1, main = "Genre Chart")

Count_less8.5 = 0
Count_less9 = 0
Count_less9.5 = 0
Count_less10 = 0

for(element in movie_data$Rating)
{
  if(element < 8.5)
  Count_less8.5 = Count_less8.5+1
  if(element >= 8.5 & element < 9)
    Count_less9 = Count_less9+1
  if(element >= 9 & element < 9.5)
    Count_less9.5 = Count_less9.5+1
  if(element >= 9.5)
    Count_less10 = Count_less10+1
    
}

Rating_list <- c(Count_less8.5, Count_less9, Count_less9.5, Count_less10)
Rating_name <- c("Less than 8.5", "Between 8.5 and 9", "Between 9 and 9.5", "Greater than 9,5")
pie(Rating_list, Rating_name, radius = 1, main = "Rating Chart")

