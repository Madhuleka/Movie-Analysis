############
#This R program reads the csv file with data from IMDb website and makes
#multiple plots to help visualise the data. 
#############
# Package names
packages <- c("ggplot2", "readr", "dplyr", "tidyverse", "rvest", "knitr", "xml2", "sjmisc", "ggcorrplot")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE)

movie_data <- read.csv("movie_data.csv")

#Creates list that initialises variables to count frequency of genre and
#plots a bar chart to visualise the distribution of genres in the top 1000 list

list_1 = c(0,0,0,0,0,0,0)
names(list_1) <- c("Genre_1","Genre_2","Genre_3","Genre_4","Genre_5","Genre_6", "Genre_7")

for(element in movie_data$Genre)
{ 
  y = unlist(strsplit(element,","))
  if(str_contains(y, c("Action","War","Adventure"),ignore.case = TRUE,logic = "or"))
    list_1["Genre_1"]=list_1["Genre_1"] + 1
  if(str_contains(y, c("Drama","Family"),ignore.case = TRUE,logic = "or"))
    list_1["Genre_2"] = list_1["Genre_2"] + 1
  if(str_contains(y, c("Fantasy","Sci-Fi"),ignore.case = TRUE,logic = "or"))
    list_1["Genre_3"] = list_1["Genre_3"] + 1
  if(str_contains(y, c("Crime","Mystery","Thriller"),ignore.case = TRUE,logic = "or"))
    list_1["Genre_4"] = list_1["Genre_4"] + 1
  if(str_contains(y, "Biography",ignore.case = TRUE,logic = "or"))
    list_1["Genre_5"] = list_1["Genre_5"] + 1
  if(str_contains(y, c("Comedy","Romance"),ignore.case = TRUE,logic = "or"))
    list_1["Genre_6"] = list_1["Genre_6"] + 1
  if(str_contains(y, "Horror",ignore.case = TRUE,logic = "or"))
    list_1["Genre_7"] = list_1["Genre_7"] + 1
}

genre_names<-c("Adventure,War,Action","Drama,Family","Fantasy,Sci-fi","Crime,Mystery,Thriller","Biography","Comedy,Romance","Horror")
genre_count<-c(list_1["Genre_1"],list_1["Genre_2"],list_1["Genre_3"],list_1["Genre_4"],list_1["Genre_5"],list_1["Genre_6"],list_1["Genre_7"])

barplot(genre_count,names.arg=genre_names,xlab="Genre",ylab="Count",col="blue",
        main="Genre chart")


#Creates list that initialises variables to count frequency of rating and 
#plot bar chart to visualise the distribution of rating of the movies in the 
#top 1000 list

list_2 = c(0,0,0,0)
names(list_2) <- c("Lessthan_8.5","Lessthan_9","Lessthan_9.5","Lessthan_10")

for(element in movie_data$Rating)
{
  if(element < 8.5)
  list_2["Lessthan_8.5"] = list_2["Lessthan_8.5"] + 1
  if(element >= 8.5 & element < 9)
    list_2["Lessthan_9"] = list_2["Lessthan_9"] + 1
  if(element >= 9 & element < 9.5)
    list_2["Lessthan_9.5"] = list_2["Lessthan_9.5"] + 1
  if(element >= 9.5)
    list_2["Lessthan_10"] = list_2["Lessthan_10"] + 1
    
}

Rating_list <- c(list_2["Lessthan_8.5"], list_2["Lessthan_9"], list_2["Lessthan_9.5"], list_2["Lessthan_10"])
Rating_name <- c("Less than 8.5", "Between 8.5 and 9", "Between 9 and 9.5", "Greater than 9,5")
barplot(Rating_list,names.arg=Rating_name,xlab="Rating",ylab="Frequency",col="blue",
        main="Rating chart") 

#Plots a scatterplot runtime vs the rating the movie has received

plot(movie_data$Runtime, list_rating)
model = lm(list_rating ~ movie_data$Runtime)
abline(model)

#Creates list that initialises variables to count frequency of decade of release
#of movies and plot a bar chart to visualise the distribution of the different
#decades in the top 1000 list

list_3 = c(0,0,0,0,0,0,0)
names(list_3) <- c("60s_and_before", "70s", "80s","90s","00s","10s","20s")

for(element in movie_data$Year)
{ 
  if(element < 1970)
    list_3["60s_and_before"] = list_3["60s_and_before"] + 1
  if(element >= 1970 && element < 1980)
    list_3["70s"] = list_3["70s"] + 1
  if(element >= 1980 && element < 1990)
    list_3["80s"] = list_3["80s"] + 1
  if(element >= 1990 && element < 2000)
    list_3["90s"] = list_3["90s"] + 1
  if(element >= 2000 && element < 2010)
    list_3["00s"] = list_3["00s"] + 1
  if(element >= 2010 && element < 2020)
    list_3["10s"] = list_3["10s"] + 1
  if(element >= 2020 && element < 2030)
    list_3["20s"] = list_3["20s"] + 1
}

Year_list <- c("<70s", "70s", "80s", "90s", "00s", "10s", "20s")
Year_count <- c(list_3["60s_and_before"], list_3["70s"], list_3["80s"], list_3["90s"], list_3["00s"], list_3["10s"], list_3["20s"])
barplot(Year_count, names.arg = Year_list, xlab = "Year", ylab = "Frequency", col = "blue", main = "Year chart")

#Creates list that initialises variables to count frequency of number of votes
#and plots a bar chart to visualise the distribution of the number of votes
#the movies on the top 1000 list

list_4 = c(0,0,0,0,0,0)
names(list_4) = c("<50k", "<100k", "<250k", "<500k", "<750k", "<1m")
for(element in movie_data$Number_of_Votes)
{ 
  if(element < 50000)
    list_4["<50k"] = list_4["<50k"] + 1
  if(element >= 50000 && element < 100000)
    list_4["<100k"] = list_4["<100k"] + 1
  if(element >= 100000 && element < 250000)
    list_4["<250k"] = list_4["<250k"] + 1
  if(element >= 250000 && element < 500000)
    list_4["<500k"] = list_4["<500k"] + 1
  if(element >= 500000 && element < 750000)
    list_4["<750k"] = list_4["<750k"] + 1
  if(element >= 750000 && element < 1000000)
    list_4["<1m"] = list_4["<1m"] + 1
}

Vote_list <- c("<50k", "<100k", "<250k", "<500k", "<750k", "<1m")
Vote_Count <- c(list_4["<50k"], list_4["<100k"], list_4["<250k"], list_4["<500k"], list_4["<750k"], list_4[">1m"])
barplot(Vote_Count, name.arg = Vote_list, xlab = "Vote Count", ylab = "Frequency", col = "blue", main = "Votes Chart")


#Creates a matrix with rating as rows and the number of votes as columns and
#plots a line graph to visualise the trend among different rating

list_votes = c()
for(element in movie_data$Number_of_Votes)
{
  list_votes = append(list_votes, element)
}

list_rating = c()
for(element in movie_data$Rating)
{
  list_rating = append(list_rating, element)
}

rating_count=matrix(0,5,6)
rownames(rating_count)=c("8","8.5", "9", "9.5", "10")
colnames(rating_count)=c("<50k","<100k", "<250k", "<500k", "<750k", ">750")


for(i in 1:1000)
{ 

    j = case_when(
     list_rating[i] <= 8 ~ 1,
     list_rating[i]<=8.5 ~ 2,
     list_rating[i]<=9 ~ 3,
     list_rating[i]<=9.5 ~ 4,
     list_rating[i]<=10 ~ 5
    )
    if(list_votes[i]<= 50000)
    {
      rating_count[j,1] <- rating_count[j,1] + 1
    }
    if(list_votes[i] > 50000 && list_votes[i]<= 100000)
    {
      rating_count[j,2] <- rating_count[j,2] + 1
    }
    if(list_votes[i] > 100000 && list_votes[i]<= 250000)
    {
      rating_count[j,3] <- rating_count[j,3] + 1
    }
    if(list_votes[i] > 250000 && list_votes[i]<= 500000)
    {
      rating_count[j,4] <- rating_count[j,4] + 1
    }
    if(list_votes[i] > 500000 && list_votes[i]<= 750000)
    {
      rating_count[j,5] <- rating_count[j,5] + 1
    }
    if(list_votes[i] > 750000)
    {
      rating_count[j,6] <- rating_count[j,6] + 1
    }
  }


#Plots scatter plot of rating vs the log of the number of votes


plot(log(list_votes), list_rating)
model = lm(list_rating ~ log(list_votes))
abline(model)

col_set <- rainbow(5)
par(mar=c(5.1, 4.1, 4.1, 8), xpd=TRUE)
matplot(t(rating_count), type = "l", xlab="Rating",col=col_set,lty=c(1:14),lwd=3)
xcolnames<-c("<50k","<100k", "<250k", "<500k", "<750k", ">750k")
names <- c("8","8.5", "9", "9.5", "10")
axis(side=1, at=1:length(colnames), labels=colnames)
legend("right", inset = c(-0.35,0 ),                   
       legend = names,xpd=TRUE,cex=0.8, lty=c(1:14),col=col_set,lwd=3)




#Creates a matrix with genres as rows and the number of votes as columns and
#plots a line graph to visualise the trend among different genres

list_genre = c()

for(element in movie_data$Genre)
{
  y = unlist(strsplit(element,","))
  list_genre = append(list_genre, element)
}


genre_vs_votes=matrix(0,14,6)
rownames(genre_vs_votes)=c("Adventure", "War", "Action", "Drama", "Family", 
                           "Fantasy", "Sci-Fi", "Crime", "Mystery", "Thriller", 
                           "Biography", "Comedy", "Romance", "Horror")
colnames(genre_vs_votes)=c("<50k","<100k", "<250k", "<500k", "<750k", ">750")




for(i in 1:1000)
{ 
  
  j = case_when(
    grepl("Adventure", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 1,
    grepl("War", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 2,
    grepl("Action", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 3,
    grepl("Drama", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 4,
    grepl("Family", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 5,
    grepl("Fantasy", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 6,
    grepl("Sci-Fi", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 7,
    grepl("Crime", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 8,
    grepl("Mystery", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 9,
    grepl("Thriller", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 10,
    grepl("Biography", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 11,
    grepl("Comedy", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 12,
    grepl("Romance", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 13,
    grepl("Horror", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 14
  )

  if(list_votes[i]<= 50000)
  {
    genre_vs_votes[j,1] <- genre_vs_votes[j,1] + 1
  }
  if(list_votes[i] > 50000 && list_votes[i]<= 100000)
  {
    genre_vs_votes[j,2] <- genre_vs_votes[j,2] + 1
  }
  if(list_votes[i] > 100000 && list_votes[i]<= 250000)
  {
    genre_vs_votes[j,3] <- genre_vs_votes[j,3] + 1
  }
  if(list_votes[i] > 250000 && list_votes[i]<= 500000)
  {
    genre_vs_votes[j,4] <- genre_vs_votes[j,4] + 1
  }
  if(list_votes[i] > 500000 && list_votes[i]<= 750000)
  {
    genre_vs_votes[j,5] <- genre_vs_votes[j,5] + 1
  }
  if(list_votes[i] > 750000)
  {
    genre_vs_votes[j,6] <- genre_vs_votes[j,6] + 1
  }
}

#Plots line graph depicting the trend in number of votes among 
#different genres
col_set <- rainbow(14)
colnames<-c("<50k","<100k", "<250k", "<500k", "<750k", ">750")
genres = c("Adventure", "War", "Action", "Drama", "Family", 
           "Fantasy", "Sci-Fi", "Crime", "Mystery", "Thriller", 
           "Biography", "Comedy", "Romance", "Horror")

par(mar=c(5.1, 4.1, 4.1, 8), xpd=TRUE)
matplot(t(genre_vs_votes), type = "l", xlab="Number of Votes",col=col_set,lty=c(1:14),lwd=3)
axis(side=1, at=1:length(colnames), labels=colnames)
legend("right", inset = c(-0.35,0 ),                   
       legend = genres,xpd=TRUE,cex=0.8, lty=c(1:14),col=col_set,lwd=3)


#Creates a matrix with genres as rows and the rating as columns and
#plots a line graph to visualise the trend among different genres

genre_vs_rating=matrix(0, 14, 5)
rownames(genre_vs_rating)=c("Adventure", "War", "Action", "Drama", "Family", 
                           "Fantasy", "Sci-Fi", "Crime", "Mystery", "Thriller", 
                           "Biography", "Comedy", "Romance", "Horror")
colnames(genre_vs_rating)=c("8","8.5", "9", "9.5", "10")

for(i in 1:1000)
{ 
  
  j = case_when(
    grepl("Adventure", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 1,
    grepl("War", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 2,
    grepl("Action", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 3,
    grepl("Drama", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 4,
    grepl("Family", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 5,
    grepl("Fantasy", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 6,
    grepl("Sci-Fi", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 7,
    grepl("Crime", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 8,
    grepl("Mystery", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 9,
    grepl("Thriller", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 10,
    grepl("Biography", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 11,
    grepl("Comedy", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 12,
    grepl("Romance", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 13,
    grepl("Horror", list_genre[i], fixed=TRUE,ignore.case = FALSE) ~ 14
  )

if(list_rating[i]<= 8)
{
  genre_vs_rating[j,1] <- genre_vs_rating[j,1] + 1
}
if(list_rating[i] > 8 && list_rating[i]<= 8.5)
{
  genre_vs_rating[j,2] <- genre_vs_rating[j,2] + 1
}
if(list_rating[i] > 8.5 && list_rating[i]<= 9)
{
  genre_vs_rating[j,3] <- genre_vs_rating[j,3] + 1
}
if(list_rating[i] > 9 && list_rating[i]<= 9.5)
{
  genre_vs_rating[j,4] <- genre_vs_rating[j,4] + 1
}
if(list_rating[i] > 9.5 && list_rating[i]<= 10)
{
  genre_vs_rating[j,5] <- genre_vs_rating[j,5] + 1
}
}



#Plots line graph depicting the trend in rating among 
#different genres

col_set <- rainbow(14)
colnames<-c("8","8.5", "9", "9.5", "10")

par(mar=c(5.1, 4.1, 4.1, 8), xpd=TRUE)
matplot(t(genre_vs_rating), type = "l", xlab="Rating",col=col_set,lty=c(1:14),lwd=3)
axis(side=1, at=1:length(colnames), labels=colnames)
legend("right", inset = c(-0.35,0 ),                   
       legend = genres,xpd=TRUE,cex=0.8, lty=c(1:14),col=col_set,lwd=3)
