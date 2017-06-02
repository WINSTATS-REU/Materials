#Scraping data usign R 
library("rvest")

#Web Scrape #1
url <- "http://course1.winona.edu/cmalone/dsci210/Exams/USQuickFacts_Webscrape1.html"
page <- read_html(url)
temptable <- html_table(page, header=TRUE)
df<-as.data.frame(temptable)

#Web Scrape #2 - table within a table
url <- "http://course1.winona.edu/cmalone/dsci210/Exams/USQuickFacts_Webscrape2.html"
page <- read_html(url)
temptable <- html_table(page, header=TRUE)[[2]]
page3<-as.data.frame(temptable)


#Web Scrape #3 
url<-"https://www.ssa.gov/oact/babynames/numberUSbirths.html"
page <- read_html(url)
temp<-html_text(html_nodes(page, ".border td"))
#temp<-html_table(html_nodes(page, "table")[[2]])
df<-as.data.frame(temp)

rowid<-rep(c(1,2,3,4),135)
Year<-as.numeric(as.character(df$temp[rowid==1]))
Males<-as.numeric(gsub(",","",df$temp[rowid==2]))
Females<-as.numeric(gsub(",","",df$temp[rowid==3]))

plot(Year,Males,type='n')
lines(Year,Males,col="blue")
lines(Year,Females,col='pink')



#Example #4: 
hanks<- read_html("http://www.imdb.com/name/nm0000158/")
hanks_links <- hanks %>% 
  html_nodes("#filmo-head-actor+ .filmo-category-section b a") %>%
  html_attr("href")

hanks_movies<-substr(hanks_links,8,16)
hanks_movie_links<-lapply(hanks_movies,function(x) paste("http://www.imdb.com/title/",x,"/",sep=""))

number_hanks<-length(hanks_movie_links)
hanks_ratings<-rep(NA,number_hanks)

for(i in 1:number_hanks){
  Sys.sleep(0.5)
  temp <- read_html(hanks_movie_links[[i]])
  temp2 <- html_nodes(temp,".ratingValue strong span")
  temp3 <- html_text(temp2)
  if(length(temp3)>0){
    hanks_ratings[i] <- as.numeric(temp3)
  }
}


#Getting information on Julia Roberts
roberts<- read_html("http://www.imdb.com/name/nm0000210/")
roberts_links <- roberts %>% 
  html_nodes("#filmo-head-actress+ .filmo-category-section b a") %>%
  html_attr("href")

roberts_movies<-substr(roberts_links,8,16)
roberts_movie_links<-lapply(roberts_movies,function(x) paste("http://www.imdb.com/title/",x,"/",sep=""))

number_roberts<-length(roberts_movie_links)
roberts_ratings<-rep(NA,number_roberts)

for(i in 1:number_roberts){
  Sys.sleep(0.5)
  temp <- read_html(roberts_movie_links[[i]])
  temp2 <- html_nodes(temp,".ratingValue strong span")
  temp3 <- html_text(temp2)
  if(length(temp3)>0){
    roberts_ratings[i] <- as.numeric(temp3)
  }
}



#Shifting Roberts ratings to align with Hanks, i.e. start of career 
#How much shift is necessary
shiftamount<-length(hanks_ratings) - length(roberts_ratings)
#Shifting the data for Roberts
roberts_ratings<-c(rep(NA,shift_amount),roberts_ratings)


#Plotting of ratings over time
plot(hanks_ratings, xlab="", ylab="Rating", ylim=c(0,10),axes=F, type="n")
lines(hanks_ratings, col="blue")
axis(1, at=c(0,80), labels=c("Most Recent", "Start of Career"))
axis(2)
lines(roberts_ratings, col="red")

#Add legend to plot
legend("bottomright", c("Hanks","Roberts"),lty=c(1,1), col=c("blue","red"))

#Mean differnce in ratings
difference<-hanks_ratings - roberts_ratings
mean(difference, na.rm=T)
title(paste("Average of (Hanks - Roberts) is",round(mean(difference,na.rm=T),2)))





