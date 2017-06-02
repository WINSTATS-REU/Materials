#Packages to be used for Twitter Pull
library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)

#Specified from Twitter Account - DSCI_210 is app name
consumer_key <- "<consumer_key>"
consumer_secert <- "<consumer_secert>"
token_key <- "<token_key>"
token_secert <- "<token_secert>"


#Setting up OAUTH in R
setup_twitter_oauth(consumer_key,consumer_secert,token_key,token_secert)

#Gettting a pull on recent Ecuador Earthquake
pull <- searchTwitter("#trump", n=1000, lang="en")

#Check to see if returned object is a list
is.list(pull)

#Converting list to dataframe
df <- do.call("rbind", lapply(pull, as.data.frame))

#Writing data.frame out to *.csv file
write.csv(df,file="<path>/Trump.csv")


#Summaries for screenName

#Get # posts by screenName
table(df$screenName)
#Plotting results
plot(table(df$screenName))

#adding a horizontal line at 3
abline(h=3)

#Idenitfy screenNames with more than 3 counts
which(table(df$screenName)>3)


#Summaries for created
#First, understand strucutre of created variable/field
str(df)

#Plotting twitter pull across days
plot(table(as.Date(df$created)))

#The following can be used to pull hour off created variable
as.POSIXlt(df$created)$hour

#Next, table/plot outcome
plot(table(as.POSIXlt(df$created)$hour))


#usign the tm library for text mining
myCorpus <- Corpus(VectorSource(df$text))

#Clean up text using the tm_map() function
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)

#Getting rid of common english words
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)

#Creating data.frame for wordcloud
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=25)
