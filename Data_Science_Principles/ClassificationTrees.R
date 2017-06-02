#Libraries for Classificatin Trees
library(rpart)
library(rpart.plot)
library(dplyr)

###########################################################################
#
# Golf Example


#Reading in the golf data and viewwing
golf_df <- read.csv(file.choose(),header=T,stringsAsFactors = TRUE)
View(golf_df)

#Building the classification tree
golf_tree <- rpart(Play ~ Outlook+Temperature+Humidity+Windy, data=golf_df, control = rpart.control(minsplit=1))
#Plotting the tree
plot(golf_tree)
#Printing the text for the tree
text(golf_tree)

#Understanding the generic labels
levels(golf_df$Outlook)

#Using the rpart.plot package and the prp() plotting function
library(rpart.plot)
prp(golf_tree,type=4,extra=3)

#An alternative method to view a plot, open the golf_tree.ps file -- conversion to pdf is necessary
#post(golf_tree, file="D:/Teaching/DSCI210/golf_tree.ps")

#Viewing the details of the classification tree
summary(golf_tree)

###########################################################################
#
# Mammals Example


#Reading in the mammals dataset
mammals_df <- read.csv(file.choose(),header=T,stringsAsFactors = TRUE)
View(mammals_df)

#Fitting the classification tree to the TrainingData
library(dplyr)
mammals_tree <- rpart(Mammal~Blood+Birth+X4Legs+Hibernates, data=filter(mammals_df,Designation == "TrainingData"), control=rpart.control(minsplit = 1))
#Plotting the classification tree
prp(mammals_tree,type=4,extra=3)


#Making predictions for test dataset
mammal_predict <- predict(mammals_tree,newdata=filter(mammals_df,Designation=="TestData"),type="class")


Misclassify = function(Predicted,Actual) {
  temp <- table(Predicted,Actual)
  cat("\n")
  cat("Table of Misclassification\n")
  cat("(rows: predicted, columns: actual)\n")
  print(temp)
  cat("\n")
  numcor <- sum(diag(temp))
  numinc <- length(Actual) - numcor
  mcr <- numinc/length(Actual)
  cat(paste("Misclassification Rate = ",100*round(mcr,3),"%"))
  cat("\n")
}

###############################################################################
#
# Mushrooms Example

mushrooms_df <- read.csv(file.choose(),header=T,stringsAsFactors = TRUE)
View(mushrooms_df)


#Some basic mosaic plots before building classification rules
#set up a 4x3 plotting window
par(mfrow=c(4,3))
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$CapShape),2),ylab="Percent",xlab="CapShape")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$CapSurface),2),ylab="Percent",xlab="CapSurface")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$CapColor),2),ylab="Percent",xlab="CapColor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$HasBruises),2),ylab="Percent",xlab="HasBruises")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$Odor),2),ylab="Percent",xlab="Odor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$GillAttachment),2),ylab="Percent",xlab="GillAttachment")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$GillSpacing),2),ylab="Percent",xlab="GillSpacing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$GillSize),2),ylab="Percent",xlab="GillSize")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$GillColor),2),ylab="Percent",xlab="GillColor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$StalkSurfaceAboveRing),2),ylab="Percent",xlab="StalkSurfaceAboveRing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$StalkSurfaceBelowRing),2),ylab="Percent",xlab="StalkSurfaceBelowRing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$StalkColorAboveRing),2),ylab="Percent",xlab="StalkColorAboveRing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$StalkColorBelowRing),2),ylab="Percent",xlab="StalkColorBelowRing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$VeilType),2),ylab="Percent",xlab="VeilType")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$VeilColor),2),ylab="Percent",xlab="VeilColor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$RingNumber),2),ylab="Percent",xlab="RingNumber")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$RingType),2),ylab="Percent",xlab="RingType")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$SporePrintColor),2),ylab="Percent",xlab="SporePrintColor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$Population),2),ylab="Percent",xlab="Population")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$Habitat),2),ylab="Percent",xlab="Habitat")
#Back to 1x1 plotting window
par(mfrow=c(1,1))

#Plotting the predictors with the most impact
par(mfrow=c(2,2))
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$Odor),2),ylab="Percent", xlab="Odor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$SporePrintColor),2),ylab="Percent",xlab="SporePrintCOlor")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$StalkColorBelowRing),2),ylab="Percent",xlab="StalkColorBelowRing")
barplot(prop.table(table(mushrooms_df$Poisonous,mushrooms_df$CapSurface),2),ylab="Percent",xlab="CapSurface")
par(mfrow=c(1,1))


#Setting up a test dataset
test_rows <- sample(1:8124,0.30*8124,replace=F)
head(sort(test_rows),20)


mushrooms_train <- mushrooms_df[-test_rows,]

mushroom_tree <- rpart(Poisonous ~ ., data=mushrooms_df[test_rows,])
prp(mushroom_tree,type=4,extra=3)

mushroom_tree2 <- rpart(Poisonous ~ ., data=mushrooms_df[-test_rows,], control=rpart.control(cp=0.001,minsplit = 3))
prp(mushroom_tree2,type=4,extra=3)

