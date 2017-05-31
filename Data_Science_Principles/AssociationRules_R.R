library(arules)
library(arulesViz)

BobRossPaintings <- read.csv(file.choose(),header=T,stringsAsFactors = TRUE)
View(BobRossPaintings)
str(BobRossPaintings)

#BobRoss A Rules analysis
#Create a subset to inclue only the feature columns
BobRoss2 <- as.matrix(BobRossPaintings[,4:40])

#Forcing Groceries to a transaction object
br.trans = as(BobRoss2,"transactions")

#Rule development vai apriori function
br.rules = apriori(br.trans, parameter = list(supp = 0.05, conf = 0.10, maxlen=10))
br.rules = apriori(br.trans, parameter = list(supp = 0.01, conf = 0.01, maxlen=4))


#Rules that have whole milk on right side
br.subset = subset(br.rules, subset = rhs %in% "TREES")
inspect(sort(br.subset,by="lift"))


#Print rules to screen - sorted by lift
output <- inspect(sort(br.rules,by="lift"))

View(output)

#Print rules to screen - sorted by lift
output <- head(sort(br.rules,by="lift"),40)

#More Plotting
library(arulesViz)
plot(output,method="grouped")

#Link to all paintings
#https://1209k.com/bobross/

plot(br.rules,"graph")
plot(br.rules,"graph",interactive=T)
