library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)



#DEFINITIONS
#Assume there are 100 customers
#10 out of them bought milk, 8 bought butter and 6 bought both of them.
#bought milk => bought butter
#Support = P(Milk & Butter) = 6/100 = 0.06
#confidence = support/P(Butter) = 0.06/0.08 = 0.75
#lift = confidence/P(Milk) = 0.75/0.10 = 7.5



mydata<-read_xlsx("Online Retail.xlsx",sheet = 1,col_names = TRUE)
str(mydata)
#Convert Variables Appropriately
mydata$Description<-as.factor(mydata$Description)
mydata$Country<-as.factor(mydata$Country)
mydata$Date<-as.Date(mydata$InvoiceDate)
mydata$Time<-format(mydata$InvoiceDate,"%H:%M:%S")
mydata$Hour<-format(mydata$InvoiceDate,"%H")
mydata$InvoiceNo<-as.numeric(mydata$InvoiceNo)
#Remove rows with missing values
mydata<-na.omit(mydata)
head(mydata)
summary(mydata)


#What time people make purchases
ggplot(data=mydata, aes(x=Hour)) + geom_histogram(stat="count",fill="indianred")



#Convert Matrix into transaction class to required by the apriori
test_data<-mydata[,c(1,3)]
write.csv(x = test_data,file = "edited_data.csv",row.names = FALSE)
trans<-read.transactions(file = "edited_data.csv", sep = ",",
                          format = "single", cols = c("InvoiceNo","Description"))

#Check Frequency of items
itemFrequencyPlot(trans, topN=10,type="absolute",cex.names=.6)


#Run the model
rules <- apriori(trans, parameter = list(supp=0.005, conf=0.9))
rules <- sort(rules, by='count', decreasing = TRUE)
inspect(rules[1:5])


#Run model with a particular product on LHS
selected_rules<-apriori(trans, parameter=list(supp=0.01,conf = 0.6,minlen=2), appearance = list(default="rhs", lhs="REGENCY TEA PLATE PINK"), control = list(verbose=F))
summary(selected_rules)
inspect(selected_rules[1:2])

#Plot top rules
topRules <- rules[1:10]
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")

plot(selected_rules,method="graph",interactive=TRUE,shading=NA)



























