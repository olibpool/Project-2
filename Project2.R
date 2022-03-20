setwd("~/ID5059")
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(scales)
transaction <- read.csv("train_transaction.csv", header=TRUE) %>% na_if("")
identity <- read.csv("train_identity.csv", header=TRUE) %>% na_if("")
attach(transaction)
attach(identity)


data <- merge(transaction,identity,by="TransactionID", all.x=TRUE)
data$isFraud <- as.factor(data$isFraud)

#Dropping columns with certain % NA
na = colSums(is.na(data))/nrow(data)
criteria = 0.8
data = data[,!sapply(data, function(x) mean(is.na(x)))>criteria]
#Goes from 360 to 226 variables when change criteria from 0.8 to 0.75

sapply(data,class)
table(ProductCD)
table(DeviceType)
#table(DeviceInfo) #drop as too many categories

summary(data$TransactionAmt)
boxplot(data$TransactionAmt)
data <- data[data$TransactionAmt<10000,] #Two anomolies both not fraud so remove from data

#3% of the data is Fraud
table(data$isFraud)
y<-data[,2]
x<-data[,4]
featurePlot(x,y,"box") #average higher for fraud

#impute missing values
#library(missForest)
#modelImputation <- missForest(as.data.frame(data))

#Add column for hour of day that transaction takes place                     
data['hour']=(floor(data['TransactionDT']/3600))%%24
fraction<-aggregate(as.numeric(data$isFraud)-1, list(data$hour), FUN=mean)                   
plot(x=fraction$Group.1, y=fraction$x, xlab="Hour",ylab="Fraction of fraudulent transactions",type="l")
ggplot(data, aes(x=hour, fill=isFraud))+geom_bar(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]), position="dodge") + scale_y_continuous(labels=percent)
ggplot(data, aes(x=hour))+geom_bar()


#Split data into class 1 and 0
Yes <- data[which(data$isFraud == "1"),]
No <- data[which(data$isFraud=="0"),]

#Transaction amount key factor
summary(Yes$TransactionAmt) #Median 75, mean 150
summary(No$TransactionAmt) #Median 68.5, mean 134

#Product CD key factor
table(Yes$ProductCD)/nrow(Yes)#Significant proportion of type C
table(No$ProductCD)/nrow(No) #75% are of type W

#Card4 very similar proportions
table(Yes$card4)/nrow(Yes) #double the proportion of discover
table(No$card4)/nrow(No)

#Card6 could be significant
table(Yes$card6)/nrow(Yes) #credit 48%, debit 52%
table(No$card6)/nrow(No) #credit 24%, debit 75%


#P_emaildomain should be kept
data <- data %>% mutate(P_emaildomain = fct_collapse(P_emaildomain, "google"=c("gmail","gmail.com"), "yahoo"=c("yahoo.co.jp","yahoo.co.uk","yahoo.com","yahoo.com.mx","yahoo.de","yahoo.es","yahoo.fr"),"Microsoft"=c("hotmail.co.uk","hotmail.com","hotmail.de","hotmail.es","hotmail.fr","live.com","live.com.mx","live.fr","outlook.com","outlook.es")))
data <- data %>% mutate(P_emaildomain = fct_collapse(P_emaildomain, "other"=c("web.de","servicios-ta.com","twc.com","suddenlink.net","sc.rr.com","q.com","ptd.net","protonmail.com","prodigy.net.mx","netzero.net","netzero.com","gmx.de","frontiernet.net","frontier.com","embarqmail.com","cfl.rr.com","centurylink.net","cableone.net")))
ggplot(data, aes(x=P_emaildomain, fill=isFraud))+geom_bar(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]), position="dodge") + scale_y_continuous(labels=percent)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

