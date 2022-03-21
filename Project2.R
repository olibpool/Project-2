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

#R_emaildomain
data <- data %>% mutate(R_emaildomain = fct_collapse(R_emaildomain, "google"=c("gmail","gmail.com"), "yahoo"=c("yahoo.co.jp","yahoo.co.uk","yahoo.com","yahoo.com.mx","yahoo.de","yahoo.es","yahoo.fr"),"Microsoft"=c("hotmail.co.uk","hotmail.com","hotmail.de","hotmail.es","hotmail.fr","live.com","live.com.mx","live.fr","outlook.com","outlook.es"),other_level="Other"))


#Remove all V columns
data <- data[,!grepl("V",names(data))]


#id_31 
table(data$id_31)
data <- data %>% mutate(id_31 = fct_collapse(id_31, "chrome"=c("chrome","chrome 63.0 for ios","chrome 43.0 for android","chrome 46.0 for android","chrome 49.0","chrome 49.0 for android","chrome 50.0 for android","chrome 51.0","chrome 51.0 for android","chrome 51.0 for android","chrome 52.0 for android","chrome 53.0 for android","chrome 54.0 for android","chrome 55.0","chrome 55.0 for android","chrome 56.0","chrome 56.0 for android","chrome 57.0","chrome 57.0 for android","chrome 58.0","chrome 58.0 for android","chrome 59.0","chrome 59.0 for android","chrome 60.0","chrome 60.0 for android","chrome 61.0","chrome 61.0 for android","chrome 62.0","chrome 62.0 for android","chrome 62.0 for ios","chrome 63.0","chrome 63.0 for android","chrome 64.0","chrome 64.0 for android","chrome 64.0 for ios","chrome 65.0","chrome 65.0 for android","chrome 65.0 for ios","chrome 66.0","chrome 66.0 for android","chrome 66.0 for ios","chrome 67.0","chrome 67.0 for android","chrome 69.0","chrome generic","chrome generic for android"),"opera"=c("opera","opera 49.0","opera 51.0","opera 52.0","opera 53.0","opera generic"),"samsung"=c("samsung","samsung browser 3.3","samsung browser 4.0","samsung browser 4.2","samsung browser 5.2","samsung browser 5.4","samsung browser 6.2","samsung browser 6.4","samsung browser 7.0","samsung browser generic","Samsung/SCH","Samsung/SM-G531H","Samsung/SM-G532M"),"firefox"=c("firefox","firefox 47.0","firefox 48.0","firefox 52.0","firefox 55.0","firefox 56.0","firefox 57.0","firefox 58.0","firefox 59.0","firefox 60.0","firefox generic","firefox mobile 61.0"),"edge"=c("edge","edge 13.0","edge 14.0","edge 15.0","edge 16.0","edge 17.0"),"safari"=c("safari","safari 10.0","safari 11.0","safari 9.0","safari generic","mobile safari","mobile safari 10.0","mobile safari 11.0","mobile safari 8.0","mobile safari 9.0", "mobile safari generic","mobile safari uiwebview"),other_level = "Other"))
ggplot(data, aes(x=id_31, fill=isFraud))+geom_bar(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]), position="dodge") + scale_y_continuous(labels=percent)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#id_28 and id_29 highly correlated so remove one
table(Yes$id_28)/nrow(Yes)
table(No$id_28)/nrow(No)

drop <- c("TransactionDT", "DeviceInfo","id_28")
data=data[,!names(data) %in% drop]

#convert character to factor
data[sapply(data,is.character)]<-lapply(data[sapply(data,is.character)],as.factor)

#Kaggle states that the following are categorical not numeric
data$card1 <- as.factor(data$card1)
data$card2 <- as.factor(data$card2)
data$card3 <- as.factor(data$card3)
data$card5 <- as.factor(data$card5)
data$addr1 <- as.factor(data$addr1)
data$addr2 <- as.factor(data$addr2)


#Try and reduce number of levels in categorical
data <- data %>% mutate(card3 = fct_collapse(card3, "150"="150","185"="185",other_level="other"))
data <- data %>% mutate(addr2 = fct_collapse(addr2, "87"="87",other_level = "other"))
