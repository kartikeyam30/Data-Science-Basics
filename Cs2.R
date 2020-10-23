
customer <- read.csv("R case study 2 (Credit card)\\R case study 2 (Credit card)\\Customer Acqusition.csv")
repayment <- read.csv('R case study 2 (Credit card)\\R case study 2 (Credit card)\\Repayment.csv')
spend  <- read.csv('R case study 2 (Credit card)\\R case study 2 (Credit card)\\spend.csv')

#Replacing values less than 18 with mean
customer$Age[customer$Age<18] <- mean(customer$Age)

#Replacing values in amount if limit exceeded
library(dplyr)
df1 <- left_join(customer, spend, by= 'Customer')

df1$Amount <- ifelse(df1$Amount>df1$Limit, df1$Limit/2, df1$Amount)

#Replacing repayment with limit
df2 <- left_join(customer, repayment, by='Customer')
df2$Limit <- ifelse(df2$Amount>df2$Limit, df2$Amount, df2$Limit)

#Number of customers
custcount <- aggregate(customer$City, length, by = customer['Customer'])
nrow(custcount)
#100 customers

#Number of categories
nrow(aggregate(spend$Type, length, by = spend['Type']))
#15 categories

#Avg monthly spend
sapply(spend, typeof)
spend$Month = as.Date(spend$Month, '%d-%b-%y') 
library(lubridate)
dfm <- spend
dfm$Month <- month(as.POSIXlt(dfm$Month, format="%d/%m/%Y"))
dfm <- aggregate(dfm$Amount, FUN=mean, by= dfm['Month'])

#Avg monthly repayment
repayment$Month = as.Date(repayment$Month, '%d-%b-%y') 
library(lubridate)
dfm2 <- repayment
dfm2$Month <- month(as.POSIXlt(dfm2$Month, format="%d-%b-%Y"))
dfm2 <- aggregate(dfm2$Amount, FUN=mean, by= dfm2['Month'])

#Profit of bank
spend2 <- spend
spend2$Month <- month(as.POSIXlt(spend2$Month, format="%d/%m/%Y"))
spend2 <- aggregate(spend2$Amount, FUN=sum, by= list(spend2$Month, spend2$Customer))
repayment2 <- repayment
repayment2$Month <- month(as.POSIXlt(repayment2$Month, format="%d/%m/%Y"))
repayment2 <- aggregate(repayment2$Amount, FUN=sum, by= list(repayment2$Month, repayment2$Customer))


dmp <- merge(repayment2, spend2, by= c('Group.1', 'Group.2'), all.x = TRUE, all.y = TRUE)
dmp[is.na(dmp)]<- 0
dmp <- transform(dmp, difference= x.x-x.y)
dmp <- setNames(aggregate(dmp$difference, FUN=sum, by= dmp['Group.1']), c("Month", "Profit"))

dmp<- dmp[dmp$Profit>0,]

pwi <- dmp
pwi$Profit<- pwi$Profit*102.9/100


#Top 5 products
topfp<- aggregate(spend$Type, length, by= list(spend$Type))
topfp<- head(topfp[order(-topfp$x), ], 5)

#Highest spend by city
city<- left_join(customer, spend, by= 'Customer')
city<- aggregate(city$Amount, FUN=sum, by= city["City"])
city<- head(city[order(-city$x), ], 1)

#Highest spending age group
agegrp<- left_join(customer, spend, by= 'Customer')
agegrp<- aggregate(agegrp$Amount, FUN=sum, by= agegrp["Age"])
agegrp<- head(agegrp[order(-agegrp$x),], 1)

#Top10 in repayment
rp<- repayment
rp<- aggregate(rp$Amount, FUN=sum, by= rp["Customer"])
rp<- head(rp[order(-rp$x), ], 10)

#City wise spend on product
c1 <- left_join(customer, spend, by= 'Customer')
c1 <- aggregate(c1$Amount, FUN=sum, by= c1[c("City", "Type")])

library(ggplot2)
ggplot(c1, aes(x=City, y=x))+
  geom_bar(stat='identity', fill="forest green")+
  facet_grid(.~Type)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 1.5))

#Monthly graph of spends, city wise
df1 <- left_join(customer, spend, by= 'Customer')
df1$Month <- month(as.POSIXlt(df1$Month, format="%d-%b-%Y"))
df1<- aggregate(df1$Amount, FUN=sum, by= df1[c("City", "Month")])

ggplot(df1, aes(x=City, y=x))+
  geom_bar(stat='identity', fill="forest green")+
  facet_grid(.~Month)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 1.5))

#Yearly spend on air tickets
sx<- spend
sx$Month <- year(as.POSIXlt(sx$Month, format="%d-%b-%Y"))
sx<- aggregate(sx$Amount, length, by= sx[c('Month', 'Type')])
sx <- subset(sx, Type== 'AIR TICKET')
ggplot(sx, aes(x=Month, y=x))+
  geom_bar(stat='identity', fill="forest green")+
  facet_grid(.~Type)

#Comparison of monthly spend of products
df1 <- left_join(customer, spend, by= 'Customer')
df1$Month <- month(as.POSIXlt(df1$Month, format="%d-%b-%Y"))
df1<- aggregate(df1$Amount, FUN=sum, by= df1[c("Month", "Type")])

ggplot(df1, aes(x=Month, y=x))+
  geom_bar(stat='identity', fill="forest green")+
  facet_grid(.~Type)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 1.5))
#Most products sell well in the first half of the year.
#More than 50% of total categories had lowest sales between 3-6th month, making the second quarter their worst

#Function for top 10 customers per city per type per month
funx <- function(x){
  customer <- read.csv("C:\\Users\\lenovo\\Desktop\\R Case Studies (All 3 case studies)\\R case study 2 (Credit card)\\Customer Acqusition.csv")
  repayment <- read.csv('C:\\Users\\lenovo\\Desktop\\R Case Studies (All 3 case studies)\\R case study 2 (Credit card)\\Repayment.csv')
  
  dmp <- left_join(customer, repayment, by= c('Customer'))
  dmp <- setNames(aggregate(list(dmp$Amount), FUN=sum, by= list(dmp$Month, dmp$Customer, dmp$Product)), c('Month', 'Customer', 'Product', 'Amount'))
  
  dmf <- lapply(split(dmp, dmp$Product), x)
  
}  

