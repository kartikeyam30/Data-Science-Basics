#reading files
cust <- read.csv("R case study 1 (Retail)\\R case study 1 (Retail)\\Customer.csv")
tran <- read.csv("R case study 1 (Retail)\\R case study 1 (Retail)\\transactions.csv")
prod <- read.csv("R case study 1 (Retail)\\R case study 1 (Retail)\\prod_cat_info.csv")

#merging with merge
custran <- merge(cust, tran, by.x = 'customer_Id', by.y = 'cust_id')
final <- merge(custran, prod, by = "prod_cat_code", by.x= 'prod_subcat_code', by.y= 'prod_sub_cat_code')
#merging with joins in dplyr
library(dplyr)
custran2 <- left_join(cust, tran, by=c('customer_Id'='cust_id'))
final2 <- left_join(custran, prod, by= c("prod_cat_code", 'prod_subcat_code'= 'prod_sub_cat_code'))

#column names and data types
sapply(final2, typeof)
#top 10 observations by total_amt
top10 <- head(arrange(final2, total_amt), n=10)
#frequency tables
apply(final2[-1], 2, table)

#Histograms for all continuous vars and frequency for categorical vars
hist(final2$customer_Id)
hist(final2$city_code)
hist(final2$transaction_id)
hist(final2$total_amt)
hist(final2$prod_cat_code)

library(ggplot2)
a= aggregate(final2$Gender, length, by= final2['Gender'])
barplot(a$x, names.arg = a$Gender)

b= aggregate(final2$prod_cat, length, by= final2['prod_cat'])
barplot(b$x, names.arg = b$prod_cat)

#Range of dates in years
max =as.Date(max(tran$tran_date))
min= as.Date(min(tran$tran_date))
diff= as.numeric(max-min)/365

#Count of negative values in transactions
neg = sum(tran$total_amt<0)

#categories more popular in females than males
cat= aggregate(final2, length, by = list(final2$prod_cat, final2$Gender))
cat= cat[order(cat$Qty),]
#Here, we can see that all categories except Footwear have more female count by quantity of purchase

#city code with most customers and percentage
cit= aggregate(final2, length, by = final2['city_code'])
cit= cit[order(cit$city_code)]
citper = max(cit$city_code)
tot= sum(cit$city_code)
percentage= citper*100/tot
#It is hence found to be 4 with 10.51% of the total values.

#Store type with max sale
quant = aggregate(final2$Qty, length, by = final2['Store_type'])
valmax= aggregate(final2$total_amt, by = final2['Store_type'], FUN=sum)
#Most sales in both categories is done by E-shop

#Amt earned by electronics and clothing
catearn <- aggregate(final2$total_amt, FUN= sum, by = final2['prod_cat'])
#Electronics has earned 10722464 and clothing has earned 6251137

#Money from male customers in electronics
malelec= aggregate(final2$total_amt, FUN=sum, by = list(final2$prod_cat, final2$Gender))
#It is a total of 5703109.425

#Customers with more than 10 transactions
topcustdf<- final2[final2$total_amt>0, ]
topcust <- aggregate(topcustdf$transaction_id, length, by= (topcustdf["customer_Id"]))
topcust <- topcust[topcust$x>10, ]

#Finding customers between 25-35
library(lubridate)
lastdb <- final2
lastdb$DOB <- as.Date(lastdb$DOB, '%d-%m-%Y')
lastdb$age <- as.Date(Sys.Date(), '%d-%m-%Y')
lastdb$age <- difftime(lastdb$age, lastdb$DOB, units= "days")
lastdb$age <- lastdb$age/365

lastdb <- lastdb[(lastdb$age>25 & lastdb$age<35), ]
totalspend <- aggregate(lastdb$total_amt, FUN= sum, by = lastdb['prod_cat'])
totalspend <- totalspend[c(2, 4), ] 
totalspend <- sum(totalspend$x)
#Total spend by these customers in electronics and books was 7552979.98
lastdb$tran_date <- as.Date(lastdb$tran_date, '%d-%m-%Y')

datedb = lastdb[lastdb$tran_date >= '01-01-2014' & lastdb$tran_date <= '01-03-2014', ]
datedb = na.omit(datedb)
