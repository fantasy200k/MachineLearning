# Om Mahaa Ganapataye Namah
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import the library
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))

library(data.table)

# to handle time specific data
library(lubridate)
# Separate the data column
library(tidyr)
library(ggplot2)
library(stringr)
path <- c('D:/dat1')
setwd(path)
retail <- fread('warranty.csv')
class(retail)
#retail <- na.omit(retail)
class(retail)
retail1 <- retail 
ret1 <- data.table(retail1, key='Order_ID')
ret1[,head(.SD,24),by=Order_ID]

is.na

# nchar(retail$Ship_Date)
# ? nchar
retail$Ship_dt <- retail$Ship_Date
View(retail)
retail <- separate(retail, Ship_Date, 
                   c("Ship_Month","Ship_Date",
                     "Ship_year"),
                   sep = "/")
02/03/2019

names(retail)
head(retail[,"Product Name"])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Exploration &  Cleaning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colnames(retail) <- str_replace_all(colnames(retail),
                                    "[:space:]","")

head(retail)
head(retail$Ship_Date)
str(retail$Ship_Date)

View(retail)

df1 <- head(retail[,c("Ship_Mode"),with=FALSE])
class(df1)

summary(retail)
anyNA(retail)
summary(retail$Quantity)
retail[,.(mean(Quantity),mean(Profit))]



ggplot(retail,aes(x=Category, y=Sales))+
  geom_bar(stat='identity') +
  coord_flip()

  str(retail)
  fill <- "gold1"
  line <- "goldenrod2"
  
  retail$Ship_Month <- factor(retail$Ship_Month)
  
  ggplot(retail,aes(x=Quantity, y=Sales))+
    geom_boxplot(fill=fill,color=line) +
     scale_x_discrete(name = 'Ship_Month')
     # scale_y_continuous(name = "Ship_Month",
   #                    breaks = seq(0, 175, 25),
   #                    limits=c(0, 175)) 
   #  coord_flip()  # to plot horizontaly 

f5 <- ggplot(Family_summary, aes(x=StateCode, y=Average_CoupleAndOneDependent))+
   geom_bar(aes(fill = StateCode), stat="identity")+
   ggtitle("Average Couple Rate +1 dependent per State")+
   labs(x="State", y="Rates")
   

fill <- "#4271AE"
line <- "#1F3552"
"
ggplot(retail,aes(x=Ship_Month, y=Shipping_Cost))+
  geom_boxplot(fill=fill,color=line) +
  coord_flip()
"

ggplot(retail,aes(x=Ship_Month, y=Shipping_Cost, fill='Category'))+
  geom_bar(stat = 'identity') 
  

ggplot(retail,aes(x=Ship_Month, y=Sales,fill=Category))+
  geom_tile(color='blue') 
summary(retail)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# By default, .SD takes all 
# continuous variables (excluding grouping variables)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
retail[,lapply(.SD,mean)]

# Group by 
# the below commented one was wrong, dot was missing.
# retail[,.(mean(Profit)), by = (Ship_Mode,Category)]
retail[,.(mean(Profit)),
       by = .(Ship_Mode,sort(Category))]

retail[, 
       (avgSales = mean(Quantity,na.rm=TRUE))]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Month wise, Year wise Sales beyond 2011
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

beyond_2012 <- retail[Ship_year > 2011,]
View(beyond_2012)
beyond_2012[Ship_year > 2011,.(mean(Profit),sum(Profit)), 
            by = .(sort(Category, 
                        decreasing = FALSE),
                   sort(as.numeric(Ship_Month),
                        decreasing = FALSE))]

retail[Category == 'Furniture',
   .(avgSales = mean(Quantity,na.rm=TRUE))]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shipping Cost for the items returned 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
select - .()
filter()
group_by - by = .()

beyond_2012[Returned == 1,.(avgProfit =mean(Profit),
                            shp_Cost =sum(Shipping_Cost)), 
             by = .(sort(Category, decreasing = FALSE),
                   sort(as.numeric(Ship_Month),
                        decreasing = FALSE))]



beyond_2012[Returned == 1,.(avgProfit =mean(Profit),
                          shp_Cost =sum(Shipping_Cost))] 
                              

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No. of Shipments and the Shipping cost  by Demographic 
# Shipment Delivery Duration by Order 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ship_Date - Order_Date
str(retail)
# Demographic

shp <- retail[  ,.(No_Of_Shipments = .N,
                 Category,
          Ship_Cost = sum(Shipping_Cost)), 
           by = .(sort(Ship_Mode),sort(Category))] 
  
write.csv(shp,"D:/dat1/shp.csv")

ggplot(shp,aes(No_Of_Shipments,Category,
               fill=Ship_Cost))+
          geom_tile(color='blue')
#~~~~~~~~~~~~~~~~~
anyNA(retail)
duration = (as.Date(retail$Ship_Date,format="%m/%d/%y") - as.Date(retail$Order_Date,format="%m/%d/%y"))

 dt <- beyond_2012[,.(avgProfit =mean(Profit),
                          duration = duration),
                 by =  .(Order_ID)]


ggplot(dt,aes(ProductName,duration, fill=avgProfit) )+
   geom_tile(color='blue') +
  coord_flip()

ggplot(dt,aes(substr(ProductName,1,10),duration ) )+
  geom_bar(position='dodge',stat='identity',color='darkred')+
  coord_flip()

 #~~~~~~~~~~~~~~~~~
dt <- beyond_2012[Returned == 1,.(avgProfit =mean(Profit),
                            shp_Cost =sum(Shipping_Cost), 
                          Ship_Month),
            by = .(sort(Category, decreasing = FALSE),
                   sort(as.numeric(Ship_Month),
                       decreasing = FALSE)) ]

#~~~~~~~~~~~~~~~~~
   ggplot(beyond_2012, 
        aes(Category,as.numeric(Ship_Month), 
            fill=Sales)) +
    geom_tile(color= "darkred") +
     ggtitle("Heat Map by Category by month")
   
#~~~~~~~~~~~~~~~~~
   ggplot(beyond_2012, aes(Ship_Month,Sales,
                           fill = ProductName)) + 
     geom_tile(color = 'darkred') +
     # scale_y_continuous(labels = comma) +
     ggtitle("Sales by Category and Month") 
#~~~~~~~~~~~~~~~~~
   ggplot(beyond_2012, aes(Ship_Date,Category,
                           fill = Sales)) + 
     geom_tile(color = 'darkred') +
     # scale_y_continuous(labels = comma) +
     ggtitle("Sales by Category and Month") 
   
   
ggplot(beyond_2012, aes(Ship_Month, 
                           fill = Category)) + 
     geom_bar(position = "dodge") +
        # scale_y_continuous(labels = comma) +
     ggtitle("Sales by Category and Month") 
     # scale_fill_manual(values = mycolors)
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  No. of Items Ordered & Amount Spent by customer by Month
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# str(beyond_2012)

#  retail$Sales <- gsub(",","",retail$Sales)
#retail$Sales <- gsub("$","",retail$Sales)
#  str(retail)
# #   

# is.character(retail$Sales)

# str_replace_all(retail$Sales,'$','')
#retail <- na.omit(retail)
retail$Sales <- as.numeric(gsub("$,","",as.character(retail$Sales)))
# head(retail$Sales)
str(retail$Sales)

aSpent <- beyond_2012[,.(sort(ProductName),
                         Count = .N,
                         Qty = Quantity,
              Amt_Spent =sum(Sales), 
               month = as.numeric(Ship_Month)
                ),
             by = .(sort(CustomerName), 
            month1= sort(as.numeric(Ship_Month),
                         decreasing = FALSE),
            Prod = sort(ProductName))
            ]


write.csv(aSpent,'D:/dat1/aSpent.csv')
names(retail)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# apply, sapply, lapply, tapply,
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

x <- c(10,16,24,12)
x1 <- c(12,18,22,32)
dff <- data.frame(x,x1)
dff

a1 <- apply(dff,1,sum)
class(a1)
apply(dff,2,mean)


tapply(dff$x1,list(dff$x),mean)
# 
# ? tapply
# 
# ind <- list(c(1, 2, 2), 
#             c("A", "A", "B"))
# table(ind)
# tapply(1:3, ind) #-> the split vector
# tapply(1:3, ind, sum)

# sapply will work on a vector
l <- sapply(1:3, function(x) x^2)
class(l)
print(l)

# lapply will work on a list
l1 <- lapply(1:3,function(x) x^2)
l1
class(l1)

# to sum the 1st element, 2nd element 
mapply(sum,1:2,2:3)
rep(1:3,2)

x <- list(1,1:3,2:6)
vapply(x,FUN=sum,FUN.VALUE=0L)

vapply(a=1,b=1:3, c=2:6)


b <- matrix(c(2,4,6,8),nrow=2,ncol=2)
a <- 2

b * a

b1 <- matrix(c(2,2,2,2),nrow=2,ncol=2)
b * b1

b1+b


library(sqldf)
head(retail)

sqldf('select Shipping_Cost from retail;')

# tapply() applies a function or operation on subset of the vector broken down by a given factor variable.
tapply(retail$Sales,retail$Order_Priority,mean)

# rapply() recursively apply 
x=list(1,list(2,3),4,list(5,list(6,7)))
rapply(x,function(x){x^2})

,class=c("numeric")

cities <- c("New York", "London", "Cape Town")
vapply(cities, nchar, numeric(1))

set.seed(2)
x <- c(1,2,3,4,5,8,7)
sample(x,5)
