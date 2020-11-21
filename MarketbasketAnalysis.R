'm_'
rm(list=ls(all=TRUE))
#install.packages("arules")
library(arules)
library(arulesViz)
library(datasets)

data(Groceries)
class(Groceries)
dim(Groceries)
str(Groceries)
 

class(Groceries)


inspect(Groceries[1:3])

itemLabels(Groceries)
show(Groceries)
Groceries@itemInfo[1:20,]
level1 <- itemInfo(Groceries)[['level1']]
agg <- aggregate(Groceries,level1)
inspect(agg)

Groceries@data

# Support - how frequently an item occurs in the data.
# in otherwards what proporiton of transactions there's a particular
# item show up 

itemFrequency(Groceries[,1])


itemFrequency(Groceries[,1:6])
itemFrequencyPlot(Groceries,support=0.05)
itemFrequencyPlot(Groceries,support=0.20)
itemFrequencyPlot(Groceries,support=0.10)

itemFrequencyPlot(Groceries,topN=20)


# most freq item is whole milk in otherwords the support is high for this 
itemFrequencyPlot(Groceries,topN=5)

# Confidence its a measure of the 
# proportion of the transactiosn where the presence
# of an item or a set of items results in the
# presence of another set of items
# its a conditional prob , if I buy item A and 
# Item B how likely i also buy item c 
# so the confidence of AB => C is given  A & B is how 
# likely they buy C

itemFrequencyPlot(Groceries,topN=10)

# conf{AB} => {C} this is called association rules
rules <- apriori(Groceries)
summary(rules)
# inspect(it1[1:3])
rules <- apriori(Groceries,
                 parameter = 
                   list(support=0.007,
                        confidence=0.25,
                        minlen=2))
summary(rules)

inspect(rules[1:2])

# rule 1 says , customer those purchased herbs
# also likely to purchase root vegetables
# lift is simply confidence over support 

inspect(sort(rules, by='lift')[1:4])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rules2 <- apriori(Groceries,
                  parameter = list(support=0.009, 
                              confidence=0.25,
                              minlen=2,
                              target="Frequent Itemsets"))

summary(rules2)

it1 <- inspect(rules2[1:5])
# CUSTOMERS who bought items herbs also bought items root veg 
# it has a support .7 
# lift is something confidence over support over y (root vegetables)
# higher lift is always going to be better

# with herbs 4 times more likely that root vegetables going
# to be there than in the general transactions all k
inspect(sort(,by="lift")[1:4])


write.csv(it1,'D:/dat1/Rules.csv')
# Lift: The ratio by which by the confidence of a rule exceeds the expected confidence. 
# Note: if the lift is 1 it indicates that the items on the left and right are independent.

# may be v can put berries and whippe/sour cream close to eafch other in the aisles

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,
     shading=NA)

# What are customers likely to buy before they purchase "Product A"
summary(rules)


# converting data frame to transactions

df <- data.frame(
  age   = as.factor(c(6, 6, 8, 8, NA, 9, 16)),
  grade = as.factor(c("A", "C", "C", "C", "F", NA, "C")),
  pass  = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)) 
## convert to transaction
class(df)
trans3 <- as(df, "transactions")
show(trans3)
summary(trans3)
inspect(trans3)
class(trans3)



sqrt((8-2)^2 + (2-1)^2)