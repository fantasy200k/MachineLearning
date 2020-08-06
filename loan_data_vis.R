print("Om Mahaa Ganapataye Namah")

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library("readr") # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('stringr') # string manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
library('purrr') # data manipulation
library('cowplot')
library(plotly)
library(maps)
library(fiftystater)
library(viridis)
loan <- fread("D:/Loan/loan1.csv", sep=',')

# View the Summary and get a glimpse of your data

summary(loan)
glimpse(loan)


sum(duplicated(loan))




colSums(is.na(loan))

options(repr.plot.width = 6, repr.plot.height = 4)

libary(dplyr)

loanstat <- 
  loan %>% group_by(loan_status) %>%
  summarise(count = n()) %>%=====
  mutate(pct = count/sum(count))

ggplot(loanstat,aes(x=reorder(loan_status, pct),
                    y=pct,  fill=loan_status)) +
  geom_bar(stat='identity') + 
  coord_flip()


loanstat <- 
  loan %>% group_by(grade) %>%
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)) %>%
  ggplot(aes(x=reorder(grade, -pct),
                      y=pct)) +
  geom_bar(stat='identity') + 
  coord_flip()







=