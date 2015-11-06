data <- read.csv("OnDeck Analytics Asssignment.csv")

#easier to read and manipulate in this format
library(plyr)
plydata <- tbl_df(data) 
head(plydata)
summary(plydata)


#cut delinquent days into factors, inclusive on the right side
plydata$days_delinquent_old <- cut(plydata$days_delinquent_old, c(0,1,5,10,30,60,180), labels = c("0", "1-5", "5-10", "10-30", "30-60", "60+"), right=FALSE, include.lowest=TRUE) 
plydata$days_delinquent_new <- cut(plydata$days_delinquent_new, c(0,1,5,10,30,60,180), labels = c("0", "1-5", "5-10", "10-30", "30-60", "60+"), right=FALSE, include.lowest=TRUE)


#creating transition matrix--note NaNs because there were no 0 day delinquencies to begin with
change <- select(plydata, days_delinquent_new, days_delinquent_old)
library(markovchain)
mcX<-markovchainFit(change)$estimate


#create another transition matrix showing the probability of movement from one group to another, weighted by outstanding principal balance.
#this should really be the old outstanding principal because we're looking at a time series to predict loan behavior
principal <- plydata$new_outstanding_principal_balance
second <- cbind(change, principal)


#standardizing principal to percent of mean
second$principal <- second$principal/mean(second$principal)


#discrete values so you can estimate the transition probabilities by the sample proportions
library(questionr)
weightcount <- wtd.table(second$days_delinquent_old, second$days_delinquent_new, weights=second$principal)
weightcount <- weightcount/rowSums(weightcount)                                                                      


#would expect the coverage ratio (approximated here by avg bank balance/new outstanding principal) to be key
test$coverage <- test$average_bank_balance__c/test$new_outstanding_principal_balance

#subsetting the data to begin investigating those loans that go from 10-30 days overdue to to 30-60 days
test <- subset(plydata, days_delinquent_old=="10-30")
test$worse <- test$days_delinquent_new == "30-60"

#modeling fico here as well
fit <- lm(formula = worse ~ fico + coverage, data = test)

#coverage is statistically significant-worth further investigation
summary(fit)

