rm(list = ls())
set.seed(31860346)
setwd("C:/Users/Predator/Desktop/monash/y3s1/fit3152/assignment 1")
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ]

library(ggplot2)
library(dplyr)
#Question 1
#===============================================================================
# can use boxplot to show how distribution of numerical attributes but not only way

#1a)
# count the dimension of the the datasets
dim(cvbase)

# find the data type of each attributes
str(cvbase)

# numerical data set
num_attr = cvbase[, -c(50)]
# show the distribution of numerical attributes
boxplot(num_attr, las = 2, main = "boxplot of numerical attributes")
# summary of number of numerical attributes
summary(num_attr)

# categorical data set
cat_attr = cvbase[, c(50)]
unique(cat_attr)
# show the distribution of the coded country
plot(as.factor(cat_attr), las = 2, main = "histogram of categorical attributes")
# summary of number of occurrence of the coded country
occurrence <- as.factor(cat_attr)
summary(occurrence)

count_table = table(cvbase$coded_country)
sort(count_table, decreasing = TRUE)

# count the total number of NA in the data set
sum(is.na(cvbase))

# 1b)
# replace the NA in employment status to 0
employ = cvbase[,c(21:30)]
employ[is.na(employ)] <- 0
cvbase[,c(21:30)] <- employ

# replace the NA in job insecurity to 0
job_insec = cvbase[,c(17:20)]
job_insec[is.na(job_insec)] <- 0
cvbase[,c(17:20)] <- job_insec

# checking the proportion of NA in each column
prop_na <- sort(colMeans(is.na(cvbase)))
print(prop_na)

# removing the columns of NA which is more than 20%
cvbase$trustGovCtry <- NULL
cvbase$trustGovState <- NULL

# remove all the rows with NA
cvbase <- cvbase[complete.cases(cvbase),]
# dimension after pre-processing
dim(cvbase)

#Question 2
#===============================================================================
# 2a)
# data of Malaysia
Mas_df <- cvbase[(cvbase$coded_country == "Malaysia"),]
# data other than Malaysia
other_df <- cvbase[(cvbase$coded_country != "Malaysia"),]
# remove coded_country from Mas_df as all are Malaysia
Mas_df$coded_country <- NULL
#removing coded-country from other_df as country is not important
other_df$coded_country <- NULL

# dimension of malaysia datasets and other country datasets
dim(Mas_df)
dim(other_df)

# showing all distribution of Malaysia data set in box-plot
boxplot(Mas_df, las = 2, main = "Malaysia data set boxplot")
# showing all distribution of other country data set in box-plot
boxplot(other_df, las = 2, main = "other country data set boxplot")

summary(Mas_df)
summary(other_df)

# 2b)
# show the dimension of Mas_df
dim(Mas_df)
attach(Mas_df)

# predict c19ProSo01
# correlation of c19ProSo01 and all other predictors with Malaysia datasets
round(cor(Mas_df, c19ProSo01), digits = 2)
# create a linear regression model to check the significant predictors to 
# predict c19ProSo01 in Malaysia data set in the model summary
mas.c19ProSo01.fit <- lm(c19ProSo01 ~ ., data = Mas_df)
summary(mas.c19ProSo01.fit)
# plot the linear regression model with the best predictor chosen
mas.c19ProSo01.betterfit <- lm(c19ProSo01 ~ c19ProSo02 + c19ProSo03 + c19NormShould + c19ProSo04, data = Mas_df)
#check the performance of new build linear model
summary(mas.c19ProSo01.betterfit)

# predict c19ProSo02
# correlation of c19ProSo02 and all other predictors with Malaysia datasets
round(cor(Mas_df, c19ProSo02), digits = 2)
# create a linear regression model to check the significant predictors to 
# predict c19ProSo02 in Malaysia data set in the model summary
mas.c19ProSo02.fit <- lm(c19ProSo02 ~ ., data = Mas_df)
summary(mas.c19ProSo02.fit)
# plot the linear regression model with the best predictor chosen
mas.c19ProSo02.betterfit <- lm(c19ProSo02 ~ gender + c19ProSo01 + c19ProSo03 + affAnx + affNerv + jbInsec01 + PFS03 + c19IsOrg + c19ProSo04, data = Mas_df)
#check the performance of new build linear model
summary(mas.c19ProSo02.betterfit)

# backward stepwise AIC 
# null <- lm(c19ProSo02~ 1, data = Mas_df)
# stepAIC(mas.c19ProSo02.fit, scope = list(lower = null, upper = mas.c19ProSo02.fit),
#        data = Mas_df, direction = "backward")
# mas.c19ProSo02.betterfit <- lm(formula = c19ProSo02 ~ affAnx + affNerv + affExh 
#                               + jbInsec01 + employstatus_6 + employstatus_7 + 
#                                 employstatus_9 + PFS03 + fail01 + MLQ + c19IsOrg 
#                               + gender + c19ProSo01 + c19ProSo03 + c19ProSo04, 
#                               data = Mas_df)
#summary(mas.c19ProSo02.betterfit)

# predict c19ProSo03
# correlation of c19ProSo03 and all other predictors with Malaysia datasets
round(cor(Mas_df, c19ProSo03), digits = 2)
# create a linear regression model to check the significant predictors to 
# predict c19ProSo03 in Malaysia data set in the model summary
mas.c19ProSo03.fit <- lm(c19ProSo03 ~ ., data = Mas_df)
summary(mas.c19ProSo03.fit)
# plot the linear regression model with the best predictor chosen
mas.c19ProSo03.betterfit <- lm(c19ProSo03 ~ c19ProSo01 + c19ProSo02 + c19ProSo04 + affDepr + employstatus_5 + PFS01 + c19NormShould + c19IsStrict, data = Mas_df)
#check the performance of new build linear model
summary(mas.c19ProSo03.betterfit)

# predict c19ProSo04
# correlation of c19ProSo04 and all other predictors with Malaysia datasets
round(cor(Mas_df, c19ProSo04), digits = 2)
# create a linear regression model to check the significant predictors to 
# predict c19ProSo04 in Malaysia data set in the model summary
mas.c19ProSo04.fit <- lm(c19ProSo04 ~ ., data = Mas_df)
summary(mas.c19ProSo04.fit)
# plot the linear regression model with the best predictor chosen
mas.c19ProSo04.betterfit <-lm(c19ProSo04 ~ c19ProSo03 + c19NormDo + c19IsStrict + c19ProSo01 + c19ProSo02, data = Mas_df)
#check the performance of new build linear model
summary(mas.c19ProSo04.betterfit)

# 2c)
attach(other_df)

round(cor(other_df, other_df$c19ProSo01), digits = 2)
other.c19ProSo04.fit <- lm(other_df$c19ProSo01 ~ ., data = other_df)
summary(other_fit_01)


round(cor(other_df, other_df$c19ProSo02), digits = 2)
other_fit_02 <- lm(other_df$c19ProSo02 ~ ., data = other_df)
summary(other_fit_02)

round(cor(other_df, other_df$c19ProSo03), digits = 2)
other_fit_03 <- lm(other_df$c19ProSo03 ~ ., data = other_df)
summary(other_fit_03)

round(cor(other_df, other_df$c19ProSo04), digits = 2)
other_fit_04 <- lm(other_df$c19ProSo04 ~ ., data = other_df)
summary(other_fit_04)

#Question 3
#===============================================================================
# 3a)
