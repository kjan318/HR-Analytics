library(tidyverse)
library(xlsx)
library(summarytools)

data <- read.xlsx("raw data/Hands-on+Dataset.xlsx")

dim(data)

tail(data)

#univariaate Analysis  #####

str(data)

table(data$status)

tibble_data <- as_tibble(data)

str(tibble_data)

#cover field to factor type
tibble_data$status <- as.factor(tibble_data$status)

#convert to factor, because we want to count the number of people at every job level in the organization
#to covert those number field which you won't use it to do math, instead of you want to know the count of the type.
tibble_data$job_level <- as.factor(tibble_data$job_level)
tibble_data$no_of_promotions <- as.factor(tibble_data$no_of_promotions)
tibble_data$risk_of_attrition <- as.factor(tibble_data$risk_of_attrition)
tibble_data$potential_rating <- as.factor(tibble_data$potential_rating)

summary(tibble_data)
summary(data)

#get full picture of dataset
dfSummary(tibble_data)

#get miss value field
colSums(is.na(tibble_data))


#Feature Engineer Part 1   #####

##creating new field  #####
tibble_data$var_rating <- as.factor(tibble_data$performance_rating_2018 - tibble_data$performance_rating_2017)

tibble_data$percent_salary <- (tibble_data$salary_2018 - tibble_data$salary_2017)/  tibble_data$salary_2017 * 100

tibble_data$age <- 2021 - tibble_data$year_of_birth

dfSummary(tibble_data$percent_salary)
dfSummary(tibble_data$var_rating)

##drop variable not using ####

tibble_data[ ,c('year_of_birth','salary_2018','salary_2017','performance_rating_2018','performance_rating_2017')] <- list(NULL)

tibble_data[ ,c('hire_date','e_code')] <- list(NULL)

#Bi-Variate Analysis Part-1 ####
##Categorical - Categorical ####

library(vcd)

mosaic(~ gender + status, data = tibble_data, gp= gpar(fill=matrix(c("red","blue"),2,2)))

#we're moving from the approach of upgrading a chart moving into numbers.

tab <- table(tibble_data$status)
prop.table(tab)

tab2 <- table(tibble_data$status, tibble_data$gender)
round(prop.table(tab2,2)* 100, digits = 2)

##compare service_agreement & status ####

mosaic(~ service_agreement + status, data = tibble_data, gp= gpar(fill=matrix(c("red","blue"),2,2)))

tab <- table(tibble_data$status)
prop.table(tab)

tab2 <- table(tibble_data$status, tibble_data$service_agreement)
round(prop.table(tab2,2)* 100, digits = 2)

##compare Job Level & status ####

mosaic(~ job_level + status , data = tibble_data, gp= gpar(fill=matrix(c("grey30","grey40","grey50","grey60","grey70"),5,2)))

tab <- table(tibble_data$status)
prop.table(tab)

tab2 <- table(tibble_data$status, tibble_data$job_level)
# setup 1 for rows margin, 2 for column
round(prop.table(tab2,1)* 100, digits = 2)
round(prop.table(tab2,2)* 100, digits = 2)

##compare Var_rating & status ####
mosaic(~ var_rating + status , data = tibble_data, gp= gpar(fill=matrix(c("grey30","grey40","grey50","grey60","grey70"),5,2)))

tab2 <- table(tibble_data$status, tibble_data$var_rating)

round(prop.table(tab2,2)* 100, digits = 2)

# no_of_promotions vs leaving %
tab2 <- table(tibble_data$status, tibble_data$no_of_promotions)


round(prop.table(tab2,2)* 100, digits = 2)

##compare risk of attrition & status ####
mosaic(~ risk_of_attrition + status , data = tibble_data, gp= gpar(fill=matrix(c("grey40","grey50","grey60","grey70"),4,2)))

tab2 <- table(tibble_data$status, tibble_data$risk_of_attrition)
round(prop.table(tab2,1)* 100, digits = 2)
round(prop.table(tab2,2)* 100, digits = 2)

##compare potential_rating & status ####
mosaic(~ potential_rating + status , data = tibble_data, gp= gpar(fill=matrix(c("grey30","grey40","grey50","grey60","grey70"),5,2)))

tab2 <- table(tibble_data$status, tibble_data$potential_rating)
round(prop.table(tab2,1)* 100, digits = 2)
round(prop.table(tab2,2)* 100, digits = 2)

##compare awards & status ####

mosaic(~ awards + status, data = tibble_data, gp= gpar(fill=matrix(c("red","blue"),2,2)))

tab2 <- table(tibble_data$status, tibble_data$awards)
round(prop.table(tab2,2)* 100, digits = 2)
round(prop.table(tab2,1)* 100, digits = 2)

##compare signon & status ####

mosaic(~ signon + status, data = tibble_data, gp= gpar(fill=matrix(c("red","blue"),2,2)))

tab2 <- table(tibble_data$status, tibble_data$signon)
round(prop.table(tab2,2)* 100, digits = 2)
round(prop.table(tab2,1)* 100, digits = 2)





#Bi-Variate Analysis Part-2 (C-C H Testing)####

#Hypothesis 1: There is no relationship between the variables
#2. There is a listenership between the two variables so null is that there is
#   that they both are independent and alternate is that there is some level
#   of dependence there is dependency
# first assumption: trying to say that gender and status are independent and authorities 
# there will be an output in terms of P value which are well True
# And => if the P value is less than 0.05 we will accept our hypothesis and if it is more damaging and unified 
# we will accept the null hypothesis.

#A small p -value (typically ≤ 0.05) indicates strong evidence against the null hypothesis, so you reject the null hypothesis.
#A large p -value (> 0.05) indicates weak evidence against the null hypothesis, so you fail to reject the null hypothesis.
#p -values very close to the cutoff (0.05) are considered to be marginal (could go either way). ...

# So it is like this "if P is low null will go if P is high null then fly" that that is the slogan which
# you have to just remember.


## gender P-value ####
tab <- table(tibble_data$gender, tibble_data$status)
chisq.test(tab)

# X-squared = 0.029895, df = 1, p-value = 0.8627 (independent)

## Service_agreement P-value ####
tab <- table(tibble_data$service_agreement, tibble_data$status)
chisq.test(tab)

#X-squared = 6.7242, df = 1, p-value = 0.009511 (dependent) ≤ 0.05

## job_level P-value ####
tab <- table(tibble_data$job_level, tibble_data$status)
chisq.test(tab)
#X-squared = 2.3378, df = 4, p-value = 0.6739 (independent)

## no_of_promotions P-value ####
tab <- table(tibble_data$no_of_promotions, tibble_data$status)
chisq.test(tab)

#X-squared = 16.235, df = 8, p-value = 0.03914 (dependent)

## risk_of_attrition P-value ####
tab <- table(tibble_data$risk_of_attrition, tibble_data$status)
chisq.test(tab)

#X-squared = 56.128, df = 3, p-value = 3.945e-12 (dependent)

## potential_rating P-value ####
tab <- table(tibble_data$potential_rating, tibble_data$status)
chisq.test(tab)

#X-squared = 26.966, df = 4, p-value = 2.02e-0 (dependent)

## signon P-value ####
tab <- table(tibble_data$signon, tibble_data$status)
chisq.test(tab)

#X-squared = 0.74494, df = 1, p-value = 0.3881 (independent)

## var_rating P-value ####
tab <- table(tibble_data$var_rating, tibble_data$status)
chisq.test(tab)

#X-squared = 15.647, df = 4, p-value = 0.003532 (dependent)

#Bi-Variate Analysis Part-3 (Numerical-Categorical) ####
## Status vs age  ####
library(ggplot2)

p <- ggplot(tibble_data, aes(x=status, y=age)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$age, tibble_data$status, mean)
avg

med <- by(tibble_data$age, tibble_data$status, median)
med

mini <- by(tibble_data$age, tibble_data$status, min)
mini

maxi <- by(tibble_data$age, tibble_data$status, max)
maxi

## Status vs distance_from_home  ####
p <- ggplot(tibble_data, aes(x=status, y=distance_from_home)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$distance_from_home, tibble_data$status, mean)
avg

med <- by(tibble_data$distance_from_home, tibble_data$status, median)
med

mini <- by(tibble_data$distance_from_home, tibble_data$status, min)
mini

maxi <- by(tibble_data$distance_from_home, tibble_data$status, max)
maxi

## Status vs manager_changes  ####
p <- ggplot(tibble_data, aes(x=status, y=manager_changes)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$manager_changes, tibble_data$status, mean)
avg

med <- by(tibble_data$manager_changes, tibble_data$status, median)
med

mini <- by(tibble_data$manager_changes, tibble_data$status, min)
mini

maxi <- by(tibble_data$manager_changes, tibble_data$status, max)
maxi

## Status vs employee_sat  ####
p <- ggplot(tibble_data, aes(x=status, y=employee_sat)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$employee_sat, tibble_data$status, mean)
avg

med <- by(tibble_data$employee_sat, tibble_data$status, median)
med

mini <- by(tibble_data$employee_sat, tibble_data$status, min)
mini

maxi <- by(tibble_data$employee_sat, tibble_data$status, max)
maxi

## Status vs bonus  ####
p <- ggplot(tibble_data, aes(x=status, y=bonus)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$bonus, tibble_data$status, mean)
avg

med <- by(tibble_data$bonus, tibble_data$status, median)
med

mini <- by(tibble_data$bonus, tibble_data$status, min)
mini

maxi <- by(tibble_data$bonus, tibble_data$status, max)
maxi

#four is actually a thing more bonus or an average and people are to living with.
#This is a question let's look at the of the median median is zero which is good information but then
#there is a high divide between median and mean it tells us the details skewed.
#Let's go to the minimum value as the graph tells me Mom as at zero for both of them.

## Status vs no_courses_taken  ####
p <- ggplot(tibble_data, aes(x=status, y=no_courses_taken)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$no_courses_taken, tibble_data$status, mean)
avg

med <- by(tibble_data$no_courses_taken, tibble_data$status, median)
med

mini <- by(tibble_data$no_courses_taken, tibble_data$status, min)
mini

maxi <- by(tibble_data$no_courses_taken, tibble_data$status, max)
maxi




## Status vs time_in_position  ####
p <- ggplot(tibble_data, aes(x=status, y=time_in_position)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$time_in_position, tibble_data$status, mean)
avg

med <- by(tibble_data$time_in_position, tibble_data$status, median)
med

mini <- by(tibble_data$time_in_position, tibble_data$status, min)
mini

maxi <- by(tibble_data$time_in_position, tibble_data$status, max)
maxi

## Status vs percent_salary  ####
p <- ggplot(tibble_data, aes(x=status, y=percent_salary)) + geom_violin()

# trim = TRUE or FALSE is whether to trim the graphic head / tail 
p + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(tibble_data$percent_salary, tibble_data$status, mean)
avg

med <- by(tibble_data$percent_salary, tibble_data$status, median)
med

mini <- by(tibble_data$percent_salary, tibble_data$status, min)
mini

maxi <- by(tibble_data$percent_salary, tibble_data$status, max)
maxi





#Bi-Variate Analysis Part-4 (N-C H Testing) #####

t.test(age ~ status, data=tibble_data) #t = -0.30302, df = 310.18, p-value = 0.7621 (independent)
t.test(distance_from_home ~ status, data=tibble_data) #t = -3.9852, df = 213.76, p-value = 9.25e-05 close to 0 (dependent)
t.test(manager_changes ~ status, data=tibble_data) #t = 1.7954, df = 334.54, p-value = 0.07349 (independent)
t.test(manager_sat ~ status, data=tibble_data) #t = 18.569, df = 515, p-value < 2.2e-16 (dependent)
t.test(employee_sat ~ status, data=tibble_data) #t = -3.9852, df = 213.76, p-value = 9.25e-05 (dependent)
t.test(bonus ~ status, data=tibble_data) #t = -1.5958, df = 218.83, p-value = 0.112  (independent)
t.test(no_courses_taken ~ status, data=tibble_data) #t = 11.234, df = 498.21, p-value < 2.2e-16 (dependent)
t.test(time_in_position ~ status, data=tibble_data) #t = -1.0624, df = 304.71, p-value = 0.2889 (independent)
t.test(percent_salary ~ status, data=tibble_data) #t = 6.7462, df = 550.78, p-value = 3.849e-11  (dependent)

#Remove fields which P-Value shows > 0.05 -> independent variables #####

tibble_data[ , c('gender','job_level','signon','age','manager_changes','bonus','time_in_position')] <- list(NULL)
names(tibble_data)


#Feature Engineer Part 2 (dummy variable creation)   #####
library(fastDummies)

results_dummy <- dummy_cols(tibble_data, remove_most_frequent_dummy = T) #creation dummy variable

results_dummy[ , c('status','Service_agreement','no_of_promotions' ,'risk_of_attrition', 'potential_rating','awards')] <- list(NULL)

names(results_dummy)  #finding invalid filed name e.g. var_rating_-1

colnames(results_dummy)[26] <- "var_rating_minus1"
colnames(results_dummy)[27] <- "var_rating_minus3"

#Data Split   #####
# Random Split & Time Series Split
library(caret)

#Part one is laws the part was the column so we're saying to a sampling of this shuffling of this spiral
## data shuffling ########
results_dummy <- results_dummy[sample(nrow(results_dummy)),]   #data shuffling

#the data we would require a dummy index to recreate the index is nothing but a column with some numbers
#and these numbers have risen random numbers which we will send into the main database which is result
#called a screen index in which we will use the function columns create data partition

# list = FALSE get row level list = TRUE get columns level
trainIndex <- createDataPartition(results_dummy$status_1, p = 0.7, list = FALSE) #create 70 percent of the total number of rows we had.

x_train <- results_dummy[trainIndex,] #create train data set by trainIndex

x_test <- results_dummy[-trainIndex,] #crate Test data to exclude trainIndex

dim(x_train)
dim(x_test)

#Model Selection & Building - random forest ####
library(randomForest)
library(randomForestExplainer)

x_train$status_1 <- as.factor(x_train$status_1) #cover status_1 to factor instead of numeric
x_test$status_1 <- as.factor(x_test$status_1)


model_1 <- randomForest(status_1 ~., data = x_train) #should run classification not regression
#Type of random forest: classification

model_1
#Confusion matrix:
#OOB estimate of  error rate: 5.82% 
#     P N  Y
#       0  1 class.error
#A N 0 357 12  0.03252033 Act No
#A Y 1  16 96  0.14285714 Act Yes

getTree(model_1, 1)
#  status = 1 basically means it is not ending and that's how you complemented with prediction.
# It basically says it would move on to the next node minus one is that it is ending.

#   left daughter right daughter split var split point status prediction
#1              2              3         4   79.500000      1          0
#2              4              5         2    0.700000      1          0
#3              0              0         0    0.000000     -1          1

# the key takeaway is that we know the the random forest vote on the logic of winning the majority voting

##create a function for measure importance ####
importance_frame <- measure_importance(model_1) #load randomForestExplainer
importance_frame # p_value higher is more significant, gini_decrease will be lower

## Understanding GINI Impurity ####
#Hence Jenny Curtis reaches zero. Will all the records in the group fall into a single category.
#Mean decreasing Gini is the average mean off of the labels total decrease in the nodes impurity vetted
#by the proportion of pulses reaching that note in each individual decision tree in the random florist.
#This is this is effectively a measure of how important a variable is for estimating the value of the
#target variable across all the trees that make up the forest.
#A higher mean decreasing Gini indicates higher variable importance variables are stored and displayed
# The most important variables to the model will be highest in the plot and the largest mean decrease

varImpPlot(model_1) #display gini_decrease










