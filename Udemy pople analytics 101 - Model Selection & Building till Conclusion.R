library("readxl")
data <- read_xlsx('raw data/Hands-on+Dataset.xlsx')
dim(data)

head(data)
head(data,10)
tail(data,10)
View(data)
str(data)
table(data$status)
(170/686)*100

summary(data)
data$status <- as.factor(data$status)
data$no_of_promotions <- as.factor(data$no_of_promotions)
data$risk_of_attrition <- as.factor(data$risk_of_attrition)
data$potential_rating <- as.factor(data$potential_rating)

summary(data)
data$job_level <- as.factor(data$job_level)
summary(data)

library(summarytools)
dfSummary(data)

colSums(is.na(data))

data$var_rating <-as.factor(data$performance_rating_2018 - data$performance_rating_2017)
data$var_rating

data$percent_salary_change <- (data$salary_2018-data$salary_2017)/data$salary_2017*100
data$percent_salary_change

data$age <- 2018 - data$year_of_birth
data$age

dfSummary(data$percent_salary_change)
dfSummary(data$age)
dfSummary(data$var_rating)

data[ ,c('year_of_birth','salary_2017','salary_2018','performance_rating_2017','performance_rating_2018')] <- list(NULL)

data[ ,c('hire_date','e_code')] <- list(NULL)

head(data)

library("vcd")
mosaic(~ gender + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))

tab <-table(data$status)
prop.table(tab)

tab2 <-table(data$status,data$gender)
tab2

round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ service_agreement + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab2 <-table(data$status,data$service_agreement)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ job_level + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black", "yellow"),5,2)))
tab2 <-table(data$status,data$job_level)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ var_rating + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black", "yellow"),5,2)))
tab2 <-table(data$status,data$var_rating)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

tab2 <-table(data$status,data$no_of_promotions)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ risk_of_attrition+ status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black"),4,2)))
tab2 <-table(data$status,data$risk_of_attrition)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ potential_rating + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black","yellow"),5,2)))
tab2 <-table(data$status,data$potential_rating)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ awards + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab2 <-table(data$status,data$awards)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

mosaic(~ signon + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab2 <-table(data$status,data$signon)
round(prop.table(tab2,1)*100, digits=2)
round(prop.table(tab2,2)*100, digits=2)

tab <- table(data$gender, data$status)
chisq.test(tab)

tab <- table(data$service_agreement, data$status)
chisq.test(tab)

tab <- table(data$job_level, data$status)
chisq.test(tab)

tab <- table(data$no_of_promotions, data$status)
chisq.test(tab)


tab <- table(data$risk_of_attrition, data$status)
chisq.test(tab)

tab <- table(data$potential_rating, data$status)
chisq.test(tab)

tab <- table(data$awards, data$status)
chisq.test(tab)

tab <- table(data$signon, data$status)
chisq.test(tab)

tab <- table(data$var_rating, data$status)
chisq.test(tab)

library(ggplot2)

p <- ggplot(data, aes(x=status, y=age)) + geom_violin()
p + geom_violin(trim=FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$age, data$status, mean)
avg

med <- by(data$age, data$status, median)
med

mini <- by(data$age, data$status, min)
mini

maxi <- by(data$age, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=age)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

p <- ggplot(data, aes(x=status, y=distance_from_home)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$distance_from_home, data$status, mean)
avg

med <- by(data$distance_from_home, data$status, median)
med

mini <- by(data$distance_from_home, data$status, min)
mini

maxi <- by(data$distance_from_home, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=manager_sat)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$manager_sat, data$status, mean)
avg

med <- by(data$manager_sat, data$status, median)
med

mini <- by(data$manager_sat, data$status, min)
mini

maxi <- by(data$manager_sat, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=employee_sat)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$employee_sat, data$status, mean)
avg

med <- by(data$employee_sat, data$status, median)
med

mini <- by(data$employee_sat, data$status, min)
mini

maxi <- by(data$employee_sat, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=bonus)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$bonus, data$status, mean)
avg

med <- by(data$bonus, data$status, median)
med

mini <- by(data$bonus, data$status, min)
mini

maxi <- by(data$bonus, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=no_courses_taken)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$no_courses_taken, data$status, mean)
avg

med <- by(data$no_courses_taken, data$status, median)
med

mini <- by(data$no_courses_taken, data$status, min)
mini

maxi <- by(data$no_courses_taken, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=time_in_position)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$time_in_position, data$status, mean)
avg

med <- by(data$time_in_position, data$status, median)
med

mini <- by(data$time_in_position, data$status, min)
mini

maxi <- by(data$time_in_position, data$status, max)
maxi

p <- ggplot(data, aes(x=status, y=percent_salary_change)) + geom_violin()
p + geom_violin(trim=TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$percent_salary_change, data$status, mean)
avg

med <- by(data$percent_salary_change, data$status, median)
med

mini <- by(data$percent_salary_change, data$status, min)
mini

maxi <- by(data$percent_salary_change, data$status, max)
maxi

t.test(age ~ status, data=data)

t.test(distance_from_home ~ status, data=data)

t.test(manager_changes ~ status, data=data)

t.test(manager_sat ~ status, data=data)

t.test(employee_sat ~ status, data=data)

t.test(bonus ~ status, data=data)

t.test(no_courses_taken ~ status, data=data)

t.test(time_in_position ~ status, data=data)

t.test(percent_salary_change ~ status, data=data)

data[ ,c("gender","job_level","signon", "manager_changes", "bonus","time_in_position")] <- list(NULL)

names(data)


library(fastDummies)

results_dummy <- dummy_cols(data, remove_most_frequent_dummy = T)

names(results_dummy)

results_dummy[ ,c("status","service_agreement","no_of_promotions","risk_of_attrition","potential_rating","awards")] <- list(NULL)

names(results_dummy)

colnames(results_dummy)[27] <- "var_rating_minus1"

colnames(results_dummy)[26] <- "var_rating_minus3"

names(results_dummy)

library(caret)

set.seed(45)

results_dummy <- results_dummy[sample(nrow(results_dummy)),]

trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list = FALSE)

trainIndex

trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list = TRUE)

trainIndex

trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list = FALSE)
X_train <- results_dummy[trainIndex,]
X_test <- results_dummy[-trainIndex,]

dim(X_train)

dim(X_test)

library(randomForest)

model_1 <- randomForest(status_1 ~.,data=X_train) 
model_1 

str(X_train)

X_train$status_1 <- as.factor(X_train$status_1)
X_test$status_1 <- as.factor(X_test$status_1)
model_1 <- randomForest(status_1 ~.,data=X_train) 
model_1 

getTree(model_1,1)

library(randomForestExplainer)

importance_frame <- measure_importance(model_1)
importance_frame
varImpPlot(model_1)

# model evaluation technique 

predictions2 <- predict(model_1, X_train)
table(X_train$status_1, predictions2)
(357+124)/(nrow(X_train))*100

 # use test data to evaluate model 

predictions3 <- predict(model_1,X_test)
table(X_test$status_1, predictions3)
(142+36)/(nrow(X_test))*100

# comments: results not very different from X_train model 

library(pROC)

result.roc <- roc(as.numeric(as.character(X_test$status_1)), as.numeric(as.character(predictions3)))
plot(result.roc)

