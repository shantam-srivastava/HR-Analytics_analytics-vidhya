###Importing the dataset
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

##structure(to get a glimpse of the data)
str(train)
str(test)


##Summary
summary(train)
summary(test)

###packages
library(dplyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(car)
library(caTools)
library(mice)
library(randomForest)
library(ggplot2)


###DATA CLEANING
##NA's
colSums(is.na(train))      ###4124 NA's in "previous year rating"
colSums(is.na(test))       ###1812 NA's in "previous year rating".

##Blanks
colSums(train=="")     ###2409 blanks in education"
colSums(test=="")     ###1034 blanks in "education"
##A lot of blank values are present therefore converting it into NA's.
##using na.strings="" in the start, and check.names=F.

train <- read.csv("train.csv", stringsAsFactors = F, na.strings = "", check.names = F)
test <- read.csv("test.csv", stringsAsFactors = F, na.strings = "", check.names = F)

#joining both the data sets for data cleaning in one go.
final <- bind_rows(train, test)

#Checkind distinct observations
sapply(train, n_distinct)
sapply(test, n_distinct)


##Checkin NA's.
colSums(is.na(train))      
colSums(is.na(test)) 
colSums(is.na(final))       ##NA's=23490 in is_promoted because complete column is absent in test file.

#NA's (previous year rating=5936)
#     (education=3443)

##Checking Blanks

colSums(train=="")    ###blanks are replaced with NA's.
colSums(test=="") 


##CHecking Unique values 
sapply(final,n_distinct)


##checking Duplicates.
sum(duplicated(train[,-1]))
train_1 <- train[!duplicated(train[ , -1]), ]
sum(duplicated(test[,-1]))
final <- bind_rows(train_1, test)
sum(duplicated(final))
sum(duplicated(final[,-1]))  ##26 duplicated values are still present due to the test data, therefore not altering the data further.


summary(final) #At last, NA -> education=3428, previous year rating=5874, is_promoted=23490(because of the absence of complete this row in test data).
table(final$is_promoted)  ##4665 candidates got promoted, 50025 were not promoted.
prop.table(table(final$is_promoted))*100 #only 8.5% members are promoted.

##Analyzing NA's in Eduction seperately.
education_na <- final[is.na(final$education),]
View(subset(education_na, is_promoted==1))

##no common pattern found that can surely conclude that they were promoted/not promoted.
#therefore naming the NA category as others.

final$education[is.na(final$education)] <- "others"
table(final$education)


##UNIVARIATE ANALYSIS
##DATA VISUALISATION based on each variable.
#1.DEPARTMENT.(Categoriacal Variable)
promoted_plot <- ggplot(final[1:54690,], aes(fill = as.factor(is_promoted)))

promoted_plot + geom_bar(aes(x = department))  #maximum no. of promotions are from sales and marketing department.Hence some departmental bias is there.

knitr::kable(table(final$department,final$is_promoted))
final$department <- as.factor(final$department)

#2.REGION
final$region <- as.factor(final$region)
promoted_plot + geom_bar(aes(x = region)) #not clear.There are way too many categories.
ggplot(final, aes(x=factor(is_promoted), fill=as.factor(region)))+geom_bar(position = "dodge")
regionwise <- final%>%group_by(region)%>%summarise(total_promoted=sum(is_promoted,na.rm = T))%>%arrange(desc(total_promoted))  #highest promotions are from region2.
#therefore unnecessary variable.
final$region <- NULL

#3.EDUCATON
promoted_plot + geom_bar(aes(x = education))
educationwise <- final%>%group_by(education)%>%summarise(total_promoted=sum(is_promoted,na.rm = T))%>%arrange(desc(total_promoted))
#bachelors are maximum promoted.Hence education of a person does matter in an Industry.
knitr::kable(table(final$education,final$is_promoted))
final$education <- as.factor(final$education)


#4.GENDER
promoted_plot + geom_bar(aes(x = gender)) 
knitr::kable(table(final$gender,final$is_promoted)) #more no. male candidates are promoted but there are more number of male employees too therefore no gender bias as such.
final$gender <- as.factor(final$gender)

#5.RECRUITMENT CHANNEL
promoted_plot + geom_bar(aes(x = recruitment_channel)) #maximum candidates from "other" recruitment channel are promoted.
knitr::kable(table(final$recruitment_channel,final$is_promoted))
final$recruitment_channel <- as.factor(final$recruitment_channel)


#6.NO. OF TRAININGS
n_distinct(final$no_of_trainings)
promoted_plot + geom_bar(aes(x = no_of_trainings)) #not clear.
trainingnwise <- final%>%group_by(no_of_trainings)%>%summarise(total_promoted=sum(is_promoted,na.rm = T))%>%arrange(desc(total_promoted)) #candidates with 1 training are promoted max.
table(final$no_of_trainings)
#Treating the outliers.
plot(quantile(final$no_of_trainings, seq(0,1,0.01)))
quantile(final$no_of_trainings, seq(0,1,0.01))
final$no_of_trainings[final$no_of_trainings > 4] <- 5
trainingnwise1 <- final%>%group_by(no_of_trainings)%>%summarise(total_promoted=sum(is_promoted,na.rm = T))%>%arrange(desc(total_promoted))


#7.AGE
n_distinct(final$age)  ##numeric variable.
table(final$age)
promoted_plot + geom_histogram(aes(x = age), binwidth = 8, col = 'black')
plot(quantile(final$age, seq(0,1,0.01), na.rm = T)) #no outliers as such. OK curve.

#8. PREVIOUS YEAR RATING
table(final$previous_year_rating)
sum(is.na(final$previous_year_rating))

promoted_plot + geom_bar(aes(x = previous_year_rating))
ratingnwise <- final%>%group_by(previous_year_rating)%>%summarise(total_promoted=sum(is_promoted,na.rm = T))%>%arrange(desc(total_promoted)) 


##IMPUTATION OF MISSING VALUES USING KNN method.
require(DMwR)

knnOutput <- knnImputation(final[, !names(final) %in% "is_promoted"])  # perform knn imputation.
anyNA(knnOutput)
knnOutput$previous_year_rating <- as.integer(knnOutput$previous_year_rating) #to convert the float values into proper integer again though with some error.

final$previous_year_rating <- knnOutput$previous_year_rating
final$previous_year_rating <- as.factor(final$previous_year_rating)
sum(is.na(final$previous_year_rating))

rm(education_na, educationwise, regionwise, ratingnwise, trainingnwise, trainingnwise1, train_1)

#8. LENGTH OF SERVICE.
n_distinct(final$length_of_service)  ##numeric variable.
table(final$length_of_service)
promoted_plot + geom_histogram(aes(x = length_of_service), binwidth = 6, col = 'black')
plot(quantile(final$length_of_service, seq(0,1,0.01), na.rm = T))  ##outliers present.
final$length_of_service[final$length_of_service>20] <- 22
plot(quantile(final$length_of_service, seq(0,1,0.01), na.rm = T)) ##seems fine.


#9. KPIs MET>80%
table(final$`KPIs_met >80%`)
final$`KPIs_met >80%` <- as.factor(final$`KPIs_met >80%`)
promoted_plot + geom_bar(aes(factor(x = `KPIs_met >80%`))) #candidates with KPIs>80% have been promoted more.
knitr::kable(table(final$`KPIs_met >80%`,final$is_promoted)) ##hence KpI does matter.

#10. AWARDS WON?
table(final$`awards_won?`)
final$`awards_won?` <- as.factor(final$`awards_won?`)
promoted_plot + geom_bar(aes(factor(x = `awards_won?`)))   #candidates with awards=0, have been promoted more.
final$`awards_won?` <- as.factor(final$`awards_won?`)
knitr::kable(table(final$`awards_won?`,final$is_promoted))  ##but awards does matter on the employee performance.


#11. AVERAGE TRAINING SCORE.
n_distinct(final$avg_training_score)
summary(final$avg_training_score)
promoted_plot + geom_histogram(aes(x = avg_training_score), binwidth = 20, col = 'black')
plot(quantile(final$avg_training_score, seq(0,1,0.01), na.rm = T))  ##outlier present.
quantile(final$avg_training_score, seq(0,1,0.01), na.rm = T)
final$avg_training_score[final$avg_training_score>91] <- 93
final$avg_training_score[final$avg_training_score<44] <- 42
plot(quantile(final$avg_training_score, seq(0,1,0.01), na.rm = T)) ##much better.

str(final)

#cleaning complete.
rm(promoted_plot)

# It will be good idea to treat continuous and categorical variables separately.
colnames(final)
sapply(final, n_distinct)

cont_vars <- final[,c("no_of_trainings", "age", "length_of_service",
                       "avg_training_score")]

# Check correlation among continuous variables
cormat <- cor(cont_vars)

require(corrplot)
corrplot(cormat, method = 'number')
# High correlation between some variables.

# Scale continuous variables
cont_vars <- as.data.frame(sapply(cont_vars, scale))

# Now lets see categorical variables
cat_vars <- final[, !colnames(final) %in% colnames(cont_vars)]
names(cat_vars)

sapply(cat_vars, n_distinct)

# Joing them to create master dataset again
master <- cbind(cat_vars[,1], cont_vars, cat_vars[,-1])

colnames(master)[1] <- "employee_id"

str(master)
#EDA complete.

#####MODEL BUILDING####

train_mod <- master[which(!is.na(master$is_promoted)),]
test_mod <- master[which(is.na(master$is_promoted)),]
prop.table( table(train_mod$is_promoted)) * 100

#creating training and validation dataset.
set.seed(123)
index = sample.split(train_mod$is_promoted, SplitRatio = 0.75)

train.data <- train_mod[index, -1]
valid.data <- train_mod[!index, -1]

#Creating the dummy dataset.
require(dummies)
train.dum <- dummy.data.frame(train_mod[,-1])
test.dum <- dummy.data.frame(test_mod[,-1])


tr.dum <- train.dum[index, ]
val.dum <- train.dum[!index, ]



rm(cat_vars, cont_vars, cormat, master, index)
dev.off()


#model 
model_1 <- glm(is_promoted ~ ., data = tr.dum, family = 'binomial')
summary(model_1)
#AIC=16241

require(MASS)
model_2 <- stepAIC(model_1, direction = 'both')

summary(model_2)
#AIC=16238

require(car)
sort(vif(model_2), decreasing = T)
#vif of some variables are high but they are important to our analysis too, therefore removing as per the p values.

#education= bachelors
model_3 <- glm(formula = is_promoted ~ no_of_trainings + age + length_of_service + 
                 avg_training_score + departmentAnalytics + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` +`educationMaster's & above` + previous_year_rating1 + previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + `KPIs_met >80%0` + 
                 `awards_won?0`, family = "binomial", data = tr.dum)

summary(model_3)
# Now all variables are significant. So this is Final logistic model.
log_model <- model_3

# Lets predict.
pred_probability <- predict(log_model, newdata = val.dum, type = 'response')

summary(pred_probability)
actual_promoted <- factor(ifelse(val.dum$is_promoted == 1,"1","0"))

# Calculating the optimal probalility cutoff.
s <- seq(min(pred_probability), max(pred_probability), length=500)

OUT <- matrix(0,500,3)
colnames(OUT) <- c("Sensitivity", "Specificity", "Accuracy")

require(caret)
# Creating custom function to find cutoff probability at which
# sensitivity, sepeificity and overall accuracy are all equal.

cutoff_finder <- function(cutoff) {
  predicted_promoted <- factor(ifelse(pred_probability > cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_promoted, actual_promoted, positive = "1")
  out <- c(conf$byClass[1], conf$byClass[2], conf$overall[1]) 
  return(out) }

for(i in 1:500) {OUT[i,] <- cutoff_finder(s[i])}

cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

# Thus, predicting final booking as per cutoff value of predicted probability
predicted_promoted <- factor(ifelse(pred_probability >= cutoff, "1", "0"))

con_mat <- confusionMatrix(predicted_promoted, actual_promoted, positive = "1")
con_mat

# Logistic regression model achieves 77.65% accuracy at cutoff probability.

pred_probability <- predict(log_model, newdata = test.dum, type = 'response')

test$is_promoted <- factor(ifelse(pred_probability >= cutoff, "1", "0"))
write.csv(test[,c('employee_id', 'is_promoted')], 'HR_LOGREG.csv', row.names = F)
