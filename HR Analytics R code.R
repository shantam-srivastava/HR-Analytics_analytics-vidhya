#-------HR-Analytics Assignment-------#

#Importing datasets
train <- read.csv("train_HR Analytics.csv", stringsAsFactors = F)
test <- read.csv("test_HR Analytics.csv", stringsAsFactors = F)

#Combining two data sets for data cleaning
require(dplyr)
df <- bind_rows(train, test)

#Checking what's inside the data
str(df)
summary(df)

# Case matching
df <- mutate_if(df, is.character, toupper)
sapply(df, n_distinct)

#Checking for missing values and NA
sum(is.na(df))
colSums(is.na(df))  

# there are 5936 NA's in "previous_year_rating"

colSums(df == "")
#there are 3443 missing values in "education"

# Check duplicates
sum(duplicated(df[ , -1]))

#There are 144 duplicated rows in df

sum(duplicated(train[,-1]))
#There are 188 dupilcated rows in train

train_1 <- train[!duplicated(train[ , -1]), ]
df <- bind_rows(train_1, test)

require(ggplot2)

#Let's start the univariate analysis
#1. "employee_id" is a categorial variable and it does not have any effect on the prediction
length(unique(df$employee_id))

#2. "department" is a categorial variable
table(df$department)
df$department <- as.factor(df$department)
promotion_plot <- ggplot(df, aes(x = factor(is_promoted)))
promotion_plot + geom_bar(aes(fill = factor(department)))
promotion_plot + geom_bar(aes(fill = factor(department)), position = "fill")

df %>% group_by(department) %>% 
                summarise(count = n())
knitr::kable(table(df$department, df$education))
knitr::kable(table(df$department, df$gender))
knitr::kable(table(df$department, df$previous_year_rating))
knitr::kable(table(df$department, df$recruitment_channel))
knitr::kable(table(df$department, df$KPIs_met..80.))


#3. "region" is a categorial variable
table(df$region)
df$region <- as.factor(df$region)
knitr::kable(table(df$region, df$department))

#4. "education" is a categorial variable
table(df$education)
#there are 3443 missing values in "education"

df %>% group_by(education) %>% summarise(count = n())

#treating the missing values
df$education[df$education == ""] <- "BETWEEN SECONDARY & BACHELOR'S"

table(df$education)
df$education <- as.factor(df$education)
promotion_plot <- ggplot(df, aes(x = factor(is_promoted)))
promotion_plot + geom_bar(aes(fill = factor(education)), position = "fill")

#5. "gender" is a categorial variable
table(df$gender)
df$gender <- as.factor(df$gender)
promotion_plot <- ggplot(df, aes(x = factor(is_promoted)))
promotion_plot + geom_bar(aes(fill = factor(gender)))
promotion_plot + geom_bar(aes(fill = factor(gender)), position = "fill")

#6. "recruitment_channel" is a categorial variable
table(df$recruitment_channel)
df$recruitment_channel <- as.factor(df$recruitment_channel)
promotion_plot <- ggplot(df, aes(x = factor(is_promoted)))
promotion_plot + geom_bar(aes(fill = factor(recruitment_channel)))
promotion_plot + geom_bar(aes(fill = factor(recruitment_channel)), position = "fill")

#7. "no_of_trainings" is a numerical variable
summary(df$no_of_trainings)
table(df$no_of_trainings)
promotion_plot <- ggplot(df, aes(x = factor(is_promoted)))
promotion_plot + geom_bar(aes(fill = factor(no_of_trainings)))
promotion_plot + geom_bar(aes(fill = factor(no_of_trainings)), position = "fill")

df$no_of_trainings <- as.factor(df$no_of_trainings)

#8. "age" is a numerical variable
length(unique(df$age))
summary(df$age)
sum(df$age == 60)
#there are 306 person of age 60 (test + train)
sum(train$age == 60)
#there are 217 person of age 60 in train data only

knitr::kable(table(df$age, df$is_promoted))


#9. "previous_year_rating" is an interval-type variable
table(df$previous_year_rating)
sum(is.na(df$previous_year_rating))

#checking for the reason of NA's
df_prev_rating <- filter(df, is.na(df$previous_year_rating))
#the length of service of all these people is 1, so these NA's are valid.

#treating this variable as catagorical variable
df$previous_year_rating[is.na(df$previous_year_rating)] <- "unknown"

table(df$previous_year_rating)
sum(is.na(df$previous_year_rating))

df$previous_year_rating <- as.factor(df$previous_year_rating)

#10. "length_of_service" is a numerical variable
table(df$length_of_service)
summary(df$length_of_service)

table(test$length_of_service)

ggplot(df, aes(x = length_of_service)) + geom_histogram(aes(fill = factor(is_promoted)), binwidth = 5, col = "black")


#11. "KPIs_met..80." is a categorial variable
table(df$KPIs_met..80.)
df$KPIs_met..80. <- as.factor(df$KPIs_met..80.)

#12. "awards_won." is a categorial variable
table(df$awards_won.)
df$awards_won. <- as.factor(df$awards_won.)

#13. "avg_training_score" is a continuous variable
summary(df$avg_training_score)
ggplot(df, aes(x = avg_training_score)) + geom_histogram(aes(fill = factor(is_promoted)), binwidth = 5, col = "black")

table(df$avg_training_score)

knitr::kable(table(df$avg_training_score, df$is_promoted))
table(test$avg_training_score)

#14. "is_promoted" is a target variable
table(df$is_promoted)
str(df)

df$recruitment_channel <- NULL

#let's try to do feature engineering
#create a new column for average training score department wise
df_department_region <- df %>% group_by(region) %>% group_by(department) %>% 
                         summarise(mean_score = mean(avg_training_score))


df <- full_join(df, df_department_region, by = 'department', copy = FALSE)
df$avg_training_score_scaled_department <- df$avg_training_score/df$mean_score


colSums(is.na(df))  

colSums(df == "")

str(df)

#### EDA complete

rm(df_prev_rating, df_department_region, promotion_plot)

dev.off()

#### RF
df$is_promoted <- as.factor(df$is_promoted)

tr.data <- df[1:54690, ]
ts.data <- df[54691:78180, ]

i <- sample.split(tr.data$is_promoted, SplitRatio = 0.8)

trn <- tr.data[i, ]
val <- tr.data[!i, ]

# Building the model 
require(randomForest)

rf_model <- randomForest(is_promoted ~ ., data = trn[, -1], ntree = 1000,
                         proximity = F, do.trace = T, importance = T)

predicted_probability <- predict(rf_model, val[,-c(1, 13)], type = "prob")[,2]
summary(predicted_probability)

actual_promoted <- as.factor(val$is_promoted)

OUT <- matrix(nrow = 100, ncol = 3)
s <- seq(min(predicted_probability), max(predicted_probability), length.out = 100)

require(caret)

cutoff_finder <- function(cutoff) {
  pred_promoted <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))
  conf <- confusionMatrix(pred_promoted, actual_promoted, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- c(sens, spec, acc) 
  return(out) }

for(j in 1:100) {OUT[j,] <- cutoff_finder(s[j])}


cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

# Thus, predicting final booking as per cutoff value of predicted probability
pred_promoted <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))

con_mat <- confusionMatrix(pred_promoted, actual_promoted, positive = "1")
con_mat

ts.data$is_promoted <- factor(ifelse(predict(rf_model, ts.data, type = "prob")[,2] > cutoff, "1", "0"))
table(ts.data$is_promoted)

write.csv(ts.data[, c(1,13)], "sub_rf.csv", row.names = F)


