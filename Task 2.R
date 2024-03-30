# Logistic regression

# install the package
installed.packages("dplyr")

# loading package
library(dplyr)
library(psych)
library(tidyverse)
library(ROCR)
library(caTools)

leadsdata <- read.csv("D:\\Leads.csv")


# DATA INSPECTION ################################
# describes function from psych library
# descriptive statistics
describe(leadsdata)

# data types of each column
str(leadsdata)

# no of rows and columns
dim(leadsdata)

# summary of the dataset
summary(leadsdata)




# DATA CLEANING ##################################

# make column names to lowercase
colnames(leadsdata) <- tolower(names(leadsdata))
colnames(leadsdata)


# DUPLICATE VALUES ###############################
# find duplicates
duplicated(leadsdata)

# extract duplicates
leadsdata[duplicated(leadsdata)]
# no duplicates




# check NA values in all columns
colSums(is.na(leadsdata))

# to find total no of NA values
sum(colSums(is.na(leadsdata)))
# 8710 NA values available





# for the below 2 columns, calculate the mean without the NA and replacing the NA values with the mean
leadsdata$totalvisits = ifelse(is.na(leadsdata$totalvisits), 
                               mean(leadsdata$totalvisits, na.rm = TRUE), 
                               leadsdata$totalvisits)


leadsdata$page.views.per.visit = ifelse(is.na(leadsdata$page.views.per.visit), 
                                        mean(leadsdata$page.views.per.visit, na.rm = TRUE), 
                                        leadsdata$page.views.per.visit)





# Impute city column ###########################################################

unique(leadsdata$city)
# Create a bar plot to check the max count
ggplot(leadsdata, aes(x = city)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of city", x = "city", y = "Count")
# Mumbai shows the max count hence it is possible to impute Mumbai in the missing values & 'Select' values

leadsdata$city <- ifelse(trimws(leadsdata$city) == "Select", "Mumbai", leadsdata$city)
leadsdata$city <- ifelse(trimws(leadsdata$city) == "", "Mumbai", leadsdata$city)

# re check unique values
unique(leadsdata$city)
# re check bar chart
ggplot(leadsdata, aes(x = city)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of city", x = "city", y = "Count")


unique(leadsdata$lead.profile)# Select cells need to be replaced with NA
leadsdata$lead.profile <- ifelse(trimws(leadsdata$lead.profile) == "Select", "Other Leads", leadsdata$lead.profile)
leadsdata$lead.profile <- ifelse(trimws(leadsdata$lead.profile) == "", "Other Leads", leadsdata$lead.profile)
unique(leadsdata$lead.profile)




# Replace NA values with mode (categorical field)################################################

# Create a bar plot of lead.source
ggplot(leadsdata, aes(x = lead.source)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of lead source", x = "Lead source", y = "Count") 


# Calculate the frequency count for each category 
frequency <- table(leadsdata$lead.source) 
# Find the category with the highest count (mode) / max frequency value gives the mode value
mode <- names(frequency)[which.max(frequency)] 
# Print the mode 
print(mode)




# filling missing values in lead.source
leadsdata$lead.source[is.na(leadsdata$lead.source)] <- mode
# Create a bar plot of lead.origin after removing missing values  
ggplot(leadsdata, aes(x = lead.source)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of lead source", x = "Lead source", y = "Count") 


##################################################################################################




###for better understanding
unique(leadsdata$asymmetrique.activity.score)
table(leadsdata$asymmetrique.activity.score)

# Replace NA values with median (numeric fields)################################################
# Calculate the median 
median <- median(leadsdata$asymmetrique.activity.score,na.rm = TRUE) 
# filling missing values 
leadsdata$asymmetrique.activity.score <- ifelse(is.na(leadsdata$asymmetrique.activity.score),median, leadsdata$asymmetrique.activity.score)

unique(leadsdata$asymmetrique.activity.score)
table(leadsdata$asymmetrique.activity.score)








# remove blank cells
unique(leadsdata$last.activity)
leadsdata$last.activity <- ifelse(trimws(leadsdata$last.activity) == "", "other", leadsdata$last.activity)


# remove blank cells
unique(leadsdata$country)
leadsdata$country <- ifelse(trimws(leadsdata$country) == "", "other", leadsdata$country)


# remove blank cells
unique(leadsdata$tags)
leadsdata$tags <- ifelse(trimws(leadsdata$tags) == "", "Other", leadsdata$tags)


# convert all blank spaces into NA
#columns <- colnames(leadsdata)
#for (column in columns){
#  leadsdata[[column]] <- ifelse(trimws(leadsdata[[column]]) == "", NA, leadsdata[[column]])
#}


# find NA values 
colSums(is.na(leadsdata))
# no NA vales







# drop unwanted columns / so many NA values
leadsdata$asymmetrique.profile.score <- NULL

# drop unwanted columns / unique values: id and number
leadsdata$prospect.id <- NULL
leadsdata$lead.number <- NULL

# drop dependent variables / occupation and specialization columns are inter dependent columns 
# find unique values
unique(leadsdata$specialization)
unique(leadsdata$what.is.your.current.occupation)
# # drop unwanted column
leadsdata$specialization <- NULL
leadsdata$what.is.your.current.occupation <- NULL


# drop unwanted columns / columns that has only one categorical variable
unique(leadsdata$magazine)
unique(leadsdata$receive.more.updates.about.our.courses)

leadsdata$magazine <- NULL
leadsdata$receive.more.updates.about.our.courses <- NULL


# drop unwanted column
leadsdata$how.did.you.hear.about.x.education <- NULL
leadsdata$what.matters.most.to.you.in.choosing.a.course <- NULL




# drop unwanted column
unique(leadsdata$get.updates.on.dm.content)# only one categorical value / needs to drop
leadsdata$get.updates.on.dm.content <- NULL



# drop unwanted column
unique(leadsdata$update.me.on.supply.chain.content)# only one categorical value / needs to drop
leadsdata$update.me.on.supply.chain.content <- NULL


# drop unwanted column
unique(leadsdata$i.agree.to.pay.the.amount.through.cheque)# only one categorical value hence needs to drop the column
leadsdata$i.agree.to.pay.the.amount.through.cheque <- NULL


# drop unwanted column
leadsdata$a.free.copy.of.mastering.the.interviewlast.notable.activity <- NULL


















unique(leadsdata$lead.quality)
# Levels:  High in Relevance Low in Relevance Might be Not Sure Worst

#Converting Ordinal to Factor
leadsdata$lead.quality = factor(leadsdata$lead.quality, levels = c("Worst","Low in Relevance","Not Sure","Might be","High in Relevance",""), labels = c(1,2,3,4,5,5))
unique(leadsdata$lead.quality)
# Levels: 1 2 3 4 5


unique(leadsdata$asymmetrique.activity.index)
# Levels:  01.High 02.Medium 03.Low

leadsdata$asymmetrique.activity.index = factor(leadsdata$asymmetrique.activity.index, levels = c("01.High","02.Medium","03.Low",""), labels = c(1,2,3,3))
unique(leadsdata$asymmetrique.activity.index)


unique(leadsdata$asymmetrique.profile.index)
# Levels:  01.High 02.Medium 03.Low

leadsdata$asymmetrique.profile.index = factor(leadsdata$asymmetrique.profile.index, levels = c("01.High","02.Medium","03.Low",""), labels = c(1,2,3,3))
unique(leadsdata$asymmetrique.profile.index)






# COMPARE LEAD ORIGIN WITH "CONVERTED"
unique(leadsdata$lead.origin)
ggplot(leadsdata, aes(x = lead.origin)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of lead origin", x = "lead origin", y = "Count")


p <- leadsdata %>% 
  select(converted, lead.origin) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = converted, fill = lead.origin))

p + geom_histogram(binwidth = 0.5, position = "dodge")







# COMPARE LEAD SOURCE

unique(leadsdata$lead.source)
ggplot(leadsdata, aes(x = lead.source)) + geom_bar(fill = "steelblue") + labs(title = "Distribution of lead source", x = "lead source", y = "Count")

# Direct traffic and google have max frequencies


p <- leadsdata %>% 
  select(converted, lead.source) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = converted, fill = lead.source))

p + geom_histogram(binwidth = 0.5, position = "dodge")


# Google and Direct traffic generates maximum number of leads.
# Conversion Rate of reference leads and leads through welingak website is high.




# Likewise, it is possible to check the impact on each feature against the lead conversion rate one by one. 








# HANDLING OUTLIERS

#Plotting boxplot of continuous variable before winsorizing
boxplot(leadsdata[c("totalvisits", "total.time.spent.on.website", "page.views.per.visit")])
boxplot(leadsdata[c("asymmetrique.activity.index","asymmetrique.profile.index")])

# Create a Function to Winsorize Data
winsor <- function(x, multiplier) {
  if(length(multiplier) != 1 || multiplier <= 0) {
    stop("bad value for 'multiplier'")}
  
  quartile1 = summary(x)[2] # Calculate lower quartile
  quartile3 = summary(x)[5] # Calculate upper quartile
  iqrange = IQR(x) # Calculate interquartile range
  
  y <- x
  boundary1 = quartile1 - (iqrange * multiplier)
  boundary2 = quartile3 + (iqrange * multiplier)
  
  y[ y < boundary1 ] <- boundary1
  y[ y > boundary2 ] <- boundary2
  y
}




#Winsorizing data for total visits
leadsdata$totalvisits <- winsor(leadsdata$totalvisits, 1.5)
leadsdata$page.views.per.visit <- winsor(leadsdata$page.views.per.visit, 1.5)

#leadsdata$lead.quality <- winsor(leadsdata$lead.quality, 1.5)



#Boxplot after winsorizing
with(leadsdata, boxplot(totalvisits))
with(leadsdata, boxplot(page.views.per.visit))
#No more outliers. Heavily right skewed




#with(leadsdata, boxplot(lead.quality))# still outliers have







# Handling outliers method 2##########################

# Create box plots for the numerical columns 
boxplot(leadsdata[c("totalvisits", "page.views.per.visit")])

# Define the columns for which you want to calculate the bounds 
columns <- c("totalvisits", "page.views.per.visit") 

# Define the threshold for outliers 
outlier_threshold <- 1.5 

# Create empty vectors to store the lower and upper bounds 
lower_bounds <- c() 
upper_bounds <- c()

# Calculate the IQR and bounds for each column using a for loop 
for (column in columns) { 
  column_iqr <- IQR(data[[column]]) 
  column_lower_bound <- quantile(data[[column]], 0.75) - outlier_threshold * column_iqr 
  column_upper_bound <- quantile(data[[column]], 0.25) + outlier_threshold * column_iqr 
  
  lower_bounds <- c(lower_bounds, column_lower_bound) 
  upper_bounds <- c(upper_bounds, column_upper_bound) 
} 

# remove outliers for each column using a for loop 
for (i in 1:length(columns)) { 
  column <- columns[i] 
  lower_bound <- lower_bounds[i] 
  upper_bound <- upper_bounds[i] 
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ] 
}

# Create a combined box plot 
boxplot_data <- leadsdata[, columns, drop = FALSE] 
boxplot(boxplot_data, names = columns, main = "Box Plot of Numerical Columns")
#####




# check through recommendations' effect on conversion 

unique(leadsdata$through.recommendations)

library("ggplot2")
mine.heatmap <- ggplot(data = leadsdata, mapping = aes(x = through.recommendations, y = tags , fill = converted)) +
  geom_tile() + xlab(label = "through recommendations")

mine.heatmap

# Recommendations are not that much effected related to conversion.










#library(data.table)
#setDT(leadsdata)[, c("lead.origin","lead.source","do.not.email") := NULL]
#setDT(leadsdata)[, c("do.not.call","last.activity","through.recommendations","tags","lead.quality","lead.profile","city","asymmetrique.activity.index","asymmetrique.profile.index","a.free.copy.of.mastering.the.interview","last.notable.activity") := NULL]
#setDT(leadsdata)[, c("country","search","newspaper.article","x.education.forums","newspaper","digital.advertisement") := NULL]






# HEATMAP

#library(dplyr)

#remove columns
#leadsdata = select(leadsdata, -c('prospect.id':'lead.origin'))
#leadsdata[!(colnames(leadsdata) %in% c('',''))]




#remove decimal places
#leadsdata$page.views.per.visit = as.numeric(format(round(leadsdata$page.views.per.visit, 2)))
#leadsdata$totalvisits = as.numeric(format(round(leadsdata$totalvisits, 2)))
#leadsdata$converted = as.numeric(format(round(leadsdata$converted, 2)))

#remove columns
#setDT(leadsdata)[, c("total.time.spent.on.website") := NULL]



# heat map
ggplot(leadsdata, aes(x = totalvisits  , y = total.time.spent.on.website, fill = converted )) + geom_tile()










#installed.packages('tidyverse')





# for ROC curve / to evaluate the model
installed.packages("ROCR")


# loading package


# set seeds / when splitting data the same way it will split even when the user runs this model in a future date.
set.seed(100)

# split the data
split <- sample.split(leadsdata, SplitRatio = 0.8)
split

# split the dataset with respect to a single column
# TRUE values will goes to the training dataset
# FALSE values will goes to the test dataset
# train the model using the train dataset
# check the accuracy using the test dataset
train_reg <- subset(leadsdata, split == "TRUE")
test_reg <- subset(leadsdata, split == "FALSE")

#colnames(leadsdata)


# training the model
# Converted = binary classification / target variable
# TotalVisits + Page.Views.Per.Visit = independent variables
# the model calculates the probability of given values
# pass the train dataset when building the model
logistic_model <- glm(converted ~ totalvisits +  page.views.per.visit + total.time.spent.on.website,  
data = train_reg, family = "binomial")


  

logistic_model



# dimension
dim(train_reg)
dim(test_reg)


# model summary
summary(logistic_model)


# predict test data based on model
# use the test dataset
predict_reg <- predict(logistic_model, test_reg, type = "response")

predict_reg

test_reg


length(predict_reg)
length(test_reg)


########################################
fitted <- predict(logistic_model)
resid <- residuals(logistic_model) # The residuals are the length of the vertical dashed lines from the data to the line.
fitted
resid
plot(x=fitted, y=resid)

########################################





# changing probabilities
# if the prediction value is greater than 0.5 then the class is 1, otherwise the class is 0
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

predict_reg


# evaluating model accuracy
# use confusion matrix
test_reg$converted

table(test_reg$converted, predict_reg)



missing_classerr <- mean(predict_reg != test_reg$converted)
print(paste('Accuracy = ', 1 - missing_classerr))


# ROC-AUC curve
# true positive vs false positive
ROCPred <- prediction(predict_reg, test_reg$converted) 
# tpr = true positive rate (y axis)      
# fpr = false positive rate (x axis)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr") 


# auc = area under the curve
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
# rounding to 4 decimal places
auc <- round(auc, 4)
auc



# plot the curve
plot(ROCPer)
# add colors and cutoff points
plot(ROCPer, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1), main = "ROC Curve")
abline(a = 0, b = 1)

legend(.16, .8, auc, title = "AUC", cex = 0.5)
## Add Legend
#legend("bottomright", c("Full Model", "Stepwise Model"), lty=2:3, col = c("red", "green"), bty="n", cex=0.8)







#Applying grades################################################################
# use case 1 

# Sample dataset with scores
scores <- c(85, 92, 78, 95, 60, 74, 89, 45, 70, 88)

# Function to assign grades
assign_grade <- function(score) {
  if (score >= 90) {
    return("A")
  } else if (score >= 80) {
    return("B")
  } else if (score >= 70) {
    return("C")
  } else if (score >= 60) {
    return("D")
  } else {
    return("F")
  }
}

# Applying the function to the dataset
grades <- sapply(scores, assign_grade)

# Displaying the result
result <- data.frame(Score = scores, Grade = grades)
print(result)



#################################################################
# use case 2

# Sample dataset with features and target variable
set.seed(123)
data <- data.frame(X1 = rnorm(100), X2 = rnorm(100), Y = rnorm(100))
data

# Train a linear regression model
model <- lm(Y ~ X1 + X2, data = data)

# New data for prediction
new_data <- data.frame(X1 = rnorm(10), X2 = rnorm(10))

# Make predictions using the model
predictions <- predict(model, newdata = new_data)
predictions
summary(predictions)

# Function to assign grades based on predicted values
assign_grade <- function(prediction) {
  if (prediction >= 2) {
    return("A")
  } else if (prediction >= 1) {
    return("B")
  } else if (prediction >= 0) {
    return("C")
  } else if (prediction >= -1) {
    return("D")
  } else {
    return("F")
  }
}

# Applying the function to the predicted values
grades <- sapply(predictions, assign_grade)

# Displaying the result
result <- data.frame(Prediction = predictions, Grade = grades)
print(result)

##################################################################


#Applying grades for predictions #################################

summary(predict_reg)
#min = 0.1061
#max = 0.9221


# Function to assign grades based on predicted values
assign_grade <- function(predict_reg) {
  if (predict_reg >= 0.9999) {
    return("A")
  } else if (predict_reg >= 0.7500) {
    return("B")
  } else if (predict_reg >= 0.5000) {
    return("C")    
  } else if (predict_reg >= 0.2500) {
    return("D")
  } else if (predict_reg >= 0.0001) {
    return("E")
  } else {
    return("F")
  }
}
  

# Applying the function to the predicted values
grades <- sapply(predict_reg, assign_grade)

# Displaying the result
result <- data.frame(Prediction = predict_reg, Grade = grades)
print(result)




# drawing a histogram
unique(result$Grade)
ggplot(result, aes(x = Grade)) + geom_bar(fill = "skyblue") + labs(title = "Distribution of Grades", x = "Grade", y = "Count")








# find corelation
x <- leadsdata$converted
y <- leadsdata$asymmetrique.activity.score
cor(x,y)
plot(x,y)













# clear memory
rm(list = ls())






























































































































