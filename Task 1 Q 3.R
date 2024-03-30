library(dplyr)

datacsv <- read.csv("D:\\Placement_Data_Full_Class.csv")

# dimension 
dim(datacsv)

# 1st 5 rows 
head(datacsv, 5)

# last 5 rows
tail(datacsv, 5)

# column names
names(datacsv)

# summary statistics of the dataset
describe(datacsv)

# data types
str(datacsv)


# Data Cleaning & Formatting#######
# Handling missing values
colSums(is.na(datacsv))

# imputing missing values 
install.packages("imputeTS")

library(imputeTS)

datacsv$salary <- imputeTS::na.mean(datacsv$salary)


colSums(is.na(datacsv))


# Seeing missing values  
sum(colSums(is.na(datacsv)))

str(datacsv)

#Standardization#########################

standardized_datacsv <- data.frame(datacsv) 

# Create a new variable 
standardized_sl_no <- scale(standardized_datacsv$sl_no) 

# Print the first few values 
head(standardized_sl_no) 


standardized_ssc_p <- scale(standardized_datacsv$ssc_p) 
standardized_hsc_p <- scale(standardized_datacsv$hsc_p) 
standardized_degree_p <- scale(standardized_datacsv$degree_p) 
standardized_etest_p <- scale(standardized_datacsv$etest_p) 
standardized_mba_p <- scale(standardized_datacsv$mba_p) 
standardized_salary <- scale(standardized_datacsv$salary) 


head(standardized_salary)

# creating a new dataframe and adding standardized columns

updated_datacsv <- NULL

updated_datacsv <- cbind(datacsv, standardized_sl_no)
updated_datacsv <- cbind(updated_datacsv, standardized_ssc_p)
updated_datacsv <- cbind(updated_datacsv, standardized_hsc_p)
updated_datacsv <- cbind(updated_datacsv, standardized_degree_p)
updated_datacsv <- cbind(updated_datacsv, standardized_etest_p)
updated_datacsv <- cbind(updated_datacsv, standardized_mba_p)
updated_datacsv <- cbind(updated_datacsv, standardized_salary)


head(updated_datacsv) 








#Normalization########################### 

normalized_datacsv <- data.frame(updated_datacsv) 

normalized_sl_no <- (normalized_datacsv$sl_no - min(normalized_datacsv$sl_no)) / (max(normalized_datacsv$sl_no) - min(normalized_datacsv$sl_no)) 

# Print the first few values 
head(normalized_sl_no)



normalized_ssc_p <- (normalized_datacsv$ssc_p - min(normalized_datacsv$ssc_p)) / (max(normalized_datacsv$ssc_p) - min(normalized_datacsv$ssc_p)) 
normalized_hsc_p <- (normalized_datacsv$hsc_p - min(normalized_datacsv$hsc_p)) / (max(normalized_datacsv$hsc_p) - min(normalized_datacsv$hsc_p)) 
normalized_degree_p <- (normalized_datacsv$degree_p - min(normalized_datacsv$degree_p)) / (max(normalized_datacsv$degree_p) - min(normalized_datacsv$degree_p)) 
normalized_etest_p <- (normalized_datacsv$etest_p - min(normalized_datacsv$etest_p)) / (max(normalized_datacsv$etest_p) - min(normalized_datacsv$etest_p)) 
normalized_mba_p <- (normalized_datacsv$mba_p - min(normalized_datacsv$mba_p)) / (max(normalized_datacsv$mba_p) - min(normalized_datacsv$mba_p)) 
normalized_salary <- (normalized_datacsv$salary - min(normalized_datacsv$salary)) / (max(normalized_datacsv$salary) - min(normalized_datacsv$salary)) 

# view data
normalized_mba_p
normalized_salary

# adding normalized columns

updated_datacsv_normalized <- NULL

updated_datacsv_normalized <- cbind(updated_datacsv, normalized_sl_no)

head(updated_datacsv_normalized)

updated_datacsv_normalized <- cbind(updated_datacsv, normalized_ssc_p)
updated_datacsv_normalized <- cbind(updated_datacsv, normalized_hsc_p)
updated_datacsv_normalized <- cbind(updated_datacsv, normalized_degree_p)
updated_datacsv_normalized <- cbind(updated_datacsv, normalized_etest_p)
updated_datacsv_normalized <- cbind(updated_datacsv, normalized_mba_p)
updated_datacsv_normalized <- cbind(updated_datacsv, normalized_salary)


head(updated_datacsv_normalized) 



# data types
str(updated_datacsv_normalized)




# represent categorical variables as binary vectors / one hot encoding ######################################

# Create a new data frame with the one-hot encoded variables 
encoded_datacsv_ohe <- NULL


encoded_datacsv_ohe_gender <- as.data.frame(model.matrix(~ gender - 1, updated_datacsv_normalized)) 

# Add the new columns to the encoded data frame 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_gender, updated_datacsv_normalized) 

# Print the first few rows of the encoded data frame 
head(encoded_datacsv_ohe)


encoded_datacsv_ohe_ssc_b <- as.data.frame(model.matrix(~ ssc_b - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_ssc_b, encoded_datacsv_ohe) 


encoded_datacsv_ohe_hsc_b <- as.data.frame(model.matrix(~ hsc_b - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_hsc_b, encoded_datacsv_ohe) 


encoded_datacsv_ohe_hsc_s <- as.data.frame(model.matrix(~ hsc_s - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_hsc_s, encoded_datacsv_ohe) 


encoded_datacsv_ohe_degree_t <- as.data.frame(model.matrix(~ degree_t - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_degree_t, encoded_datacsv_ohe) 

encoded_datacsv_ohe_workex <- as.data.frame(model.matrix(~ workex - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_workex, encoded_datacsv_ohe) 


encoded_datacsv_ohe_specialisation <- as.data.frame(model.matrix(~ specialisation - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_specialisation, encoded_datacsv_ohe) 


encoded_datacsv_ohe_status <- as.data.frame(model.matrix(~ status - 1, updated_datacsv_normalized)) 
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_status, encoded_datacsv_ohe) 


head(encoded_datacsv_ohe)






#Ordinal encoding - encoding categorical variables with mapping #########################

head(encoded_datacsv_ohe)

# check unique values
unique(encoded_datacsv_ohe$specialisation)


# Create a copy of the original data frame
encoded_datacsv_ohe_oe <- NULL
encoded_datacsv_ohe_oe <- encoded_datacsv_ohe

# Define the mapping of categories to numerical values 
mapping <- c("Mkt&HR" = 0, "Mkt&Fin" = 1) 

# Apply the ordinal encoding 
encoded_datacsv_ohe_oe$specialisation_oe <- NULL
encoded_datacsv_ohe_oe$specialisation_oe <- mapping[as.character(encoded_datacsv_ohe_oe$specialisation)] 

# adding new column to the existing data frame.
encoded_datacsv_ohe <- cbind(encoded_datacsv_ohe_oe$specialisation_oe, encoded_datacsv_ohe_oe) 

# Print the first few rows of the encoded data frame 
head(encoded_datacsv_ohe)

#using similar method you can do it for other columns




# check the length of the data points 
length(encoded_datacsv_ohe$degree_p)
length(encoded_datacsv_ohe$mba_p)

# x = independant variable
# y = dependant variable
x <- encoded_datacsv_ohe$degree_p
y <- encoded_datacsv_ohe$mba_p

# correlation 
# the default correlation method (Pearson) is been used because the two parameters contain numerical data 
cor(x,y)

# above result = 0.4023638
# there is no good correlation between mba_p and degree_p / no strong correlation between the x and y








# manually defining the other method Spearman
# x and y variables data not eqaually distributed so Spearman's correlation can be used
# to check the correlation 
cor(x,y,method = "spearman")

# the above result = 0.3794934



# scatter plot
plot(x,y, main="degree_p vs mba_p", xlab="degree_p", ylab="mba_p", col="blue", cex=1)

# the visualization of the above scatter plot describes that there is no strong correlation between the two
# selected parameters 



# linear regression model 
# applying the linear regression function
# y = target/response variable 
# x = predictor
model_lr <- lm(y ~ x)
print(model_lr)

# result intercept 41.109 x 0.319
# if x increase by 1 unit then y will increase by 0.319 unit
# when x = 0 then y = 41.109



# summary of the model
summary(model_lr)


# linear regression model / data fram
encoded_datacsv_ohe.regression <- lm(y ~ x, data = encoded_datacsv_ohe)

# summary
summary(encoded_datacsv_ohe.regression)

# view attribute details
View(encoded_datacsv_ohe.regression)


# adding columns
View(encoded_datacsv_ohe)

# data frame
encoded_datacsv_ohe.data <- data.frame(y, x)

# viewing only the data frame / x and y columns
View(encoded_datacsv_ohe.data)

# adding the residuals column
encoded_datacsv_ohe.data2 <- encoded_datacsv_ohe.data
encoded_datacsv_ohe.data2$residuals <- encoded_datacsv_ohe.regression$residuals


# viewing 3 columns
head(encoded_datacsv_ohe.data2)

# adding predicted values column
encoded_datacsv_ohe.data2$fitted.values <- encoded_datacsv_ohe.regression$fitted.values


# viewing 4 columns
head(encoded_datacsv_ohe.data2)



# check confidance intervals
confint(encoded_datacsv_ohe.regression)



# add regression line
abline(encoded_datacsv_ohe.regression, col="red")




# re-making the model with hsc_p and ssc_p variables ##################################################

# check the length of the data points 
length(encoded_datacsv_ohe$hsc_p)
length(encoded_datacsv_ohe$ssc_p)

# x = independant variable
# y = dependant variable
x <- encoded_datacsv_ohe$hsc_p
y <- encoded_datacsv_ohe$ssc_p




# correlation 
# the default correlation method (Pearson) is been used because the two parameters contain numerical data 
cor(x,y)


# scatter plot
plot(x,y, main="hsc_p vs ssc_p", xlab="hsc_p", ylab="ssc_p", col="purple", cex=1)
# add regression line
abline(encoded_datacsv_ohe.regression, col="red")



# linear regression model 
# applying the linear regression function
# y = target/response variable 
# x = predictor
model_lr <- lm(y ~ x)
print(model_lr)



# summary of the model
summary(model_lr)



# linear regression model / data fram
encoded_datacsv_ohe.regression <- lm(y ~ x, data = encoded_datacsv_ohe)


# summary
summary(encoded_datacsv_ohe.regression)


# data frame
encoded_datacsv_ohe.data <- data.frame(y, x)


# adding the residuals column
encoded_datacsv_ohe.data2 <- encoded_datacsv_ohe.data
encoded_datacsv_ohe.data2$residuals <- encoded_datacsv_ohe.regression$residuals

# viewing 3 columns

head(encoded_datacsv_ohe.data2)

# adding predicted values column
encoded_datacsv_ohe.data2$fitted.values <- encoded_datacsv_ohe.regression$fitted.values


# viewing 4 columns
head(encoded_datacsv_ohe.data2)



# check confidance intervals
confint(encoded_datacsv_ohe.regression)













































# using ggplot2 library #########################################################
library(dplyr)
library(ggplot2)


# Y= b0+ b1X + e
# b0 is the intercept of the regression line; that is the predicted value when X = 0
# b1 is the slope of the regression line / coefficient
# e is the error term (also known as the residual errors), the part of Y that cannot be explained by the regression model


placement.df <- read.csv("D:\\Placement_Data_Full_Class.csv")

# select only relevant columns
colnames(placement.df)
placement.reg <- select(placement.df, degree_p, mba_p)
str(placement.reg)
placement.reg %>%  cor()



# Basic Visualisation
ggplot(placement.reg, aes(degree_p, mba_p)) + geom_point()

# outliers
ggplot(placement.reg, aes(x = cut(degree_p, breaks = 5), y = mba_p)) + geom_boxplot()


ggplot(placement.reg, aes(degree_p, mba_p)) + geom_point() + geom_smooth()



# lm() can be used to determine the beta coefficients of the linear model.

# model
# mba_p = b0 + b1*degree_p
model1 <- lm(mba_p~degree_p, data = placement.reg)
model1

# above result
# b0 (intercept) = 41.109 and b1 (slope / coefficient)  = 0.319




# Regression equation:-
# mba_p = 41.109 + 0.319*degree_p 
# degree_p = independent variable
# mba_p = dependent variable
# 41.109 = intercept
# 0.319 = coefficient



# Interpreting the above result:-
# when degree percent (degree_p) increase by 1%, the map percent (mba_p) will increase by 0.319% on an average.




# The regression line
ggplot(placement.reg, aes(degree_p, mba_p)) + geom_point() + geom_smooth(method = "lm", se = FALSE)









# clear memory
rm(list = ls())


