
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))


library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(psych)

# It is common for factors to be read as quantitative variables when importing a dataset into R
# to avoid that, it is better to define data types at the time of reading data
cropdata <- read.csv("D://crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))


# DATA INSPECTION ################################
# describes function from psych library
# descriptive statistics
describe(cropdata)


# data types of each column
str(cropdata)

# no of rows and columns
dim(cropdata)

# summary of the dataset
summary(cropdata)


# check NA values in all columns
colSums(is.na(cropdata))

# to find total no of NA values
sum(colSums(is.na(cropdata)))




# check distributions 

# scatter plot 
ggplot(cropdata) + aes(x = density, y = yield , color = density) + geom_jitter() + theme(legend.position = "none")

ggplot(cropdata) + aes(x = block , y = yield , color = block ) + geom_jitter() + theme(legend.position = "none")


ggplot(cropdata) + aes(x = fertilizer , y = yield , color = fertilizer ) + geom_jitter() + theme(legend.position = "none")

# bell curve
# density variable
x <- cropdata$density
summary(x)

x = seq(-3, 3, 0.1)
plot(x = x, y = dnorm(x), type="l", bty="n")

# normal distribution probability curve
plot(x, pnorm(x), type="l")


# Normal Probability Plot
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + labs(title = "Normal Probability Plot")



# block variable
b <- cropdata$block
summary(b)

b = seq(0, 25, 0.1)
plot(x = b, y = dnorm(b), type="l", bty="n", main="Distribution of block variable")


# fertilizer variable
f <- cropdata$fertilizer
summary(f)

f = seq(0, 33, 0.1)
plot(x = f, y = dnorm(f), type="l", bty="n", main="Distribution of fertilizer variable", xlab="Fertilizer variable")



# yield variable
e <- cropdata$yield
summary(e)
e = seq(170, 180, 0.1)
plot(x = e, y = dnorm(e), type="l", bty="n", main="Distribution of yield dependant variable", xlab="yield variable")


# Handling outliers ##########################

# Create box plots for the numerical columns 
boxplot(cropdata[c("density", "block", "fertilizer")])




# Perform the ANOVA test

# Normality
res_aov <- aov(yield ~ fertilizer, data = cropdata)
# check normality visually
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)



# QQ-plot - Normality checking
library(car)

# id = FALSE to remove point identification
qqPlot(res_aov$residuals, id = FALSE)
















# fertilizer = independent variable
one.way <- aov(yield ~ fertilizer, data = cropdata)

summary(one.way)







# Two-way ANOVA
two.way <- aov(yield ~ fertilizer + density, data = cropdata)

summary(two.way)








# Adding interactions between variables
interaction <- aov(yield ~ fertilizer*density, data = cropdata)

summary(interaction)








# Adding a blocking variable

blocking <- aov(yield ~ fertilizer + density + block, data = cropdata)

summary(blocking)







# Find the best-fit model = the model that best explains the variation in the dependent variable.

library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)




# Check for homoscedasticity ###############################################

# Equality of variances - homogeneity
# Boxplot - fertilizer 
boxplot(yield ~ fertilizer, data = cropdata)

boxplot(yield ~ density, data = cropdata)

boxplot(yield ~ block, data = cropdata)


# Dotplot
library("lattice")

dotplot(yield ~ fertilizer, data = cropdata)




# Diagnostic plots.
# Q-Q plot plots.
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))




# post-hoc test

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way







# find which group means are statistically different from one another / another ANOVA test

tukey.plot.aov<-aov(yield ~ fertilizer:density, data=cropdata)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)





# Make a data frame with the group labels
# We can make three labels for our graph 
# A = 1:1
# B = all intermediate combinations
# C = 3:2

mean.yield.data <- cropdata %>% group_by(fertilizer, density) %>% summarise(yield = mean(yield))

mean.yield.data$group <- c("a","b","b","b","b","c")

mean.yield.data





#Plot the raw data

two.way.plot <- ggplot(cropdata, aes(x = density, y = yield, group=fertilizer)) + 
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot


# Add the means and standard errors to the graph

two.way.plot <- two.way.plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) + 
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') + geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

# hard to read so Split up the data
# showing which groups are different from one another / use facet_wrap function

two.way.plot <- two.way.plot + geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) + facet_wrap(~ fertilizer)

two.way.plot




two.way.plot <- two.way.plot + theme_classic2() + 
  labs(title = "Crop yield in response to fertilizer mix and planting density", x = "Planting density (1=low density, 2=high density)", y = "Yield (bushels per acre)")

two.way.plot



