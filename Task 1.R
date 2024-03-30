library(readxl)

data <- read_excel("D:\\Canada1.xlsx")

# dimension - number of rows and columns
dim(data)

head(data)

tail(data)

print(data)

# column names
names(data)

# no of rows
nrow(data)
print(nrow(data))

# no of columns
ncol(data)

# data types
str(data)

# min value
min(data$`2013`)

# max value
max(data$`2013`)

# display 3 rows and 2 columns
data[1:3, 1:2]

# histogram
hist(data$`2013`)

# 10 breaks 
hist(data$`2013`,breaks = 10)

# breaks = Scott Method. 
hist(data$`2013`,breaks = "Scott")

# breaks = Freedman-Diaconis Method.
hist(data$`2013`,breaks = "Freedman-Diaconis")

# breaks = specify the number of cells
# limits for x and y axis
# labels for x & y axis
# Name of the graph / header
hist(data$`2013`,breaks = 20, xlim = c(0, 290000), ylim = c(0, 200), 
     xlab = 'Number of Immigrants', ylab = 'Number of Countries', 
     main = 'Histogram of Immigration in 2013', col = "darkmagenta", border = "pink")


# The hist() function returns a list with 6 components.
h <- hist(data$`2013`,breaks = 20, xlim = c(0, 290000), ylim = c(0, 200), 
          xlab = 'Number of Immigrants', ylab = 'Number of Countries', 
          main = 'Histogram of Immigration in 2013', col = "darkmagenta", border = "pink")
print(h)


# place the counts on top of each cell
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))







# breaks = specify the number of cells
hist(data$`2013`,breaks = seq(min(data$`2013`),max(data$`2013`), length.out = 10),
     xlim = c(0, 290000), ylim = c(0, 200), 
     xlab = 'Number of Immigrants', ylab = 'Number of Countries', 
     main = 'Histogram of Immigration in 2013', col = "skyblue", border = "pink")




# breaks = specify the number of cells
# defining breakpoints between the cells as a vector
hist(data$`2013`,breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000,275000,300000),
     xlim = c(0, 300000), ylim = c(0, 200), 
     xlab = 'Number of Immigrants', ylab = 'Number of Countries', 
     main = 'Histogram of Immigration in 2013', col = "skyblue", border = "pink")
