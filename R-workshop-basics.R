# Code part 1 of "R workshop at USF" in 2015 

## Variable assignments
a <- 64
b <- sqrt(a)
word <- "house"

## Basic functions
round(19.5877)
?round
abs(-35.6)
log(100)
log10(100)

## Creating vectors functions: seq, c, rep, ":"
## elements in a vector have the same type
seq(2, 20, 3)
c(2, 3, 5, 6)
rep("A", 10)
c(rep("A", 5), rep("B", 2))
x <- 1:7
c(2, 3, 5, 6, "a")

## Creating random vectors
x <- rnorm(10, mean=0, sd=1)
x <- rnorm(100, mean=1, sd=2.5)

## Common numeric functions
sum(x)
mean(x)
sd(x)
var(x)
summary(x)
max(x)
min(x)
which.max(x)
which.min(x)
sort(x)
prod(x)
range(x)
median(x)
quantile(x, prob=seq(0,1,0.1))

## Vectorized computations
x <- 1:10
x + 2
y <- 2:11
length(x)
lenght(y)
z <- x*y
x^2
x <- rnorm(10) # rnorm have a default mean and variance
abs(x)
x > 0 
y <- x > 2 | x < -2
sum(y)

vec1 <- 1:100
vec2 <- 5 * rnorm(100) + 1
vec3 <- factor(c(rep("a",50), rep("b",50)))
vec4 <- rbinom(100, 2, 0.1)

## Creating data frames
## data frames are tables
d0 <- data.frame(vec1, vec2, vec3, vec4)

## Create a data frame with with names
d00 <- data.frame(index=vec1, normal=vec2, factor=vec3, binom=vec4)

## You can change column names using "names"
names(d00)

## Set your working directory.
## Where is R currently pointing to?
getwd()
setwd("/Users/yannet/teaching/USF-R-Workshop/workshop-files")

## Load data set into a data frame
?read.csv # help for this function

## Name your dataset something short
d <- read.csv("survey-data.csv")

# Examine the overall structure of the data set.
# How many users do we have in this survey?
str(d) 
summary(d) # to see overall summary stats of all variables
head(d)

# To access a column
# what is the difference? hint: use "str"
d$educ
d["educ"]
d[["educ"]]

table(d$educ)
summary(d$educ) 
## R is treating educ as a numeric variable. How do we know that?
## educ is a categorical variable
## use factor for categorical variables
d$educ <- factor(d$educ)

## table buils a contingency table that counts 
## each combination of factor levels
table(d$educ)
## Let's give is a name so that R can store it
## Note educ is different than d$educ
educ <- table(d$educ)
barplot(educ)

# the numbers don't say much
# Let's create a vector data with names for the values of the 
# categorical variable d$educ
educ_strings <- c("G1-8", "G9-11", "HS", "Vocational", "Some Col.",
                  "College", "Post-Grad", "Unknown" )
educ_strings
levels(d$educ) <- educ_strings 

# let's try again
educ <- table(d$educ)
barplot(educ)
barplot(educ, las=2) # get labels text perpendicular to axis 
barplot(educ, las=2, main="Education Levels")

png("educ.png")
barplot(educ, las=2)
dev.off()
## look to see the file saved in your working directory

## Data set of pleople with College degrees
dcol <- subset(d, d$educ == "College")
table(dcol$educ)

## College or Post-Grad
dcolplus <- subset(d, d$educ %in% c("College", "Post-Grad"))
table(dcolplus$educ)

## How to get rid of empty levels?
dcolplus$educ <- droplevels(dcolplus$educ)
table(dcolplus$educ)

## Subseting data frames
## Just interested in the fist 10 rows
dr10 <- d[1:10,]
## A subset of the columns
n <- c("age", "educ", "internetUse")
dc3 <- d[,n]
str(dc3)
head(dc3)

## Do you think it make sense to merge some of these vars
## for example G1-8 + G9-11 = G1-11
## Vocational either with some college or high-school?

## We can define a new variable
## Easier if we convert them first to characters 
d$educ <- as.character(d$educ)
d$neweduc <- ifelse(d$educ %in% c("G1-8","G9-11"), "G1-11", d$educ)
d$neweduc <- ifelse(d$neweduc %in% c("Vocational"), "HS", d$neweduc)

barplot(table(d$neweduc), las=2)

## We have to get these variables back in some order
n <- unique(d$newedu) ## getting a vector with the new variables
n
n_order <- n[c(5, 3, 1 , 2, 4, 6)]
d$neweduc <- factor(d$neweduc, n_order)

barplot(table(d$neweduc), las=2)

## Relationship between education and internet use
## Age and internet use
dd <- table(d$neweduc, d$internetUse)
percent <- 100 * dd[,1]/ (dd[,2] + dd[,1])
dd 

barplot(percent, las=2)
barplot(percent[1:5], las=2)
barplot(percent[1:5], ylab="Percent of user that use Internet",
        las=2)

## let's look at age
table(d$age)
summary(d$age)
quantile(d$age, prob=seq(0,1,0.1))
hist(d$age, breaks=50)
## 99 looks very suspicious but don't drop people from the data set

aa <- d[, c("internetUse", "age")]
table(aa)
aa <- subset(aa, age < 99)
## agregate by age group use of "cut"
breaks <- c(17, 34, 49, 64, 94)
aa$age_group <- cut(aa$age, breaks=breaks)
table(aa$age_group)

## aggregate and ave functions
?aggregate
aa$user <- 1
oo <- aggregate(aa["user"], aa[c("age_group", "internetUse")], sum)
oo$total_by_internet_use <- ave(oo$user, oo["internetUse"], FUN=sum)
oo$percent <- with(oo, 100* user/ total_by_internet_use)

a <- table(aa$age_group, aa$internetUse)
percent <- 100*a[,1]/ (a[,2] + a[,1])
barplot(percent, xlab="Age Group", ylab="Percent of internet users")

## Time for practice
## What is the relationship between income and internet use?


## Sex and payAmount
## Do males pay more for online content?
summary(d$payAmount) ## there are some missing values

## equivalent to d4 <- d[,c("sex", "payAmount")]
d4 <- subset(d, select = c("sex", "payAmount")) 
## Remove observations with missing values.
d4 <- na.omit(d4)
d4$sex <- factor(d4$sex)
levels(d4$sex) <- c("Male", "Female")

# Let's look at the distribution 
# Looks like there are some outliers. 
plot(table(d4$payAmount))
### what is the 9998?
### code for "I don't know"
d4 <- subset(d4, payAmount < 9997)
# Do males spend more money on online?
d4mean <- aggregate(d4["payAmount"], d4["sex"], mean)
d4mean
d4median <- aggregate(d4["payAmount"], d4["sex"], median)
d4median
d4q <- aggregate(d4["payAmount"], d4["sex"], quantile, probs = seq(0, 1, 0.1))
d4q

## boxplot to compare payAmount distribution versus sex
boxplot(d4$payAmount ~ d4$sex)
## How can we improve this plot?


## Suppose I want the mean and the median in the same dataframe
## Fist change names then merge
names(d4mean)
names(d4mean)[2] <- "payAmount.mean"
names(d4mean)

names(d4median)[2] <- "payAmount.median"
dmm <- merge(d4mean, d4median, by="sex")
dmm


## FUNCTIONS
## let's write a function that changes the names of columns
## in a dataframe
change_name <- function(d, index, newname) {
  names(d)[index] <- newname
  d # returns 
}

d4median2 <- change_name(d4median, 2, "median")
d4median2

## We can define default values
## The last expression evaluated in a function becomes the return value
change_name <- function(d, index=2, newname="median") {
  names(d)[index] <- newname
  d # returns 
}


### Time for practice 4
# Let's look at payAmountMonth by age
## Part 1
## First compute mean and median by age
## plot mean and median (you can use the function plot or barplot )

# Then make two groups 
# 18-49 and 50-94
# What do you find?
# Make a boxplot

##################
### Are certain kinds of online content purchased more frequently?
## Let's look at payMusic, PayVideo etc ...
## a trick let's find vars with the word "pay" in the name

index <- grep("pay", names(d)) ## an array with the indices  
names(d)[index] ## names
## I am not interested in "payAmount" and "payAmountMonth"
index <- index[1:9] # cuts the last two
pay <- d[,index] # to select just these columns
nrow(pay) ## how many users we have
pay <- na.omit(pay)
nrow(pay) ## check how many users again
## we are not done; what about 8's
summary(pay) ## Again these are categorical vars

## If you wanted to keep this data in a different file
write.csv(pay, "payment_online_content.csv", quote=FALSE,row.names=FALSE )
## go to the terminal to your director and do 
## cat payment_online_content.csv 

## loop
for (i in 1:ncol(pay)) {
  pay[,i] <- factor(pay[,i])
}
summary(pay) # Much better, not many 8's

# try
sapply(pay, function(x) { sum(x==1)})

# percent of people that pay
percent_pay <- sapply(pay, function(x) { 100 * sum(x==1) / (sum(x==1) + sum(x==2))})

# we can also write a function to improve readability of our code
PercentPay <- function(x) { 100 * sum(x==1) / (sum(x==1) + sum(x==2))}
percent_pay <- sapply(pay, PercentPay)  

## let's plot this
barplot(percent_pay, las=2) 
# we should order the bars in decreasing order
o <- order(percent_pay, decreasing = TRUE)
percent_pay_ordered <- percent_pay[o]
barplot(percent_pay_ordered, las=2) 

## Time for practice 5
## Compare the usage of facebook, email
# .... all variables with the word "Use"


##### Second Part Flu data 

flu.txt <- url("http://www.google.org/flutrends/about/data/flu/us/data.txt")
flu <- read.csv(flu.txt, skip = 11)

flu$Date <- as.Date(flu$Date)

plot(flu$Date, flu$United.States)
plot(flu$Date, flu$United.States, main="US Flu Trends", ylab="Measure of US flu-related queries", col="tomato", type="p", lwd=2, xlab="")

### comparing san francisco and sunnyvale

names(flu)
grep("Francisco", names(flu))
names(flu)[grep("Francisco", names(flu))]
names(flu)[grep("[Ff]rancisco", names(flu))]
### for fun
names(flu)[grep("[Ss]an", names(flu))]

plot(flu$Date, flu$San.Francisco..CA)

## Time for practice 6
### compute mean, median for a set of cities in CA 
# hint grep "CA"
