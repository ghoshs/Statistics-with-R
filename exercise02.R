###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.

# download the data file "digsym.csv" from the moodle and save it in your working directory. 
# The read in the data into a variable called "data".
data <- read.csv("digsym.csv")

# Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# how many rows, how many columns does that data have?
nrow(data)
#[1] 3700
ncol(data)
#[1] 11

# take a look at the structure of the data frame using "glimpse"
glimpse(data)

# view the first 20 rows, view the last 20 rows
head(data, n=20)
tail(data, n=20)

# Is there any missing data in any of the columns?
sapply(data, function(y) sum(length(which(is.na(y)))))

# get rid of the row number column
data <- data[, -1]
##data1<-subset(data, select=-c(X))

# put the Sub_Age column second
head(select(data, ExperimentName, Sub_Age, Group:Sub_Age))
##data1<-subset(data1, select=c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9))

# replace the values of the "ExperimentName" column with something shorter, more legible
data$ExperimentName <- "DSK"

# keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to data and finally remove data2.
data2 <- subset(data, List=='Trial:2')
data <- data2
rm(data2)

# separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
data <- separate(data, Sub_Age, c("Subject", "Age"), sep=" _ ")

# make subject a factor
data$Subject <- factor(data$Subject)


# extract experimental condition from the "File" column:
# the stimulus that people saw - did it correspond to a wrong or right digit-symbol combination?
select(data, File)

# 1) using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
data$File <- str_pad(data$File, 8, side=c("right"), pad="0")

# create a new column ("condition" (levels:right, wrong)) by extracting "right"/"wrong" using substr
data$condition <- substr(data$File, 3, 7)

# get rid of obsolete File column
data$File <- NULL

# missing values, outliers:

# do we have any NAs in the data, and if so, how many and where are they?
## No, not anymore
sapply(data, function(y) sum(length(which(is.na(y)))))

# create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0
data$accuracy <- ifelse(data$StimulDS1.RESP == data$StimulDS1.CRESP, 1, 0)

# how many wrong answers do we have in total?
sum(data$accuracy == 0)

# whats the percentage of wrong responses?
sum(data$accuracy == 0)/nrow(data) *100

# create correct_RT column
data$correct_RT <- ifelse(test=data$accuracy, yes=data$StimulDS1.RT, no=NA)

# create boxplot of correct_RT - any outliers?
boxplot(data$correct_RT)
## too many outliers on higher end. Subject took too long to respond? 

# create histogram of correct_RT with bins set to 50
hist(data$correct_RT, bins=50)
# positively skewed. 

# describe the two plots - any tails? any suspiciously large values?
# Both plots have many single instances of a large response time.
# One very suspicious RT is of 13.8sec - too long - was it wrongly recorded?

# view summary of correct_RT
summary(data$correct_RT)

# we got one apparent outlier: 13850

# There is a single very far outlier. Remove it.
data <- data[-c(which(grepl(13852, data$correct_RT))),]
# which(grepl(13852, data$correct_RT)) now gives 0

# dealing with the tail of the distribution: 
# outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean
sd2<-2.5*sd(data$correct_RT, na.rm=TRUE)
m <- mean(data$correct_RT, na.rm=TRUE)
ul<- m+sd2
ll<- m-sd2

# create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff
data$correct_RT_2.5sd <- ifelse(test=(data$correct_RT>ul), yes=NA, no=data$correct_RT)

# take a look at the outlier observations
# any subjects who performed especially poorly?
boxplot(data$correct_RT_2.5sd)
# no there are no subjects who perform exceptionally poorly

# how many RT outliers in total?
uw <- boxplot(data$correct_RT_2.5sd)$stats[5]
length(which(data$correct_RT_2.5sd>uw))
# 83 outliers

# plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?
hist(data$correct_RT_2.5sd)
boxplot(data$correct_RT_2.5sd)

# Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".
library(reshape)
data1 <- cast(data, Subject~condition, mean, value="accuracy", na.rm=TRUE)

# sort in ascending order or plot the average accuracies per subject.
data2 <- gather(data1, key="condition", value="avrg_accuracy", -Subject)
data_right <- subset(data2, condition == "right")
data_right <- data_right[order(data_right$avrg_accuracy),]
data_wrong <- subset(data2, condition == "wrong")
data_wrong <- data_wrong[order(data_wrong$avrg_accuracy),]

# library(ggplot2)
# ggplot(data_right, aes(x=avrg_accuracy, y=factor(Subject)))+
#   geom_point()

# would you exclude any subjects, based on their avrg_accuracy performance?
# depends if I have any thresholds, like if I want subjects to have at least 90% accuracy,
# I shall remove all subjects who have an average accuracy < 90%.
# Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".