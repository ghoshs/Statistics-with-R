### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, October 30. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 

## Change the name of the file by adding your matriculation numbers

## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
# use help.search("get working directory") to determine the function 
getwd()

## b) Get help with this function.
help(getwd)
?(getwd)

## c) Change your working directory to another directory.
# Change directory to the parent directory
setwd("../")



###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
# Use the following command to install the package --> install.packages("languageR", "/path/to/packages/directory/")
install.packages("languageR", "./")
# Add path to list of places R looks for a package --> .libPaths("/path/to/packages/directory/")
# Load the package
library(languageR)


## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?
head(dutchSpeakersDistMeta)
#prints the first (six) observations of the dataframe.
tail(dutchSpeakersDistMeta)
#pritns the last (six) observations of the dataframe.
summary(dutchSpeakersDistMeta)
# gives a summary of the dataset with respect to each variable. 
# We get a count of the values that each variable takes in the dataset. 
# For  example the sex variable has two fcators Male and Female.
# Summary shows the count of observations of Male, Female and missing values (denoted by NA)


## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
nrow(dutchSpeakersDistMeta)
# return 165 - number of speakers


## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
# png("boxplot.png") #optional to save plot as an image file
boxplot(AgeYear~Sex, data=dutchSpeakersDistMeta,main="Sex distribution for AgeYear", xlab="Sex", ylab="Age")
# dev.off() # used when saving the plot as image file
## NOTE:: boxplot(x)$stats to get the quartiles and intr quartile ranges.

## e) Does it seem as if either of the two groups has more variability in age?
# Discounting the outliers in the male group, we see that the spread of female gropup is more. 
# Hence, we can conclude that females have more variability in age.

## f) Do you see any outliers in either of the two groups?
# The male group has 2 outliers

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?
# use the aggregate() to find the mean and standard deviation of dataset grouped by Sex. The NA are ignored
aggregate(AgeYear~Sex, dutchSpeakersDistMeta, mean)
# The results are
#     Sex  AgeYear
#1 female 1966.889
#2   male 1967.301

aggregate(AgeYear~Sex, dutchSpeakersDistMeta, sd)
# The results are
#     Sex  AgeYear
#1 female 15.87411
#2   male 14.66258


## h) What do the whiskers of a boxplot mean?
# The lower whisker extends below the Lower Quartile upto the last datapoint which
# is greater than the value - Lower Quartile minus 1.5 times the Inter-Quartile range.
# The upper whisker extends above the Upper Quartile upto the last datapoint which 
# is less than the value - Upper Quartile plus 1.5 times the Inter-Quartile range.
# Datapoints lying beyond the whiskers are considered as outliers and plotted individually.
##NOTE:: get the value for upper and lower whiskers for the boxplots


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
# The data is in ratio scale. This is because there is a natural zero - when a child does 
# not use 'and then'.
# Also, the numerical values are meaningful - we can add, divide, subtract and multiply.
# For eg., a child who uses 'and then' 25 times is 5 times more frequent than a chils who 
# uses the phrase only 5 times.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
# The main reason of using dataframes is because the variables can be of different types. 
# In this example, the participant id variable is of nominal scale and the frequency is 
# of ratio scale.
# Thus, using a dataframe instead of a matrix to store the dataset makes sense. 

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
pps <- c(1:25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps, obs)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
class(stories$obs)
class(stories$pps)
# Class of variable 'pps' is "integer"

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
stories$pps <- factor(stories$pps)
# Since the pps variable is a nominal scale, it stores categorical data. 
# Factors are used to represent categorical data

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks=8, main="Histogram of obs", xlab="obs", ylab="Frequency")


## i) Create a kernel density plot using density().
plot(density(stories$obs))

## j) What is the difference between a histogram and a kernel density plot?
# A Kernel density plot gives a continous and smooth distribution of the observations. 
# When a kernel density plot is used, we do not have to worry regarding the selection 
# of bins as in histogram plots. 
## NOTE Yes, but the kernel density plot has its own parameters, namely the choice of 
## the kernel. Even if the kernel is a normal probability density function then we still
## need to specify its standard deviation. The distribution depends on the #bins for
## a histogram.

## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)
hist(stories$obs, breaks=8, main="Histogram of obs", xlab="obs", ylab="Relative Frequency", prob=TRUE)
lines(density(stories$obs))


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(from=-5, to=5, by=0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y <- dnorm(x, mean=0, sd=1)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x, y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x, 2*y, "l")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v=0, lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b1mean <- mean(b1temp)
# mean is 36.86219
b1sd <- sd(b1temp)
# sd is 0.1934217
plot(b1temp, dnorm(b1temp, mean=b1mean, sd=b1sd))

## h) We observe two tempareatures (36.91 and 38.13). What's the likelihood that
##    these temperatures respectively come from the normal distribution from g)?
# By observing the pnorms we can estimate the likelihood of the temperatures. 
# Since pnorm returns the area under curve till that temperature value.
# 1 - pnorm(36.91, b1mean, b1sd) = 0.40. This means that 36.91 has a probability 
# of 0.4 of belonging to the said distribution 
# 1 - pnorm(38.13, b1mean, b1sd) = 2.78908e-11. This implies that 38.13 
# has a very low chance of belonging to the said distribution
# pnorm(36.91, m, s, lower.tail=FALSE)*2
## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histrogram based on this sample.
##    Repeat 5 times. What do you observe?
s1 <- rnorm(20, b1mean, b1sd)
hist(s1, breaks=8, main="Histogram of obs", xlab="obs", ylab="Frequency")
# On repeating the sampling and plotting of histogram it is observed that the frequency 
# distribution jumps a lot from one plot to the next. 
# Some samples resemble a Gaussian distribuiton, while some do not (some 
# distributions seem similar to bimodal or skewed distributions). 
# All distributions lie close around the mean temperature of 36.86.