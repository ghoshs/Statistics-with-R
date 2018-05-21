### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, November 27. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)


# set your wd and load the data frame digsym_clean.csv
setwd("./")
digsym = read.csv("digsym_clean.csv")
str(digsym)
summary(digsym)
head(digsym)

# get rid of the column "X"
digsym<-subset(digsym, select=-c(X))
head(digsym)
# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)
##digsym$condition <- substr(digsym$File, 3, 7)
##digsym$accuracy <- ifelse(digsym$StimulDS1.RESP == digsym$StimulDS1.CRESP, 1, 0)

result <- summarySE(data=digsym, "accuracy", "condition")
result
## result contains the standard deviation, standard error of the mean, 
## and a (default 95%) confidence interval of accuracy measure for the 'right'
## and 'wrong' conditions

# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?

library(ggplot2)
ggplot(result, aes(condition, accuracy, fill=condition)) + 
  geom_bar(stat="identity", show.legend=F) +
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), width=.2)
## should not be accuracy-sd, bcz we are plotting sampling distribution mean.
## The difference does not seem huge.

# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?

## The independence assumption of the t test is violated
# In this data set we look into the Sub_Age variable, we see that 
# the same subject has been used multiple times. To correctly 
# use the t test we need to consider one response per subject which can be done by 
# using average accuracy of each subject.
boxplot(data=digsym, StimulDS1.RT[StimulDS1.RT<2000] ~ Subject[StimulDS1.RT<2000])

# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)

##create two variables Subject and Age frm Sub_Age variable
##digsym <- separate(digsym, Sub_Age, c("Subject", "Age"), sep=" _ ")
library(reshape)
digsym_cast <- cast(digsym, Subject+condition~., mean, value="accuracy", na.rm=TRUE)
head(digsym_cast)
colnames(digsym_cast)[3]<-'Avg_accuracy'

# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side

ggplot(digsym_cast, aes(Avg_accuracy, fill=condition))+
  geom_histogram(position="dodge", binwidth=0.01)

ggplot(digsym_cast, aes(Avg_accuracy)) +
  geom_histogram(bins=15)+facet_grid(. ~ condition)

# Display the same data in a density plot 
ggplot(digsym_cast, aes(Avg_accuracy, fill=condition))+
  geom_histogram(position="dodge", binwidth=0.01) +
  geom_density(alpha=0.2)

ggplot(digsym_cast, aes(Avg_accuracy)) +
  geom_density()+facet_grid(. ~ condition)
# Based on the histograms and the density plots - are these data normally 
# distibuted?

# The data is negatively skewed (left tail is longer) for both plots. 

# Create a boxplot of the accuracy data

ggplot(digsym_cast, aes(x=condition, y=Avg_accuracy, fill=condition)) +
  geom_boxplot(show.legend=F)

ggplot(digsym_cast, aes(condition, Avg_accuracy)) +
  geom_boxplot()

# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?

# The test is PAIRED
t.test(Avg_accuracy ~ condition, data = digsym_cast, paired = TRUE)
# t = 3.7691, df = 36, p-value = 0.000588
# The difference is significant. p-value is < alpha OR 0 does not 
# fall inside CI [0.01303888, 0.04341757]

# What does the output tell you? What conclusions do you draw?
# H0 : distribution of accuracy for right condition and wrong condition is same.
# H1 : the two distributions are different, i.e., they have a difference of 
# mean greater than 0
# Let significance level be 0.05.
# t = 3.7691, df = 36, p-value = 0.000588
# The p-value is less than significance level of 0.05, 
# thus we reject the null hypothesis.
##################################################
## We reject the null hypothesis but on paired t test
right_acc <- digsym_cast$Avg_accuracy[digsym_cast$condition == 'right']
wrong_acc <- digsym_cast$Avg_accuracy[digsym_cast$condition == 'wrong']
t.test(wrong_acc, right_acc, paired=TRUE)  ## 2 sample test
t.test(wrong_acc-right_acc) ## OR 1 sample test on diff of means



# Compute the effect size using CohensD 
cohensD(Avg_accuracy ~ condition, data = digsym_cast) 
#######################################
cohensD(wrong_acc, right_acc, method="paired") ### OR
cohensD(wrong_acc-right_acc)

# How big it is? How do you interpret this result?
# d estimate: 0.7765678
# We say that the difference of the means of the two distributions is 
# 0.7765678 times the pooled standard deviation.
######################################
## 0.6196291 is moderate

# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.
dataw<-spread(digsym_cast, condition, Avg_accuracy)
head(dataw)
# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.
t.test(dataw$right, dataw$wrong, paired=TRUE)
## t = 3.7691, df = 36, p-value = 0.000588
## mean of the differences : 0.02822823
## we need a paired test because we want to test if 
# the accuracy of a person is changes if the digit-symbol combination
# is right from wrong. The accuracy of each Subject has been recorded twice
# once for right dig-sym combination and one for wrong dig-sym combination.
# The difference is significant (considering significance level 0.05) 
# => subjects have higher accuracy when given wrong dig-sym combination.

# Compare the t-test results from the wide-format and the long-format data.
# long-format::  t = 3.3401, df = 71.876, p-value = 0.00133
# wide-format::  t = 3.7691, df = 36, p-value = 0.000588
# Both give statistically significant results. Long format tells that
# the samples belong to 2 different distributions (right and wrong)
# The wide format tells us that mean accuracy of right distribution is more
# than mean accuarcy of wrong distribution by 0.02822823.
#########################################
# Results obtained are SAME

# Compute CohensD on the wide format data.
cohensD(dataw$right, dataw$wrong, method="paired")
# d estimate: 0.6196291 (medium)

# Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)
digsym_gender <- cast(digsym, Gender~condition, mean, value="accuracy", na.rm=TRUE)
##############################################
digsym_gender <- cast(digsym, Subject+Gender~., mean, value="StimulDS1.RT", na.rm=TRUE)
colnames(digsym_gender) <- c('Subject', 'condition', 'AvgRT')

# Take a look at the resulting data frame using head()
head(digsym_gender)

# Compute the t-test to compare the **RT** means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?
t.test(digsym_gender$right, digsym_gender$wrong, var.equal = TRUE)
# t = 6.6692, df = 2, p-value = 0.02175
# Here paired test is not used because the two samples are from different subjects
# One distribution has samples drawn from females, and the other from males.
# If significance level = 0.05, then p-value < 0.05 and we conclude that
# the two distributions are different.
###############################################
sd(digsym_gender$AvgRT[digsym_gender$condition == 'male'])
sd(digsym_gender$AvgRT[digsym_gender$condition == 'female'])
## variance is large therefore var.equal = FALSE (Welch Test)
t.test(AvgRT ~ condition, digsym_gender, paired=F)
# t = -0.97411, df = 9.097, p-value = 0.3552
# not significant. Good idea to use Welch test bcz we are not sure
# if the sd is same for both samples. t.test by default uses Welch test.