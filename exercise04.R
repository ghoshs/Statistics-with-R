### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, November 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Shrestha Ghosh, Anilkumar Erappanakoppal Swamy, Gopinath Mylapura Anjaneyareddy
## Matriculation number: 2567717, 2571210, 2562765


## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 


## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.

# Ans:
# number of trails, n = 12
# binomial random variable -> number of success that results from the binomial experiment, x = 4
# Probability of correct answer, p = (1/5) = 0.2
plot(0:12, dbinom(0:12, size=12, prob=0.2))
abline(v=4)
# dbinom() gives density of the random variable
dbinom(x = 4, size = 12, prob = 0.2)
# 0.1328756

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
sum(dbinom(0:4, size=12, prob=0.2))
plot(0:12, pbinom(0:12, size=12, prob=0.2))
abline(v=4)
# Ans:
# pbinom() gives distribution function for the given set of random variables
pbinom(q = 4, size = 12, prob = 0.2, lower.tail = TRUE)#lower.tail=TRUE is default


##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
library(dplyr)
library("languageR")
summary("dutchSpeakersDistMeta")
head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
str(dutchSpeakersDistMeta)
glimpse(dutchSpeakersDistMeta)
sapply(dutchSpeakersDistMeta, is.factor)
# Factor variables: Speaker, Sex, AgeGroup, ConversationType, EduLevel



## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.

help(table)
attach(dutchSpeakersDistMeta)
age <- table(Sex, AgeGroup)

library(ggplot2)
ggplot(data = na.omit(dutchSpeakersDistMeta), aes(x = AgeGroup, y = as.numeric(Sex), fill = Sex)) +
  geom_bar(stat = 'identity')
# converted Sex to numeric to represent the counts in the y-axis.

## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?

age

# yes, there are significance differences: Minimum difference in all gpoups is 5%.
# But, below from chi-sq. test it is found be no significant difference.
# Does it mean that differences cannot be guessed accurately by looking at the data??


## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?

# H0 = There is no difference in age groups.
# H1 = There is significanct difference in age groups.
# Let us set the significance level to 5%
help(chisq.test)
chisq_test = chisq.test(age)
chisq_test
# X-squared = 3.2785, df = 4, p-value = 0.5124

# Critical value from the chi-sq. table for a df = 4 and p-value =  0.5124 is 3.35669.
# Since critical value(	3.35669) less than x-squared value(3.2785),
# thus, there is no significant relationship.

##discussion on rock paper scissors from lecture slide 14
test=data.frame(Rock=c(30, 25), Paper=c(21, 25), Scissors=c(24, 25))
rownames(test)=c('Observed', 'Expected')
chisq.test(test[1,])
chi2 = sum(((test[1,]-test[2,])^2)/test[2,])
chi2
pval = pchisq(q=chi2, df=2, lower.tail=FALSE)
pval
alpha=0.05
crit.val = qchisq(p=0.95, df=2)
crit.val
# crit.val = 5.99 > chi2 value (1.68) -> the point lies outside the rejection region
# thus we cannot reject the null hypothesis.

## lecture slide 29
test2=data.frame(yes=c(33, 33), no=c(251, 508))
rownames(test2)=c('nw','w')
test2
chisq.test(test2)

##by hands
expected_test=data.frame(yes=c((284*66/825), (541*66/825)), no=c((284*759/825),(541*759/825)))
##OR
expected_test=rowSums(test2) %*% t(colSums(test2))/sum(test2)
expected_test
chi2=sum(((test2-expected_test)^2)/expected_test)
chi2
pval = pchisq(q=chi2, df=1, lower.tail = FALSE)
pval
alpha=0.05
crit.val=qchisq(p=0.95, df=1)
## crit val (3.841) < chi2 (7.7) -> we reject the null hypothesis
chisq.test(test2, correct=FALSE)
chisq.test(test2)## with Yate's continuity correction


## e) What are the degrees of freedom for our data? How are they derived?

# DF = 4 => (nrow - 1) * (ncol - 1) => (2-1)(4-1) = 4

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

# H0: Therapeutic phenomenon is non-real phenomenon and 123 times are happened by chance.
# And, chance of performance is 50% on this task.
# H1: Therapeutic phenomenon is a real phenomenon and has effect performance.

# As per the performance chance of NULL hypotheses, percentage of chance is 50% and 
# raw number is = 0.5 * 280 = 140


## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 

# To perform a chi-sq. test, create observation list.
obs <- c(123, 280-123)
chisq.test(obs)
# Results: X-sq. value = 4.1286, df=1, p-value = 0.04216
# Let critical value be 5%.
# X-sq. value = 4.1286 is greater than critical value for 5% '3.84'. 
# Besides, p-value is less
# than 5%. Hence H0 is rejected.
# But alternate hypothesis correctness chance(44%) is less than 50% and hence therapeutic touch
# cannot be practiced with greater success rate.


## c) Now calculate significance using the binomial test as we used it in exercise 1.
pbinom(q = 123, size = 280, prob = 0.5) #  0.02420056. Probability is less than 2.5%


## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

# Binomial test is better because, it is probability is way lesses than 5% and it doesn't
# make any approximations unlike chi-sq. test.


##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?


# Situation: Referred from online:

# A group of patients has thier hemoglobin measure and documented as 
# either 'at goal' and 'not at goal' prior to the Diabetes Medicaiton. 
# The patients are followed up after a month to see whether there have
# been changes in their goal status.
# H0: There will not be change is goal status 
# H1: There will be change in goal status

# Here the data are paired, i.e is the obervations are dependent.
# In the above example when the goal status changes from 'at goal' to 'not at goal',
# the observations will have paired effect as they are dependent.

# If we take the less number of obervations for the above example then,
# approximating binomial function using the normal distribution becasue the obervations are less.
# i.e if Np or Nq < 5. then it is not suitable way to approximate using binomial function.
# In such case with paired observations and less number of observations, McNemars test is used.