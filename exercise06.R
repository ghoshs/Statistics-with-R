### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 4. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.
data <- ratings

# Take a look at the data frame.
str(data)

# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
library(ggplot2)
ggplot(data=data, aes(x=Length, y=Frequency))+
  geom_point()
##  geom_point(position='jitter')

# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?
# From the plot it seems that the word frequency and length are negatively correlated.
# The strength seems moderate.

# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Length, data$Frequency, use='pairwise.complete.obs')

# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?
# The correlation (-0.4281462) suggests a medium effect in the negative direction.

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Length, data$Frequency, use='pairwise.complete.obs', method='kendall') #-0.316297

# What about significance? Use the more user-friendly cor.test!
cor.test(data$Length, data$Frequency, use='pairwise.complete.obs') # -0.4281462
cor.test(data$Length, data$Frequency, use='pairwise.complete.obs', method='kendall') #-0.316297 

# Take a look at the output and describe what's in there.
# What do you conclude?
# The cor.test tests the null hypothesis H0: there is no correlation between x and y.
# which means that the mean of the sampling distribution (samples of cor values) must be at 0
# with 1 SD.
# The z-value is -3.9186 => p-value < 0.05 (our significance level)
# Thus H0 is rejected. This means that the correlation between the variables is significantly 
# different from zero

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor(data$Length, data$Frequency, method='spearman')
# -0.4311981

###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
modelname <- lm(Frequency ~ Length, data = data)
summary(modelname)

# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.
ggplot(data=data, aes(x=Length, y=Frequency)) +
  geom_point()+
  geom_smooth(method='lm', se=F)

ggplot(data=data, aes(x=Length, y=Frequency)) +
  geom_point()+
  geom_abline(slope=coef(modelname)[2], intercept=coef(modelname)[1])

# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data=data, aes(x = Length, y = Frequency, label = Word)) +
  geom_label()+
  geom_smooth(method = 'lm', se = F)


###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
digsym <- read.csv('digsym_clean.csv')
str(digsym)

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
mod_digsym <- lm(correct_RT_2.5sd ~ Age, data = digsym)

# Let's cast the data to compute an RT mean for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.

#digsym_cast <- cast(digsym, Subject+condition~., mean, value="accuracy", na.rm=TRUE)
digsym_2 <- cast(digsym, Subject+Age~., mean, value='correct_RT_2.5sd', na.rm=T)
head(digsym_2)
colnames(digsym_2) <- c('Subject','Age','Avg_corr_RT')

# Fit the regression model.
mod_digsym_2 <- lm(Avg_corr_RT ~ Age, data = digsym_2)

# Let's go over the output - what's in there?
# How do you interpret the output?
summary(mod_digsym_2)
# Contains the residual values for each datapoint, the regression line intercept (637.930) 
# and slope (21.223) values.
# For every unit of Age, average correct RT increases 21 times.
# When Age data is not available average correct RT is close to 637.93.

# Again plot the data points and the regression line. 
ggplot(data=digsym_2, aes(x = Age, y = Avg_corr_RT)) +
  geom_point()+
  geom_smooth(method = 'lm', se = F)


# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?
ggplot(mod_digsym_2, aes(residuals(mod_digsym_2)))+
  geom_histogram()
# The distribution resembles a normal but slightly skewed
ggplot(mod_digsym_2, aes(sample=residuals(mod_digsym_2)))+
  geom_qq()
# A normal distribution would give a perfectly straight line


# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.
library(broom)
digsym_clean <- augment(mod_digsym_2, digsym_2)
ggplot(digsym_clean, aes(y = .cooksd, x = Subject))+
  geom_point(aes(size= .cooksd))
## Is this correct way to plot cooks distance?

# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.

# What is the problem with observation 37?
digsym_clean[37,]
#   Subject Age Avg_corr_RT  .fitted  .se.fit   .resid      .hat   .sigma
#37      40  45    1776.757 1592.973 155.7144 183.7838 0.5105612 216.4669
#    .cooksd .std.resid
#37 0.757924   1.205461
## The age of the Subject (45 yrs) is quite far away from the mean of 25.35 years.

# Run the plotting command again and have R display the subjects that belong to each point.
ggplot(digsym_clean, aes(x = .cooksd, y = .resid, label=Subject))+
  geom_point()+
  geom_text(aes(label=Subject))


# Make a subset of "cast" by excluding this subject and name it cast2.
digsym_new <- subset(digsym_2, Subject != 40)
str(digsym_new)

# Fit the model again, using cast2, and take a good look at the output.
mod_new <- lm(Avg_corr_RT ~ Age, data = digsym_new)

# What's different about the output?
# (Intercept)          Age  
#      862.05        11.98
# We see that for evey unit of Age, the result changes 11.98 times (not 21.223 as in prev model)
# The intercept is also different. 
# When Age data is not available average correct RT is close to 862 (vs. 637 previously).

# How does that change your interpretation of whether age is predictive of RTs?
# Age is not very predictive of RT because for every unit of age there is an increase of 
# only 11.2 units of 
# RT.

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?
ggplot(data=digsym_new, aes(x = Age, y = Avg_corr_RT)) +
  geom_point()+
  geom_smooth(method = 'lm', se = F)

# Display the two plots side by side to better see what's going on.
library(gridExtra)

p1 <- ggplot(data=digsym_2, aes(x = Age, y = Avg_corr_RT)) +
  geom_point()+
  geom_smooth(method = 'lm', se = F) 
#  geom_smooth(data=digsym_new, method = 'lm', se = F)

p2 <- ggplot(data=digsym_new, aes(x = Age, y = Avg_corr_RT)) +
  geom_point()+
  geom_smooth(method = 'lm', se = F)

grid.arrange(p1, p2, ncol=2)

# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.
digsym_new <- augment(mod_new, digsym_new)
RSS <- sum((digsym_new$Avg_corr_RT - digsym_new$.fitted)^2)
TSS <- sum((digsym_new$Avg_corr_RT - mean(digsym_new$Avg_corr_RT))^2)
R <- 1-RSS/TSS
# 0.03493231

# How do you interpret this number?
# The age predictor explains only 3% of the variance in the data.

