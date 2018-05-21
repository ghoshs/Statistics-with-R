### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number.

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# For the further reference please use ?amis. It may take some time to understand the dataset. 

# Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
data <- amis
str(data)
summary(data)
pairs(data)
# period (3 levels), warning (2 levels) and pair (14 levels) are categorical variables
ggplot(data, aes(x=speed, fill=as.factor(period))) +
  geom_density(alpha=0.3) +
  facet_grid(.~warning)
# The distributions for different values of period for warning == 1 seems to vary slightly  
# though the significance cant be judged. For warning == 2, the 3 distributions are 
# almost completely overlapping.

# All our columns have numeric type. Convert the categorial columns to factors.
data$period <- factor(data$period)
data$warning <- factor(data$warning)
data$pair <- factor(data$pair)
str(data)

# Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
library(ggplot2)
levels(data$period) <- c("Before", "Immediately after", "After Some Time")
levels(data$warning) <- c("Sign", "No sign")
head(data)

ggplot(data=data, aes(x=period, y=speed))+
  geom_boxplot() +
  facet_grid(. ~ warning)

# What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?
# People drive slowly immidiately after warning sign is put up. Their speeds go up after some time
# to the speed same as before. This may be because, immidiately after the warning was erected people
# took time to read it and follow it. After some time they became habituated to the presence of the 
# sign board and therefore the driving speeds increased.

# What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?
# The data on warning==2 was collected maybe to account for factors such as traffic volume and 
# type which was same in both locations of a pair. Thus, any changes in speed due which happened 
# to factor other than the erection of sign boards could be recorded in warning==2 measurement.


#######################
### PART 2: 1-way ANOVA
#######################
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1.


# First, let's check the ANOVA assumptions 
# 1.) Independence assumption is violated, because we have repeated measurements from 
# the same location (column 'pair'), which introduces dependencies. So you need to 
# average the speed over each `pair` and `period` (using the function cast as in ex5). 
# But please note that this
# is not the best way to do it. In a few lectures we will learn about Linear Mixed
# Models, that are designed to handle the situations, where the assumption of 
# independence is violated due to multiple measurements per same subject (remember ex5) or 
# per item (this exercise).
data_ind <- cast(subset(data, warning == 'Sign'), pair + period ~ ., mean, value="speed", na.rm=TRUE)
head(data_ind)
colnames(data_ind) <- c("pair", "period", "Avg_speed")

# Please, note that we will be working only with this new data frame in this section.
# Again, build a boxplot of the average speed depending on period
ggplot(data=data_ind, aes(x=period, y=Avg_speed))+
  geom_boxplot()


# How do you think if there is a difference between periods?
# Yes, there seems to be a difference immdeiately after the sign is put up.
# Mean of Avg_speed for period 2 is lower than period 1. For period 3 the 
# mean Avg_speed increases to more than mean of previous period values. 
# The significance in the difference cannot be said for now.

# 2.) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)
# the normality of residuals can be checked by plotting the quantile-quantile (or QQ) plot.
# The plot plots the quantiles of a normal distribution vs the quantiles of the 
# residual distribution. If the residuals belong to a normal distribution the plot is a y=x 
# type of line (straight line). 

model <- aov(Avg_speed~period, data_ind)
ggplot(model, aes(sample=residuals(model)))+
  geom_qq()
# The qqplot is not exactly straight, but close to linear (there are small bumps).
# So we can say that data is normal.
######################
resid <- residuals(model)
qqnorm(resid)
qqline(resid, col='red')
shapiro.test(resid)

# 3.) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)
# pooled sd !paired
# ANOVA assumes homogeneity of variance i.e., there is only one population variance,
# instead of variance for each group. We can perform the levene's test
# to check variance of residuals.

library(car)
leveneTest(Avg_speed~period, data_ind)
# p-value = .8383 (>0.05) => assumption of homogeneity of variance is not violated.
########################

leveneTest(Avg_speed~period, data_ind, center=mean)

# Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,
# report p-value and interpret the result in details
anova <- aov(Avg_speed~period, data_ind)
summary(anova)
##           Df Sum Sq Mean Sq F value Pr(>F)
#period       2   25.0   12.52   0.986  0.382
#Residuals   39  494.8   12.69 

# 0.382 p-value ( > 0.05) -> null hypothesis, that the observations of
# periods come from the same distribution (and not 3 different distributions) cannot be rejected
#######################
#fval = msb/msw
#msb=ssb/dfb
#msw=ssw/dfw
#dfb=3-1
#dfw=length(data_ind$Avg_Speed)-3
# H0: means of 3 distributions is same.

# Please do a pairwise t-test with pairwise.t.test()
pairwise.t.test(data_ind$Avg_speed, data_ind$period)

# Report pair-wise p-values and interpret the result in details
# (Immediately after, Before) - 0.59
# (Immediately after, After some time) - 0.17
# (After some time, Before) - 0.40

# Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?
pairwise.t.test(data_ind$Avg_speed, data_ind$period, p.adjust.method = "none")
pairwise.t.test(data_ind$Avg_speed, data_ind$period, p.adjust.method = "bonferroni")
# (Immediately after, Before) - 1.0
# (Immediately after, After some time) - 0.51
# (After some time, Before) - 1.0
# The results change, There are 3 pairs of tests and to reject the null hypothesis,
# new p-value = (3*no_adjust_p-value) < 0.05. Since no p-value < 0.05, we do not reject the
# null hypothesis.

#######################
### PART 3: 2-way ANOVA
#######################
# Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period`.
data2 <- cast(data, warning + period + pair ~ ., mean, value="speed", na.rm=TRUE)
colnames(data2)[4] <- 'Avg_speed'

# Calculate the mean for each of 6 pairs of `period` and `warning`
d1<-subset(data2, period == 'Before' & warning == 'Sign', select=c(Avg_speed))
d2<-subset(data2, period == 'Before' & warning == 'No sign', select=c(Avg_speed))
d3<-subset(data2, period == 'Immediately after' & warning == 'Sign', select=c(Avg_speed))
d4<-subset(data2, period == 'Immediately after' & warning == 'No sign', select=c(Avg_speed))
d5<-subset(data2, period == 'After Some Time' & warning == 'Sign', select=c(Avg_speed))
d6<-subset(data2, period == 'After Some Time' & warning == 'No sign', select=c(Avg_speed))
aggregate(Avg_speed~period+warning, data2, mean)

# Do you think there is a significant difference in some of the groups?
data2
#The Avg_speed values are very close. The difference for warning == 1
# and (Before and After Some Tme) is 1.89 which may be significant.
# Otherwise, there seems to be no significant difference.

# Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details

anova2 <- aov(Avg_speed ~ warning * period, data2)
summary(anova2)
# warning == 2 is a control group. Putting up a sign has significant change bcz overall 
# avg speed in warning == 1 is different from warning == 2. But period effect is not 
#significant ie, putting up a sign has no significance.


library(HSAUR2)
str(weightgain)

ggplot(data=weightgain, aes(x=type, y=weightgain)) +
  geom_boxplot(aes(fill=source))

plot.design(weightgain)
# source has less effect. Type has more effect (protein -> high weight gain)

with(weightgain, interaction.plot(x.factor=type, trace.factor=source, response=weightgain))
# for high protein weightgain is high in beef(highest)
# For low protein weightgain is high in cereal

model2 <- aov(weightgain~source+type+source:type, data=weightgain)
summary(model2)
# The p-value is same as conclusions we made.

model3 <- lm(weightgain~source+type, data=weightgain)
summary(model3)

model4 <- lm(weightgain~source*type, data=weightgain)
summary(model4)
anova(model4)
summary(model2)

model5 <- lm(weightgain~type*source, data=weightgain)
anova(model5)
# model4 = model5

weightgain2 <- weightgain[-c(1:6, 34:40),]
model5 <- lm(weightgain~source*type, data=weightgain2)
model6 <- lm(weightgain~type*source, data=weightgain2)
anova(model5)
anova(model6)
## different results
summary(model5)
summary(model6)
## lm model is same
plot.design(weightgain2)
