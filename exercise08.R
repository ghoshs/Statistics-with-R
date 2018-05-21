### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Shrestha Ghosh, Anilkumar Erappanakoppal Swamy, Gopinath Mylapura Anjaneyareddy
## Matriculation number: 2567717, 2571210, 2562765

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############################################################################
###############################################################################

########
### Please, use ggplot to make plots in all exercises below!
########

# a) Read in the data kidiq.txt and take a look at the data summary.
#    It contains information about the mum's iq and their child's iq.
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.

data <- read.table("kidiq.txt", header = T)
summary(data)
head(data)

# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
#install.packages("ggplot2")
library("ggplot2")
ggplot(data, aes(x = mom_iq, y = kid_score)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = F) +
  labs(x = "Mom's IQ", y = "Kid's IQ")
  

# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.
lm.fit1 = lm(data$kid_score ~ data$mom_hs, data = data)
summary(lm.fit1)


summary(lm.fit1)$residuals
# Residuals represents the difference of predicted or modeled output to the true output
# for every data points.

summary(lm.fit1)$coefficients
# Slope(77.54839 ) and intercept(11.77126) of the regression line.
# This also explains that for every unit increase in mom_hs score, kid_score increases by 11.77126 units.
# And, irrespective of mom_hs score, every kid will have minimum IQ score of 77.54839

coef(summary(lm.fit1))[, "Std. Error"]
# Standard error: 2.058612 for intercept and 2.322427 for slope. This represents
# the measure of variability in the in the estimate of slope and intercept. 
# Lower value is always better.

coef(summary(lm.fit1))[, "t value"]
# t-value: Score that measures whether or not the coefficient for respective variable 
# is meaningful for the model.

coef(summary(lm.fit1))[, "Pr(>|t|)"]
# p-value: Its a probability that the respective variable is not related to mode.
# It indicates how the variable is not related to model interms of probability.
# Thus lower value is always good for model.

#The significance codes for each p-value denote the different thresholds for significance level
#alpha.

sd(summary(lm.fit1)$residuals)
# Residual Standard error: 19.85 is the Standard deviation of the residuals.

summary(lm.fit1)$df
# Degrees of freedom: 432 is The Degrees of Freedom is the difference between the number of
# observations included in your training sample and the number of variables used in your model.
# In this case, 434 - 2 = 432. Intercepts counts as one variable.

summary(lm.fit1)$r.squared
# Multiple R - Squared: 0.05613 is the propotion of the variance in the outcome variable that can 
# be accounted for by the predictor. In this case, value 0.05613 means, preictor variable mom_hs
# explains the 5.613% of variance in the outcome.

summary(lm.fit1)$adj.r.squared
# Adjsuted R-squared:0.05394 is also R-squared value but it considers the dof into account for 
# calcualtion. This value will increase with the addition of predictors only when those added 
# predictors improves the performance of the model.

# F-statistic: 25.69 is a F-test value that represents the signigicance boost for a variable 
# added to the model. Corresponding p-value represents of NOT significant boost. Thus the value
# should be as small as possible.

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.
lm.fit2 = lm(data$kid_score ~ data$mom_hs + data$mom_iq, data = data)
summary(lm.fit2)

# (Q) ggplot for linear regression with multipe predictor??

# Theoretical meaning of the summary of model remains same as explained for above model.
# comparision is as explained below: Important factors are compared.

# slope coefficients are 5.95012 and 0.56391 respectively for mom_hs and mom_iq.
# intercept is 25.73154.
# This means for every unit change in mom_hs and mom_iq, kid_score increases by 5.95012 and 0.56391 
# units respectively. And, regardless of mom_hs and mom_iq, kid_score is minimum 25.73154.

# Residual Standard error decrease to 18.14 from 19.85. This is indicates improvement in model fit.
# dof is 431, because number of variables increased by one in this model.
# R-squared value increased to 21.41% from 5.613%. So does adjusted r-squared value; increased to
# 21.05% from 5.394%.

# F-statistic increased almost more than double(25.69 -> 58.72). This indiacates a significance
# boost with addition of variable 'mom_iq' to the model and p-value has reduced even more.


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.

#	 HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))



library(gridExtra)
pred = data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(lm.fit2))
head(pred)
pred_sub_1 <- subset(pred, pred$mom_hs==1)
pred_sub_2 <- subset(pred, pred$mom_hs==0)

lm.pred_sub_1 <- lm(pred_sub_1$kid_score_pred ~ pred_sub_1$mom_iq+pred_sub_1$mom_hs)
coef(lm.pred_sub_1)
lm.pred_sub_2 <- lm(pred_sub_2$kid_score_pred ~ pred_sub_2$mom_iq+pred_sub_2$mom_hs)
coef(lm.pred_sub_2)

ggplot(data, aes(x = mom_iq, y = kid_score, color = as.factor(mom_hs))) +
  geom_point(shape = 1) +
  #ggplot(pred_sub_1, aes(x = mom_iq, y = kid_score_pred)) +
  #geom_abline(slope = 0.563906, intercept = 31.681655) +
  #geom_abline(slope = 0.563906, intercept = 25.731538)
  geom_line(data=pred, aes(x=mom_iq, y=kid_score_pred, col=as.factor(mom_hs)))+
  labs(x = 'IQ of Mother', y = 'Kids score', title = 'Kids Score vs. Mother\'s IQ', col = "Mom HS degree")


#install.packages("broom")
library("broom")
lm.fit1.aug <- augment(lm.fit1)


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.
lm.fit3 <- lm(kid_score ~ mom_hs * mom_iq, data = data)
summary(lm.fit3)
library(effects)
plot(effect(term="mom_hs:mom_iq", mod=lm.fit3, xlevels=list(mom_hs=c(0,1))), multiline=T)
# This linear model will have four terms, one inercept and three other terms are
# data$mom_hs, data$mom_iq, data$mom_hs:data$mom_iq(basically interaction term). 
# These three terms will have their respective coefficients.

# compared to linear model without interaction terms but same predictors,
# Residual standard error has slightly reduced(from 18.14 -> 17.97).
# Also, r-squared(21.41% -> 23.01%) and adjusted r-squared(21.05% -> 22.47%) 
# values have increased.

# The predictors and the interaction term have significant effect on the outcome.
# Having a HS degree has a positive effect on the kid score. Same is true for higher IQ.
# The interaction term is significant and negative. This means that at higher IQ values, 
# mothers who have a degree, their kids score less than mothers with a high IQ but no HS degree

# F-statistic value decreased(58.72 -> 42.84) and dof becomes 431 -> 430 as 
# interaction term gets added additionaly. Overall, we can say that model fits the
# data points better becuase of less residual standard error and higher r-squared values.
# But reduction in F-statistic represents there is no significance boost.
 
# (Q) confirm this with tutor!! Please let me know if anythin important is missed in the interpretation.


# g) Next, let's plot the results of this model.
par(mfcol=c(2,3))
plot(lm.fit3, which=seq(1,6))

pred2 <- data.frame(mom_iq=data$mom_iq, mom_hs=data$mom_hs, kid_score_pred=fitted(lm.fit3))
ggplot(data = data, aes(x=mom_iq, y=kid_score, col=as.factor(mom_hs))) +
  geom_point() +
  #geom_line(data=pred2, aes(x=mom_iq, y=kid_score_pred, col=as.factor(mom_hs)))+
  geom_smooth(method='lm', aes(group=as.factor(data$mom_hs), color=as.factor(data$mom_hs)))+
  labs(x = 'IQ of Mother', y = 'Kids score', title = 'Kids Score vs. Mother\'s IQ', col = "Mom HS degree")


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.
x_new <- data.frame(mom_hs=1, mom_iq=100)
x_new

predict.lm(lm.fit3, x_new, interval="confidence", level=0.95)
#      fit     lwr      upr
# 88.24766 52.8719 123.6234
# predicted IQ = 88.24
# The argument interval = 'confidence' not 'prediction'
#      fit      lwr      upr
# 88.24766 86.31365 90.18167

# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?

ggplot(data, aes(x = mom_iq, y = kid_score)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm) +
  labs(x = "Mom's IQ", y = "Kid's IQ")

# The purpose of taking a random sample from a lot or population and computing a statistic, 
# such as the mean from the data, is to approximate the mean of the population. 
# How well the sample statistic estimates the underlying population value is always an issue. 
# A confidence interval addresses this issue 
# because it provides a range of values which are likely to contain the population parameter of 
# interest. In this case interval is 95%.

# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

par(mfcol=c(2,3))
plot(lm.fit3, which=seq(1,6))

library(car)
par(mfcol=c(1,1))
qqPlot(lm.fit3)
# Residuals vs Fitted plot shows that there are no non linear relationships in the model.
# Because points are equally spread around the horizonatal line without distinct patterns.
### Homogeneity of variance of residuals assumption holds from Residual vs Fitted plot. 
### Since the red line is almost linear we can say that the relationship between the 
### predictor and outcome is linear.
### outlier, leverage and influential. How to interpret the plots:
### 1. Residuals vs. leverage
### 2. Cook's distance vs. obs.no
### 3. Cook's distance vs. leverage
# The residual are normally distributed, but not perfectly.
# There are no outliers that can affect or influence the model.
# There are three leverage points 231, 286 and 111.

# 111 has very low kids score even though the mother has a high IQ. In both observation the
# mothers do not have a HS degree. From , previous plot we have seen that when IQ of mother is 
# greater than around 105, having a HS degree has a negative effect on the kid score
# (interaction term). The kids score in observation no. 111 is very low and that in 213
# is very high compared to other observations with mom_hs=0 around them. 

influencePlot(lm.fit3)
## 273 and 111 are outliers (high residuals)
## 73 is high leverage point
outlierTest(lm.fit3)
lm4 <- lm(kid_score ~ mom_hs*mom_iq, data=data[-c(273, 111),])
summary(lm4)
influencePlot(lm4)
### What is a vif plot