# homework: play around with the simulation code. The code simulates how a dataset may be generated. The advantage over using a real data set is that we know exactly how the data was generated, and can observe whether the model manages to correctly identify the original model structure and coefficients.

# IMPORTANT! run each model simulation code several times to see how stable the results are -- this is necessary because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.
### Stats with R Exercise sheet 10

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, January 01. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Shrestha Ghosh, Anilkumar Erappanakoppal Swamy, Gopinath Mylapura Anjaneyareddy
## Matriculation number: 2567717, 2571210, 2562765

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

library(lme4)
library(car)

n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)
# WRITE DOWN WHAT VALUES YOU WOULD HOPE FOR THE MODEL TO ESTIMATE
# IN THE IDEAL CASE
# intercept= 25
# predA= 1
# predB= 1.2
# predA:predB = -1

m1<- lm(resp~predA*predB, data=d)
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  
# Can the model recover the original model structure and estimate correct coefficients for the predictors?
# For the above scenario with 200 observations and error term with a variance of 30,
# the m1 lm model is unable to recover the original model. The summary shows that the intercept 
# estimate is way off (>2 times the original value). The coefficient estimate for the interaction
# term is calculated as not significant by the m1 model even though the estimate is close.
# Rsqare value is 0.6226

# What happens if you change the number of subjects?
# We can check if we can improve model m1 by increasing the number of observations 10 times.
n <- 2000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m2<- lm(resp~predA*predB, data=d)
summary(m2)
# The coefficient estimates of m2 are better and closer to the actual values. The interaction term
# is still not significant as predicted by the model m2. R2 value (0.56) is worse than that of m1.
# Let's check by taking 100 times more obs.
n <- 20000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m3<- lm(resp~predA*predB, data=d)
summary(m3)
# Now, all the coeff are significant and the coefficient estimates are close to the original model. 
# The R2 value (0.57) has no significant improvement. Can we address this issue by further 
# increasing the #obs?
n <- 2000000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m4<- lm(resp~predA*predB, data=d)
summary(m4)
# As we can see, further increasing the #obs. does not improve the model.

# What happens if you change the variance of the error term?
n <- 200
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m5<- lm(resp~predA*predB, data=d)
summary(m5)
# In this model, the estimates are are quite close except for the intercept. The intercept and
# the interaction term estimates are computed as being not significant. The Rsquare value
# is quite high (0.84) so it is better than previous models in explaining the variance.
# For the same error variance if #obs = 2000, maybe the model gets better at estimation the 
# original model.
n <- 2000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m6<- lm(resp~predA*predB, data=d)
summary(m6)
# model m6 is quite close to the original model and the Rsqare value (0.85) is slightly higher 
# than the prev models which may not be very significant. All the coefficeients are found 
# significant by this model. The estimate of the intercept term is still not accurate though.
# If we further increase the obs?
n <- 20000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m7<- lm(resp~predA*predB, data=d)
summary(m7)
# The R-square value is also similar (0.8425) and does not improve much, The coefficient estimates 
# get better.
# Lets keep #obs = 2000, and reduce error variance further to 10.
n <- 2000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 10)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m8<- lm(resp~predA*predB, data=d)
summary(m8)

n <- 2000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- 0 #rnorm (n, 0, 10)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m8.1<- lm(resp~predA*predB, data=d)
summary(m8.1)

n <- 20000
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 10)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m8.2<- lm(resp~predA*predB, data=d)
summary(m8.2)
# The Rsquare value (0.92) is higher than all prev models and the coeff estimates except 
# the intercept term are quite close to the original. We can conclude that further increasing 
# the #obs, will not greatly improve the model, the Rsquare improves very little. However, the
# lower the error variance, the higher is the Rsqaure value and better the coefficient estimates. 
# For an ideal situation with no error (model m8.1), the model is recovered exactly,

# What happens if you change the effect sizes?
# We will start building on model m6, #obs = 2000, error variance=15, R2 = 0.85
# let var of predA = 10 (increasing effect size by keeping var of predB distribution constt at 30)
#############################
# Changing effect size means changing the coeff. If coeff of A is increased to say 100, 
# then the effect size of A on the respone increases.

n <- 2000
predA <- rnorm(n, 100, 10)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m9<- lm(resp~predA*predB, data=d)
summary(m9)
# m9 does not improve upon m6, the intercet and interaction coefficient estimates are not good. 
# The Rsquare value is 0.80 which is lower than model m6.
# Let us also decrease predB variance to 15. Distributions are more concentrated around
# the means of both predictors. 
n <- 2000
predA <- rnorm(n, 100, 10)
predB <- rnorm (n, 60, 15)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m10<- lm(resp~predA*predB, data=d)
summary(m10)
# The model degrades further. The Rsquare decreases significantly (0.57) and the coeff estimates are 
# significant only for predA and predB. Clearly, reducing the variance of predictors does not
# give good model estimates
# Lets keep the variance of predA = predB = 40 and 
n <- 2000
predA <- rnorm(n, 100, 40)
predB <- rnorm (n, 60, 40)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m11<- lm(resp~predA*predB, data=d)
summary(m11)

n <- 20000
predA <- rnorm(n, 100, 40)
predB <- rnorm (n, 60, 40)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m11.1<- lm(resp~predA*predB, data=d)
summary(m11.1)

n <- 2000
predA <- rnorm(n, 100, 100)
predB <- rnorm (n, 60, 100)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 15)
resp <- 25 + predA + 1.2*predB - interact + error
d <- data.frame(predA, predB, resp)

m11.2<- lm(resp~predA*predB, data=d)
summary(m11.2)
# We recover good Rsquare(0.92) in momdel m11. Further increasing the variance improves the R2 term
# as seen in model m11.2 where the data is very flat (variance is 100!). Increasing the number of 
# observations does not give a greatly improved model (m11.1).
# Decreasing the error variance and increasing the predictor variance and number of observations,
# gives highly accurate coeff estimates and very high R2 values.

# Next, we want to observe the effect of scaling the predictors. 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)
# by hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))

sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
vif(m1)
#predA       predB predA:predB 
#4.57673    20.17212    24.71051 
vif(sm1)

# Are the predictors currently correlated? What does the vif value mean?
cor(d)
# the cor(scale(predA), scale(predB)) = 0.04. There is practically no correlation between the 
# predictors.
# The vif values for unnormalized model m1 is greater than 1 and very high (>10) for predB
# and interaction term. The R2 term is same in both models. the standard error in normalizedd model
# is very low, but, this is because the data set values in this model is scaled.
# Normalization reduced the vif values in model sm1

# - check whether normalization also has a large effect when there is no interaction present in the model

sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)

# Here the vif value of unnormalized data does not improve once the data is normalized. Moreover, 
# the vif values of unnormalized data are already very low (close to 1) and the vif values of sm2 
# equals vif values of m2

# Try out what happens if there was originally no interaction in the data.
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB + error

d <- data.frame(predA, predB, resp)
nd <- as.data.frame(scale(d))

sm3<- lm(resp~predA+predB, data=nd)
m3 <- lm(resp~predA+predB, data=d)
summary(sm3)
summary(m3)
vif(sm3)
vif(m3)
# The residual standard error of sm3 (0.58) is quite lower than that of m3 (30.79). It is also 
# lower than the sm2 model. The R2 value in the previous models was 0.46 for both sm2 and m2.
# The R2 is 0.65 in both models sm3 and m3 which means that these mdoels are better estimators.
# This is obvious because both data and the models have no interactions terms.
# The VIF value is comparable to the sm2 and m2 models (models with no interaction terms).

# next, we want to calculate interpretable estimates
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)
nd <- as.data.frame(scale(d))

sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
summary(m1)
names(sm1)
coef(sm1)
coef(m1)
denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
denormPredA
# expected: 1, obtained: 1.01385 
# Explain in your own words, why the denormalization for predictor A works this way.
# R~ coefA*A + coefB*B + ceofAB*A*B <-unnormalized
# (R - mean(R))/sd(R) ~ dcoefA*(A-mean(A))/sd(A) + dcoefB*(B-mean(B))/sd(B) + 
#                     dceofAB*(A-mean(A))*(B-mean(B))/(sd(A)sd(B)) <- normalized model
# Solving this for R A and B we get,
# R ~ dcoefA*(A-mean(A))*sd(R)/sd(A) + dcoefB*(B-mean(B))*sd(R)/sd(B) + 
#                     dceofAB*(A-mean(A))*(B-mean(B))*sd(R)/(sd(A)sd(B)) + mean(R)
# From here we can see that the coef of A is dcoefA*sd(R)/sd(A)

############################# Above is for model without interaction#######################
denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) -
  (coef(sm1)[4]*sd(d$resp)*mean(predB))/(sd(predA)*sd(predB))
denormPredA

denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
denormPredB
# expected: 1.2 obtained:0.9278087 
denormPredB <- coef(sm1)[3] *sd(d$resp)/ sd(d$predB) -
  (coef(sm1)[4]*sd(d$resp)*mean(predA))/(sd(predA)*sd(predB))
denormPredB

denormIntercept<- coef(sm1)[1] * sd(d$resp) + mean(d$resp) -
(denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
denormIntercept
# expected: 25, obtained: 23.15998 

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract
#expected: -0.002, obtained: -0.005577005 

denormIntercept<- denormIntercept + 
  (denormInteract*mean(predA)*mean(predB))
denormIntercept
################# After taking the interaction coeff of the normalized model

# next, we create correlated variables 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
m2<-lm(respcor ~ predA * predC + predB, data=d2)
summary(m2)

sd2 <-as.data.frame(scale(d2))
sm2<-lm(respcor ~ predA * predC + predB, data=sd2)
summary(sm2)

# What do you observe regarding the results from the models? Do the models obtain the same 
# or different results with / without normalization?
# In the model without normalization (on data d2), the model estimates are close, but the model 
# wrongly calculates that the intercept and predA are not significant (p-value >5%). The residual 
# error is 30 and R2 value is 0.91. The normalized model (on data sd2), the model correctly 
# calculates predC as being not significant, the residual error is very low (0.28), but the R2
# value is comparable (0.92). We can check the estimate accuracy of the normalized model below. 

# Denormalize the coefficients.
coef(sm2)
denorm.predA <- coef(sm2)[2] * sd(d2$respcor) / sd(d2$predA)
denorm.predA 
denorm.predC <- coef(sm2)[3] * sd(d2$respcor) / sd(d2$predC)
denorm.predC 
denorm.predB <- coef(sm2)[4] * sd(d2$respcor) / sd(d2$predB)
denorm.predB 
denorm.intercept<-coef(sm2)[1] * sd(d2$respcor)+mean(d2$respcor)-
  (denorm.predA*mean(d2$predA) + denorm.predC* mean(d2$predC) + denorm.predB* mean(d2$predB))
denorm.intercept
denormInteract <- coefficients(sm2)[5] / (sd(d2$predA)*sd(d2$predB)*sd(d2$predC)) * sd(d2$respcor)
denormInteract

# finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#explain the difference between models m0 m1 and m2
# m0 <- linear model on a data set where response has mixed effects
# m1 <- an lmer model on a data set where response has mixed effects
# m2 <- an lmer model on a data set where response has no mixed effects
################################
# can we use anova?
anova(m0)
anova(m1)
## anova(m0, m1) -> cannot compare
AIC(m0, m1)
# m1 is better 

# play around with the size of the by item and by subject effects (here: intercepts only)
##halve the # effects
subjno <- 10 #number of subjects; 
itemno<- 10 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
# The significant changes in m0 model from the prev m0 model which had 20 participants and 20 items
# The R2 term is slightly less (number of data points is less)

m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)
# The effect of subj on intercept decreases (low subj variance on intercept)
# The variance of residuals is also lower than prev m1 of 20 participants and items.

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)
# The mixed effects variance and sd is zero. This is because we have modeled the mixed effects
# but the response has no mixed effects of the response variables.

##increase the # effects 2 times
subjno <- 40 #number of subjects; 
itemno<- 40 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
# R2 is higher. It has improved because of higher #datapoints.

m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)
# The standard errors of coeff estimates have decreased and t value has decreased.

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)
# The standard errors are low. However the model estimates some variance in the mixed effects
# even though there is no mixed effects in the model

# generate the data such that subjects differ in terms of how much predictor A affects them.
lmerd$respr2 <- 25+newd$subjint + newd$itemint + newd$subjint*newd$predA + 1.2*newd$predB + error
########################## or simply
lmerd$respr2 <- 25 + newd$subjint*newd$predA + error

# then build a mixed effects model that includes a random slope for subjects.
m3<-lmer(respr2 ~ predA + predB + (1+predA|subj) + (1| item), data=lmerd)
summary(m3)
