# To test for differences in bee survival between pollen diet treatments,
# we fit a mixed effects Cox proportional hazards model using the "coxme" package in R.
#To assess significance of treatment effect on mortality, 
# we compared log-likelihood of nested models with and without 
# diet treatment as a predictor using a chi-squared test




library(survival)
library(coxme)
library(car)

data<-read.csv("/Volumes/GoogleDrive/My Drive/POLLINATOR RESEARCH/PHD Work/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 4 _Proportions Exp/Round 4 analysis/Round 4 Deaths.csv", header=TRUE)
View(data)
head(data)
str(data)

#Mixed effects Cox proportional hazards model
coxme <- coxme(Surv(TimeBeforeDeath,Death) ~ Treatment  + WingSize +  (1|ColonyID), data=data)
summary(coxme) #no error, no treat effects
# Cox mixed-effects model fit by maximum likelihood
# Data: data
# events, n = 12, 101 (6 observations deleted due to missingness)
# Iterations= 5 31 
# NULL Integrated    Fitted
# Log-likelihood -54.70167  -48.17624 -45.19292
# 
# Chisq   df         p  AIC  BIC
# Integrated loglik 13.05 5.00 0.0229070 3.05 0.63
# Penalized loglik 19.02 5.71 0.0033403 7.60 4.83
# 
# Model:  Surv(TimeBeforeDeath, Death) ~ Treatment + WingSize + (1 | ColonyID) 
# Fixed coefficients
# coef exp(coef)  se(coef)    z     p
# Treatment100% 0.9470526  2.578100 1.2267645 0.77 0.440
# Treatment25%  2.0716796  7.938145 1.0864019 1.91 0.057
# Treatment50%  1.2549842  3.507783 1.1552716 1.09 0.280
# WingSize      1.0987862  3.000522 0.6896248 1.59 0.110
# 
# Random effects
# Group    Variable  Std Dev  Variance
# ColonyID Intercept 2.274066 5.171378


#Anova in "car" only takes certain kinds of model object
Anova(coxme)
# Analysis of Deviance Table (Type II tests)
# 
# Response: Surv(TimeBeforeDeath, Death)
# Df  Chisq Pr(>Chisq)
# Treatment  3 4.8899     0.1800
# WingSize   1 2.5386     0.1111

#you should be able to compare a model that does not have treatment
#with the full model
coxme.notreat<-coxme(Surv(TimeBeforeDeath,Death) ~  WingSize + (1|ColonyID), data=data)
#now anova command to compare full model vs model with no pollen treat
#anova(fullmodel, reducedmodel)
anova(coxme, coxme.notreat)
#"to assess significance of treatment effect on mortality, we compared log-likelihood of #nested models with and without diet treatment as a predictor using a chi-squared test"

# Analysis of Deviance Table
# Cox model: response is  Surv(TimeBeforeDeath, Death)
# Model 1: ~Treatment + WingSize + (1 | ColonyID)
# Model 2: ~WingSize + (1 | ColonyID)
# loglik Chisq Df P(>|Chi|)
# 1 -48.176                   
# 2 -51.018 5.684  3     0.128


