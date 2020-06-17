# To test for differences in bee survival between pollen diet and Crithidia strain treatments,
# we fit a mixed effects Cox proportional hazards model using the "coxme" package in R.
#To assess significance of treatment effect on mortality, 
# we compared log-likelihood of nested models with and without 
# diet treatment as a predictor using a chi-squared test




library(survival)
library(coxme)
library(car)

data<-read.csv("/Volumes/GoogleDrive/My Drive/POLLINATOR RESEARCH/PHD Work/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 12_SRC Exp. /SRC_DEATHS.csv", header=TRUE)
View(data)
head(data)
str(data)

data$ColonyID <- as.factor(data$ColonyID)

#Mixed effects Cox proportional hazards model
coxme <- coxme(Surv(TimeBeforeDeath,Death) ~ Pollen*Strain  + WingSize +  (1|ColonyID), data=data)
summary(coxme) #no error, no treat effects
# Cox mixed-effects model fit by maximum likelihood
# Data: data
# events, n = 26, 108
# Iterations= 8 36 
# NULL Integrated    Fitted
# Log-likelihood -118.4567  -115.9279 -113.5886
# 
# Chisq   df       p   AIC    BIC
# Integrated loglik  5.06 5.00 0.40890 -4.94 -11.23
# Penalized loglik  9.74 5.55 0.11065 -1.37  -8.36
# 
# Model:  Surv(TimeBeforeDeath, Death) ~ Pollen * Strain + WingSize + (1 |      ColonyID) 
# Fixed coefficients
# coef exp(coef)  se(coef)     z    p
# PollenW          0.02922456 1.0296558 0.5016299  0.06 0.95
# StrainB         -0.55657465 0.5731690 0.5758004 -0.97 0.33
# WingSize        -0.19182126 0.8254544 1.0695353 -0.18 0.86
# PollenW:StrainB  0.01246473 1.0125427 0.8104580  0.02 0.99
# 
# Random effects
# Group    Variable  Std Dev   Variance 
# ColonyID Intercept 0.7114960 0.5062265

drop1(coxme, test = "Chisqr")

#Anova in "car" only takes certain kinds of model object
Anova(coxme, type = "III")
# Analysis of Deviance Table (Type II tests)
# 
# Response: Surv(TimeBeforeDeath, Death)
# Df  Chisq Pr(>Chisq)
# Pollen         1 0.0075     0.9309
# Strain         1 1.8406     0.1749
# WingSize       1 0.0322     0.8577
# Pollen:Strain  1 0.0002     0.9877

#you should be able to compare a model that does not have treatment
#with the full model
coxme.noStrain<-coxme(Surv(TimeBeforeDeath,Death) ~  Pollen + WingSize + (1|ColonyID), data=data)
Anova(coxme.noStrain)
coxme.noPollen<-coxme(Surv(TimeBeforeDeath,Death) ~  Strain + WingSize + (1|ColonyID), data=data)
Anova(coxme.noPollen)
#now anova command to compare full model vs model with no pollen treat
#anova(fullmodel, reducedmodel)
anova(coxme, coxme.noStrain, coxme.noPollen)
#"to assess significance of treatment effect on mortality, we compared log-likelihood of #nested models with and without diet treatment as a predictor using a chi-squared test"

# Analysis of Deviance Table
# Cox model: response is  Surv(TimeBeforeDeath, Death)
# Model 1: ~Pollen * Strain + WingSize + (1 | ColonyID)
# Model 2: ~WingSize + (1 | ColonyID)
# loglik  Chisq Df P(>|Chi|)
# 1 -115.93                    
# 2 -116.88 1.9042  3    0.5925


