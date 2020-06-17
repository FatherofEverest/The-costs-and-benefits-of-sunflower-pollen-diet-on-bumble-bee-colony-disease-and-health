# Crithidia infection prevalence analysis for Exp 5B: Whole-colony sunflower exp.
# 5/9/2020
# J.J. Giacomini

####################################################################################################################################
####################################################################################################################################
#Final model: 
Prev.M2 <- glmmTMB(Infection ~ Pollen * SampleWeek + WingSize + (1 | ColonyID),
                   family = binomial(link = "logit"),
                   data = data)
Anova(Prev.M2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
#                        Chisq Df Pr(>Chisq)  
#   Pollen             3.8054  1    0.05109 .
#   SampleWeek        18.8103  8    0.01591 *
#   WingSize           3.9689  1    0.04635 *
#   Pollen:SampleWeek 19.0486  8    0.01460 *


# Get estimated marginal means
emmeans_fit_PrevM2<-emmeans(Prev.M2, pairwise ~ Pollen | SampleWeek, type = "response")
# $emmeans
# SampleWeek = 2:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.854 0.0776 1393    0.633    0.952
# W      0.776 0.0911 1393    0.554    0.907
# 
# SampleWeek = 3:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.836 0.0680 1393    0.659    0.931
# W      0.870 0.0524 1393    0.730    0.943
# 
# SampleWeek = 4:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.767 0.0771 1393    0.586    0.885
# W      0.896 0.0413 1393    0.783    0.953
# 
# SampleWeek = 5:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.802 0.0682 1393    0.636    0.904
# W      0.934 0.0293 1393    0.848    0.973
# 
# SampleWeek = 6:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.872 0.0515 1393    0.734    0.944
# W      0.964 0.0188 1393    0.902    0.987
# 
# SampleWeek = 7:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.815 0.0665 1393    0.650    0.913
# W      0.950 0.0237 1393    0.877    0.980
# 
# SampleWeek = 8:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.910 0.0412 1393    0.790    0.964
# W      0.919 0.0337 1393    0.823    0.965
# 
# SampleWeek = 9:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.856 0.0579 1393    0.703    0.938
# W      0.975 0.0150 1393    0.921    0.992
# 
# SampleWeek = 10:
#   Pollen  prob     SE   df lower.CL upper.CL
# S      0.848 0.0591 1393    0.694    0.932
# W      0.989 0.0085 1393    0.951    0.998
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 
# 
# $contrasts
# SampleWeek = 2:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        1.6826 1.366 1393  0.641  0.5215 
# 
# SampleWeek = 3:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.7633 0.517 1393 -0.398  0.6903 
# 
# SampleWeek = 4:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.3837 0.236 1393 -1.556  0.1199 
# 
# SampleWeek = 5:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.2873 0.183 1393 -1.962  0.0499 
# 
# SampleWeek = 6:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.2575 0.182 1393 -1.919  0.0552 
# 
# SampleWeek = 7:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.2331 0.154 1393 -2.198  0.0281 
# 
# SampleWeek = 8:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.8926 0.600 1393 -0.169  0.8657 
# 
# SampleWeek = 9:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.1530 0.118 1393 -2.434  0.0150 
# 
# SampleWeek = 10:
#   contrast odds.ratio    SE   df t.ratio p.value
# S / W        0.0614 0.056 1393 -3.058  0.0023 
# 
# Tests are performed on the log odds ratio scale 



#Final plot of prevalence:
p.PrevM2


# Wing Size 
# B = -0.76594



####################################################################################################################################
####################################################################################################################################

rm(list=ls()) #clear memory
#setwd("/Users/rg9403/Google Drive/POLLINATOR RESEARCH/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen 
#Experiments/Exp 11_Lab Colony Sun pollen Supplements/Analyses/Final analyses")


# Packages that we'll need

library(tidyverse)
library(dplyr)
library(ggplot2)
library(bnpa)
library(glmmTMB)

#Upload the data for Crithidia infection
data<-read.csv("Exp 11_Parasite load data.csv", header = TRUE)
str(data)
summary(data)

#Let's remove the "c" from ColonyID. and set ColonyID as a factor
data$ColonyID <- gsub("c", "", as.character(data$ColonyID))
data$ColonyID<-as.factor(data$ColonyID)
str(data$ColonyID)


#First we created a new binomial variable called "Infection". 
data$Infection<-data$Crithidia
data$Infection[data$Infection>0] <- 1

str(data$Infection) #It worked

#Check for NA's
check.na(data) #There is a total of  0  NAs on this file[1] 0

# Visualize the raw data
interaction.plot(x.factor = data$SampleWeek,
                 trace.factor = data$Pollen,
                 response = data$Infection,
                 fun = mean,
                 type = "b",
                 legend = (text.width = 10),
                 trace.label = "Pollen",
                 ylab = "prevalence",
                 xlab = "Week",
                 col = c("#E69F00","black"),
                 pch = c(17, 16),
                 lty = c(2, 1),
                 lwd = 2,
                 leg.bty = "n")
#Let's see the error bars
Prev.summary.df <- data %>% 
  group_by(Pollen, SampleWeek) %>% 
  dplyr::summarise(mean = mean(Infection), sd = sd(Infection), n = n(), se = sd/sqrt(n))

ggplot(data=Prev.summary.df, aes(x=SampleWeek, y=mean, colour=Pollen, group = Pollen)) +
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
  theme_classic()
# Clear difference between Sunflower and Wildflower 



# Fit the models

names(data)
str(data)

#Set SampleWeek as a factor
data$SampleWeek <- as.factor(data$SampleWeek)

# Model 1: Full model 
Prev.M1<-glmmTMB(Infection ~ Pollen * SampleWeek + WingSize + (1|ColonyID) + (1|Round), 
                    family = binomial(link = "logit"),
                    data=data) 
summary(Prev.M1) 
library(car)
Anova(Prev.M1)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
#                        Chisq Df Pr(>Chisq)  
#   Pollen             3.8054  1    0.05109 .
#   SampleWeek        18.8103  8    0.01591 *
#   WingSize           3.9689  1    0.04635 *
#   Pollen:SampleWeek 19.0486  8    0.01460 *
library(emmeans)

emmeans_Prev.M1<-emmeans(Prev.M1, pairwise ~ Pollen | SampleWeek, type = "response")



#Model 2: Model 1 with random effect of Round (blocking factor) dropped
Prev.M2<-glmmTMB(Infection ~ Pollen * SampleWeek + WingSize + (1|ColonyID), 
                 family = binomial(link = "logit"),
                 data=data) 
summary(Prev.M2) 
Anova(Prev.M2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
#                       Chisq Df Pr(>Chisq)  
#   Pollen             3.8054  1    0.05109 .
#   SampleWeek        18.8103  8    0.01591 *
#   WingSize           3.9689  1    0.04635 *
#   Pollen:SampleWeek 19.0486  8    0.01460 *

# Model 3: Model 1 with random effect of ColonyID dropped
Prev.M3<-glmmTMB(Infection ~ Pollen * SampleWeek + WingSize + (1|Round), 
                 family = binomial(link = "logit"),
                 data=data) 
summary(Prev.M3) 
Anova(Prev.M3)

library(bbmle)
#Model comparsions
AICtab(Prev.M1,Prev.M2, Prev.M3) 
# dAIC df
# Prev.M2  0.0 20 # Best fit: dropped Round, but included ColonyID
# Prev.M1  2.0 21
# Prev.M3 78.9 20

# visualize effects

library(effects)
ae <- allEffects(Prev.M2, se = TRUE)
plot(ae)


########## 
library(emmeans)
library(multcomp)



########### Crithidia infection prevalence plot #############
library(ggplot2)
ylabel.prev<- expression(bold(italic(Crithidia)~prevalence, sep="")) 
xlabel <- expression(bold(Time (weeks)))
Colors <- c("#E69F00","#000000")
Lines<- c("solid", "dotted")


# Get estimated marginal means
emmeans_fit_PrevM2<-emmeans(Prev.M2, ~ Pollen*SampleWeek, type = "response")
emmeans_fit_PrevM2 <-data.frame(emmeans_fit_PrevM2)


p.PrevM2 <- ggplot(emmeans_fit_PrevM2, aes(x=SampleWeek, y=prob, group = Pollen)) +
  geom_line(aes(linetype = Pollen, color = Pollen), size = 1.5)+
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE),linetype = "solid", size = 1, width=0.3)+
  geom_point(aes(shape = Pollen, color = Pollen), size = 6)+
  scale_color_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"), values = Colors)+
  scale_shape_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = c(15,17))+
  scale_linetype_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = Lines) +  
  ylab(ylabel.prev) +
  xlab("Time (wks)") + ylim(c(0.6,1))+
  theme_classic() +
  theme(legend.position = "top")+
  theme(text = element_text(face = "bold", size = 20, colour = "black"))




ggsave("Crithidia Prevalence.pdf", height = 6, width =10)


################################################################################
# visualize effects of Bee Size
summary(Prev.M2)


library(effects)
ae.prev <- allEffects(Prev.M2, se = TRUE)
plot(ae.prev$WingSize)

BeeSize.prev.df <- as.data.frame(ae.prev$WingSize)

ggplot(BeeSize.prev.df, aes(x = WingSize, y = fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=fit - se, ymax=fit + se), alpha = 0.4) + 
  theme_classic() + 
  ylab(ylabel.prev) +
  xlab("Bee Size (mm)") + 
  geom_point(data = data, aes(x = WingSize, y = Infection), size = 1, alpha = 0.2, color = "blue") + 
  theme(text = element_text(face = "bold", size = 20, colour = "black"))

ggsave("Crithidia Prevalence x Bee Size.pdf", height = 6, width =6)







#There are many solutions to test for the equality (homogeneity) of variance across groups, including:
# Fligner-Killeen test: a non-parametric test which is very robust against departures from normality.
#test of the null that the variances in each of the groups (samples) are the same.

fligner.test(Infection ~ Pollen, data = data)

# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by Pollen
# Fligner-Killeen:med chi-squared = 33.568, df = 1, p-value =
#   6.883e-09


fligner.test(Infection ~ SampleWeek, data = data)

# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by SampleWeek
# Fligner-Killeen:med chi-squared = 21.531, df = 8, p-value =
#   0.005862


fligner.test(Infection ~ WingSize, data = data)
# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by WingSize
# Fligner-Killeen:med chi-squared = 690.29, df = 706, p-value =
#   0.6567

fligner.test(Infection ~ interaction(Pollen, SampleWeek), data = data)
# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by interaction(Pollen, SampleWeek)
# Fligner-Killeen:med chi-squared = 66.927, df = 17, p-value =
#   7.243e-08


# Make subset of data excluding each pollen treatment and re-run flinger test for SampleWeek

Sun.df <- data %>% 
  filter(Pollen == "S")
fligner.test(Infection ~ SampleWeek, data = Sun.df)

# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by SampleWeek
# Fligner-Killeen:med chi-squared = 6.5208, df = 8, p-value = 0.5891


Wild.df <- data %>% 
  filter(Pollen == "W")
fligner.test(Infection ~ SampleWeek, data = Wild.df)

# Fligner-Killeen test of homogeneity of variances
# 
# data:  Infection by SampleWeek
# Fligner-Killeen:med chi-squared = 34.769, df = 8, p-value =
#   2.946e-05


bartlett.test(Infection ~ SampleWeek, data=Wild.df)
# Bartlett test of homogeneity of variances
# 
# data:  Infection by SampleWeek
# Bartlett's K-squared = 155.37, df = 8, p-value < 2.2e-16


bartlett.test(Infection ~ SampleWeek, data=Sun.df)
# Bartlett test of homogeneity of variances
# 
# data:  Infection by SampleWeek
# Bartlett's K-squared = 8.1115, df = 8, p-value = 0.4227

bartlett.test(Infection ~ Pollen, data = data)
# Bartlett test of homogeneity of variances
# 
# data:  Infection by Pollen
# Bartlett's K-squared = 70.48, df = 1, p-value < 2.2e-16


bartlett.test(Infection ~ interaction(Pollen, SampleWeek), data = data)
# Bartlett test of homogeneity of variances
# 
# data:  Infection by interaction(Pollen, SampleWeek)
# Bartlett's K-squared = 240.96, df = 17, p-value < 2.2e-16








# Influence measures
#Influence measures quantify the effects of particular observations, or groups of observations,
#on the results of a statistical model; leverage and Cook’s distance are the two most common formats for influence measures.

source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

PrevM2_influence.time <- system.time(
  PrevM2_influence <- influence_mixed(Prev.M2, groups="ColonyID")
)

car::infIndexPlot(PrevM2_influence)

inf <- as.data.frame(PrevM2_influence[["fixed.effects[-ColonyID]"]])
inf <- transform(inf,
                 ColonyID=rownames(inf),
                 cooks=cooks.distance(PrevM2_influence))
inf$ord <- rank(inf$cooks)
library(reshape2)
inf_long <- melt(inf, id.vars=c("ord","ColonyID"))
gg_infl <- (ggplot(inf_long,aes(ord,value))
            + geom_point()
            + facet_wrap(~variable, scale="free_y")
            + scale_x_reverse(expand=expand_scale(mult=0.15))
            + scale_y_continuous(expand=expand_scale(mult=0.15))
            + geom_text(data=subset(inf_long,ord>19),
                        aes(label=ColonyID),vjust=-1.05))
print(gg_infl)

# Colony 32 - S is the most influenctial based on Cook's D (value = 0.34199460), over the threshold of 4/n = 4/20 = 0.2. 
ColonyID.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()

ColonyID.32.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 32) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.32.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line() + ylab(ylabel.prev) +
  xlab("Time (wks)") + ylim(c(0,1))+
  theme_classic() +
  theme(text = element_text(face = "bold", size = 20, colour = "black"))

ggsave("Crithidia prevalence Colony32.pdf", height = 6, width =10)
# That one really low data point at week 5 is from Colony 32 - S, which is probably driving the Cook's D pattern above


# Drop colony 32 and re-run the model


No32.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(!ColonyID == 32)


# Model 1: Full model w/o AR1 covariance structure
Prev.M1.No32<-glmmTMB(Infection ~ Pollen * SampleWeek + WingSize + (1|ColonyID) + (1|Round), 
                 family = binomial(link = "logit"),
                 data=No32.df) 
summary(Prev.M1.No32) 

Anova(Prev.M1.No32)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
# Chisq Df Pr(>Chisq)   
# Pollen             2.3733  1   0.123423   
# SampleWeek        16.3043  8   0.038227 * 
# WingSize           4.0868  1   0.043221 * 
# Pollen:SampleWeek 22.1517  8   0.004642 **


emmeans_Prev.M1.No32<-emmeans(Prev.M1.No32, pairwise ~ Pollen | SampleWeek, type = "response")
# $emmeans
# SampleWeek = 2:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.903 0.06384 1318    0.690    0.975
# W      0.776 0.09016 1318    0.556    0.905
# 
# SampleWeek = 3:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.839 0.06993 1318    0.654    0.935
# W      0.869 0.05201 1318    0.730    0.942
# 
# SampleWeek = 4:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.818 0.06855 1318    0.645    0.917
# W      0.895 0.04104 1318    0.783    0.952
# 
# SampleWeek = 5:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.877 0.05145 1318    0.737    0.948
# W      0.933 0.02915 1318    0.848    0.972
# 
# SampleWeek = 6:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.925 0.03772 1318    0.809    0.973
# W      0.963 0.01874 1318    0.903    0.987
# 
# SampleWeek = 7:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.832 0.06554 1318    0.664    0.926
# W      0.949 0.02364 1318    0.877    0.980
# 
# SampleWeek = 8:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.914 0.04270 1318    0.785    0.968
# W      0.918 0.03352 1318    0.824    0.964
# 
# SampleWeek = 9:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.829 0.06942 1318    0.650    0.927
# W      0.975 0.01502 1318    0.921    0.992
# 
# SampleWeek = 10:
#   Pollen  prob      SE   df lower.CL upper.CL
# S      0.805 0.07360 1318    0.622    0.912
# W      0.989 0.00852 1318    0.951    0.998
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 
# 
# $contrasts
# SampleWeek = 2:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        2.6973 2.4110 1318  1.110  0.2672 
# 
# SampleWeek = 3:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.7839 0.5394 1318 -0.354  0.7235 
# 
# SampleWeek = 4:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.5281 0.3331 1318 -1.012  0.3116 
# 
# SampleWeek = 5:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.5127 0.3408 1318 -1.005  0.3151 
# 
# SampleWeek = 6:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.4655 0.3518 1318 -1.012  0.3118 
# 
# SampleWeek = 7:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.2643 0.1792 1318 -1.963  0.0498 
# 
# SampleWeek = 8:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.9498 0.6630 1318 -0.074  0.9412 
# 
# SampleWeek = 9:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.1256 0.0978 1318 -2.664  0.0078 
# 
# SampleWeek = 10:
#   contrast odds.ratio     SE   df t.ratio p.value
# S / W        0.0456 0.0417 1318 -3.374  0.0008 
# 
# Tests are performed on the log odds ratio scale 

Anova(Prev.M1)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
# Chisq Df Pr(>Chisq)  
# Pollen             3.8054  1    0.05109 .
# SampleWeek        18.8103  8    0.01591 *
# WingSize           3.9689  1    0.04635 *
# Pollen:SampleWeek 19.0486  8    0.01460 *

emmeans_Prev.M1.No32 <- as.data.frame(emmeans_Prev.M1.No32$emmeans)

p.PrevM1.No32 <- ggplot(emmeans_Prev.M1.No32, aes(x=SampleWeek, y=prob, group = Pollen)) +
  geom_line(aes(linetype = Pollen, color = Pollen))+
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE),linetype = "solid", size=0.5, width=0.1)+
  geom_point(aes(shape = Pollen, color = Pollen), size = 3)+
  scale_color_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"), values = Colors)+
  scale_shape_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = c(15,17))+
  scale_linetype_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = Lines) +  
  ylab(ylabel.prev) +
  xlab("Time (wks)") + ylim(c(0.6,1))+
  theme_classic() +
  theme(legend.position = "top")+
  theme(text = element_text(face = "bold", size = 20, colour = "black"))




ggsave("Crithidia Prevalence No 32_overall figure.pdf", height = 6, width =10)





# Colony 21 - W seems to be driving the drop in prevalence at week 8 for W

ColonyID.21.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 21) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.21.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()

# Perhaps this correlates with the switch from producing workers to producing sexuals
# How many sexuals did 21 produce? 


production.df<- read.csv("/Volumes/GoogleDrive/My Drive/POLLINATOR RESEARCH/PHD Work/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 11_Lab Colony Sun pollen Supplements/Analyses/Final analyses/Colony Performance/Data_Final Colony Dissections.csv", header = TRUE)
production.df$TotalSex <- production.df$TotalQueens + production.df$TotalDrones
production.df$ColonyID <- as.factor(production.df$ColonyID)

production.df <- production.df %>% 
  subset(select = c(Pollen, Infection, ColonyID, DeltaWeight, 
                                     Eggs, Larvae, Pupae,
                                     DeadLarvae, TotalDrones,
                                     TotalWorkers, TotalQueens, 
                    TotalSex))

ColonyID.21.Sex.df <- production.df %>% 
  group_by(ColonyID) %>% 
  filter(ColonyID == 21) 

# Wow! Colony 21 didnt produce any new queens or drones

# Let's see how colonie sthat produce a lot of new queesn and drones did

#Colony 28 produced 123 Queens and 7 drones (most queens)
ColonyID.28.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 28) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.28.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()
# There was a drop at week 4, but huge spike at week 5 and from week 6 to 10 there was 100% infection rate


# Colony 30 produced the most sexuals over all (Drones = 203 and queens = 67; total = 270)
ColonyID.30.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 30) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.30.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()
 #Prevalence was super high all the way through

# Colony 47 had the most workers (n = 621), and no new sexuals
ColonyID.47.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 47) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.47.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()


# Colony 1 had the second most workers (n = 514), and no new sexuals
ColonyID.1.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 1) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.1.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()

#Colony 6 had the fewest workers (n = 58), but a lot of queens (n = 37)
ColonyID.6.df <- data %>% 
  group_by(ColonyID, SampleWeek) %>% 
  filter(ColonyID == 6) %>% 
  dplyr::summarise(mean = mean(Infection)) 

ggplot(ColonyID.6.df, aes(x = SampleWeek, y = mean, group = ColonyID)) +
  geom_point() + geom_line()

#Reached 100% infection rate by week 4, dropped to ~75% at week 10 



