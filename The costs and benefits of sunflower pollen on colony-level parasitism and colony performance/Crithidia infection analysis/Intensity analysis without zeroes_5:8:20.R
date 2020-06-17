# Crithidia infection intensity analysis (zeroes excluded) for Exp 11: Whole-colony sunflower exp.
# 5/8/2020
# J.J. Giacomini

####################################################################################################################################
####################################################################################################################################

#Final model: 
nbinom.TMB2 <- glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1 | ColonyID),
              family = "nbinom2",
              data = NoZero.df)
Anova(nbinom.TMB2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Crithidia
#                       Chisq Df Pr(>Chisq)    
#   Pollen             5.6645  1    0.01731 *  
#   SampleWeek        36.0437  8  1.724e-05 ***
#   WingSize           4.7919  1    0.02859 *  
#   Pollen:SampleWeek 11.1337  8    0.19424 

emmeans_nbinom.TMB2<-emmeans(nbinom.TMB2, pairwise ~ Pollen | SampleWeek, type = "response")
emmeans_nbinom.TMB2_Pollen<-emmeans(nbinom.TMB2, ~ Pollen, type = "response")
# Pollen response   SE   df lower.CL upper.CL
# S          29.9 3.75 1193     23.4     38.3
# W          42.9 4.80 1193     34.4     53.4
# 
# Results are averaged over the levels of: SampleWeek 
# Confidence level used: 0.95 
# Intervals are back-transformed from the log scale 
emmeans_nbinom.TMB2_SampleWeek<-emmeans(nbinom.TMB2, pairwise ~ SampleWeek, type = "response")
# SampleWeek response   SE   df lower.CL upper.CL
# 2              38.3 6.50 1193     27.4     53.4
# 3              44.9 5.90 1193     34.7     58.2
# 4              22.5 2.70 1193     17.8     28.5
# 5              37.3 4.28 1193     29.7     46.7
# 6              33.9 3.81 1193     27.2     42.3
# 7              36.2 4.13 1193     29.0     45.3
# 8              35.4 4.07 1193     28.3     44.4
# 9              35.0 4.13 1193     27.7     44.1
# 10             44.2 5.08 1193     35.3     55.4
# 
# Results are averaged over the levels of: Pollen 
# Confidence level used: 0.95 
# Intervals are back-transformed from the log scale 
# 
# $contrasts
# contrast ratio     SE   df t.ratio p.value
# 2 / 3    0.851 0.1570 1193 -0.874  0.9943 
# 2 / 4    1.699 0.3012 1193  2.989  0.0703 
# 2 / 5    1.027 0.1770 1193  0.154  1.0000 
# 2 / 6    1.128 0.1941 1193  0.699  0.9988 
# 2 / 7    1.056 0.1821 1193  0.313  1.0000 
# 2 / 8    1.080 0.1884 1193  0.440  1.0000 
# 2 / 9    1.094 0.1925 1193  0.513  0.9999 
# 2 / 10   0.865 0.1495 1193 -0.841  0.9956 
# 3 / 4    1.996 0.2780 1193  4.962  <.0001 ****
# 3 / 5    1.206 0.1643 1193  1.377  0.9063 
# 3 / 6    1.325 0.1766 1193  2.112  0.4653 
# 3 / 7    1.240 0.1679 1193  1.589  0.8108 
# 3 / 8    1.269 0.1714 1193  1.761  0.7082 
# 3 / 9    1.286 0.1782 1193  1.814  0.6729 
# 3 / 10   1.016 0.1390 1193  0.114  1.0000 
# 4 / 5    0.604 0.0753 1193 -4.040  0.0019  ***
# 4 / 6    0.664 0.0805 1193 -3.377  0.0215  *
# 4 / 7    0.621 0.0767 1193 -3.858  0.0038  **
# 4 / 8    0.636 0.0783 1193 -3.677  0.0076  **
# 4 / 9    0.644 0.0826 1193 -3.429  0.0181  *
# 4 / 10   0.509 0.0638 1193 -5.390  <.0001  ****
# 5 / 6    1.098 0.1289 1193  0.800  0.9969 
# 5 / 7    1.028 0.1220 1193  0.232  1.0000 
# 5 / 8    1.052 0.1268 1193  0.417  1.0000 
# 5 / 9    1.066 0.1323 1193  0.513  0.9999 
# 5 / 10   0.842 0.1015 1193 -1.426  0.8877 
# 6 / 7    0.936 0.1091 1193 -0.568  0.9997 
# 6 / 8    0.957 0.1124 1193 -0.371  1.0000 
# 6 / 9    0.970 0.1171 1193 -0.249  1.0000 
# 6 / 10   0.767 0.0902 1193 -2.258  0.3688 
# 7 / 8    1.023 0.1218 1193  0.191  1.0000 
# 7 / 9    1.037 0.1270 1193  0.295  1.0000 
# 7 / 10   0.819 0.0972 1193 -1.682  0.7575 
# 8 / 9    1.014 0.1247 1193  0.109  1.0000 
# 8 / 10   0.801 0.0966 1193 -1.842  0.6540 
# 9 / 10   0.790 0.0966 1193 -1.928  0.5944 
# 
# Results are averaged over the levels of: Pollen 
# P value adjustment: tukey method for comparing a family of 9 estimates 
# Tests are performed on the log scale 

#Final plot of intensity:
pM2


# Wing Size 
# B =  0.28519



####################################################################################################################################
####################################################################################################################################


rm(list=ls()) #clear memory
#setwd("/Users/rg9403/Google Drive/POLLINATOR RESEARCH/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen 
#Experiments/Exp 11_Lab Colony Sun pollen Supplements/Analyses/Final analyses")


# Packages that we'll need
library(mgcv)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(bnpa)

#Upload the data for Crithidia infection
data<-read.csv("Exp 11_Parasite load data.csv", header = TRUE)
str(data)
summary(data)



#Let's remove the "c" from ColonyID. and set ColonyID as a factor
data$ColonyID <- gsub("c", "", as.character(data$ColonyID))
data$ColonyID<-as.factor(data$ColonyID)
str(data$ColonyID)


# Remove zeroes from the data; We're only analyzing intensity for those bees with actual infection
#Then we need to run the analysis for prevalence. First we created a new binomial variable called "Infection". 
data$Infection<-data$Crithidia
data$Infection[data$Infection>0] <- 1

#Make new data frame with zeroes excluded
NoZero.df <- data %>% 
  filter(Infection == 1)
str(NoZero.df)
View(NoZero.df)

#Check for NA's
check.na(NoZero.df) #There is a total of  0  NAs on this file[1] 0

# Visualize the raw data
plot(NoZero.df$SampleWeek, NoZero.df$Crithidia)



interaction.plot(x.factor = NoZero.df$SampleWeek,
                 trace.factor = NoZero.df$Pollen,
                 response = NoZero.df$Crithidia,
                 fun = mean,
                 type = "b",
                 legend = (text.width = 10),
                 trace.label = "Pollen",
                 ylab = "Mean parasite load",
                 xlab = "Week",
                 col = c("#E69F00","black"),
                 pch = c(17, 16),
                 lty = c(2, 1),
                 lwd = 2,
                 leg.bty = "n")

#Let's see the error bars
NoZero.summary.df <- NoZero.df %>% 
  group_by(Pollen, SampleWeek) %>% 
  summarise(mean = mean(Crithidia), sd = sd(Crithidia), n = n(), se = sd/sqrt(n))

ggplot(data=NoZero.summary.df, aes(x=SampleWeek, y=mean, colour=Pollen)) +
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
  theme_classic()

# Clear difference between Sunflower and Wildflower on intensity fo infection. 


# How is the data distributed? Normal, Poisson, Neg.Bin ???
library(car)
library(MASS)

hist(NoZero.df$Crithidia) # Right tailed skew...

NoZero.W.df <- NoZero.df %>% 
  filter(Pollen == "W")
hist(NoZero.W.df$Crithidia)
  
NoZero.S.df <- NoZero.df %>% 
  filter(Pollen == "S")
hist(NoZero.S.df$Crithidia)

qqp(NoZero.df$Crithidia, "norm")
### Definitely not normal, as expected
qqp(NoZero.df$Crithidia, "lnorm")
### emmmmmm....

poisson <- fitdistr(NoZero.df$Crithidia, "Poisson")
qqp(NoZero.df$Crithidia, "pois", lambda = poisson$estimate)
# Not a good fit

nbinom <- fitdistr(NoZero.df$Crithidia, "Negative Binomial")
qqp(NoZero.df$Crithidia, "nbinom", size = nbinom$estimate[[1]],mu = nbinom$estimate[[2]])
#Looks great, Negative binomial it is!



# Fit the models
library(glmmTMB)
library(car)
names(NoZero.df)
str(NoZero.df)

#Set SampleWeek as a factor
NoZero.df$SampleWeek <- as.factor(NoZero.df$SampleWeek)

# Model 1: Full model w/o AR1 covariance structure
nbinom.TMB<-glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1|ColonyID) + (1|Round), 
                    family = "nbinom2",
                    data=NoZero.df) 
summary(nbinom.TMB) 
Anova(nbinom.TMB)
#   Pollen             5.6645  1    0.01731 *  
#   SampleWeek        36.0431  8  1.724e-05 ***
#   WingSize           4.7918  1    0.02860 *  
#   Pollen:SampleWeek 11.1342  8    0.19421 
drop1(nbinom.TMB, test = "Chisq")


#Model 2: Model 1 with random effect of Round (blocking factor) dropped
nbinom.TMB2<-glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1|ColonyID), 
                    family = "nbinom2",
                    data=NoZero.df) 
summary(nbinom.TMB2) 
Anova(nbinom.TMB2)

# Model 3: Model 1 with random effect of ColonyID dropped
nbinom.TMB3<-glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1|Round), 
                     family = "nbinom2",
                     data=NoZero.df) 
summary(nbinom.TMB3) 
Anova(nbinom.TMB3)

#Model comparsions
library(bbmle)
AICtab(nbinom.TMB,nbinom.TMB2, nbinom.TMB3) # ColonyID as a random effect provides better fit dAIC ~90



#Model 4: Model 2 with AR1 covariance structure 
nbinom.TMB4<- glmmTMB(Crithidia~  Pollen*SampleWeek + WingSize + (1|ColonyID) + ar1(0 + SampleWeek |ColonyID),
             family = "nbinom2",
             data = NoZero.df)
summary(nbinom.TMB4)
Anova(nbinom.TMB4)





# Check for lag effect
acf(residuals(nbinom.TMB2)) # Lag effect is sig at Lag = 1
acf(residuals(nbinom.TMB4)) # Lag affect mitigated at Lag = 1 with addition of AR1 covariance structure; 
#but created a sig lag effect at 2 and 5;
# Since story stays the same, let's not use the AR1 covariance structure


# visualize effects

library(effects)
ae <- allEffects(nbinom.TMB2, se = TRUE)
plot(ae)

########## 
library(emmeans)
library(multcomp)

# Get estimated marginal means
emmeans_nbinom.TMB2<-emmeans(nbinom.TMB2, pairwise ~ Pollen | SampleWeek, type = "response")
emmeans_nbinom.TMB2_Pollen<-emmeans(nbinom.TMB2, ~ Pollen, type = "response")
emmeans_nbinom.TMB2_SampleWeek<-emmeans(nbinom.TMB2, ~ SampleWeek, type = "response")



########### Crithidia infection intensity plot #############
library(ggplot2)
ylabel<- expression(bold(italic(Crithidia)~count~
                           "(cells * 0.02"~mu~L^-1*")", sep=""))  #check for encoding
xlabel <- expression(bold(Time (weeks)))
Colors <- c("#E69F00","#000000")
Lines<- c("solid", "dotted")


# Get estimated marginal means
emmeans_fit<-emmeans(nbinom.TMB2, ~ Pollen*SampleWeek, type = "response")

# rename for ease
eznb1 <-emmeans_fit


# Add significance letters
library(multcomp)
Lets_eznb1<-cld(eznb1, Letters = letters)
Lets_eznb1
Lets_eznb1<- dplyr::select(Lets_eznb1, Pollen:SampleWeek, .group)
Lets_eznb1

eznb1 <-data.frame(emmeans_fit)

# merge emmeans and letters
library(plyr)
emmeans.df.mergedM3 <- join(eznb1, Lets_eznb1)  #### need to fix this for plot
emmeans.df.mergedM3
emmeans.df.mergedM3$.group<-gsub(pattern = " " , replacement="", x= emmeans.df.mergedM3$.group)
emmeans.df.mergedM3


pM2 <- ggplot(emmeans.df.mergedM3, aes(x=SampleWeek, y=response, group = Pollen)) +
  geom_line(aes(linetype = Pollen, color = Pollen), size = 1.5)+
  geom_errorbar(aes(ymin=response - SE, ymax=response + SE),linetype = "solid", size=1, width=0.3)+
  geom_point(aes(shape = Pollen, color = Pollen), size = 6)+
  scale_color_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"), values = Colors)+
  scale_shape_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = c(15,17))+
  scale_linetype_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = Lines) +  
  ylab(ylabel) +
  xlab("Time (wks)") + ylim(c(0,70))+
  theme_classic() +
  theme(legend.position = "top")+
  theme(text = element_text(face = "bold", size = 20, colour = "black"))




ggsave("Crithidia Intensity No Zeroes.pdf", height = 6, width =10)



emmeans_nbinom.TMB2_SampleWeek.df <- as.data.frame(emmeans_nbinom.TMB2_SampleWeek$emmeans)

SampleWeek.plot <- ggplot(emmeans_nbinom.TMB2_SampleWeek.df, aes(x=SampleWeek, y=response)) +
  geom_bar(aes(x=SampleWeek, y=response))+
  geom_errorbar(aes(ymin=response - SE, ymax=response + SE),linetype = "solid", size=0.5, width=0.1)+
  ylab(ylabel) +
  xlab("Time (wks)") + ylim(c(0,70))+
  theme_classic() +
  theme(text = element_text(face = "bold", size = 20, colour = "black"))




ggsave("Crithidia Intensity No Zeroes.pdf", height = 6, width =10)


###################################################################################################
# Bee size x Cithidia infetcion intensity plot

nbinom.TMB2<-glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1|ColonyID), 
                     family = "nbinom2",
                     data=NoZero.df) 

# visualize effects

library(effects)
ae <- allEffects(nbinom.TMB2, se = TRUE)
plot(ae)

BeeSize.df <- as.data.frame(ae$WingSize)

ggplot(BeeSize.df, aes(x = WingSize, y = fit)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=fit - se, ymax=fit + se), alpha = 0.4) + 
  theme_classic() + 
  ylab(ylabel) +
  xlab("Bee Size (mm)") + 
  geom_point(data = NoZero.df, aes(x = WingSize, y = Crithidia), size = 1, alpha = 0.2, color = "blue") + 
  theme(text = element_text(face = "bold", size = 20, colour = "black"))

ggsave("Crithidia Intensity x Bee Size.pdf", height = 6, width =6)
  




###################################################################################################

#There are many solutions to test for the equality (homogeneity) of variance across groups, including:
# Fligner-Killeen test: a non-parametric test which is very robust against departures from normality.
#test of the null that the variances in each of the groups (samples) are the same.

nbinom.TMB2<-glmmTMB(Crithidia ~ Pollen * SampleWeek + WingSize + (1|ColonyID), 
                     family = "nbinom2",
                     data=NoZero.df) 
summary(nbinom.TMB2)
library(emmeans)
summary(emmeans(nbinom.TMB2, pairwise ~ Pollen, type = "response"))

plot(Crithidia ~ Pollen, data = NoZero.df)


fligner.test(Crithidia ~ Pollen, data = NoZero.df)


# Fligner-Killeen test of homogeneity of variances
# 
# data:  Crithidia by Pollen
# Fligner-Killeen:med chi-squared = 37.387, df = 1, p-value = 9.686e-10

fligner.test(Crithidia ~ SampleWeek, data = NoZero.df)
plot(Crithidia ~ SampleWeek, data = NoZero.df)

# Fligner-Killeen test of homogeneity of variances
# 
# data:  Crithidia by SampleWeek
# Fligner-Killeen:med chi-squared = 24.831, df = 8, p-value = 0.001661


# Variation in Crithidia ainfection intensity was lower for sunflower colonies and a 
# Fligner-Killeen test of homogeneity of variances indicated a significant difference (37.387, P < 0.0001)



# Influence measures
#Influence measures quantify the effects of particular observations, or groups of observations,
#on the results of a statistical model; leverage and Cook’s distance are the two most common formats for influence measures.

source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

M2_influence.time <- system.time(
  M2_influence <- influence_mixed(M2, groups="ColonyID")
)

car::infIndexPlot(M2_influence)

inf <- as.data.frame(M2_influence[["fixed.effects[-ColonyID]"]])
inf <- transform(inf,
                 ColonyID=rownames(inf),
                 cooks=cooks.distance(M2_influence))
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

# Setting a Cook's distance threshold of 0.125 gives us a single colony above said treshold (ColonyID = 45)
# Cook's D = 0.127525994, which is barely above that treshold

# Cook’s Distance is a measure of how influential an instance is to the computation of a regression, 
# e.g. if the instance is removed would the estimated coeficients of the underlying model be substantially
# changed? Because of this, Cook’s Distance is generally used to detect outliers in standard, OLS regression. 
# In fact, a general rule of thumb is that D(i) > 4/n is a good threshold for determining highly influential 
# points as outliers and this visualizer can report the percentage of data that is above that threshold.

# 4/n = 4/20 = 0.2

#Using a threshold of 4/n = 0.2, then we really don't have any ColonyID's that substantially change the outcome of the model
# Using a threshold of 4/(N - k - 1), where N is the number of observations and k is the numebr of explanatory variables
   # the threshold = 4/(20 - 4 - 1) = 4/15 = 0.2666667, there are still no outliers

#(1) Fox, John. (1991). Regression Diagnostics: An Introduction. Sage Publications.

#Cook's distance refers to how far, on average, predicted y-values will move if the observation 
# in question is dropped from the data set. dfbeta refers to how much a parameter estimate changes 
# if the observation in question is dropped from the data set. Problem is that I can't seem to find a dfbeta function that works with glmmTMB



# Let's visualize Crithidia counts for the colonies
library(dplyr)
ColonyID.df <- NoZero.df %>% 
  group_by(ColonyID, SampleWeek) %>% 
  summarise(mean = mean(Crithidia)) 

ggplot(ColonyID.df, aes(x = SampleWeek, y = mean, group = Pollen, colour = Pollen)) +
  geom_point() + geom_line()

Colony45.df <- ColonyID.df %>% 
  filter(ColonyID == 45)

ggplot(Colony45.df, aes(x = SampleWeek, y = mean, group = ColonyID, colour = ColonyID)) +
  geom_point() + geom_line()

# We can see from the plot that at week 8 the Average Crithidia load spikes
# Looking back at the inlfuence plot, we can see that for SampleWeek 8, Colony 45 is 
# above all other colonies


### So, let's drop it form the model and re-run

#First, update df to exclude Colony 45


NoZero.No45.df <- NoZero.df %>% 
  filter(ColonyID != 45) # It worked!

# Second, re-run model M2 with new No45 df
M2.No45 <- glmmTMB(Crithidia~  Pollen*SampleWeek + WingSize + (1|ColonyID) + ar1(0 + SampleWeek |ColonyID),
             family = "nbinom2",
             data = NoZero.No45.df)
summary(M2.No45)
Anova(M2.No45)
Anova(M2)

# Pollen*SampleWeek interaction still not sig, although P-value did drop from 0.45 to 0.18
# WingSize still sig
# SampleWeek still sig, although much more so
# Pollen now only moderately sig, chnage from P = 0.024 to 0.058

emmeans_No45<-emmeans(M2.No45, ~ Pollen*SampleWeek, type = "response")

# No huge change in estimated marginal means when dropping colony 45

# Overall, I woudl argue that colony 45 not a statistical outlier and dropping it from the final model not warranted
# however, it is worth noting that even though S treatment appears to alter intensity of infection, there is variation 
# between colonies that consume S, such that some react differently. In the case of Colony 45, there is a substantial spike 
# in intensity at week 8

# We should look at the patter of intensity over time for only the S colonies

#Plot raw means for S colonies only

S.Only.df <- NoZero.df %>% 
  filter(Pollen == "S") %>% 
  group_by(SampleWeek, ColonyID) %>% 
  summarise(mean = mean(Crithidia))


S_only_plot <- ggplot(S.Only.df, aes(x=SampleWeek, y=mean, group = ColonyID)) +
  geom_line(aes(linetype = ColonyID, color = ColonyID))+
  geom_point(aes(shape = ColonyID, color = ColonyID), size = 3)


#Plot raw means  for W colonies only

W.Only.df <- NoZero.df %>% 
  filter(Pollen == "W") %>% 
  group_by(SampleWeek, ColonyID) %>% 
  summarise(mean = mean(Crithidia))


W_only_plot <- ggplot(W.Only.df, aes(x=SampleWeek, y=mean, group = ColonyID)) +
  geom_line(aes(linetype = ColonyID, color = ColonyID))+
  geom_point(aes(shape = ColonyID, color = ColonyID), size = 3)

require(gridExtra)

grid.arrange(S_only_plot, W_only_plot, ncol=2)
#Tremendous variation due to genotype: some colonies from both treatments behave the same (i.e. 2-S and 43-W), 
# where both start off with highest infetcion loads, drop at week 3 and 4, cycle up and down from weeks 4 to 8,
 # and then increase sharply at weeks 9 and 10. Note that mean load drops earlier for the S colony (week 3), whereas 
# there's an increase from week 2 to 3 for the W colony and then the drop occurs a week later. 

# Note that at week 4, there was a drop in mean load for all but one S colony, while there was a drop for 5 out of 10 W colonies
# the rest either saw no change (n = 3) or an increase (n = 2). Also worth noting that the one S colony that didnt have a drop 
# at week 4, did have a drop at week 3 and then only a slight increase at week 4, which is probably more of a no change.

# In the final weeks (9 and 10), we see high mean loads (>50) for only only 2 S colonies, and downward trajectories for 6 colonies,
# steep upward trajectories for 2 and a mild (if not a no-change) for 1. For W colonies, we see 7 out of 10 with high loads at week 9 or 10, 
# and 9 out of 10 with upward trajectories and 1 with a no-change from week 9 to 10. Overall, this pattern suggests that in the final weeks of the exp,
# Which coincides with the end of the Bombus life-cycle, the mean infection loads are much higher for W fed colonies compared to S fed colonies. 


# Let's look at contrasts between Pollen diet by SmapleWeek
Pairwise.comps<-emmeans(M2, ~ Pollen | SampleWeek, type = "response")
pairs(Pairwise.comps)

# We can see that t-test is not sig at week 10
# SampleWeek = 10:
#   contrast ratio    SE   df t.ratio p.value
# S / W    0.642 0.173 1191 -1.646  0.1000 

# Drop Colony 2 from the model and lets see what happens for week 10

NoZero.No2.df <- NoZero.df %>% 
  filter(ColonyID != 2) # It worked!

# Second, re-run model M2 with new No45 df
M2.No2 <- glmmTMB(Crithidia~  Pollen*SampleWeek + WingSize + (1|ColonyID) + ar1(0 + SampleWeek |ColonyID),
                   family = "nbinom2",
                   data = NoZero.No2.df)
Anova(M2.No2)
#Same general pattern as the model that inlcudes Colony 2
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Crithidia
# Chisq Df Pr(>Chisq)   
# Pollen             8.9899  1   0.002715 **
#   SampleWeek        21.8183  8   0.005264 **
#   WingSize           3.1331  1   0.076720 . 
# Pollen:SampleWeek  9.7351  8   0.284111  

Pairwise.comps.No2<-emmeans(M2.No2, ~ Pollen | SampleWeek, type = "response")
pairs(Pairwise.comps.No2)

# SampleWeek = 10:
#   contrast ratio    SE   df t.ratio p.value
# S / W    0.526 0.141 1138 -2.393  0.0169 

# Now there's a sig dif at week 10





########### Crithidia infection intensity plot #############
library(ggplot2)
ylabel<- expression(bold(italic(Crithidia)~count~
                           "(cells * 0.02"~mu~L^-1*")", sep=""))  #check for encoding
xlabel <- expression(bold(Time (weeks)))
Colors <- c("#E69F00","#000000")
Lines<- c("solid", "dotted")


# Get estimated marginal means
emmeans_fit_M2<-emmeans(M2, ~ Pollen*SampleWeek, type = "response")

# rename for ease
eznb1 <-emmeans_fit_M2


# Add significance letters
Lets_eznb1<-cld(eznb1, Letters = letters)
Lets_eznb1
Lets_eznb1<- dplyr::select(Lets_eznb1, Pollen:SampleWeek, .group)
Lets_eznb1

eznb1 <-data.frame(emmeans_fit_M2)

# merge emmeans and letters
library(plyr)
emmeans.df.mergedM3 <- join(eznb1, Lets_eznb1)  #### need to fix this for plot
emmeans.df.mergedM3
emmeans.df.mergedM3$.group<-gsub(pattern = " " , replacement="", x= emmeans.df.mergedM3$.group)
emmeans.df.mergedM3


pM2 <- ggplot(emmeans.df.mergedM3, aes(x=SampleWeek, y=response, group = Pollen)) +
  geom_line(aes(linetype = Pollen, color = Pollen))+
  geom_errorbar(aes(ymin=response - SE, ymax=response + SE),linetype = "solid", size=0.5, width=0.1)+
  geom_point(aes(shape = Pollen, color = Pollen), size = 3)+
  scale_color_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"), values = Colors)+
  scale_shape_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = c(15,17))+
  scale_linetype_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = Lines) +  
  ylab(ylabel) +
  xlab("Time (wks)") + ylim(c(0,70))+
  theme_classic() +
  theme(legend.position = "top")+
  theme(text = element_text(face = "bold", size = 20, colour = "black"))




ggsave("Crithidia Intensity No Zeroes.pdf", height = 6, width =10)

