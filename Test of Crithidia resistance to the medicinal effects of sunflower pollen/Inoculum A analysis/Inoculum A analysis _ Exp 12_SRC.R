##############################################################################
##############################################################################
#### Part of Experiment 12_SRC 

#### Purpose: To determine if there is a significant difference between the mean
# parasite loads of three different bumble bee colonies used to make the PRS Inoculum (aka A)
# in Exp 12_SRC.

#### Parasite loads fit a noraml distribution, so I used a simple ANOVA with 'ColonyID' as the factor and 'Count' as the
# response variable.

##############################################################################
##############################################################################

rm(list=ls()) #clear memory
setwd("~/Google Drive/POLLINATOR RESEARCH/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 12_SRC Exp. ")


data<-read.csv("Exp 12_SRC_A Inoculum.csv", header = TRUE)
str(data)
# 'data.frame':	21 obs. of  3 variables:
# $ Bee.    : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ColonyID: Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 1 2 2 2 ...
# $ Count   : int  3 167 108 59 224 311 1 119 186 98 ...

########################################################################################
#### Distribution ####
########################################################################################
library(car)
library(MASS)
qqp(data$Count, "norm")
### Looks great!


res<-resid(Mod1)
fit<-fitted(Mod1)
plot(fit, res, xlab = "Fitted values", ylab = "Residuals") + abline(h=0, lty= "dashed") #Pretty!!!!

plot(Mod1)

########################################################################################
#### Model ####
########################################################################################

plot(data$ColonyID, data$Count, xlab = "Colony", ylab = "Parasite Load")

Mod1<-aov(Count~ColonyID, data = data)
summary(Mod1)
#              Df Sum Sq Mean Sq F value Pr(>F)
# ColonyID     2   4578    2289   0.319  0.731
# Residuals   18 129114    7173   


#### Pairwise Comparisons not neccessary ####
### Tukey's HSD ###
library(multcomp)
Tukey<-glht(Mod1, mcp(ColonyID = "Tukey"))
summary(Tukey)
# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: Tukey Contrasts
# 
# 
# Fit: aov(formula = Count ~ ColonyID, data = data)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)
# B - A == 0    15.00      45.27   0.331    0.941
# C - A == 0   -21.00      45.27  -0.464    0.889
# C - B == 0   -36.00      45.27  -0.795    0.711
# (Adjusted p values reported -- single-step method)

########################################################################################
#### Plot #####
########################################################################################
library(emmeans)
lsm<-emmeans(Mod1, ~ColonyID)
lsm
# ColonyID   lsmean       SE df lower.CL upper.CL
# A        124.7143 32.01119 18 57.46126 191.9673
# B        139.7143 32.01119 18 72.46126 206.9673
# C        103.7143 32.01119 18 36.46126 170.9673
# 
# Confidence level used: 0.95 

library(multcompView)
LetsA<-cld(lsm, Letters = letters)

LetsA


library(ggplot2)
x.axis<-c( "A","B", "C")
pd<-position_dodge(0.9)
plot0<- ggplot(LetsA, aes(x=ColonyID, y=emmean)) +
  geom_bar(position=pd, stat="identity", colour="black" ) + ##Uncomment for bar chart
  #geom_point(position=pd, stat="identity", colour="black" ) + ##Uncomment for interval chart
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE),
                size=1, width=0.2,  # Width of the error bars
                position=pd)
plot0
ylabel<- expression(bold(italic(Crithidia)~count~
                           "(cells * 0.02"~mu~L^-1*")", sep="")) #check for encoding

plot02<- plot0 + ylab(ylabel) + xlab("PRS Colony ID")+ theme(text = element_text(face = "bold", size = 20))
plot02  
plot03<-plot02 + theme_classic()
plot03

plot04<- plot03 + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) + theme(axis.line = element_line(size = 1, colour = "black"))  
plot04  
plot05 <- plot04+ theme(text = element_text(face = "bold", size = 20))
plot05
plot06<- plot05 + geom_text(aes(y = upper.CL + 2, label = .group),size = 8)
plot06




