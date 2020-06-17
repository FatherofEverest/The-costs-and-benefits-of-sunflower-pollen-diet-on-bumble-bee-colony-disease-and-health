############################################################################################################################################################
############################################################################################################################################################
#### Parasite load analysis - Exp 12 - Sunflower Resistant Crithidia ###

# Purpose: To test if sustained (3 months) colony-wide sunlfower pollen supplementation selects for sunlfower resistant Crithidi? 

# We ran a 2 x 2 factorial experiment in which individual Bombus impatiens workers, sourced from 3 different colonies (#4, #5 and #6), were (Factor 1) inoculated with 
# a standardized dose of Crithidia bombi sourced from either a colony fed strictly wildflower pollen (inoculumm "B") or colonies fed strictly sunflower pollen
# (inoculum "A") for a period of three months and (Factor 2) either fed sunflower pollen or wildflower pollen for seven days following inoculation. We measured 
# Crithidia infection intensity as the number of Crithidia cells per sample per bee on a haemocytometer using previously established methods (see Richardson et al. 2015).
# We also measured the radial cell (aka marginal cell) of each bee's right forewing as a proxy for bee size (see XXX), which was used as a covariate in the analysis.


#Responses: 
#    Count: Crithdiia infection intensity (i.e. total number of Crithidia cells counted on a haemocytometer)
#    Infection: Crithidia prevalence (i.e. the proportion of bees with detectable infection)

#Factors: 
#    Pollen: "S" or "W" (Sunflower or Wildflower pollen) 
#    Inoculum: "A" is PRS (Potentially Resistant Strain) or "B" is NRS (Non-Resistant Strain).
#    Each bee inoculated with Crithdia sourced from Bombus colony fed only W (B) or fed only S (A))
#    Wing: Covariate - proxy for bee size - lenght of radial cell in right forwing (mm)

#random effect: "ColonyID" 

##### Summary of results #####
# For Crithidia infection intensity (i.e. comparing mean parasite loads between treatments) 
# there was no significant interaction between pollen diet and inoculum source (P =  0.3428). 
# There was a significant effect of pollen diet (P = 9.397e-06), but not for inoculum source (P = 0.7940). 
# Mean parasite loads for bees fed wildflower pollen were almost 4 times greater than bees fed sunflower 
# pollen (see plot06).  A similar pattern was observed for Crithidia prevalence
# (i.e. the proportion of bees with detectable infection). There was no significnat interaction
# between pollen diet and inoculum source (P = 0.5997). There was a significant effect by pollen diet 
# (P = 4.541e-09) and no significant effect by inoculum source (P = 0.2182).  Between 84% and 91% of bees
# fed wildflower pollen had detectable Crithidia after 7 days, compared to only 20% to 34% of bees fed 
# sunflower pollen.   

############################################################################################################################################################
############################################################################################################################################################

rm(list=ls()) #clear memory

setwd("~/Google Drive/POLLINATOR RESEARCH/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 12_SRC Exp. /Parasite load analysis")

data<-read.csv("Exp 12_SRC_Parasite analysis.csv", header = TRUE)
str(data) # need to change some variables to factors

data$ColonyID<-as.factor(data$ColonyID)
data$BeeID<-as.factor(data$BeeID)
str(data)
# 'data.frame':	82 obs. of  7 variables:
#   $ BeeID    : Factor w/ 82 levels "1","2","3","4",..: 5 15 18 22 26 34 36 52 55 59 ...
# $ Treatment: Factor w/ 4 levels "SA","SB","WA",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ ColonyID : Factor w/ 3 levels "4","5","6": 1 1 1 1 1 2 2 2 2 3 ...
# $ Count    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Wing     : num  2.8 2.7 2.75 3 2.7 2.95 3 3 2.6 2.75 ...
# $ Pollen   : Factor w/ 2 levels "S","W": 1 1 1 1 1 1 1 1 1 1 ...
# $ Inoculum : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
plot(Count~Treatment, data = data)

length(which(data$ColonyID == 4)) # n = 33
length(which(data$ColonyID == 5)) # n = 24
length(which(data$ColonyID == 6)) # n = 25

length(which(data$Treatment == "SA")) # n = 19
length(which(data$Treatment == "SB")) # n = 22
length(which(data$Treatment == "WA")) # n = 19
length(which(data$Treatment == "WB")) # n = 22

# Create NoZero data frame that excludes all bees without infection for the intensity analysis
# Remove zeroes from the data; We're only analyzing intensity for those bees with actual infection
#Then we need to run the analysis for prevalence. First we created a new binomial variable called "Infection". 
data$Infection<-data$Count
data$Infection[data$Infection>0] <- 1

#Make new data frame with zeroes excluded
NoZero.df <- data %>% 
  filter(Infection == 1)
str(NoZero.df)
View(NoZero.df)

#Check for NA's
check.na(NoZero.df) #There is a total of  0  NAs on this file[1] 0

# Visualize the raw data
plot(NoZero.df$Treatment, NoZero.df$Count)

############################################################################################################################################################
# Distribution
############################################################################################################################################################
library(car)
library(MASS)
qqp(NoZero.df$Count, "norm")
### Definitely not normal, as expected
qqp(NoZero.df$Count, "lnorm")
### No good


poisson <- fitdistr(NoZero.df$Count, "Poisson")
qqp(NoZero.df$Count, "pois", lambda = poisson$estimate)
# Not a good fit

nbinom <- fitdistr(NoZero.df$Count, "Negative Binomial")
qqp(NoZero.df$Count, "nbinom", size = nbinom$estimate[[1]],mu = nbinom$estimate[[2]])
#Looks ok, Negative binomial it is!


############################################################################################################################################################
# Crithidia Infection intensity model - Negative Binomial Model 
############################################################################################################################################################
library(glmmTMB)
nbinom.TMB<-glmmTMB(Count ~ Pollen * Inoculum  + Wing + (1|ColonyID), 
                    family = "nbinom2",
                    data=NoZero.df) 
summary(nbinom.TMB) 
Anova(nbinom.TMB)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Count
# Chisq Df Pr(>Chisq)  
# Pollen          4.1022  1    0.04283 *
# Inoculum        0.0002  1    0.98851  
# Wing            0.4150  1    0.51943  
# Pollen:Inoculum 0.0060  1    0.93815  


nbinom.TMB2<-glmmTMB(Count ~ Pollen * Inoculum + (1|ColonyID), 
                    family = "nbinom2",
                    data=NoZero.df) 
summary(nbinom.TMB2) 
Anova(nbinom.TMB2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Count
# Chisq Df Pr(>Chisq)  
# Pollen          4.6945  1    0.03026 *
# Inoculum        0.0125  1    0.91103  
# Pollen:Inoculum 0.0003  1    0.98596 



library(emmeans)
nbinom.TMB2.emms <- emmeans(nbinom.TMB2,  pairwise ~Pollen*Inoculum, type = "response")

# $emmeans
# Pollen Inoculum response    SE df lower.CL upper.CL
# S      A            9.54  6.98 41     2.18     41.8
# W      A           27.38 15.86 41     8.50     88.2
# S      B           10.15  8.52 41     1.86     55.3
# W      B           28.63 16.78 41     8.76     93.5
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the log scale 
# 
# $contrasts
# contrast  ratio    SE df t.ratio p.value
# S,A / W,A 0.348 0.213 41 -1.725  0.3241 
# S,A / S,B 0.940 0.842 41 -0.069  0.9999 
# S,A / W,B 0.333 0.225 41 -1.628  0.3746 
# W,A / S,B 2.698 2.061 41  1.299  0.5688 
# W,A / W,B 0.956 0.447 41 -0.095  0.9997 
# S,B / W,B 0.355 0.274 41 -1.344  0.5411 
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 
# Tests are performed on the log scale 


nbinom.TMB2.emms.Pollen <- emmeans(nbinom.TMB2,  pairwise ~Pollen, type = "response")
# $emmeans
# Pollen response    SE df lower.CL upper.CL
# S          9.84  6.37 41     2.66     36.4
# W         28.00 14.95 41     9.52     82.3
# 
# Results are averaged over the levels of: Inoculum 
# Confidence level used: 0.95 
# Intervals are back-transformed from the log scale 

nbinom.TMB2.emmeans.df <- as.data.frame(nbinom.TMB2.emms$emmeans)




#######################################################################################################################################################
#######################################################################################################################################################
#   Prevalence (i.e. comparing the proportion of bees in a given treatment with >1 Crithidia cell per sample)
#######################################################################################################################################################
#######################################################################################################################################################


aggregate(FUN = mean,data$Infection~data$Pollen:data$Inoculum)
# data$Pollen data$Inoculum data$Infection
# 1           S             A      0.3684211
# 2           W             A      0.8947368
# 3           S             B      0.1818182
# 4           W             B      0.8636364

ProportionMod1<-glmmTMB(Infection~ Pollen*Inoculum + Wing + 
                          (1|ColonyID),  family = "binomial",
                        data=data) 

summary(ProportionMod1)
Anova(ProportionMod1)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
# Chisq Df Pr(>Chisq)    
# Pollen          25.0514  1  5.582e-07 ***
# Inoculum         1.4549  1     0.2277    
# Wing             0.0400  1     0.8414    
# Pollen:Inoculum  0.2791  1     0.5973    
# No significnat interaction between Pollen and Inoculum

ProportionMod2<-glmmTMB(Infection~ Pollen*Inoculum + 
                          (1|ColonyID),  family = "binomial",
                        data=data) 

summary(ProportionMod2)
Anova(ProportionMod2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Infection
# Chisq Df Pr(>Chisq)    
# Pollen          25.0660  1   5.54e-07 ***
# Inoculum         1.5384  1     0.2149    
# Pollen:Inoculum  0.3048  1     0.5809   



Prop.TMB.emms <- emmeans(ProportionMod2,  pairwise ~Pollen*Inoculum, type = "response")
# $emmeans
# Pollen Inoculum  prob     SE df lower.CL upper.CL
# S      A        0.368 0.1107 77   0.1845    0.601
# W      A        0.895 0.0704 77   0.6574    0.974
# S      B        0.182 0.0822 77   0.0688    0.401
# W      B        0.864 0.0732 77   0.6477    0.956
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 
# 
# $contrasts
# contrast  odds.ratio      SE df t.ratio p.value
# S,A / W,A     0.0686  0.0608 77 -3.024  0.0175 
# S,A / S,B     2.6250  1.9142 77  1.323  0.5510 
# S,A / W,B     0.0921  0.0721 77 -3.048  0.0163 
# W,A / S,B    38.2499 35.5617 77  3.920  0.0011 
# W,A / W,B     1.3421  1.3045 77  0.303  0.9903 
# S,B / W,B     0.0351  0.0292 77 -4.028  0.0007 
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 
# Tests are performed on the log odds ratio scale 

Prop.TMB.emms.pollen <- emmeans(ProportionMod2, ~ Pollen,  type = "response")
# Pollen  prob     SE df lower.CL upper.CL
# S      0.265 0.0710 77    0.148    0.427
# W      0.880 0.0513 77    0.736    0.951
# 
# Results are averaged over the levels of: Inoculum 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 


nbinom.TMB2.emmeans.df <- as.data.frame(nbinom.TMB2.emms$emmeans)

Prop.TMB.emms.df <- as.data.frame(Prop.TMB.emms$emmeans)
############################################################################################################################################################
#####   FIGURES ############################################################################################################################################
############################################################################################################################################################

library(ggplot2)
pollen.axis<-c( "Sunflower", "Wildflower")
treatment.names<-c("PRS", "NRS")
pd<-position_dodge(0.9)
Colors <- c("white","grey49")
ylabel<- expression(bold(italic(Crithidia)~count~
                           "(cells * 0.02"~mu~L^-1*")", sep=""))  #check for encoding
ylabel.prob<- expression(bold(italic(Crithidia)~prevalence, sep=""))  #check for encoding

##############################################################################
#### First, Crithidia Infection Intensity

plot0<- ggplot(nbinom.TMB2.emmeans.df, aes(x=Pollen, y=response, fill = Inoculum)) +
  #, color = Treatment)) + #for interval chart
  geom_bar(position=pd, stat="identity", colour="black" ) + ##Uncomment for bar chart
  #geom_point(position=pd, stat="identity", colour="black" ) + ##Uncomment for interval chart
  geom_errorbar(aes(ymin=response - SE, ymax=response + SE),
                size=1, width=0.2,  # Width of the error bars
                position=pd)
plot0

plot02<-plot0 + ylab(ylabel) +#y label
  xlab("Pollen diet") + # x axis label
  scale_x_discrete(breaks=levels(nbinom.TMB2.emmeans.df$Pollen) , labels=pollen.axis)+
  scale_fill_manual(labels = treatment.names, values = Colors)+
  #scale_color_discrete(labels = treatment.names, values = Colors) + #Use this for interval plot
  theme(text = element_text(face = "bold", size = 30))+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, 49))
  
plot02
 
plot03<-plot02 + theme_classic()
plot03

plot04<- plot03 + theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), 
                        panel.border = element_blank(),
                        panel.background = element_blank()) + theme(axis.line = element_line(size = 1, colour = "black"))
plot04
plot05 <- plot04 + theme(text = element_text(size = 20, face="bold"))
plot05

plot06<- plot05 + theme(legend.position = c(0.2, 0.8))
plot06

############################################################################################################################################################
##### Now, Prevalence of infection
plotProp<- ggplot(Prop.TMB.emms.df, aes(x=Pollen, y=prob, fill = Inoculum)) +
  #, color = Treatment)) + #for interval chart
  geom_bar(position=pd, stat="identity", colour="black" ) + ##Uncomment for bar chart
  #geom_point(position=pd, stat="identity", colour="black" ) + ##Uncomment for interval chart
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE),
                size=1, width=0.2,  # Width of the error bars
                position=pd)
plotProp


plotProp2<-plotProp + ylab(ylabel.prob) +#y label
  xlab("Pollen diet") + # x axis label
  scale_x_discrete(breaks=levels(Prop.TMB.emms.df$Pollen) , labels=pollen.axis)+
  scale_fill_manual(labels = treatment.names, values = Colors)+
  #scale_color_discrete(labels = treatment.names, values = Colors) + #Use this for interval plot
  theme(text = element_text(face = "bold", size = 30))+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, .99))
  
plotProp2
plotProp3<-plotProp2 + theme_classic()
plotProp3

plotProp4<- plotProp3 + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), 
                              panel.border = element_blank(),
                              panel.background = element_blank()) + theme(axis.line = element_line(size = 1, colour = "black"))
plotProp4
plotProp5 <- plotProp4 + theme(text = element_text(size = 20, face="bold"))
plotProp5

plotProp6<- plotProp5 + theme(legend.position = c(0.2, 0.8))
plotProp6



##############################################################################
library(cowplot)
FinalPlot<-plot_grid(plotProp6, plot06, labels=c("a", "b"), ncol = 2, nrow = 1, label_size = 30)
FinalPlot
FinalPlot2 <- FinalPlot + theme(text = element_text(size = 30, face="bold"))
FinalPlot2
ggsave("Crithidia Infection Panel.pdf", height = 7, width =12)



