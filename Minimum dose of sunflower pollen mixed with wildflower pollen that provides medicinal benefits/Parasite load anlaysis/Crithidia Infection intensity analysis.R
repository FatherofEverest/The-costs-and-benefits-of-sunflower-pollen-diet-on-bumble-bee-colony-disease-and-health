####analysis of Crithidia loads in Proportions (Ratio) exp - Round 4 ####### 
## June 2017 NCSU ##
## 4 pollen diets - all bees infected with Crithidia
## 100% pure sunflower
##  50% Sunflower/50% Wildflower mix
##  25% Sunflower/75% Wildflower mix
##   0% Sunflower/100% Wildflower mix

library(ggplot2)
library(cowplot)
library(glmmTMB)
library(lme4)
library(plyr)
library(dplyr)
library(emmeans)
library(lsmeans)


#### Final model -> Mod1
#### Plot -> P3
#### Means -> Lsmeans.df.merged
###############################################################
###############################################################
##############################################################################################################################
#Model:Count ~ Treatment + WingSize + (1 | ColonyID) + (1 | Date)
#            Df    AIC     LRT  Pr(>Chi)    
# Treatment  3 516.71 18.6750 0.0003191 ***
# WingSize   1 505.95  3.9136 0.0478979 *  
####
# contrast     estimate        SE df z.ratio p.value
# 100% - 50% -0.6696263 0.8094737 NA  -0.827  0.8416
# 100% - 25% -2.2482218 0.7973865 NA  -2.819  0.0248 *
# 100% - 0%  -3.1262721 0.6679317 NA  -4.681  <.0001 ***
# 50% - 25%  -1.5785955 0.7369816 NA  -2.142  0.1399
# 50% - 0%   -2.4566458 0.7488034 NA  -3.281  0.0057 **
# 25% - 0%   -0.8780502 0.6941228 NA  -1.265  0.5853

# Pollen    lsmean        SE df  asymp.LCL asymp.UCL     Count       btlo       bthi .group
# 1   100% 0.6567868 0.9334939 NA -1.1728277  2.486401  1.928585  0.7582765   4.905126      a
# 2    50% 1.3264131 0.9386563 NA -0.5133195  3.166146  3.767505  1.4736713   9.631793     ab
# 3    25% 2.9050086 0.9247343 NA  1.0925626  4.717455 18.265400  7.2447295  46.050698     bc
# 4     0% 3.7830588 0.8894695 NA  2.0397306  5.526387 43.950272 18.0580092 106.967851      c


exp(0.9334939) #2.54338
exp(0.8894695) #2.433838
###############################################################
###############################################################
###############################################################



rm(list=ls()) #clear memory
#set working directory
data<-read.csv("Round4CrithidiaAnalysis.csv",header=TRUE)

str(data) 
# 'data.frame':	90 obs. of  6 variables:
# $ BeeID    : int  1 5 10 11 13 14 15 17 20 21 ...
# $ ColonyID : Factor w/ 3 levels "A","B","C": 1 2 1 2 1 2 3 2 2 3 ...
# $ Date     : Factor w/ 2 levels "06/12/17","06/14/17": 1 1 1 1 1 1 1 1 1 1 ...
# $ Treatment: Factor w/ 4 levels "0%","100%","25%",..: 2 2 4 3 2 4 3 2 1 2 ...
# $ Count    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ WingSize : num  2.8 2.8 2.7 2.95 2.8 3 2.65 2.85 2.9 2.6 ...

head(data)
# BeeID ColonyID     Date Treatment Count WingSize
#     1        A 06/12/17      100%     0     2.80
#     5        B 06/12/17      100%     0     2.80
#    10        A 06/12/17       50%     0     2.70
#    11        B 06/12/17       25%     0     2.95
#    13        A 06/12/17      100%     0     2.80
#    14        B 06/12/17       50%     0     3.00



#reorder levels so that plotting will look better
print(levels(data$Treatment))
Pollen = factor(data$Treatment,levels(data$Treatment)[c(2,4,3,1)])
print(levels(Pollen))

########### Crithidia Infection Intensity Model ############

help(glmmTMB)
Mod1<-glmmTMB(Count ~ Pollen + WingSize +(1|ColonyID)  + (1|Date), family = "nbinom2",data=data) 

summary(Mod1)
#Use drop1() because car::Anova not supported
drop1(Mod1, test = "Chisq")
# Single term deletions
# Model:Count ~ Treatment + WingSize + (1 | ColonyID) + (1 | Date)
#           Df    AIC     LRT  Pr(>Chi)    
# <none>       504.03                      
# Treatment  3 516.71 18.6750 0.0003191 ***
# WingSize   1 505.95  3.9136 0.0478979 *  

#Pairwise comparisons  #Source Ben Bolker function
#source("glmmTMB.to.lsmeans copy.R")
lsm.TMB<-emmeans(Mod1, ~Pollen)
Pairs<- contrast(lsm.TMB, "pairwise") 
Pairs
# contrast     estimate        SE df z.ratio p.value
# 100% - 50% -0.6696263 0.8094737 NA  -0.827  0.8416
# 100% - 25% -2.2482218 0.7973865 NA  -2.819  0.0248 *
# 100% - 0%  -3.1262721 0.6679317 NA  -4.681  <.0001 ***
# 50% - 25%  -1.5785955 0.7369816 NA  -2.142  0.1399
# 50% - 0%   -2.4566458 0.7488034 NA  -3.281  0.0057 **
# 25% - 0%   -0.8780502 0.6941228 NA  -1.265  0.5853
#P value adjustment: tukey method for comparing a family of 4 estimates
library(multcomp)
Lets<-cld(lsm.TMB, Letters = letters)
Lets

# Treatment    lsmean        SE df  asymp.LCL asymp.UCL .group
# 100%      0.6567935 0.9334910 NA -1.1728152  2.486402  a    
# 50%       1.3264128 0.9386534 NA -0.5133141  3.166140  ab   
# 25%       2.9050058 0.9247311 NA  1.0925660  4.717445   bc  
# 0%        3.7830496 0.8894655 NA  2.0397292  5.526370    c  
# 
# Confidence level used: 0.95 
# P value adjustment: tukey method for comparing a family of 4 estimates 
# significance level used: alpha = 0.05

#lsmeans(Mod1, ~Treatment, type = "response") #This works too

library(lsmeans)
library(ggplot2)
#Use lsmeans to get mean counts
My.Lsmeans <- emmeans(Mod1, ~Pollen) 
My.Lsmeans



##################################################################################################
######### Binomial model for proportion of bees infected with Crithidia ###################
##################################################################################################
##################################################################################################


data$Infection<-data$Count
data$Infection[data$Infection>0] <- 1
data$Infection

aggregate(FUN = mean,data$Infection~data$Treatment)
#data$Treatment        data$Infection
# 1             0%      0.9200000
# 2           100%      0.1304348
# 3            25%      0.5714286
# 4            50%      0.3333333

ProportionMod1<-glmmTMB(Infection~Pollen  + WingSize + (1|Date)+
                  (1|ColonyID),  family = "binomial",
                data=data) 

summary(ProportionMod1)


drop1(ProportionMod1, test = "Chisq")
# Single term deletions
# Model: Infection ~ Pollen + WingSize + (1 | Date) + (1 | ColonyID)
#            Df     AIC    LRT  Pr(>Chi)    
# <none>       99.322                     
# Pollen    3 131.975 38.654 2.055e-08 ***
# WingSize  1  97.898  0.576    0.4477    

ProportionMod2<-glmmTMB(Infection~Pollen + (1|Date)+
                          (1|ColonyID),  family = "binomial",
                        data=data) 

summary(ProportionMod2)
drop1(ProportionMod2, test = "Chisq")
# Single term deletions
# Model:Infection ~ Pollen + (1 | Date) + (1 | ColonyID)
#         Df     AIC    LRT  Pr(>Chi)    
# <none>     97.898                     
# Pollen  3 130.324 38.426 2.296e-08 ***


#Source Ben Bolker function
#source("glmmTMB.to.lsmeans copy.R") ###deprecated
#library(lsmeans)
lsm.Prop<-emmeans(ProportionMod2, ~Pollen)
Prop.Pairs<- contrast(lsm.Prop, "pairwise")
Prop.Pairs
# contrast    estimate        SE df z.ratio p.value
# 100% - 50% -1.289823 0.7935701 NA  -1.625  0.3642
# 100% - 25% -2.319774 0.7925186 NA  -2.927  0.0180 *
# 100% - 0%  -4.523172 1.0034596 NA  -4.508  <.0001 ***
# 50% - 25%  -1.029951 0.6640489 NA  -1.551  0.4070
# 50% - 0%   -3.233349 0.8963221 NA  -3.607  0.0018 **
# 25% - 0%   -2.203398 0.8774342 NA  -2.511  0.0582 .

LetsProp<-cld(lsm.Prop, Letters = letters)
LetsProp

# Pollen     lsmean        SE df  asymp.LCL  asymp.UCL .group
# 100%   -1.9533120 0.6976159 NA -3.3206141 -0.5860099  a    
# 50%    -0.6634888 0.5578391 NA -1.7568334  0.4298558  ab   
# 25%     0.3664619 0.5476480 NA -0.7069085  1.4398322   bc  
# 0%      2.5698599 0.8136997 NA  0.9750378  4.1646821    c  


#Use lsmeans to get mean counts
My.LsmeansProp <- emmeans(ProportionMod2, ~Pollen) 
My.LsmeansProp
library(boot)
#inv.logit(x)
LsmeansProp.df<-as.data.frame(summary(lsmeans(ProportionMod2, ~Pollen)))
LsmeansProp.df$btmean<-inv.logit(LsmeansProp.df$lsmean)
LsmeansProp.df$btlo<-inv.logit(LsmeansProp.df$lsmean - LsmeansProp.df$SE) #lower bound
LsmeansProp.df$bthi<-inv.logit(LsmeansProp.df$lsmean + LsmeansProp.df$SE) #upper bound
View(LsmeansProp.df)
LsmeansProp.df

LetsProp2<- dplyr::select(LetsProp, Pollen, .group)
LetsProp2
library(plyr)
LsmeansProp.df.merged<- join(LsmeansProp.df, LetsProp2, by = "Pollen")
LsmeansProp.df.merged$.group<-gsub(pattern = " " , replacement="", x= LsmeansProp.df.merged$.group)
LsmeansProp.df.merged
# Pollen     lsmean        SE df  asymp.LCL  asymp.UCL    btmean       btlo      bthi .group
# 1   100% -1.9533120 0.6976159 NA -3.3206141 -0.5860099 0.1241927 0.06593184 0.2217157      a
# 2    50% -0.6634888 0.5578391 NA -1.7568334  0.4298558 0.3399563 0.22770284 0.4736121     ab
# 3    25%  0.3664619 0.5476480 NA -0.7069085  1.4398322 0.5906038 0.45482698 0.7138404     bc
# 4     0%  2.5698599 0.8136997 NA  0.9750378  4.1646821 0.9288964 0.85272810 0.9671868      c


################## PLOT ON SCALE OF RESPONSE ##########

#### First, Crithidia infection intensity model####
#Let's exponentiate original mean+SE and mean-SE
Lsmeans.df<-as.data.frame(summary(emmeans(Mod1, ~Pollen)))
Lsmeans.df$Count<-exp(Lsmeans.df$emmean)
Lsmeans.df$btlo<-exp(Lsmeans.df$emmean - Lsmeans.df$SE) #lower bound
Lsmeans.df$bthi<-exp(Lsmeans.df$emmean + Lsmeans.df$SE) #upper bound
Lsmeans.df
Lets2<- dplyr::select(Lets, Pollen, .group)
Lets2
Lsmeans.df.merged<- join(Lsmeans.df, Lets2, by = "Pollen")
Lsmeans.df.merged
Lsmeans.df.merged$.group<-gsub(pattern = " " , replacement="", x= Lsmeans.df.merged$.group)
Lsmeans.df.merged
# Pollen    lsmean        SE df  asymp.LCL asymp.UCL     Count       btlo       bthi .group
# 1   100% 0.6567868 0.9334939 NA -1.1728277  2.486401  1.928585  0.7582765   4.905126      a
# 2    50% 1.3264131 0.9386563 NA -0.5133195  3.166146  3.767505  1.4736713   9.631793     ab
# 3    25% 2.9050086 0.9247343 NA  1.0925626  4.717455 18.265400  7.2447295  46.050698     bc
# 4     0% 3.7830588 0.8894695 NA  2.0397306  5.526387 43.950272 18.0580092 106.967851      c

#x-axis:
pollen.axis<-c("Sun","50% Sun","25% Sun","Wild")
#y-axis:
ylabelA<- expression(italic(Crithidia)~count~
                      "(cells * 0.02"~mu~L^-1*")", sep="")  #check for encoding
Colors <- c("orange", "red", "gray", "blue")

#ready to plot?
pd<-position_dodge(width=0.75)


p<-ggplot(Lsmeans.df.merged, aes(x = Pollen, y = Count)) + 
  geom_bar(aes(fill = Pollen), color = "black", 
           stat = "identity", position = pd, size = 2)+
  geom_errorbar(aes(ymin = btlo, ymax = bthi), 
                size = 2, width=0.6, color = "black", position = pd) +
  scale_x_discrete(labels=pollen.axis) +
  ylab(ylabelA) + 
  xlab("Pollen") + 
  theme(axis.line = element_line(size = 8),
        text = element_text (size = 30, face = "bold"))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.2 * max(Lsmeans.df$bthi)))+ 
  scale_fill_manual(values = Colors) + 
  theme_classic()
p
#P2<-p + geom_text(aes(y = bthi + 0.05 * max(bthi), label = .group),
                        #size = 10)
P2 <- p
P3 <- P2 + theme(text = element_text(size = 50, face="bold"))
P4 <- P3 + theme(axis.text.x = element_text(size=40), axis.text.y =element_text(size=50))
P4 <- P4 + theme(legend.position = "none")
P4

#Exported as 6" x 6" pdf




#### Second, Proportion infected model ####
#x-axis:
pollen.axis<-c("Sun","50% Sun","25% Sun","Wild")
#y-axis:
ylabel<- expression(italic(Crithidia)~prevalence, sep="")  #check for encoding
Colors <- c("orange", "red", "gray", "blue")

#ready to plot?
pd<-position_dodge(width=0.75)
pX<-ggplot(LsmeansProp.df.merged, aes(x = Pollen, y = btmean)) + 
  geom_bar(aes(fill = Pollen), color = "black", 
           stat = "identity", position = pd, size = 2)+
  geom_errorbar(aes(ymin = btlo, ymax = bthi), 
                size = 2, width=0.6, color = "black", position = pd) +
  scale_x_discrete(labels=pollen.axis) +
  ylab(ylabel) + 
  xlab("Pollen")+
  theme(axis.line = element_line(size = 9),
        text = element_text (size = 30, face = "bold"))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1))+
  scale_fill_manual(values = Colors) + 
  theme_classic()
pX
#PX2<-pX + geom_text(aes(y = bthi + 0.05 * max(bthi), label = .group),
                 # size = 10)
PX2 <- pX
PX3 <- PX2 + theme(text = element_text(size = 50, face="bold"))
PX3
PX4 <- PX3 + theme(axis.text.x = element_text(size=40), axis.text.y =element_text(size=50))
PX4 <- PX4 + theme(legend.position = "none")
PX4

plot_grid(PX4, P4, labels=c("a", "b"), ncol = 2, nrow = 1, label_size = 60)

ggsave("Crithidia Infection Panel.pdf", height = 15, width =26)
