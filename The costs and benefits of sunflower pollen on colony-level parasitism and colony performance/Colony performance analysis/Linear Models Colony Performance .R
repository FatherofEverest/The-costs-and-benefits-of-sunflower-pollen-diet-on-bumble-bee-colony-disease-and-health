########## Exp 11: Laboratory Bombus colonies supplemented with Sunflower pollen ###############
########## Analysis of Colony Performance: #############

#May 9, 2020: re-analyze the following colony performance varianbles per Becky:
# 1 - Workers (Counted at the end of the exp) + TotalWorkers + Workers Weight
# 2 - Total queens produced + Queen Wieght + Prob. of prod.
# 3 - Total drones produced + Drone Weight + Prob. of prod. 
# 4 - Immatures (Combine eggs, larvae, and pupae); is weight corr. with no. ? If no, then combine wieght and run sep.
# 5 - Immature weight
# Worker Size
# Queen Size
# Drone Size
# 9 - Colony weight gain (i.e., Biomass gain)

# The Analysis starts here...
# Upload the data
setwd("/Users/rg9403/Google Drive/POLLINATOR RESEARCH/Sunflower Pollen Experiments/Irwin Lab_Sunflower Pollen Experiments/Exp 11_Lab Colony Sun pollen Supplements/Analyses/Final analyses/Colony Performance")
data<- read.csv("Data_Final Colony Dissections.csv", header = TRUE)

names(data)

# Create a new variable called LarvalWeight
data$LarvalWeight<- data$LarvaeWt/data$Larvae

is.na(data$LarvalWeight) <- sapply(data$LarvalWeight, is.infinite) #change INF to NA

# Create a new variable called PupalWeight
data$PupalWeight<- data$PupaeWt/data$Pupae

# Create a new variable called EggWeight
data$EggWeight<- data$EggWt/data$Eggs
is.na(data$EggWeight) <- sapply(data$EggWeight, is.na)

# Create a new variable called WorkerWeight
data$WorkersWeight<- data$WorkerWt/data$Workers

# Create new variable called QueenWeight
data$QueenWeight<- data$QueenWt/data$TotalQueens

#Create new variable call DroneWeight
data$DroneWeight <- data$DroneWt/data$TotalDrones


# Get rid of un-needed variables; Note that the "Workers" variable refers to the No. counbted at colony termination, 
#while TotalWorkers referrs to all produced throughout the exp
data2 <- subset(data, 
               select = c(ColonyID, Round, Pollen, Infection,  
                          Eggs, Larvae, Pupae, Workers, TotalWorkers, TotalDrones, TotalQueens,
                          EggWeight,LarvalWeight, PupalWeight, WorkersWeight, QueenWeight, DroneWeight ))

head(data2)
summary(data2)

# Create new variable called "Immatures", which is a combination of Eggs, Larvae, and Pupae
data2$Immatures <- data2$Eggs + data2$Larvae + data2$Pupae

# Create new variable called "ImmaturesWeight", which is a combination of EggWeight, LarvalWeight, and PupalWeight
data2$ImmaturesWeight <- data2$EggWeight + data2$LarvalWeight + data2$PupalWeight


# Change ColonyID to a factor
data2$ColonyID <- as.factor(data2$ColonyID)


##########################################################################################

##########################################################################################

##########################################################################################






########## Workers & WorkersWeight & TotalWorkers


library(lmerTest)
library(emmeans)

library(car)
library(MASS)

hist(data2$Workers) # Kind of normal
hist(data2$TotalWorkers) # Very normal
qqp(data2$Workers, "norm") # Looks good
qqp(data2$TotalWorkers, "norm") # Looks good


Workers.mod1 <- lmerTest::lmer(Workers~ Infection*Pollen + (1|Round), data = data2)
summary(Workers.mod1)
anova(Workers.mod1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# Infection          3806    3806     1 33.085  0.3424 0.5624
# Pollen            19113   19113     1 34.997  1.7194 0.1983
# Infection:Pollen  31746   31746     1 33.335  2.8557 0.1004
Workers.emmeans<- emmeans(Workers.mod1, specs = pairwise ~ Pollen:Infection, type = "response")
Workers.emmeans.df <- Workers.emmeans$emmeans %>% 
  as.data.frame()



hist(data2$WorkersWeight) # Normal
qqp(data2$WorkersWeight, "norm") # Looks ok; Colony 6 outside boundary
WorkersWeight.mod1 <- lmerTest::lmer(WorkersWeight ~ Infection*Pollen + (1|Round), data = data2)
summary(WorkersWeight.mod1)
anova(WorkersWeight.mod1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                   Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
# Infection           1.04    1.04     1    36  0.0007 0.9786
# Pollen           1909.72 1909.72     1    36  1.3337 0.2558
# Infection:Pollen  562.67  562.67     1    36  0.3930 0.5347
WorkersWeight.emmeans<- emmeans(WorkersWeight.mod1, specs = pairwise ~ Pollen:Infection, type = "response")
WorkersWeight.emmeans.df <- WorkersWeight.emmeans$emmeans %>% 
  as.data.frame()



TotalWorkers.mod1 <- lmerTest::lmer(TotalWorkers~ Infection*Pollen + (1|Round), data = data2)
summary(TotalWorkers.mod1)
anova(TotalWorkers.mod1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# Infection           147     147     1 33.097  0.0106 0.9185
# Pollen            12139   12139     1 35.246  0.8804 0.3545
# Infection:Pollen  36163   36163     1 33.390  2.6228 0.1147
TotalWorkers.emmeans<- emmeans(TotalWorkers.mod1, specs = pairwise ~ Pollen:Infection, type = "response")
TotalWorkers.emmeans.df <- TotalWorkers.emmeans$emmeans %>% 
  as.data.frame()


##########################################################################################
########## Drones an Prob. of Drone prod.

hist(data2$TotalDrones) # Right tailed skewed
qqp(data2$TotalDrones, "norm") # Skewed
qqp(data2$TotalDrones, "lnorm") # meh

poisson <- fitdistr(data2$TotalDrones, "Poisson")
qqp(data2$TotalDrones, "pois", lambda = poisson$estimate)
# Not a good fit

nbinom <- fitdistr(data2$TotalDrones, "Negative Binomial")
qqp(data2$TotalDrones, "nbinom", size = nbinom$estimate[[1]],mu = nbinom$estimate[[2]])
#Looks good, Negative binomial it is! Use a glmmTMB model

TotalDrones.mod <- glmmTMB(TotalDrones ~ Infection*Pollen + (1|Round),
                           family = "nbinom2",
                           data = data2)
summary(TotalDrones.mod)
Anova(TotalDrones.mod)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# Response: TotalDrones
#                     Chisq Df Pr(>Chisq)  
# Infection        0.7806  1     0.3769  
# Pollen           0.0612  1     0.8047  
# Infection:Pollen 4.6923  1     0.0303 *

TotalDrones.emmeans<- emmeans(TotalDrones.mod, specs = pairwise ~ Pollen:Infection, type = "response")
TotalDrones.emmeans.df <- TotalDrones.emmeans$emmeans %>% 
  as.data.frame()

# Probabiity of producing at least a single drone
data2$ProbDrone <-  as.numeric(data2$TotalDrones > 0.5)

Drones.Prob.mod <- glmmTMB(ProbDrone ~ Infection*Pollen + (1|Round),
                           family = binomial(link = "logit"),
                           data = data2)
summary(Drones.Prob.mod)
Anova(Drones.Prob.mod)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: ProbDrone
#                      Chisq Df Pr(>Chisq)  
#   Infection        4.5105  1    0.03369 *
#   Pollen           0.0005  1    0.98252  
#   Infection:Pollen 0.5796  1    0.44647  

Drones.Prob.emmeans<- emmeans(Drones.Prob.mod, specs = pairwise ~ Pollen:Infection, type = "response")
Drones.prob.df <- Drones.Prob.emmeans$emmeans %>% 
  as.data.frame()


# Drone weight
hist(data2$DroneWeight)
qqp(data2$DroneWeight, "norm") # Looks good

DroneWeight.mod <- lmer(DroneWeight ~ Infection*Pollen + (1|Round), 
                        data = data2)
summary(DroneWeight.mod )
anova(DroneWeight.mod )
# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# Infection        1047.2  1047.2     1 15.269  0.3998 0.5366
# Pollen            862.8   862.8     1 15.269  0.3294 0.5744
# Infection:Pollen 3452.5  3452.5     1 14.499  1.3181 0.2695

DroneWeight.Prob.emmeans<- emmeans(DroneWeight.mod, specs = pairwise ~ Pollen:Infection, type = "response")
DronesWeight.prob.df <- DroneWeight.Prob.emmeans$emmeans %>% 
  as.data.frame()


##########################################################################################
##########Queens and Prob. of Queen prod.

hist(data2$TotalQueens) # Right tailed skewed
qqp(data2$TotalQueens, "norm") # Skewed
qqp(data2$TotalQueens, "lnorm") # meh


poisson <- fitdistr(data2$TotalQueens, "Poisson")
qqp(data2$TotalQueens, "pois", lambda = poisson$estimate)
# Not a good fit

nbinom <- fitdistr(data2$TotalQueens, "Negative Binomial")
qqp(data2$TotalQueens, "nbinom", size = nbinom$estimate[[1]],mu = nbinom$estimate[[2]])
#Looks good, Negative binomial it is! Use a glmmTMB model

TotalQueens.mod <- glmmTMB(TotalQueens ~ Infection*Pollen + (1|Round),
                           family = "nbinom2",
                           data = data2)
summary(TotalQueens.mod)
Anova(TotalQueens.mod)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: TotalQueens
# Chisq Df Pr(>Chisq)  
# Infection        1.2906  1    0.25594  
# Pollen           1.1322  1    0.28730  
# Infection:Pollen 3.6292  1    0.05677 .

TotalQueens.emmeans<- emmeans(TotalQueens.mod, specs = pairwise ~ Pollen:Infection, type = "response")
TotalQueens.emmeans.df <- TotalQueens.emmeans$emmeans %>% 
  as.data.frame()

# Probabiity of producing at least a single drone
data2$ProbQueen <-  as.numeric(data2$TotalQueens > 1)

Queens.prob.mod <- glmmTMB(ProbQueen ~ Infection*Pollen + (1|Round),
                           family = binomial(link = "logit"),
                           data = data2)
summary(Queens.prob.mod)
Anova(Queens.prob.mod)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: ProbQueen
# Chisq Df Pr(>Chisq)  
# Infection        0.0148  1     0.9031  
# Pollen           0.0960  1     0.7567  
# Infection:Pollen 6.4178  1     0.0113 * 

Queens.Prob.emmeans<- emmeans(Queens.prob.mod, specs = pairwise ~ Pollen:Infection, type = "response")
Queens.prob.df <- Queens.Prob.emmeans$emmeans %>% 
  as.data.frame()


#Queen weight
hist(data2$QueenWeight)
qqp(data2$QueenWeight, "norm") # Looks good

QueenWeight.mod <- lmer(QueenWeight ~ Infection*Pollen + (1|Round), 
                        data = data2)
summary(QueenWeight.mod )
anova(QueenWeight.mod )
# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
# Infection        104622  104622     1 29.598  2.4368 0.12914  
# Pollen             3946    3946     1 31.826  0.0919 0.76374  
# Infection:Pollen 138969  138969     1 29.437  3.2368 0.08226 .

QueenWeight.Prob.emmeans<- emmeans(QueenWeight.mod, specs = pairwise ~ Pollen:Infection, type = "response")
QueenWeight.prob.df <- QueenWeight.Prob.emmeans$emmeans %>% 
  as.data.frame()


##########################################################################################
########## Immatures

hist(data2$Immatures) # Binomial???
qqp(data2$Immatures, "norm") # Looks ok
qqp(data2$Immatures, "lnorm") # Looks ok as well...

Immatures.mod1 <- lmerTest::lmer(Immatures~ Infection*Pollen + (1|Round), data = data2)
summary(Immatures.mod1)
anova(Immatures.mod1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                   Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# Infection          914.8   914.8     1 33.038  0.0659 0.7990
# Pollen            1078.1  1078.1     1 33.980  0.0776 0.7822
# Infection:Pollen 29376.7 29376.7     1 33.149  2.1153 0.1552
Immatures.emmeans<- emmeans(Immatures.mod1, specs = pairwise ~ Pollen:Infection, type = "response")
Immatures.emmeans.df <- Immatures.emmeans$emmeans %>% 
  as.data.frame()

#Does Immature No. corr with ImmaturesWeight?
plot(data2$ImmaturesWeight, data2$Immatures)
cor.test(data2$ImmaturesWeight, data2$Immatures, method = "pearson", use = "complete.obs")
# Pearson's product-moment correlation
# 
# data:  data2$ImmaturesWeight and data2$Immatures
# t = -2.049, df = 31, p-value = 0.049
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.615680383 -0.002340785
# sample estimates:
# cor 
# -0.3453722 

hist(data2$ImmaturesWeight) # Normal; slight right tail
qqp(data2$ImmaturesWeight, "norm") # Looks ok, little wiggle
qqp(data2$ImmaturesWeight, "lnorm") # Looks ok 

ImmaturesWeight.mod1 <- lmerTest::lmer(log(ImmaturesWeight)~ Infection*Pollen + (1|Round), data = data2)
summary(ImmaturesWeight.mod1)
anova(ImmaturesWeight.mod1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                   Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
# Infection        0.19135 0.19135     1 26.557  0.5120 0.48050  
# Pollen           0.89733 0.89733     1 28.098  2.4011 0.13244  
# Infection:Pollen 1.85628 1.85628     1 27.003  4.9671 0.03435 *

ImmaturesWeight.emmeans<- emmeans(ImmaturesWeight.mod1, specs = pairwise ~ Pollen:Infection, type = "response")
ImmaturesWeight.emmeans.df <- ImmaturesWeight.emmeans$emmeans %>% 
  as.data.frame()


#################################################################################################

##########################################################################################
########## Worker Size (wings) 
WorkerWing.df <- read.csv("WorkerWings.csv", header = TRUE)
head(WorkerWing.df)
str(WorkerWing.df)
summary(WorkerWing.df)

WorkerWing.df$SampleWeek <- as.factor(WorkerWing.df$SampleWeek)
WorkerWing.df$ColonyID <- as.factor(WorkerWing.df$ColonyID)

qqp(WorkerWing.df$WingSize, "norm") #Looks great 

WorkerSize.mod <- lmerTest::lmer(WingSize ~ Infection*Pollen*SampleWeek + (1|Round) + (1|ColonyID), 
                           data = WorkerWing.df)
summary(WorkerSize.mod)
anova(WorkerSize.mod) # Singularity issues; run a glmmTMB instead


WorkerSize.mod2 <- glmmTMB(WingSize ~ Infection*Pollen*SampleWeek + (1|Round) + (1|ColonyID) + ar1(0 + SampleWeek |ColonyID), 
                                 data = WorkerWing.df)
summary(WorkerSize.mod2)
Anova(WorkerSize.mod2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: WingSize
# Chisq Df Pr(>Chisq)
# Infection                    0.7060  1     0.4008
# Pollen                       1.4669  1     0.2258
# SampleWeek                  11.2017  8     0.1905
# Infection:Pollen             0.4700  1     0.4930
# Infection:SampleWeek         9.9009  8     0.2721
# Pollen:SampleWeek            7.9172  8     0.4416
# Infection:Pollen:SampleWeek  9.6896  8     0.2875

drop1(WorkerSize.mod2, test = "Chisq")

WorkerSize.mod3 <- glmmTMB(WingSize ~ Infection*Pollen + SampleWeek + (1|Round) + (1|ColonyID) + ar1(0 + SampleWeek |ColonyID), 
                           data = WorkerWing.df)
summary(WorkerSize.mod3)
Anova(WorkerSize.mod3)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: WingSize
# Chisq Df Pr(>Chisq)
# Infection         0.7171  1     0.3971
# Pollen            1.3916  1     0.2381
# SampleWeek       10.3247  8     0.2430
# Infection:Pollen  0.4742  1     0.4910

WorkerSize.emmeans<- emmeans(WorkerSize.mod3, specs = pairwise ~ Pollen*Infection, type = "response")
WorkerSize.emmeans.df <- WorkerSize.emmeans$emmeans %>% 
  as.data.frame()


##########################################################################################
########## Queen Size (wings) 

QueenWing.df <- read.csv("QueenWings.csv", header = TRUE)
head(QueenWing.df)
qqp(QueenWing.df$WingSize, "norm") #Looks meh
qqp(QueenWing.df$WingSize, "lnorm") #eeewwwww

QueenWing.df <- na.omit(QueenWing.df)

QueenSize.mod <- lmerTest::lmer(WingSize ~ Infection*Pollen + (1|ColonyID), 
                                 data = QueenWing.df)
summary(QueenSize.mod)
anova(QueenSize.mod)
# Type III Analysis of Variance Table with Satterthwaite's method
#                     Sum Sq   Mean Sq NumDF  DenDF F value Pr(>F)
# Infection        0.0074860 0.0074860     1 13.726  0.3282 0.5760
# Pollen           0.0025217 0.0025217     1 13.726  0.1106 0.7445
# Infection:Pollen 0.0191632 0.0191632     1 13.726  0.8402 0.3752

QueenSize.emmeans<- emmeans(QueenSize.mod, specs = pairwise ~ Pollen*Infection, type = "response")
QueenSize.emmeans.df <- QueenSize.emmeans$emmeans %>% 
  as.data.frame()

###############################################################################################
########## Drone Size (wings)
DroneWing.df <- read.csv("DroneWings.csv", header = TRUE)
head(DroneWing.df)
qqp(DroneWing.df$WingSize, "norm") #Looks great 
#need to remove NA's

DroneWing.df <- na.omit(DroneWing.df)

DroneSize.mod <- lmerTest::lmer(WingSize ~ Infection*Pollen + (1|ColonyID), 
                                data = DroneWing.df)
summary(DroneSize.mod)
anova(DroneSize.mod)
# Type III Analysis of Variance Table with Satterthwaite's method
#                     Sum Sq   Mean Sq NumDF  DenDF F value Pr(>F)
# Infection        0.0235746 0.0235746     1 17.593  0.6183 0.4422
# Pollen           0.0007151 0.0007151     1 17.593  0.0188 0.8926
# Infection:Pollen 0.0062453 0.0062453     1 17.593  0.1638 0.6906
DroneSize.emmeans<- emmeans(DroneSize.mod, specs = pairwise ~ Pollen*Infection, type = "response")
DroneSize.emmeans.df <- DroneSize.emmeans$emmeans %>% 
  as.data.frame()



##############################################################################################
########## Colony Weight Gain (Repeated measures)

data.weightgain <- read.csv("Colony Weight gain.csv", header = TRUE)
str(data.weightgain)
data.weightgain$ColonyID <- as.factor(data.weightgain$ColonyID)
data.weightgain$Week <- as.factor(data.weightgain$Week)
names(data.weightgain)

#ar1(0 + Week |ColonyID)

WeightGain.mod <- glmmTMB(Weight ~ Infection*Pollen*Week + (1|Round) + (1|ColonyID), 
                           data = data.weightgain)
summary(WeightGain.mod)
Anova(WeightGain.mod)
# Chisq Df Pr(>Chisq)    
# Infection                0.3606  1    0.54816    
# Pollen                   5.2153  1    0.02239 *  
#   Week                  1169.5922  7    < 2e-16 ***
#   Infection:Pollen         0.0005  1    0.98286    
# Infection:Week           7.5799  7    0.37109    
# Pollen:Week             14.6151  7    0.04126 *  
#   Infection:Pollen:Week    2.6902  7    0.91211 

WeightGain.mod2 <- glmmTMB(Weight ~ Infection*Pollen + Pollen*Week + (1|Round) + (1|ColonyID), 
                          data = data.weightgain)
summary(WeightGain.mod2)
Anova(WeightGain.mod2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Weight
# Chisq Df Pr(>Chisq)    
# Infection           0.3607  1    0.54814    
# Pollen              5.2153  1    0.02239 *  
# Week             1128.2137  7    < 2e-16 ***
# Infection:Pollen    0.0005  1    0.98285    
# Pollen:Week        14.0979  7    0.04947 *  

WeightGain.mod3 <- glmmTMB(Weight ~ Infection + Pollen*Week + (1|Round) + (1|ColonyID), 
                           data = data.weightgain)
summary(WeightGain.mod3)
Anova(WeightGain.mod3)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Weight
# Chisq Df Pr(>Chisq)    
# Infection      0.3606  1    0.54816    
# Pollen         5.2152  1    0.02239 *  
# Week        1128.2129  7    < 2e-16 ***
# Pollen:Week   14.0979  7    0.04947 *  

WeightGain.mod4 <- glmmTMB(Weight ~ Infection*Week + Pollen*Week + (1|Round) + (1|ColonyID), 
                           data = data.weightgain)
summary(WeightGain.mod4)
Anova(WeightGain.mod4)

WeightGain.emmeans<- emmeans(WeightGain.mod, specs =  pairwise ~ Pollen | Week, type = "response")
WeightGain.emmeans.df <- WeightGain.emmeans$emmeans %>% 
  as.data.frame()

WeightGain.emmeans.Inf<- emmeans(WeightGain.mod, specs =  pairwise ~ Infection | Week, type = "response")


WeightGain.emmeans2<- emmeans(WeightGain.mod3, specs =  pairwise ~ Pollen*Infection, type = "response")
WeightGain.emmeans2.df <- WeightGain.emmeans2$emmeans %>% 
  as.data.frame()



WeightGain.mod4 <- glmmTMB(Weight ~ Infection + Pollen*Week + (1|ColonyID), 
                           data = data.weightgain)
summary(WeightGain.mod4)
Anova(WeightGain.mod4)

library(bbmle)
AICtab(WeightGain.mod3, WeightGain.mod4)
# dAIC df
# WeightGain.mod4  0   19
# WeightGain.mod3  2   20


# 
# $contrasts
# Week = 2:
#   contrast estimate   SE  df t.ratio p.value
# S - W       -0.72 9.99 300 -0.072  0.9426 
# 
# Week = 4:
#   contrast estimate   SE  df t.ratio p.value
# S - W       -6.32 9.99 300 -0.633  0.5273 
# 
# Week = 5:
#   contrast estimate   SE  df t.ratio p.value
# S - W       -8.23 9.99 300 -0.823  0.4109 
# 
# Week = 6:
#   contrast estimate   SE  df t.ratio p.value
# S - W       -6.72 9.99 300 -0.672  0.5018 
# 
# Week = 7:
#   contrast estimate   SE  df t.ratio p.value
# S - W      -24.32 9.99 300 -2.434  0.0155 
# 
# Week = 8:
#   contrast estimate   SE  df t.ratio p.value
# S - W      -16.09 9.99 300 -1.610  0.1084 
# 
# Week = 9:
#   contrast estimate   SE  df t.ratio p.value
# S - W      -25.34 9.99 300 -2.536  0.0117 
# 
# Week = 10:
#   contrast estimate   SE  df t.ratio p.value
# S - W      -32.84 9.99 300 -3.286  0.0011 

library(ggplot2)

# Weight Gain Plot
WeightGain.emmeans.df

WeightGain_plot <- ggplot(WeightGain.emmeans.df, aes(x=Week, y=emmean, group = Pollen)) +
  geom_line(aes(linetype = Pollen, color = Pollen), size = 1.3)+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE),linetype = "solid", size=1, width=0.2)+
  geom_point(aes(shape = Pollen, color = Pollen), size = 6)+
  scale_color_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"), values = Colors)+
  scale_shape_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = c(15,17))+
  scale_linetype_manual(name = "Pollen", labels = c(S = "Sunflower", W = "Wildflower"),values = Lines) +  
  ylab("Colony weight (grams)") +
  xlab("Time (wks)") + ylim(c(600,850))+
  theme_classic() +
  theme(legend.position = "top")+
  theme(text = element_text(face = "bold", size = 20, colour = "black")) + 
  theme(legend.key.size = unit(4, "line"))

ggsave("Colony Weight Gain.pdf", height = 6, width =10)



Workers.emmeans.df <- Workers.emmeans.df %>% 
  mutate(Measure = "No. of Workers")

WorkersWeight.emmeans.df <- WorkersWeight.emmeans.df %>% 
  mutate(Measure = "Avg. Worker weight (mg)")


TotalDrones.emmeans.df <- TotalDrones.emmeans.df %>% 
  #dplyr::rename(emmean = response) %>% 
  mutate(Measure = "No. of Drones")

Drones.prob.df <- Drones.prob.df %>% 
  #dplyr::rename(emmean = prob) %>% 
  mutate(Measure = "Prob. of Drones")

DronesWeight.prob.df <- DronesWeight.prob.df %>% 
  mutate(Measure = "Avg. drone weight (mg)")

TotalQueens.emmeans.df <- TotalQueens.emmeans.df %>% 
  #dplyr::rename(emmean = response) %>% 
  mutate(Measure = "No. of Queens")

Queens.prob.df <- Queens.prob.df %>% 
  #dplyr::rename(emmean = prob) %>% 
  mutate(Measure = "Prob. of Queens")

QueenWeight.prob.df <- QueenWeight.prob.df %>% 
  mutate(Measure = "Avg. Queen weight (mg)")

Immatures.emmeans.df <- Immatures.emmeans.df %>% 
  mutate(Measure = "No. of Immatures")

ImmaturesWeight.emmeans.df <- ImmaturesWeight.emmeans.df %>% 
  #dplyr::rename(emmean = response) %>% 
  mutate(Measure = "Avg. Immature weight (mg)")

WorkerSize.emmeans.df <- WorkerSize.emmeans.df %>% 
  mutate(Measure = "Avg. Worker size (mm)")

QueenSize.emmeans.df <- QueenSize.emmeans.df %>% 
  mutate(Measure = "Avg. Queen size (mm)")

DroneSize.emmeans.df <- DroneSize.emmeans.df %>% 
  mutate(Measure = "Avg. Drone size (mm)")

WeightGain.emmeans2.df <- WeightGain.emmeans2.df %>% 
  mutate(Measure = "Avg. Colony weight gain (g)") # Averaged over weeks

Summary.df <- bind_rows(Workers.emmeans.df, WorkersWeight.emmeans.df,
                        TotalDrones.emmeans.df,
                        Drones.prob.df, DronesWeight.prob.df,
                        TotalQueens.emmeans.df, Queens.prob.df,
                        QueenWeight.prob.df, Immatures.emmeans.df,
                        ImmaturesWeight.emmeans.df, WorkerSize.emmeans.df,
                        QueenSize.emmeans.df, DroneSize.emmeans.df, 
                        WeightGain.emmeans2.df) %>% 
  subset(select = c(Pollen, Infection, emmean, SE, Measure)) %>% 
  mutate_if(is.numeric, round, digits = 2)


Summary.df

library(tidyr)

#spread() makes “long” data wider

Summary.wide.df <- Summary.df %>% 
  pivot_wider(id_cols = Measure,
              names_from = c(Pollen, Infection),
              values_from = c(emmean, SE))




Summary.wide.df <- as.data.frame(Summary.wide.df)

write.csv(Summary.wide.df,"Summary Stats.csv", row.names = FALSE)












sessionInfo()
# R version 3.5.2 (2018-12-20)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.6
# 
# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lmerTest_3.1-1    lme4_1.1-21       Matrix_1.2-18     cowplot_1.0.0     plyr_1.8.6        gridExtra_2.3     reshape2_1.4.3    xtable_1.8-4     
# [9] dotwhisker_0.5.0  broom.mixed_0.2.4 effects_4.1-4     bbmle_1.0.23.1    multcomp_1.4-12   TH.data_1.0-10    survival_3.1-11   mvtnorm_1.1-0    
# [17] emmeans_1.4.5     glmmTMB_1.0.0     MASS_7.3-51.5     bnpa_0.3.0        forcats_0.5.0     stringr_1.4.0     purrr_0.3.3       readr_1.3.1      
# [25] tidyr_1.0.2       tibble_3.0.0      tidyverse_1.3.0   mgcv_1.8-31       nlme_3.1-145      ggplot2_3.3.0     heplots_1.3-5     car_3.0-6        
# [33] carData_3.0-3     dplyr_0.8.5      
# 
# loaded via a namespace (and not attached):
#   [1] minqa_1.2.4         colorspace_1.4-1    ellipsis_0.3.0      rio_0.5.16          estimability_1.3    ggstance_0.3.3      fs_1.3.2           
# [8] rstudioapi_0.11     farver_2.0.3        fansi_0.4.1         lubridate_1.7.4     xml2_1.2.4          codetools_0.2-16    splines_3.5.2      
# [15] knitr_1.28          jsonlite_1.6.1      nloptr_1.2.1        pbkrtest_0.4-7      broom_0.5.5         dbplyr_1.4.2        compiler_3.5.2     
# [22] httr_1.4.1          backports_1.1.6     assertthat_0.2.1    survey_3.37         cli_2.0.2           tools_3.5.2         coda_0.19-3        
# [29] gtable_0.3.0        glue_1.4.0          Rcpp_1.0.4          cellranger_1.1.0    vctrs_0.2.4         xfun_0.12           openxlsx_4.1.4     
# [36] rvest_0.3.5         lifecycle_0.2.0     goftest_1.2-2       zoo_1.8-7           scales_1.1.0        hms_0.5.3           parallel_3.5.2     
# [43] sandwich_2.5-1      TMB_1.7.16          curl_4.3            bdsmatrix_1.3-4     stringi_1.4.6       nortest_1.0-4       boot_1.3-24        
# [50] zip_2.0.4           rlang_0.4.5         pkgconfig_2.0.3     lattice_0.20-40     labeling_0.3        tidyselect_1.0.0    magrittr_1.5       
# [57] R6_2.4.1            generics_0.0.2      multcompView_0.1-8  DBI_1.1.0           pillar_1.4.3        haven_2.2.0         foreign_0.8-76     
# [64] withr_2.1.2         abind_1.4-5         nnet_7.3-13         modelr_0.1.6        crayon_1.3.4        utf8_1.1.4          grid_3.5.2         
# [71] readxl_1.3.1        data.table_1.12.8   reprex_0.3.0        digest_0.6.25       numDeriv_2016.8-1.1 munsell_0.5.0       mitools_2.4 
# 
