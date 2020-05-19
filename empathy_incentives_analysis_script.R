#####################################
#Analysis script
#Financial incentives facilitate the neural computation of prosocial decisions - but only if empathy is low 
#Iotzov, Vassil; Saulin, Anne; Kaiser, Jochen; Han, Shihui; Hein, Grit
#####################################

#usefull packeges
require(MuMIn)
require(lme4)
require(car)
require(tidyverse)

#set working directory
setwd("...")

#load data frame
load("empathy_incentives_df.rda")
#conditions: 0 = Empathy-bonus, 1 = Empathy-alone

#####################################
#####################################

############ normal distribution check ############
shapiro.test(empathy_incentives_df$empathy_ratings)
hist(empathy_incentives_df$empathy_ratings)
shapiro.test(empathy_incentives_df$empathy_ratings_ln)
hist(empathy_incentives_df$empathy_ratings_ln)
shapiro.test(empathy_incentives_df$prosocial_decisions)
hist(empathy_incentives_df$prosocial_decisions)
shapiro.test(empathy_incentives_df$v)
shapiro.test(empathy_incentives_df$a)
shapiro.test(empathy_incentives_df$z)
shapiro.test(empathy_incentives_df$ec)
hist(empathy_incentives_df$ec)
###################################################

#Empathy was induced with comparable strength in both conditions
model.ind_ratio<-lmer(empathy_ratings_ln_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.ind_ratio)
Anova(model.ind_ratio)
r.squaredGLMM(model.ind_ratio)

cor.test(empathy_incentives_df$empathy_ratings_ln[1:31],empathy_incentives_df$ec[1:31], alternative = "greater")

#The financial incentive increased the frequency of prosocial decisions, in particular if empathy was low
model.prosoc_decisions<-lmer(prosocial_decisions_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.prosoc_decisions)
Anova(model.prosoc_decisions)
r.squaredGLMM(model.prosoc_decisions)

t.test(empathy_incentives_df$prosocial_decisions_percent_ratio[1:31], mu = 0, alternative = "two.sided")

model.rts<-lmer(rt_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.rts)
Anova(model.rts)
r.squaredGLMM(model.rts)

model.rt_empathy<-lmer(rt_scaled ~ condition*empathy_ratings_empathy_cond_ln + (1|id), data = empathy_incentives_df)
summary(model.rt_empathy)
Anova(model.rt_empathy)
r.squaredGLMM(model.rt_empathy)

model.prosoc_decision_ratio<-lm(scale(prosocial_decisions_percent_ratio[1:31]) ~ empathy_ratings_ln[1:31] , data = empathy_incentives_df)
summary(model.prosoc_decision_ratio)

#The financial incentive increased the speed of information accumulation, but not the initial decision preference
mean(empathy_incentives_df$v[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$v[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$v[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$v[empathy_incentives_df$condition==1])/sqrt(31)

model.hddm<-lmer(v_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.hddm)
Anova(model.hddm)
r.squaredGLMM(model.hddm)


mean(empathy_incentives_df$z[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$z[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$z[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$z[empathy_incentives_df$condition==1])/sqrt(31)

model.hddm<-lmer(z_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.hddm)
Anova(model.hddm)
r.squaredGLMM(model.hddm)


mean(empathy_incentives_df$a[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$a[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$a[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$a[empathy_incentives_df$condition==1])/sqrt(31)

model.hddm<-lmer(a_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.hddm)
Anova(model.hddm)
r.squaredGLMM(model.hddm)

#The anterior insula tracks the incentive-related facilitation of prosocial decisions and individual differences in empathy
model.betas_mean<-lm(formula=ai_betas_average_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31],data = empathy_incentives_df)
summary(model.betas_mean)

#The financial incentive has a differential effect on anterior insular activation in high and low empathic individuals
model.betas_mean<-lmer(formula=ai_betas_average_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)

#Unpacking this three-way interaction,we tested the relationship between v, induction_ratio and impression separately in emp and emp_bonus
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31]*v_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62]*v_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_emp)

#The robustness of the differential effects in the empathy-bonus and the empathy-alone conditions (meta analysis)
model.betas_mean<-lmer(formula=ai_betas_roi_meta_analysis_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean, type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)
