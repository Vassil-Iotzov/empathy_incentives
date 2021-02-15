#####################################
#Analysis script
#Financial incentives facilitate the neural computation of prosocial decisions stronger in low empathic individuals
#Iotzov, Vassil; Saulin, Anne; Kaiser, Jochen; Han, Shihui; Hein, Grit
#####################################

#required packeges
require(MuMIn)
require(lme4)
require(car)
require(tidyverse)

#set working directory
setwd("...")

#load data frame
load("empathy_incentives_df.rda")
#conditions: 0 = Empathy-bonus, 1 = Empathy-alone
#load baseline data from
load("baseline.rda")
#load additional HDDM regression model parameter
load('hddm_regression_model_parameter_df.rda')

#####################################
#####################################

############ normal distribution check ############
shapiro.test(empathy_incentives_df$empathy_ratings)
hist(empathy_incentives_df$empathy_ratings)
shapiro.test(empathy_incentives_df$empathy_ratings_ln)
hist(empathy_incentives_df$empathy_ratings_ln)
shapiro.test(empathy_incentives_df$v)
shapiro.test(empathy_incentives_df$a)
shapiro.test(empathy_incentives_df$z)
###################################################

#Empathy was induced with comparable strength in both conditions
model.ind_ratio<-lmer(empathy_ratings_ln_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.ind_ratio)
Anova(model.ind_ratio)
r.squaredGLMM(model.ind_ratio)

#The average of the individual empathy ratings in both conditions correlated significantly with the empathic concern scale (EC)
cor.test(empathy_incentives_df$empathy_ratings_ln[1:31],empathy_incentives_df$ec[1:31], alternative = "greater")

#The individual empathy ratings did not correlate with the personal distress (PD) scale
cor.test(empathy_incentives_df$empathy_ratings_ln[1:31],empathy_incentives_df$pd[1:31])

#no no significant difference in RTs
#prosocial decisions:
model.rts<-lmer(rt_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.rts)
Anova(model.rts)

#selfish decisions
model.rts<-lmer(rt_ego_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.rts)
Anova(model.rts)

#all decisions
model.rts<-lmer(rt_all_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.rts)
Anova(model.rts)

#The financial incentive increased the frequency of prosocial decisions
model.prosoc_decisions<-lmer(prosocial_decisions_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.prosoc_decisions)
Anova(model.prosoc_decisions)
r.squaredGLMM(model.prosoc_decisions)

#Percent change in prosocial decisions in the empathy-bonus condition relative to the empathy-alone condition
t.test(empathy_incentives_df$prosocial_decisions_percent_ratio[1:31], mu = 0, alternative = "two.sided")

#Significantly more prosocial decisions in the empathy-alone condition compared to the baseline condition from Hein, G., Morishima, Y., Leiberg, S., Sul, S. & Fehr, E. (2016) 'The brain's functional network architecture reveals human motives', Science, 80(351), pp. 1074-1078.
mean(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1])/sqrt(31)
mean(baseline_data$percentage)
sd(baseline_data$percentage)/sqrt(34)
t.test(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1],baseline_data$percentage)

#Higher empathy ratings predicted faster prosocial decision
model.rt_empathy<-lmer(rt_scaled ~ condition*scale(empathy_ratings_empathy_cond_ln) + (1|id), data = empathy_incentives_df)
summary(model.rt_empathy)
Anova(model.rt_empathy)
r.squaredGLMM(model.rt_empathy)

#The lower an individual's empathy ratings, the stronger the increase in the frequency of prosocial decisions in the empathy-bonus condition relative to the empathy-alone condition
model.prosoc_decision_ratio<-lm(scale(prosocial_decisions_percent_ratio[1:31]) ~ scale(empathy_ratings_empathy_cond_ln[1:31]) , data = empathy_incentives_df)
summary(model.prosoc_decision_ratio)

#The financial incentive increased the speed of information accumulation, but not the initial decision preference
#v
mean(empathy_incentives_df$v[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$v[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$v[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$v[empathy_incentives_df$condition==1])/sqrt(31)

#z
mean(empathy_incentives_df$z[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$z[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$z[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$z[empathy_incentives_df$condition==1])/sqrt(31)

#a
mean(empathy_incentives_df$a[empathy_incentives_df$condition==0])
sd(empathy_incentives_df$a[empathy_incentives_df$condition==0])/sqrt(31)
mean(empathy_incentives_df$a[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$a[empathy_incentives_df$condition==1])/sqrt(31)


#same analysis with HDDM Regression model
mean(hddm_reg_df$hddm_reg_v[hddm_reg_df$condition==0])
sd(hddm_reg_df$hddm_reg_v[hddm_reg_df$condition==0])/sqrt(31)
mean(hddm_reg_df$hddm_reg_v[hddm_reg_df$condition==1])
sd(hddm_reg_df$hddm_reg_v[hddm_reg_df$condition==1])/sqrt(31)

mean(hddm_reg_df$hddm_reg_z[hddm_reg_df$condition==0])
sd(hddm_reg_df$hddm_reg_z[hddm_reg_df$condition==0])/sqrt(31)
mean(hddm_reg_df$hddm_reg_z[hddm_reg_df$condition==1])
sd(hddm_reg_df$hddm_reg_z[hddm_reg_df$condition==1])/sqrt(31)

mean(hddm_reg_df$hddm_reg_a[hddm_reg_df$condition==0])
sd(hddm_reg_df$hddm_reg_a[hddm_reg_df$condition==0])/sqrt(31)
mean(hddm_reg_df$hddm_reg_a[hddm_reg_df$condition==1])
sd(hddm_reg_df$hddm_reg_a[hddm_reg_df$condition==1])/sqrt(31)

#The anterior insula tracks the incentive-related facilitation of prosocial decisions and individual differences in empathy
model.betas_mean<-lm(formula=ai_betas_average_average_sclaed[1:31] ~ empathy_ratings_ln_scaled[1:31],data = empathy_incentives_df)
summary(model.betas_mean)

#The financial incentive has a differential effect on anterior insular activation in high and low empathic individuals
model.betas_mean<-lmer(formula=ai_betas_average_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)

#same analysis with the beta estimates extracted from the pallidum, right lingual gyrus and left inferior lingual gyrus
#pallidum:
model.betas_pallidum<-lmer(formula=betas_pallidum_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_pallidum)
Anova(model.betas_pallidum,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_pallidum)

#right lingual gyrus
model.betas_rlg<-lmer(formula=betas_right_lingual_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_rlg)
Anova(model.betas_rlg,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_rlg)

#left inferior lingual gyrus
model.betas_llg<-lmer(formula=betas_left_lingual_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_llg)
Anova(model.betas_llg,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_llg)

#To unpack the significant condition x v-parameter x empathy rating interaction in AI, we tested the relationship between the v-parameter and the empathy ratings separately in the empathy-alone and the empathy-bonus condition
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31]*v_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)
model.betas_mean_empego<-lm(formula=ai_betas_average_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62]*v_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_empego)


model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ v_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)

model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)

model.v<-lm(formula=v_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31], data = empathy_incentives_df)
summary(model.v)

model.betas_mean_empego<-lm(formula=ai_betas_average_scaled[32:62] ~ v_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_empego)

model.betas_mean_empego<-lm(formula=ai_betas_average_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_empego)

model.v<-lm(formula=v_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62], data = empathy_incentives_df)
summary(model.v)

#Linear mixed model with the beta-estimates from an independent region of interest in the AI (defined based on the peak coordinates reported in a recent meta-analysis on empathy of pain studies (Jauniaux et al., 2019)
model.betas_mean<-lmer(formula=ai_betas_roi_meta_analysis_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)
