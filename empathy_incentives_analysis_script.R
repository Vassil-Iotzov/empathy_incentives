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
require(ggeffects)

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
#The impression ratings were comparable between confederates
model.impression<-lmer(impression ~ condition + (1|id), data = empathy_incentives_df)
summary(model.impression)
Anova(model.impression)
##################################################
#Empathy was induced with comparable strength in both conditions
model.ind_ratio<-lmer(empathy_ratings_ln_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.ind_ratio)
Anova(model.ind_ratio)
r.squaredGLMM(model.ind_ratio)

#The average of the individual empathy ratings in both conditions correlated significantly with the empathic concern scale (EC)
cor.test(empathy_incentives_df$empathy_ratings_ln[1:31],empathy_incentives_df$ec[1:31], alternative = "greater")

#The individual empathy ratings did not correlate with the personal distress (PD) scale
cor.test(empathy_incentives_df$empathy_ratings_ln[1:31],empathy_incentives_df$pd[1:31])

#EC&PD Regression
model.ec_pd<-lm(scale(empathy_ratings_ln[1:31]) ~ scale(ec[1:31]) + scale(pd[1:31]), data = empathy_incentives_df)
summary(model.ec_pd)

#The financial incentive increased the frequency of prosocial decisions
model.prosoc_decisions<-lmer(prosocial_decisions_scaled ~ condition + (1|id), data = empathy_incentives_df)
summary(model.prosoc_decisions)
Anova(model.prosoc_decisions)
r.squaredGLMM(model.prosoc_decisions)

#Significantly more prosocial decisions in the empathy-alone condition compared to the baseline condition from Hein, G., Morishima, Y., Leiberg, S., Sul, S. & Fehr, E. (2016) 'The brain's functional network architecture reveals human motives', Science, 80(351), pp. 1074-1078.
mean(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1])
sd(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1])/sqrt(31)
mean(baseline_data$percentage)
sd(baseline_data$percentage)/sqrt(34)
t.test(empathy_incentives_df$prosocial_decisions_percent[empathy_incentives_df$condition==1],baseline_data$percentage)

#not included: Percent change in prosocial decisions in the empathy-bonus condition relative to the empathy-alone condition
#t.test(empathy_incentives_df$prosocial_decisions_percent_ratio[1:31], mu = 0, alternative = "two.sided")

#empathy ratings related to the probability of prosocial decisions
model.prosoc_decis<-glmer(response ~ condition * empathy_ratings + (1 | subj_idx), data = trial_by_trial, family = binomial)
summary(model.prosoc_decis)
Anova(model.prosoc_decis)
r.squaredGLMM(model.prosoc_decis)

#The lower an individual's empathy ratings, the stronger the increase in the frequency of prosocial decisions in the empathy-bonus condition relative to the empathy-alone condition 
model.prosoc_decision_dif<-lm(scale(prosocial_decisions_percent[32:62]-prosocial_decisions_percent[1:31]) ~ scale(empathy_ratings_empathy_cond_ln[1:31]) , data = empathy_incentives_df)
summary(model.prosoc_decision_dif)

#no significant difference in RTs
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

#Higher empathy ratings predicted faster prosocial decision
model.rt_empathy<-lmer(rt_scaled ~ condition*scale(empathy_ratings_empathy_cond_ln) + (1|id), data = empathy_incentives_df)
summary(model.rt_empathy)
Anova(model.rt_empathy)
r.squaredGLMM(model.rt_empathy)

##### For HDDM Results please see HDDM Files ##################

#not included: The anterior insula tracks the incentive-related facilitation of prosocial decisions and individual differences in empathy
#model.betas_mean<-lm(formula=ai_betas_average_average_sclaed[1:31] ~ empathy_ratings_ln_scaled[1:31],data = empathy_incentives_df)
#summary(model.betas_mean)

#The financial incentive has a differential effect on anterior insular activation in high and low empathic individuals
model.betas_mean<-lmer(formula=ai_betas_average_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)

#same analysis with the beta estimates extracted from right lingual gyrus
#right lingual gyrus
model.betas_rlg<-lmer(formula=betas_right_lingual_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_rlg)
Anova(model.betas_rlg,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_rlg)

#To unpack the significant condition x v-parameter x empathy rating interaction in AI, we tested the relationship between the v-parameter and the empathy ratings separately in the empathy-alone and the empathy-bonus condition
empathy_incentives_empego_df<-empathy_incentives_df[32:62,]
model.betas_mean_empego<-lm(formula=ai_betas_average_scaled ~ empathy_ratings_ln_scaled*v_scaled, data = empathy_incentives_empego_df)
summary(model.betas_mean_empego)
empathy_incentives_emp_df<-empathy_incentives_df[1:31,]
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled ~ empathy_ratings_ln_scaled*v_scaled, data = empathy_incentives_emp_df)
summary(model.betas_mean_emp)

#significant positive relationship between empathy ratings and AI beta estimates (one-sided)
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)

#significant positive relationship between empathy ratings and drift rate (one-sided)
model.v<-lm(formula=v_scaled[1:31] ~ empathy_ratings_ln_scaled[1:31], data = empathy_incentives_df)
summary(model.v)

#significant positive relationship between v-parameter and AI beta estimates (one-sided)
model.betas_mean_emp<-lm(formula=ai_betas_average_scaled[1:31] ~ v_scaled[1:31], data = empathy_incentives_df)
summary(model.betas_mean_emp)

#significant positive relationship between v-parameter and AI beta estimates (one-sided)
model.betas_mean_empego<-lm(formula=ai_betas_average_scaled[32:62] ~ v_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_empego)
 
#no significant relationship between empathy ratings and AI beta estimates (one-sided)
model.betas_mean_empego<-lm(formula=ai_betas_average_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62], data = empathy_incentives_df)
summary(model.betas_mean_empego)

#no significant relationship between empathy ratings and drift rate (one-sided)
model.v<-lm(formula=v_scaled[32:62] ~ empathy_ratings_ln_scaled[32:62], data = empathy_incentives_df)
summary(model.v)

#Linear mixed model with the beta-estimates from an independent region of interest in the AI (defined based on the peak coordinates reported in a recent meta-analysis on empathy of pain studies (Jauniaux et al., 2019)
model.betas_mean<-lmer(formula=ai_betas_roi_meta_analysis_scaled ~ empathy_ratings_ln_scaled*v_scaled*condition +  (1|id), data = empathy_incentives_df)
summary(model.betas_mean)
Anova(model.betas_mean,   type = 3) #Type 3 because of sig. interaction effects.
r.squaredGLMM(model.betas_mean)
