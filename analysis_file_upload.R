#should include 'path_manipulated_data' and 'path_raw_data'
source("path.R")

library(foreign)
library(tidyverse)
library(lubridate)
library(geepack)
library(gee)
library(lme4)
library(psych)
library(MRTAnalysis)
library(tvem)
library(msm)

options(scipen = 16)

dat_mars_app_engage <- readRDS(file = file.path(path_manipulated_data, "dat_mars_app_engage.rds"))
demo_dat_s <- readRDS(file = file.path(path_raw_data, "covar_file.rds"))



#Binarizes condition (prompt vs. no prompt)
dat_mars_app_engage$cond_bin <- ifelse(dat_mars_app_engage$A %in% c('low_effort', 'mars'), 'Prompt', 
                              ifelse(dat_mars_app_engage$A == 'none', 'No_prompt', 
                                     dat_mars_app_engage$A))

#2Q response/completion status - binarize
dat_mars_app_engage$comp_2Q_bi <- ifelse(dat_mars_app_engage$status_survey_2qs == "completed", 1, 0)
dat_mars_app_engage$resp_2Q_bi <- ifelse(dat_mars_app_engage$status_survey_2qs == "completed" | 
                                           dat_mars_app_engage$status_survey_2qs == "timedout", 1, 0)

#intervention prompt - binarize
dat_mars_app_engage$resp_emi_bi <- ifelse(dat_mars_app_engage$emi_resp == "Ok", 1, 0)

#EMA response status - binarize
dat_mars_app_engage$resp_ema_bi <- ifelse(dat_mars_app_engage$status_survey_ema == "completed" | 
                                            dat_mars_app_engage$status_survey_ema == "incomplete" | 
                                            dat_mars_app_engage$status_survey_ema == "timedout", 1, 0)

#Set NA for decision points with no micro-randomization
dat_mars_app_engage$strat_done_wcutoff_wNA <- ifelse(is.na(dat_mars_app_engage$A), NA, 
                                                      dat_mars_app_engage$strat_done_wcutoff)


#Engagement with low-effort message
dat_mars_app_engage$tips_done_resped_p <- ifelse(is.na(dat_mars_app_engage$resp_emi_bi), NA, 
                                                 ifelse(dat_mars_app_engage$A != 'low_effort' | dat_mars_app_engage$resp_emi_bi == 0, NA,
                                                        ifelse(dat_mars_app_engage$read_tips_cutoff_p == 1, 1, 0)))

#Engagement with high-effort message
dat_mars_app_engage$mars_msg_resped_p <- ifelse(is.na(dat_mars_app_engage$resp_emi_bi), NA,
                                                  ifelse(dat_mars_app_engage$A != 'mars' | dat_mars_app_engage$resp_emi_bi == 0, NA,
                                                         ifelse(dat_mars_app_engage$first_resp_p == 1, 1, 0)))

#Starting activity after high-effort message
dat_mars_app_engage$mars_start_resped_p <- ifelse(is.na(dat_mars_app_engage$resp_emi_bi), NA,
                                                  ifelse(dat_mars_app_engage$A != 'mars' | dat_mars_app_engage$resp_emi_bi == 0, NA,
                                                         ifelse(dat_mars_app_engage$activ_started_p == 1, 1, 0)))

#Engagement with high-effort strategy
dat_mars_app_engage$mars_done_started <- ifelse(is.na(dat_mars_app_engage$mars_start_resped_p), NA,
                                                ifelse(dat_mars_app_engage$mars_start_resped_p == 0 , NA,
                                                       dat_mars_app_engage$activ_done_p))
#Engagement with high-effort strategy with reading speed considered (for supplementary analysis)
dat_mars_app_engage$mars_done_started2 <- ifelse(is.na(dat_mars_app_engage$mars_start_resped_p), NA,
                                                ifelse(dat_mars_app_engage$mars_start_resped_p == 0 , NA,
                                                       dat_mars_app_engage$activ_done_cutoff_p))

#Setting value for causal excursion analysis (avail refers to decision points that subjects are available)
dat_mars_app_engage$avail <- as.numeric(!is.na(dat_mars_app_engage$A))
dat_mars_app_engage$prompt <- ifelse(is.na(dat_mars_app_engage$cond_bin), 0,
                                     ifelse(dat_mars_app_engage$cond_bin == 'Prompt', 1, 0))

#Tobacco usage variable
dat_mars_app_engage$tobacco_ema_bi <- ifelse(is.na(dat_mars_app_engage$Q19_response), NA,
                                         ifelse((str_detect(dat_mars_app_engage$Q19_response, "Cigarettes")) |
                                                  (str_detect(dat_mars_app_engage$Q19_response, "Vape")) |
                                                  (str_detect(dat_mars_app_engage$Q19_response, "tobacco")) |
                                                  (str_detect(dat_mars_app_engage$Q19_response, "Cigars")), 1, 0))
#Stressful experience variable
dat_mars_app_engage$stress_ema_bi <- ifelse(is.na(dat_mars_app_engage$Q31_response), NA,
                                            ifelse(dat_mars_app_engage$Q31_response == "None", 0, 1))



dat_mars_app_engage[, c(85:99)] <- lapply(dat_mars_app_engage[, c(85:99)], 
                                          function(x) as.numeric(str_sub(x, 1, 1)))

# dat_mars_app_engage$neg_affect_ema <- rowMeans(dat_mars_app_engage[, c('Q4_response', 'Q5_response', 'Q7_response', "Q9_response",
#                                                                        'Q10_response', 'Q12_response', 'Q13_response', 'Q14_response',
#                                                                        'Q15_response', 'Q17_response')], na.rm = TRUE)
# dat_mars_app_engage$pos_affect_ema <- rowMeans(dat_mars_app_engage[, c('Q6_response', 'Q8_response', 'Q11_response',
#                                                                        'Q16_response', 'Q18_response')], na.rm = TRUE)
# dat_mars_app_engage[is.nan(dat_mars_app_engage$neg_affect_ema), 'neg_affect_ema'] <- NA
# dat_mars_app_engage[is.nan(dat_mars_app_engage$pos_affect_ema), 'pos_affect_ema'] <- NA


dat_mars_app_engage <- dat_mars_app_engage %>%
  group_by(mars_id) %>% 
  mutate(lead_neg_affect = dplyr::lead(negative_affect, n = 1),
         lead_cig_avail = dplyr::lead(cig_available, n = 1),
         lag_neg_affect = dplyr::lag(negative_affect, n = 1),
         lag_cig_avail = dplyr::lag(cig_available, n = 1),
         lag_resp_ema_bi = dplyr::lag(resp_ema_bi, n = 1),
         lag_substance_ema_bi = dplyr::lag(substance_ema_bi, n = 1),
         lead_substance_ema_bi = dplyr::lead(substance_ema_bi, n = 1),
         lag_cig_ema_bi = dplyr::lag(cig_ema_bi, n = 1),
         lag_tobacco_ema_bi = dplyr::lag(tobacco_ema_bi, n = 1),
         lead_tobacco_ema_bi = dplyr::lead(tobacco_ema_bi, n = 1),
         lag_stress_ema_bi = dplyr::lag(stress_ema_bi, n = 1),
         lead_stress_ema_bi = dplyr::lead(stress_ema_bi, n = 1),
         lag_strat_done_wcutoff_wNA = dplyr::lag(strat_done_wcutoff_wNA, n = 1),
         lag_strat_done_wcutoff2_wNA = dplyr::lag(strat_done_wcutoff2_wNA, n = 1),
         lag_strat_done_wcutoff3_wNA = dplyr::lag(strat_done_wcutoff3_wNA, n = 1),
         lag_in_app_wNA = dplyr::lag(in_app_wNA, n = 1),
         lag_ema_cig_avail = dplyr::lag(Q41_num, n = 1),
         lag_pos_affect_ema = dplyr::lag(pos_affect_ema, n = 1),
         lag_neg_affect_ema = dplyr::lag(neg_affect_ema, n = 1)
  ) %>% 
  ungroup()




dat_mars_app_engage$id_num <- as.numeric(str_remove(dat_mars_app_engage$mars_id, "mars_"))

dat_mars_app_engage <- dat_mars_app_engage %>% 
  left_join(demo_dat_s, by = c('mars_id' = 'Grafana_ID'))


#Dataset for analysis involving EMA items 
ema_analysis_main_pipe <- dat_mars_app_engage %>% 
  filter(mars_id != "mars_15" & mars_id != "mars_87" & mars_id != "mars_116" &
           mars_id != "mars_24" & mars_id != "mars_134")


#Dataset for analyzing antecedents of engagement 
#(except the analysis on engagement with any strategy, which used the full dataset above)
curated_prompt_data <- dat_mars_app_engage %>% 
  filter(cond_bin == 'Prompt')




### Antecedents of Engagement



## Time trend


#engagement with initial notification
#model fitting
time_openapp_gee <- gee(resp_emi_bi ~ decision_point + 
                          scr_age + gender + WvsM + partner + income + th2_1,
                        data = curated_prompt_data, id = id_num, family = poisson,
                        corstr = "independence")

summary(time_openapp_gee)

#grabbing coefficients and computing CI
exp(time_openapp_gee$coefficients[2])
exp(time_openapp_gee$coefficients[2] + c(-1, 1) * sqrt(time_openapp_gee$robust.variance[2,2]) * qnorm(0.975))



#engagement with low-effort message
time_promptdonetips_gee <- gee(tips_done_resped_p ~ decision_point + 
                                    scr_age + gender + WvsM + partner + income + th2_1,
                                  data = curated_prompt_data, id = id_num, family = poisson,
                                  corstr = "independence")
summary(time_promptdonetips_gee)
exp(time_promptdonetips_gee$coefficients[2])
exp(time_promptdonetips_gee$coefficients[2] + c(-1, 1) * sqrt(time_promptdonetips_gee$robust.variance[2,2]) * qnorm(0.975))



#engagement with high-effort message
time_promptstartmars_gee <- gee(mars_msg_resped_p ~ decision_point +
                                     scr_age + gender + WvsM + partner + income + th2_1,
                                   data = curated_prompt_data, id = id_num, family = poisson,
                                   corstr = "independence")
summary(time_promptstartmars_gee)
exp(time_promptstartmars_gee$coefficients[2])
exp(time_promptstartmars_gee$coefficients[2] + c(-1, 1) * sqrt(time_promptstartmars_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort exercise
time_donemars_gee <- gee(mars_done_started ~ decision_point +
                                   scr_age + gender + WvsM + partner + income + th2_1,
                                 data = curated_prompt_data, id = id_num, family = poisson,
                                 corstr = "independence")
summary(time_donemars_gee)
exp(time_donemars_gee$coefficients[1])
exp(time_donemars_gee$coefficients[1] + c(-1, 1) * sqrt(time_donemars_gee$robust.variance[1,1]) * qnorm(0.975))


#engagement with any strategy
time_anyactiv_gee <- gee(strat_done_wcutoff_wNA ~ decision_point + 
                           scr_age + gender + WvsM + partner + income + th2_1,
                         data = dat_mars_app_engage, id = id_num, family = poisson,
                         corstr = "independence")
summary(time_anyactiv_gee)
exp(time_anyactiv_gee$coefficients[2])
exp(time_anyactiv_gee$coefficients[2] + c(-1, 1) * sqrt(time_anyactiv_gee$robust.variance[2,2]) * qnorm(0.975))




## Receptivity


#engagement with initial notification
tailorQ_openapp_gee <- gee(resp_emi_bi ~ resp_2Q_bi + decision_point +
                             scr_age + gender + WvsM + partner + income + th2_1,
                           data = curated_prompt_data, id = id_num, family = poisson,
                           corstr = "independence")
summary(tailorQ_openapp_gee)
exp(tailorQ_openapp_gee$coefficients[2])
exp(tailorQ_openapp_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_openapp_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with low-effort message
tailorQ_prompt_tipdone_gee <- gee(tips_done_resped_p ~ resp_2Q_bi + decision_point +
                                    scr_age + gender + WvsM + partner + income + th2_1,
                                  data = curated_prompt_data, id = id_num, family = poisson,
                                  corstr = "independence")
summary(tailorQ_prompt_tipdone_gee)
exp(tailorQ_prompt_tipdone_gee$coefficients[2])
exp(tailorQ_prompt_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_prompt_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort message
tailorQ_prompt_marsmsg_gee <- gee(mars_msg_resped_p ~ resp_2Q_bi + decision_point + 
                                      scr_age + gender + WvsM + partner + income + th2_1,
                                    data = curated_prompt_data, id = id_num, family = poisson,
                                    corstr = "independence")
summary(tailorQ_prompt_marsmsg_gee)
exp(tailorQ_prompt_marsmsg_gee$coefficients[2])
exp(tailorQ_prompt_marsmsg_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_prompt_marsmsg_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort exercise
tailorQ_done_mars_gee <- gee(mars_done_started ~ resp_2Q_bi + decision_point +
                                       scr_age + gender + WvsM + partner + income + th2_1,
                                     data = curated_prompt_data, id = id_num, family = poisson,
                                     corstr = "independence")
summary(tailorQ_done_mars_gee)
exp(tailorQ_done_mars_gee$coefficients[2])
exp(tailorQ_done_mars_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_done_mars_gee$robust.variance[2,2]) * qnorm(0.975))



#engagement with any strategy
tailorQ_anyactiv_gee <- gee(strat_done_wcutoff_wNA ~ resp_2Q_bi + decision_point +
                              scr_age + gender + WvsM + partner + income + th2_1,
                            data = dat_mars_app_engage, id = id_num, family = poisson,
                            corstr = "independence")
summary(tailorQ_anyactiv_gee)
exp(tailorQ_anyactiv_gee$coefficients[2])
exp(tailorQ_anyactiv_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_anyactiv_gee2$robust.variance[2,2]) * qnorm(0.975))




## Cigarettes availability

#engagement with initial notification
cig_openapp_gee <- gee(resp_emi_bi ~ cig_available + decision_point +
                         scr_age + gender + WvsM + partner + income + th2_1,
                       data = curated_prompt_data, id = id_num, family = poisson,
                       corstr = "independence")
summary(cig_openapp_gee)
exp(cig_openapp_gee$coefficients[1])
exp(cig_openapp_gee$coefficients[1] + c(-1, 1) * sqrt(cig_openapp_gee$robust.variance[1,1]) * qnorm(0.975))


#engagement with low-effort message
cig_prompt_tipdone_gee <- gee(tips_done_resped_p ~ cig_available + decision_point +
                                scr_age + gender + WvsM + partner + income + th2_1,
                              data = curated_prompt_data, id = id_num, family = poisson,
                              corstr = "independence")
summary(cig_prompt_tipdone_gee)
exp(cig_prompt_tipdone_gee$coefficients[2])
exp(cig_prompt_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(cig_prompt_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort message
cig_prompt_marsmsg_gee <- gee(mars_msg_resped_p ~ cig_available + decision_point +
                                  scr_age + gender + WvsM + partner + income + th2_1,
                                data = curated_prompt_data, id = id_num, family = poisson,
                                corstr = "independence")
summary(cig_prompt_marsmsg_gee)
exp(cig_prompt_marsmsg_gee$coefficients[2])
exp(cig_prompt_marsmsg_gee$coefficients[2] + c(-1, 1) * sqrt(cig_prompt_marsmsg_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort exercise
cig_mars_done_gee <- gee(mars_done_started ~ cig_available + decision_point + 
                                   scr_age + gender + WvsM + partner + income + th2_1,
                                 data = curated_prompt_data, id = id_num, family = poisson,
                                 corstr = "independence")
summary(cig_mars_done_gee)
exp(cig_mars_done_gee$coefficients[2])
exp(cig_mars_done_gee$coefficients[2] + c(-1, 1) * sqrt(cig_mars_done_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with any strategy
cig_anyactiv_gee <- gee(strat_done_wcutoff_wNA ~ cig_available + decision_point +
                          scr_age + gender + WvsM + partner + income + th2_1,
                        data = dat_mars_app_engage, id = id_num, family = poisson,
                        corstr = "independence")
summary(cig_anyactiv_gee)
exp(cig_anyactiv_gee$coefficients[1])
exp(cig_anyactiv_gee$coefficients[1] + c(-1, 1) * sqrt(cig_anyactiv_gee$robust.variance[1,1]) * qnorm(0.975))



##Negative affect

#engagement with initial notification
negaff_openapp_gee <- gee(resp_emi_bi ~ negative_affect + decision_point +
                            scr_age + gender + WvsM + partner + income + th2_1,
                          data = curated_prompt_data, id = id_num, family = poisson,
                          corstr = "independence")
summary(negaff_openapp_gee)
exp(negaff_openapp_gee$coefficients[2])
exp(negaff_openapp_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_openapp_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with low-effort message
negaff_prompt_tipdone_gee <- gee(tips_done_resped_p ~ negative_affect + decision_point +
                                   scr_age + gender + WvsM + partner + income + th2_1,
                                 data = curated_prompt_data, id = id_num, family = poisson,
                                 corstr = "independence")
summary(negaff_prompt_tipdone_gee)
exp(negaff_prompt_tipdone_gee$coefficients[2])
exp(negaff_prompt_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_prompt_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with high-effort message
negaff_prompt_marsmsg_gee <- gee(mars_msg_resped_p ~ negative_affect + decision_point +
                                     scr_age + gender + WvsM + partner + income + th2_1,
                                   data = curated_prompt_data, id = id_num, family = poisson,
                                   corstr = "independence")
summary(negaff_prompt_marsmsg_gee)
exp(negaff_prompt_marsmsg_gee$coefficients[2])
exp(negaff_prompt_marsmsg_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_prompt_marsmsg_gee$robust.variance[2,2]) * qnorm(0.975))



#engagement with high-effort exercise
negaff_mars_done_gee <- gee(mars_done_started ~ negative_affect + decision_point +
                                      scr_age + gender + WvsM + partner + income + th2_1,
                                    data = curated_prompt_data, id = id_num, family = poisson,
                                    corstr = "independence")
summary(negaff_mars_done_gee)
exp(negaff_mars_done_gee$coefficients[2])
exp(negaff_mars_done_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_mars_done_gee$robust.variance[2,2]) * qnorm(0.975))


#engagement with any strategy
negaff_anyactiv_gee <- gee(strat_done_wcutoff_wNA ~ negative_affect + decision_point +
                             scr_age + gender + WvsM + partner + income + th2_1,
                           data = dat_mars_app_engage, id = id_num, family = poisson,
                           corstr = "independence")
summary(negaff_anyactiv_gee)
exp(negaff_anyactiv_gee$coefficients[1])
exp(negaff_anyactiv_gee$coefficients[1] + c(-1, 1) * sqrt(negaff_anyactiv_gee$robust.variance[1,1]) * qnorm(0.975))



### Relationship between stressful experience and tobacco use

#Regress tobacco use on stress

stress_tobacco_gee <- gee(tobacco_ema_bi ~ stress_ema_bi + decision_point + lag_tobacco_ema_bi +
                         scr_age + gender + WvsM + partner + income + th2_1, 
                       data = ema_analysis_main_pipe, id = id_num, family = poisson,
                       corstr = "independence")
summary(stress_tobacco_gee)

exp(stress_tobacco_gee$coefficients[2])
exp(stress_tobacco_gee$coefficients[2] + c(-1, 1) * sqrt(stress_tobacco_gee$robust.variance[2,2]) * qnorm(0.975))


# data for when engagement with any strategy at time t - 1 = "yes" 
curated_ema_analysis_done <- ema_analysis_main_pipe %>% 
  filter(lag_strat_done_wcutoff_wNA == 1)

# data for when engagement with any strategy at time t - 1 = "no" 
curated_ema_analysis_notdone <- ema_analysis_main_pipe %>% 
  filter(lag_strat_done_wcutoff_wNA == 0)

# Fitting model for each
stress_tobacco_acc_gee_done <- gee(tobacco_ema_bi ~ stress_ema_bi + decision_point + lag_tobacco_ema_bi +
                                  scr_age + gender + WvsM + partner + income + th2_1, 
                                data = curated_ema_analysis_done, id = id_num, family = poisson,
                                corstr = "independence")
summary(stress_tobacco_acc_gee_done)
exp(stress_tobacco_acc_gee_done$coefficients[2])
exp(stress_tobacco_acc_gee_done$coefficients[2] + c(-1, 1) * sqrt(stress_tobacco_acc_gee_done$robust.variance[2,2]) * qnorm(0.975))

stress_tobacco_acc_gee_notdone <- gee(tobacco_ema_bi ~ stress_ema_bi + decision_point + lag_tobacco_ema_bi +
                                     scr_age + gender + WvsM + partner + income + th2_1, 
                                   data = curated_ema_analysis_notdone, id = id_num, family = poisson,
                                   corstr = "independence")
summary(stress_tobacco_acc_gee_notdone)
exp(stress_tobacco_acc_gee_notdone$coefficients[2])
exp(stress_tobacco_acc_gee_notdone$coefficients[2] + c(-1, 1) * sqrt(stress_tobacco_acc_gee_notdone$robust.variance[2,2]) * qnorm(0.975))



# stress x engagement interaction


doneNA_stress_tobacco_raw_gee <- gee(tobacco_ema_bi ~ stress_ema_bi * lag_strat_done_wcutoff_wNA + decision_point + 
                                    lag_tobacco_ema_bi + 
                                    scr_age + gender + WvsM + partner + income + th2_1,
                                  data = ema_analysis_main_pipe, id = id_num, family = poisson,
                                  corstr = "independence")
summary(doneNA_stress_tobacco_raw_gee)
exp(doneNA_stress_tobacco_raw_gee$coefficients[12])
exp(doneNA_stress_tobacco_raw_gee$coefficients[12] + c(-1, 1) * sqrt(doneNA_stress_tobacco_raw_gee$robust.variance[12,12]) * qnorm(0.975))




## Running causal excursion model


#Remove missing data from demographic variables
dat_mars_app_engage_narm <- dat_mars_app_engage %>% 
  filter(!is.na(gender))

#Setting NAs to 0
dat_mars_app_engage_narm$strat_done_wcutoff_fill0 <- ifelse(is.na(dat_mars_app_engage_narm$strat_done_wcutoff), 0,
                                                            dat_mars_app_engage_narm$strat_done_wcutoff)
dat_mars_app_engage_narm$activ_done_wcutoff_fill0 <- ifelse(is.na(dat_mars_app_engage_narm$activ_done), 0,
                                                            dat_mars_app_engage_narm$activ_done)
dat_mars_app_engage_narm$read_tips_wcutoff_fill0 <- ifelse(is.na(dat_mars_app_engage_narm$read_tips_cutoff), 0,
                                                            dat_mars_app_engage_narm$read_tips_cutoff)

dat_mars_app_engage_narm$block_number_fill0 <- dat_mars_app_engage_narm$decision_point - (dat_mars_app_engage_narm$cluster_id - 1) * 6


#Create the lagged variable
dat_mars_app_engage_narm <- dat_mars_app_engage_narm %>%
  group_by(mars_id) %>% 
  mutate(
         lag_strat_done_wcutoff_fill0 = dplyr::lag(strat_done_wcutoff_fill0, n = 1, default = 0)
  ) %>% 
  ungroup()


# Prompt vs. no prompt on engagement with any strategy
pvsnp <- emee(
  data = dat_mars_app_engage_narm,
  id = "id_num",
  outcome = "strat_done_wcutoff_fill0",
  treatment = "prompt",
  rand_prob = 0.5,
  moderator_formula = ~1,
  control_formula = ~ scr_age + gender + WvsM + partner + income + th2_1 + lag_strat_done_wcutoff_fill0,
  availability = "avail")

summary(pvsnp, show_control_fit = TRUE)

exp(pvsnp$fit$beta_hat)
exp(pvsnp$fit$conf_int_adjusted)

exp(pvsnp$fit$alpha_hat[2])
exp(pvsnp$fit$alpha_hat[2] + c(-1,1) * pvsnp$fit$alpha_se_adjusted[2] * qnorm(0.975))


# Prompt vs. no prompt on engagement with any strategy (moderation by time)
pvsnp_time <- emee(
  data = dat_mars_app_engage_narm,
  id = "id_num",
  outcome = "strat_done_wcutoff_fill0",
  treatment = "prompt",
  rand_prob = 0.5,
  moderator_formula = ~decision_point,
  control_formula = ~ scr_age + gender + WvsM + partner + income + th2_1 + lag_strat_done_wcutoff_fill0,
  availability = "avail")

summary(pvsnp_time, show_control_fit = TRUE)

summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))

exp(pvsnp_time$fit$beta_hat)
exp(pvsnp_time$fit$conf_int_adjusted)

exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[3,1])
exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[3,2])
exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[3,3])

exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[4,1])
exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[4,2])
exp(summary(pvsnp_time, lincomb = rbind(c(1,10), c(1,50)))$causal_excursion_effect[4,3])




# Analysis that removes day 1 and day 10 (in supplementary material)
dat_mars_app_engage_narm_2to9 <- dat_mars_app_engage_narm %>% 
  filter(cluster_id != 1 & cluster_id != 10)


pvsnp_2to9 <- emee(
  data = dat_mars_app_engage_narm_2to9,
  id = "id_num",
  outcome = "strat_done_wcutoff_fill0",
  treatment = "prompt",
  rand_prob = 0.5,
  moderator_formula = ~1,
  control_formula = ~ scr_age + gender + WvsM + partner + income + th2_1 + lag_strat_done_wcutoff_fill0,
  availability = "avail")

summary(pvsnp_2to9, show_control_fit = TRUE)

exp(pvsnp_2to9$fit$beta_hat)
exp(pvsnp_2to9$fit$conf_int_adjusted)


### Supplementary analysis

## High-effort exercise with consideration of reading speed

#Time trend
time_donemars_gee2 <- gee(mars_done_started2 ~ decision_point +
                                    scr_age + gender + WvsM + partner + income + th2_1,
                                  data = curated_prompt_data, id = id_num, family = poisson,
                                  corstr = "independence")
summary(time_donemars_gee2)
exp(time_donemars_gee2$coefficients[2])
exp(time_donemars_gee2$coefficients[2] + c(-1, 1) * sqrt(time_donemars_gee2$robust.variance[2,2]) * qnorm(0.975))


#Receptivity
tailorQ_done_mars_gee2 <- gee(mars_done_started2 ~ resp_2Q_bi + decision_point +
                                scr_age + gender + WvsM + partner + income + th2_1,
                              data = curated_prompt_data, id = id_num, family = poisson,
                              corstr = "independence")
summary(tailorQ_done_mars_gee2)
exp(tailorQ_done_mars_gee2$coefficients[2])
exp(tailorQ_done_mars_gee2$coefficients[2] + c(-1, 1) * sqrt(tailorQ_done_mars_gee2$robust.variance[2,2]) * qnorm(0.975))


#Cigarette availability
cig_mars_done_gee2 <- gee(mars_done_started2 ~ cig_available + decision_point + 
                            scr_age + gender + WvsM + partner + income + th2_1,
                          data = curated_prompt_data, id = id_num, family = poisson,
                          corstr = "independence")
summary(cig_mars_done_gee2)
exp(cig_mars_done_gee2$coefficients[2])
exp(cig_mars_done_gee2$coefficients[2] + c(-1, 1) * sqrt(cig_mars_done_gee2$robust.variance[2,2]) * qnorm(0.975))


#Negative affect
negaff_mars_done_gee2 <- gee(mars_done_started2 ~ negative_affect + decision_point +
                               scr_age + gender + WvsM + partner + income + th2_1,
                             data = curated_prompt_data, id = id_num, family = poisson,
                             corstr = "independence")
summary(negaff_mars_done_gee2)
exp(negaff_mars_done_gee2$coefficients[2])
exp(negaff_mars_done_gee2$coefficients[2] + c(-1, 1) * sqrt(negaff_mars_done_gee2$robust.variance[2,2]) * qnorm(0.975))


## Using alternative (non-conditional) definition of engagement

#Time trend

time_donetips_gee <- gee(read_tips_cutoff_p ~ decision_point +
                           scr_age + gender + WvsM + partner + income + th2_1,
                         data = curated_prompt_data, id = id_num, family = poisson,
                         corstr = "independence")
summary(time_donetips_gee)
exp(time_donetips_gee$coefficients[2])
exp(time_donetips_gee$coefficients[2] + c(-1, 1) * sqrt(time_donetips_gee$robust.variance[2,2]) * qnorm(0.975))


time_startmars_gee <- gee(first_resp_p ~ decision_point + 
                            scr_age + gender + WvsM + partner + income + th2_1,
                          data = curated_prompt_data, id = id_num, family = poisson,
                          corstr = "independence")
summary(time_startmars_gee)
exp(time_startmars_gee$coefficients[2])
exp(time_startmars_gee$coefficients[2] + c(-1, 1) * sqrt(time_startmars_gee$robust.variance[2,2]) * qnorm(0.975))


time_donemars_gee <- gee(activ_done_p ~ decision_point +
                           scr_age + gender + WvsM + partner + income + th2_1,
                         data = curated_prompt_data, id = id_num, family = poisson,
                         corstr = "independence")
summary(time_donemars_gee)
exp(time_donemars_gee$coefficients[2])
exp(time_donemars_gee$coefficients[2] + c(-1, 1) * sqrt(time_donemars_gee$robust.variance[2,2]) * qnorm(0.975))


#Receptivity

tailorQ_tipdone_gee <- gee(read_tips_cutoff2_p ~ resp_2Q_bi + decision_point +
                             scr_age + gender + WvsM + partner + income + th2_1,
                           data = curated_prompt_data, id = id_num, family = poisson,
                           corstr = "independence")
summary(tailorQ_tipdone_gee)
exp(tailorQ_tipdone_gee$coefficients[2])
exp(tailorQ_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))


tailorQ_startmars_gee <- gee(first_resp_p ~ resp_2Q_bi + decision_point + 
                               scr_age + gender + WvsM + partner + income + th2_1,
                             data = curated_prompt_data, id = id_num, family = poisson,
                             corstr = "independence")
summary(tailorQ_startmars_gee)
exp(tailorQ_startmars_gee$coefficients[2])
exp(tailorQ_startmars_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_startmars_gee$robust.variance[2,2]) * qnorm(0.975))


tailorQ_done_mars_gee <- gee(activ_done_p ~ resp_2Q_bi + decision_point + 
                               scr_age + gender + WvsM + partner + income + th2_1,
                             data = curated_prompt_data, id = id_num, family = poisson,
                             corstr = "independence")
summary(tailorQ_done_mars_gee)
exp(tailorQ_done_mars_gee$coefficients[2])
exp(tailorQ_done_mars_gee$coefficients[2] + c(-1, 1) * sqrt(tailorQ_done_mars_gee$robust.variance[2,2]) * qnorm(0.975))



#Cigarette availability

cig_tipdone_gee <- gee(read_tips_cutoff_p ~ cig_available + decision_point + 
                         scr_age + gender + WvsM + partner + income + th2_1,
                       data = curated_prompt_data, id = id_num, family = poisson,
                       corstr = "independence")
summary(cig_tipdone_gee)
exp(cig_tipdone_gee$coefficients[2])
exp(cig_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(cig_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))


cig_marsstart_gee <- gee(first_resp_p ~ cig_available + decision_point +
                           scr_age + gender + WvsM + partner + income + th2_1,
                         data = curated_prompt_data, id = id_num, family = poisson,
                         corstr = "independence")
summary(cig_marsstart_gee)
exp(cig_marsstart_gee$coefficients[2])
exp(cig_marsstart_gee$coefficients[2] + c(-1, 1) * sqrt(cig_marsstart_gee$robust.variance[2,2]) * qnorm(0.975))


cig_marsdone_gee <- gee(activ_done_p ~ cig_available + decision_point + 
                          scr_age + gender + WvsM + partner + income + th2_1,
                        data = curated_prompt_data, id = id_num, family = poisson,
                        corstr = "independence")
summary(cig_marsdone_gee)
exp(cig_marsdone_gee$coefficients[2])
exp(cig_marsdone_gee$coefficients[2] + c(-1, 1) * sqrt(cig_marsdone_gee$robust.variance[2,2]) * qnorm(0.975))


#Negative affect

negaff_tipdone_gee <- gee(read_tips_cutoff_p ~ negative_affect + decision_point + 
                            scr_age + gender + WvsM + partner + income + th2_1,
                          data = curated_prompt_data, id = id_num, family = poisson,
                          corstr = "independence")
summary(negaff_tipdone_gee)
exp(negaff_tipdone_gee$coefficients[2])
exp(negaff_tipdone_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_tipdone_gee$robust.variance[2,2]) * qnorm(0.975))

negaff_marsstart_gee <- gee(first_resp_p ~ negative_affect + decision_point +
                              scr_age + gender + WvsM + partner + income + th2_1,
                            data = curated_prompt_data, id = id_num, family = poisson,
                            corstr = "independence")
summary(negaff_marsstart_gee)
exp(negaff_marsstart_gee$coefficients[2])
exp(negaff_marsstart_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_marsstart_gee$robust.variance[2,2]) * qnorm(0.975))

negaff_marsdone_gee <- gee(activ_done_p ~ negative_affect + decision_point +
                             scr_age + gender + WvsM + partner + income + th2_1,
                           data = curated_prompt_data, id = id_num, family = poisson,
                           corstr = "independence")
summary(negaff_marsdone_gee)
exp(negaff_marsdone_gee$coefficients[2])
exp(negaff_marsdone_gee$coefficients[2] + c(-1, 1) * sqrt(negaff_marsdone_gee$robust.variance[2,2]) * qnorm(0.975))





##Plotting fitted prob

#engagement with initial notification

int_coef <- time_openapp_gee$coefficients["(Intercept)"]
time_coef <- time_openapp_gee$coefficients["decision_point"]
gender_coef <- time_openapp_gee$coefficients["gender"]


time_grid <- c(1:60)

fitted_prob <- exp(int_coef + time_coef * time_grid + gender_coef)

stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ exp(x1 + %f * x2 + x3)", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_openapp_gee)[c(1,2,4)], 
                                time_openapp_gee$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_openapp <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_openapp <- ggplot(data = plot_df_openapp, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the initial notification", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))
  


#engagement with low-effort message

int_coef <- time_promptdonetips_gee$coefficients["(Intercept)"]
time_coef <- time_promptdonetips_gee$coefficients["decision_point"]
gender_coef <- time_promptdonetips_gee$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- exp(int_coef + time_coef * time_grid + gender_coef)

stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ exp(x1 + %f * x2 + x3)", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_promptdonetips_gee)[c(1,2,4)], 
                                time_promptdonetips_gee$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_tips <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_tips <- ggplot(data = plot_df_tips, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the message for low-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))


#engagement with high-effort message

int_coef <- time_promptstartmars_gee$coefficients["(Intercept)"]
time_coef <- time_promptstartmars_gee$coefficients["decision_point"]
gender_coef <- time_promptstartmars_gee$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- exp(int_coef + time_coef * time_grid + gender_coef)

stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ exp(x1 + %f * x2 + x3)", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_promptstartmars_gee)[c(1,2,4)], 
                                time_promptstartmars_gee$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_mars_mess <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_mars_mess <- ggplot(data = plot_df_mars_mess, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the message for high-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))



#engagement with high-effort exercise

int_coef <- time_donemars_gee$coefficients["(Intercept)"]
time_coef <- time_donemars_gee$coefficients["decision_point"]
gender_coef <- time_donemars_gee$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- exp(int_coef + time_coef * time_grid + gender_coef)


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ exp(x1 + %f * x2 + x3)", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_donemars_gee)[c(1,2,4)], 
                                time_donemars_gee$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_mars_done <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_mars_done <- ggplot(data = plot_df_mars_done, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the high-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))


#engagement with any strategy

int_coef <- time_anyactiv_gee$coefficients["(Intercept)"]
time_coef <- time_anyactiv_gee$coefficients["decision_point"]
gender_coef <- time_anyactiv_gee$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- exp(int_coef + time_coef * time_grid + gender_coef)

stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ exp(x1 + %f * x2 + x3)", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_anyactiv_gee)[c(1,2,4)], 
                                time_anyactiv_gee$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_any_activ <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_any_activ <- ggplot(data = plot_df_any_activ, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with any self-regulatory strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))



## Plotting fitted probability (logit link)

time_openapp_gee_logit <- gee(resp_emi_bi ~ decision_point + 
                          scr_age + gender + WvsM + partner + income + th2_1,
                        data = curated_prompt_data, id = id_num, family = binomial,
                        corstr = "independence")

summary(time_openapp_gee_logit)


time_promptdonetips_gee_logit <- gee(tips_done_resped_p ~ decision_point + 
                                     scr_age + gender + WvsM + partner + income + th2_1,
                                   data = curated_prompt_data, id = id_num, family = binomial,
                                   corstr = "independence")
summary(time_promptdonetips_gee_logit)


time_promptstartmars_gee_logit <- gee(mars_msg_resped_p ~ decision_point +
                                      scr_age + gender + WvsM + partner + income + th2_1,
                                    data = curated_prompt_data, id = id_num, family = binomial,
                                    corstr = "independence")
summary(time_promptstartmars_gee_logit)


time_donemars_gee_logit <- gee(mars_done_started ~ decision_point +
                                   scr_age + gender + WvsM + partner + income + th2_1,
                                 data = curated_prompt_data, id = id_num, family = binomial,
                                 corstr = "independence")
summary(time_donemars_gee_logit)


time_anyactiv_gee_logit <- gee(strat_done_wcutoff_wNA ~ decision_point + 
                            scr_age + gender + WvsM + partner + income + th2_1,
                          data = dat_mars_app_engage, id = id_num, family = binomial,
                          corstr = "independence")
summary(time_anyactiv_gee_logit)



#engagement with initial notification

int_coef <- time_openapp_gee_logit$coefficients["(Intercept)"]
time_coef <- time_openapp_gee_logit$coefficients["decision_point"]
gender_coef <- time_openapp_gee_logit$coefficients["gender"]


time_grid <- c(1:60)

fitted_prob <- 1 / (1 + exp(-int_coef - time_coef * time_grid - gender_coef))


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ 1 / (1 + exp(-x1 - %f * x2 - x3))", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_openapp_gee_logit)[c(1,2,4)], 
                                time_openapp_gee_logit$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_openapp_logit <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_openapp_logit <- ggplot(data = plot_df_openapp_logit, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the initial notification", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))



#engagement with low-effort message

int_coef <- time_promptdonetips_gee_logit$coefficients["(Intercept)"]
time_coef <- time_promptdonetips_gee_logit$coefficients["decision_point"]
gender_coef <- time_promptdonetips_gee_logit$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- 1 / (1 + exp(-int_coef - time_coef * time_grid - gender_coef))


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ 1 / (1 + exp(-x1 - %f * x2 - x3))", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_promptdonetips_gee_logit)[c(1,2,4)], 
                                time_promptdonetips_gee_logit$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_tips_logit <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_tips_logit <- ggplot(data = plot_df_tips_logit, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the message for low-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))


#engagement with high-effort message

int_coef <- time_promptstartmars_gee_logit$coefficients["(Intercept)"]
time_coef <- time_promptstartmars_gee_logit$coefficients["decision_point"]
gender_coef <- time_promptstartmars_gee_logit$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- 1 / (1 + exp(-int_coef - time_coef * time_grid - gender_coef))


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ 1 / (1 + exp(-x1 - %f * x2 - x3))", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_promptstartmars_gee_logit)[c(1,2,4)], 
                                time_promptstartmars_gee_logit$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_mars_mess_logit <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_mars_mess_logit <- ggplot(data = plot_df_mars_mess_logit, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the message for high-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))


#engagement with high-effort exercise

int_coef <- time_donemars_gee_logit$coefficients["(Intercept)"]
time_coef <- time_donemars_gee_logit$coefficients["decision_point"]
gender_coef <- time_donemars_gee_logit$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- 1 / (1 + exp(-int_coef - time_coef * time_grid - gender_coef))


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ 1 / (1 + exp(-x1 - %f * x2 - x3))", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_donemars_gee_logit)[c(1,2,4)], 
                                time_donemars_gee_logit$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_mars_done_logit <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_mars_done_logit <- ggplot(data = plot_df_mars_done_logit, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with the high-effort strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))


#engagement with any strategy

int_coef <- time_anyactiv_gee_logit$coefficients["(Intercept)"]
time_coef <- time_anyactiv_gee_logit$coefficients["decision_point"]
gender_coef <- time_anyactiv_gee_logit$coefficients["gender"]

time_grid <- c(1:60)

fitted_prob <- 1 / (1 + exp(-int_coef - time_coef * time_grid - gender_coef))


stderr_pred <- rep(NA, 60)

for (r in 1:60){
  
  form <- sprintf("~ 1 / (1 + exp(-x1 - %f * x2 - x3))", r)
  stderr_pred[r] <- deltamethod(as.formula(form), coef(time_anyactiv_gee_logit)[c(1,2,4)], 
                                time_anyactiv_gee_logit$robust.variance[c(1,2,4), c(1,2,4)])
}

plot_df_any_activ_logit <- data.frame(time = time_grid, prob = fitted_prob, std_err = stderr_pred)

plot_any_activ_logit <- ggplot(data = plot_df_any_activ_logit, mapping = aes(x = time, y = prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob - std_err, ymax = prob + std_err), width = .5) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.2), limits = c(0.1, 1.1)) +
  labs(title = "Fitted probability of engagement with any self-regulatory strategy", 
       x = "Time", 
       y = "Probability of engagement") +
  theme(plot.title = element_text(hjust = 0.5, size= 12))
