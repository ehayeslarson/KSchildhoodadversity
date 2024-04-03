library(tidyverse)
library(mice)
library(lme4)
library(broom.mixed)
library(lmerTest)

#load data -----
  load(file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mids_long.Rdata")
  

#Now run cognitive decline models

all_expvars<-c("ACE_fac1_z", "ACE_fac2_z", 
               "w1_total_ACE_count", "w1w4_total_ACE_count",
               "w1_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
               "w1_ACE_par_remarried", #/*PARENTS REMARRIED*/
               "w1_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
               "w1_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
               "w1_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
               "w1_ACE_par_jail", #/*PARENT TO JAIL*/
               "w1_ACE_fam_illness",#/*SERIOUS ILLNESS OF FAM MEMBER*/
               "w4_ACE_par_physabuse", # /*PHYS ABUSE BY PARENT*/
               "w4_ACE_hh_mentalill") #/*MENTAL ILLNESS OF FAM MEMBER*/

cog_vars<-c("exec", "vrmem")

models<-expand_grid(all_expvars,cog_vars)

#function to run crude and adjusted models using age and TOS scales, pooling across MIs
#Note: we checked models with random slopes and these have RI-RS correlation of 1.0 (singularity warning, overly complex model). 
# Coefficients did not change so we elected to keep RI-only models
cog_analysis<-function(dat=NULL){
  #Unit testing
  #i<-1
  #dat<-dat_all_imp_final_mids_long
  
  for (i in 1:nrow(models)){
  exposure<-rlang::parse_expr(models$all_expvars[i])
  cog_outcome<-rlang::parse_expr(paste0("SENAS_",models$cog_vars[i],"_poolz"))
  
  temp.mod.crude.age <-with(dat,
                        lmer(
                          eval(cog_outcome) ~
                            phone +
                            INTERVIEW_AGE_c75 * (
                              eval(exposure)) +
                            (1 |  STUDYID),
                          offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                          REML = TRUE,
                          control = lmerControl(optimizer = 'bobyqa')
                        )
                      )
  
  temp.mod.crude.age.ls <- as.list(temp.mod.crude.age)
  
  temp.mod.crude.summ.age <- summary(pool(temp.mod.crude.age.ls), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Crude",
           time = "Age",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "INTERVIEW_AGE_c75:eval(exposure)" ~ paste0("INTERVIEW_AGE_c75:",paste0(exposure)),
                            TRUE ~ term))
  
  temp.mod.adj.age1 <-with(dat,
                          lmer(
                            eval(cog_outcome) ~
                              phone +
                              INTERVIEW_AGE_c75 * (
                                eval(exposure) + w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                  w1_momordad_edu_gt12) +
                              (1 |  STUDYID),
                            offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                            REML = TRUE,
                            control = lmerControl(optimizer = 'bobyqa')
                          )
  )
  
  temp.mod.adj.age.ls1 <- as.list(temp.mod.adj.age1)
  
  
  temp.mod.adj.summ.age1 <- summary(pool(temp.mod.adj.age.ls1), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Adjusted1",
           time = "Age",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "INTERVIEW_AGE_c75:eval(exposure)" ~ paste0("INTERVIEW_AGE_c75:",paste0(exposure)),
                            TRUE ~ term))
  
  temp.mod.adj.age <-with(dat,
                          lmer(
                            eval(cog_outcome) ~
                              phone +
                              INTERVIEW_AGE_c75 * (
                                eval(exposure) + w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                  w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                                  w1_chd_everhungry + w1_chd_commstanding) +
                              (1 |  STUDYID),
                            offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                            REML = TRUE,
                            control = lmerControl(optimizer = 'bobyqa')
                          )
  )
  
  temp.mod.adj.age.ls <- as.list(temp.mod.adj.age)
  
  
  temp.mod.adj.summ.age <- summary(pool(temp.mod.adj.age.ls), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Adjusted",
           time = "Age",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "INTERVIEW_AGE_c75:eval(exposure)" ~ paste0("INTERVIEW_AGE_c75:",paste0(exposure)),
                            TRUE ~ term))
  
  
  
  temp.mod.crude.study <-with(dat,
                            lmer(
                              eval(cog_outcome) ~
                                phone +
                                study_time * ( W1_INTERVIEW_AGE_c75 +
                                  eval(exposure)) +
                                (1 |  STUDYID),
                              offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                              REML = TRUE,
                              control = lmerControl(optimizer = 'bobyqa')
                            )
  )
  
  temp.mod.crude.study.ls <- as.list(temp.mod.crude.study)
  
  temp.mod.crude.summ.study <- summary(pool(temp.mod.crude.study.ls), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Crude",
           time = "Study",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "study_time:eval(exposure)" ~ paste0("study_time:",paste0(exposure)),
                            TRUE ~ term))
  
  temp.mod.adj.study1 <-with(dat,
                            lmer(
                              eval(cog_outcome) ~
                                phone +
                                study_time * (
                                  eval(exposure) + W1_INTERVIEW_AGE_c75 + w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                    w1_momordad_edu_gt12) +
                                (1 |  STUDYID),
                              offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                              REML = TRUE,
                              control = lmerControl(optimizer = 'bobyqa')
                            )
  )
  
  temp.mod.adj.study.ls1 <- as.list(temp.mod.adj.study1)
  
  
  temp.mod.adj.summ.study1 <- summary(pool(temp.mod.adj.study.ls1), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Adjusted1",
           time = "Study",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "study_time:eval(exposure)" ~ paste0("study_time:",paste0(exposure)),
                            TRUE ~ term))
  
  temp.mod.adj.study <-with(dat,
                          lmer(
                            eval(cog_outcome) ~
                              phone +
                              study_time * (
                                eval(exposure) + W1_INTERVIEW_AGE_c75 + w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                  w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                                  w1_chd_everhungry + w1_chd_commstanding) +
                              (1 |  STUDYID),
                            offset = eval(rlang::parse_expr(paste0('offset_', models[i,"cog_vars"]))),
                            REML = TRUE,
                            control = lmerControl(optimizer = 'bobyqa')
                          )
  )
  
  temp.mod.adj.study.ls <- as.list(temp.mod.adj.study)
  
  
  temp.mod.adj.summ.study <- summary(pool(temp.mod.adj.study.ls), conf.int = TRUE)    %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = paste0(exposure),
           outcome = paste0(cog_outcome), 
           model="Adjusted",
           time = "Study",
           term = case_when(term == "eval(exposure)" ~ paste0(exposure),
                            term == "study_time:eval(exposure)" ~ paste0("study_time:",paste0(exposure)),
                            TRUE ~ term))
  
  
  if (i==1){all_res<-rbind(temp.mod.crude.summ.age, temp.mod.adj.summ.age1,temp.mod.adj.summ.age,
                           temp.mod.crude.summ.study, temp.mod.adj.summ.study1, temp.mod.adj.summ.study)} else {
    all_res <-rbind(all_res, temp.mod.crude.summ.age, temp.mod.adj.summ.age1, temp.mod.adj.summ.age,
                    temp.mod.crude.summ.study, temp.mod.adj.summ.study1, temp.mod.adj.summ.study)
  }
  
}

  return(all_res)
  
}



#Run analyses in all cognitive data and sensitivity subsamples.
all_res_cog<-cog_analysis(dat=dat_all_imp_final_mids_long)
all_res_cog_mrisamp<-cog_analysis(dat=filter(dat_all_imp_final_mids_long, MRI_sample==1))
all_res_cog_petsamp<-cog_analysis(dat=filter(dat_all_imp_final_mids_long, PET_sample==1))
all_res_cog_ge65samp<-cog_analysis(dat=filter(dat_all_imp_final_mids_long, W1_INTERVIEW_AGE >=65 ))

save(all_res_cog, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog.Rdata")
save(all_res_cog_mrisamp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_mrisamp.Rdata")
save(all_res_cog_petsamp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_petsamp.Rdata")
save(all_res_cog_ge65samp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_ge65samp.Rdata")

