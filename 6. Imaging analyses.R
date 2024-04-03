library(tidyverse)
library(mice)
library(sandwich)
library(broom)
library(lmtest)

load(file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mri_mids.Rdata")
load(file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_pet_mids.Rdata")

options(sci=999)

vols<-c("Cerebrum_tcb", "Total_hippo", 
        "Total_gray", "Total_white", "Total_wmh", "Total_brain", 
        "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
)

all_vol_outcomes<-c(paste0(vols, "_resid"), 
                    paste0(vols, "_residblom"), 
                    paste0(vols, "_residzscore"), 
                    paste0(vols, "_residlog"), 
                    "Total_wmh_log")


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


models1<-expand_grid(all_expvars,all_vol_outcomes)
models2<-expand_grid(all_expvars,vols) %>% rename(all_vol_outcomes=vols)
models<-rbind(models1, models2)

MRI_analysis<-function(dat=NULL){
  
  for (i in 1:nrow(models)){
    #i<-534
    exposure<-rlang::parse_expr(models$all_expvars[i])
    outcome<-rlang::parse_expr(models$all_vol_outcomes[i])
    
    temp.mod.crude <-with(dat,
                          lm(eval(outcome) ~ eval(exposure)))
    
    temp.mod.crude.summ <- summary(pool(temp.mod.crude), conf.int = TRUE)  %>%
      select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
      mutate(exposure = models$all_expvars[i],
             outcome = models$all_vol_outcomes[i], 
             model="Crude",
             term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                              TRUE ~ term))
    
    
    temp.mod.adj1 <-with(dat,
                        lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                             w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                             w1_momordad_edu_gt12))
    
    temp.mod.adj.summ1 <- summary(pool(temp.mod.adj1), conf.int = TRUE)  %>%
      select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
      mutate(exposure = models$all_expvars[i],
             outcome = models$all_vol_outcomes[i], 
             model="Adjusted1",
             term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                              TRUE ~ term))
    
    temp.mod.adj <-with(dat,
                        lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                             w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                             w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                             w1_chd_everhungry + w1_chd_commstanding))
    
    temp.mod.adj.summ <- summary(pool(temp.mod.adj), conf.int = TRUE)  %>%
      select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
      mutate(exposure = models$all_expvars[i],
             outcome = models$all_vol_outcomes[i], 
             model="Adjusted",
             term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                              TRUE ~ term))
    if (i==1){all_res<-rbind(temp.mod.crude.summ, temp.mod.adj.summ1, temp.mod.adj.summ)} else {
      all_res<-rbind(all_res, temp.mod.crude.summ, temp.mod.adj.summ1, temp.mod.adj.summ)
    }
    
    if (models$all_vol_outcomes[i] %in% vols){
      
      temp.mod.crude2 <-with(dat,
                             lm(eval(outcome) ~ eval(exposure) + Cerebrum_tcv))
      
      temp.mod.crude.summ2 <- summary(pool(temp.mod.crude2), conf.int = TRUE)  %>%
        select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
        mutate(exposure = models$all_expvars[i],
               outcome = models$all_vol_outcomes[i], 
               model="Crude + ICV",
               term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                                TRUE ~ term))
      
      
      temp.mod.adj21 <-with(dat,
                           lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                                w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                w1_momordad_edu_gt12 + Cerebrum_tcv))
      
      temp.mod.adj.summ21 <- summary(pool(temp.mod.adj21), conf.int = TRUE)  %>%
        select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
        mutate(exposure = models$all_expvars[i],
               outcome = models$all_vol_outcomes[i], 
               model="Adjusted1 + ICV",
               term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                                TRUE ~ term))
      
      temp.mod.adj2 <-with(dat,
                           lm(eval(outcome) ~ eval(exposure)  + age_at_mri +
                                w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                                w1_chd_everhungry + w1_chd_commstanding + Cerebrum_tcv))
      
      temp.mod.adj.summ2 <- summary(pool(temp.mod.adj2), conf.int = TRUE)  %>%
        select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
        mutate(exposure = models$all_expvars[i],
               outcome = models$all_vol_outcomes[i], 
               model="Adjusted + ICV",
               term = case_when(term == "eval(exposure)" ~ models$all_expvars[i],
                                TRUE ~ term))
      
      all_res<-rbind(all_res, temp.mod.crude.summ2, temp.mod.adj.summ21, temp.mod.adj.summ2)
      
    }
  }
  return(all_res)
  
}

all_res_mri<-MRI_analysis(dat=dat_mri_imp_final_mids)
all_res_mri_petsamp<-MRI_analysis(dat=filter(dat_mri_imp_final_mids, PET_sample==1))
all_res_mri_ge65samp<-MRI_analysis(dat=filter(dat_mri_imp_final_mids, W1_INTERVIEW_AGE >=65 ))

#PET analyses  
for (i in 1:length(all_expvars)){
 # i<-1
  exposure<-rlang::parse_expr(all_expvars[i])

#Using log poisson with robust SE. This is needed for adjusted model, 
# so using here as well for consistency.
  temp.mod.crude <-with(dat_pet_imp_final_mids,
                        coeftest(glm(Abeta_pos ~ eval(exposure), 
                                     family=poisson(link="log")),
                                 vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure), 
                                                    family=poisson(link="log")), type = "HC3")))
  temp.mod.crude.summ <- summary(pool(temp.mod.crude), conf.int = TRUE)
  temp.mod.crude.summ
  
  temp.mod.crude.summ <- summary(pool(temp.mod.crude), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(RR=exp(estimate),
           LCI_RR = exp(`2.5 %`),
           UCI_RR = exp(`97.5 %`),
           exposure = all_expvars[i],
           outcome = "Amyloid positive", 
           model="Crude",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))
  
  #Need robust SE's here because using poisson
  temp.mod.adj1 <-with(dat_pet_imp_final_mids,
                      coeftest(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                     w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                     w1_momordad_edu_gt12,
                                   family=poisson(link="log")),
                               vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                                    w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                                    w1_momordad_edu_gt12,
                                                  family=poisson(link="log")), type = "HC3")))
  
  temp.mod.adj.summ1 <- summary(pool(temp.mod.adj1), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(RR=exp(estimate),
           LCI_RR = exp(`2.5 %`),
           UCI_RR = exp(`97.5 %`),
           exposure = all_expvars[i],
           outcome = "Amyloid positive", 
           model="Adjusted1",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))
  
  #Need robust SE's here because using poisson
  temp.mod.adj <-with(dat_pet_imp_final_mids,
                      coeftest(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                           w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                           w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                           w1_chd_everhungry + w1_chd_commstanding,
                      family=poisson(link="log")),
                      vcov. = vcovHC(glm(Abeta_pos ~ eval(exposure)  + age_at_pet +
                                           w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                           w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                                           w1_chd_everhungry + w1_chd_commstanding,
                                         family=poisson(link="log")), type = "HC3")))
  
  temp.mod.adj.summ <- summary(pool(temp.mod.adj), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(RR=exp(estimate),
           LCI_RR = exp(`2.5 %`),
           UCI_RR = exp(`97.5 %`),
           exposure = all_expvars[i],
           outcome = "Amyloid positive", 
           model="Adjusted",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))
  if (i==1){all_res_pet <-rbind(temp.mod.crude.summ, temp.mod.adj.summ1, temp.mod.adj.summ)} else {
    all_res_pet <-rbind(all_res_pet, temp.mod.crude.summ, temp.mod.adj.summ1, temp.mod.adj.summ)
  }
  
}

#EHL adding SUVR models (continuous PET models)
for (i in 1:length(all_expvars)){
  #i<-1
  exposure<-rlang::parse_expr(all_expvars[i])
  
  temp.mod.SUVR.crude <-with(dat_pet_imp_final_mids,
                        lm(Landau_All_FSR ~ eval(exposure)))
  
  temp.mod.SUVR.crude.summ <- summary(pool(temp.mod.SUVR.crude), conf.int = TRUE)
  temp.mod.SUVR.crude.summ
  
  temp.mod.SUVR.crude.summ <- summary(pool(temp.mod.SUVR.crude), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = all_expvars[i],
           outcome = "SUVR", 
           model="Crude",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))

  temp.mod.SUVR.adj1 <-with(dat_pet_imp_final_mids,
                       lm(Landau_All_FSR ~ eval(exposure)  + age_at_pet +
                                      w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                      w1_momordad_edu_gt12))
  
  temp.mod.SUVR.adj.summ1 <- summary(pool(temp.mod.SUVR.adj1), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = all_expvars[i],
           outcome = "SUVR", 
           model="Adjusted1",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))
  
  temp.mod.SUVR.adj <-with(dat_pet_imp_final_mids,
                      lm(Landau_All_FSR ~ eval(exposure)  + age_at_pet +
                                     w1_female + w1_southern_birth + W1_D_RACE_SUMMARY +
                                     w1_momordad_edu_gt12 + w1_chd_okfinancially + 
                                     w1_chd_everhungry + w1_chd_commstanding))
  
  temp.mod.SUVR.adj.summ <- summary(pool(temp.mod.SUVR.adj), conf.int = TRUE)  %>%
    select(term, estimate, std.error, '2.5 %', '97.5 %') %>%
    mutate(exposure = all_expvars[i],
           outcome = "SUVR", 
           model="Adjusted",
           term = case_when(term == "eval(exposure)" ~ all_expvars[i],
                            TRUE ~ term))
  if (i==1){all_res_suvr <-rbind(temp.mod.SUVR.crude.summ, temp.mod.SUVR.adj.summ1, temp.mod.SUVR.adj.summ)} else {
    all_res_suvr <-rbind(all_res_suvr, temp.mod.SUVR.crude.summ, temp.mod.SUVR.adj.summ1, temp.mod.SUVR.adj.summ)
  }
  
}






save(all_res_mri, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri.Rdata")
save(all_res_pet, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_pet.Rdata")
save(all_res_suvr, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_suvr.Rdata")
save(all_res_mri_petsamp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri_petsamp.Rdata")
save(all_res_mri_ge65samp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri_ge65samp.Rdata")

#Note: warnings that are generated are because "the `exponentiate` argument is not supported in the `tidy()` method for `coeftest` objects and will be ignored."
#As far as I can tell this does not present a problem for calculating robust SEs with the sandiwch package