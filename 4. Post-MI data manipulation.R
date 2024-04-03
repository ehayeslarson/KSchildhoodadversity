library(lavaan)
library(tidyverse)
library(mice)
library(lme4)
library(broom.mixed)
library(lmerTest)



load("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/preimputation.Rdata")
load("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation.Rdata")


###################    
#Post-imputation data manipulation
###################  


    #checking that we have 40 total imputations + original dataset with missing data
    #(.imp = 0 is for the original dataset with missingness)
        table(dat_all_imp$.imp, exclude = NULL)
    
    #----bringing back outcome variables, W2-W4 variables and deriving new variables----
        vols<-c("Cerebrum_tcb", "Total_hippo", 
                "Total_gray", "Total_white", "Total_wmh", "Total_brain", 
                "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
        )
    
        long_merge<-all_dat_preimp %>%
          select(
            #age and cognitive outcomes
            W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE, W4_INTERVIEW_AGE,
            W1_SENAS_telephone, W2_SENAS_telephone, W3_SENAS_telephone, W4_SENAS_telephone,
            W1_SENAS_exec, W2_SENAS_exec, W3_SENAS_exec, W4_SENAS_exec, 
            W1_SENAS_vrmem, W2_SENAS_vrmem, W3_SENAS_vrmem, W4_SENAS_vrmem,
            W1_SENAS_exec_poolz, W2_SENAS_exec_poolz, W3_SENAS_exec_poolz, W4_SENAS_exec_poolz,
            W1_SENAS_vrmem_poolz, W2_SENAS_vrmem_poolz, W3_SENAS_vrmem_poolz, W4_SENAS_vrmem_poolz,
            
            #imaging vars
            Cerebrum_tcv, all_of(c(vols, 
                                   paste0(vols, "_resid"), 
                                   paste0(vols, "_residblom"), 
                                   paste0(vols, "_residzscore"), 
                                   paste0(vols, "_residlog"))), Total_wmh_log,
            age_at_mri, MRI_precovid,
            Landau_All_FSR, Abeta_pos, age_at_pet, PET_precovid, 
            MRI_sample, PET_sample,
            
            #admin
            STUDYID)
    
        #re-merge outcome data, clean other variables
        all_ACE_items_w1<-c("w1_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
                            "w1_ACE_par_remarried", #/*PARENTS REMARRIED*/
                            "w1_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
                            "w1_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                            "w1_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
                            "w1_ACE_par_jail", #/*PARENT TO JAIL*/
                            "w1_ACE_fam_illness") #/*SERIOUS ILLNESS OF FAM MEMBER*/
        #Note not including parent death in count, since it's not in factor score. 
        
        all_ACE_items_w1w4<-c("w1_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
                              "w1_ACE_par_remarried", #/*PARENTS REMARRIED*/
                              "w1_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
                              "w1_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                              "w1_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
                              "w1_ACE_par_jail", #/*PARENT TO JAIL*/
                              "w1_ACE_fam_illness",#/*SERIOUS ILLNESS OF FAM MEMBER*/
                              "w4_ACE_par_physabuse", # /*PHYS ABUSE BY PARENT*/
                              "w4_ACE_hh_mentalill") #/*MENTAL ILLNESS OF FAM MEMBER*/
        #Note not including parent death in count, since it's not in factor score. 
    
        #function to convert to numeric    
        to_num <- function(variable){
          newx <- as.numeric(as.character(variable))
          newx
        }
    
        all_chd_items<-c("w1_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
                         "w1_ACE_par_remarried", #/*PARENTS REMARRIED*/
                         "w1_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
                         "w1_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                         "w1_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
                         "w1_ACE_par_jail", #/*PARENT TO JAIL*/
                         "w1_ACE_fam_illness", #/*SERIOUS ILLNESS OF FAM MEMBER*/
                         "w1_ACE_mom_death", #/*DEATH OF MOTHER*/
                         "w1_ACE_dad_death", #/*DEATH OF FATHER*/
                         
                         
                         "w4_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
                         "w4_ACE_par_remarried", #/*PARENTS REMARRIED*/
                         "w4_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
                         "w4_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                         "w4_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
                         "w4_ACE_par_jail", #/*PARENT TO JAIL*/
                         "w4_ACE_fam_illness", #/*SERIOUS ILLNESS OF FAM MEMBER*/
                         "w4_ACE_mom_death", #/*DEATH OF MOTHER*/
                         "w4_ACE_dad_death", #/*DEATH OF FATHER*/
                         "w4_ACE_par_physabuse", # /*PHYS ABUSE BY PARENT*/
                         "w4_ACE_hh_mentalill" # /*HOUSEHOLD MENTAL ILLNESS*/)
        )
        binary.vars<-c(all_chd_items, "w1_female", "Study", "w1_usborn", "w1_southern_birth")
        
    #Merge together and derive clean variables
        dat_all_imp_1 <- dat_all_imp %>%
          dplyr::select(-c(W1_INTERVIEW_AGE, W1_SENAS_exec_poolz, W1_SENAS_vrmem_poolz))  %>%
          left_join(.,long_merge, by="STUDYID" ) %>% 
          mutate(w1_married = case_when(W1_MARITAL_STATUS %in% c(1,2) ~ 1,
                                        W1_MARITAL_STATUS %in% c(3,4,5,6) ~ 0,
                                        TRUE ~ NA),
                 
                 w1_mom_edu_gt12 = case_when(W1_MATERNAL_EDUCATION %in% c(0,88,99) ~ 0,
                                             W1_MATERNAL_EDUCATION %in% c(1,2,3,4,5) ~ 1,
                                             TRUE ~ NA),
                 w1_dad_edu_gt12 = case_when(W1_PATERNAL_EDUCATION %in% c(0,88,99) ~ 0,
                                             W1_PATERNAL_EDUCATION %in% c(1,2,3,4,5) ~ 1,
                                             TRUE ~ NA),
                 w1_momordad_edu_gt12 = case_when((w1_mom_edu_gt12 ==1 | w1_dad_edu_gt12 ==1)  ~ 1,
                                                  (w1_mom_edu_gt12  == 0 & w1_dad_edu_gt12 ==0) ~ 0,
                                                  TRUE ~ NA),
                 w1_chd_okfinancially = case_when(W1_GROWINGUP_FINANCE %in% c(1,2) ~ 1,
                                                  W1_GROWINGUP_FINANCE %in% c(3,4) ~ 0,
                                                  TRUE ~ NA),
                 w1_chd_everhungry = case_when(W1_GROWINGUP_GOHUNGRY == 1 ~ 0,
                                               W1_GROWINGUP_GOHUNGRY %in% c(2,3,4,5) ~ 1,
                                               TRUE ~ NA),
                 w1_chd_commstanding = case_when(W1_LADDER1 %in% c(1:10) ~ W1_LADDER1,
                                                 TRUE ~ NA), 
                 W1_INCMRANGE_HMNZD = factor(W1_INCMRANGE_HMNZD,
                                             levels=c(1:9),
                                             labels =c("Less than $10k",
                                                       "$10k to less than $15k",
                                                       "$15k to less than $20k",
                                                       "$20k to less than $25k",
                                                       "$25k to less than $35k",
                                                       "$35k to less than $75k",
                                                       "$75k to less than $100k",
                                                       "$100k to less than $125k",
                                                       "$125k or more")),
                 W1_INCMRANGE_HMNZD2 = as.numeric(W1_INCMRANGE_HMNZD),
                 W1_INCMRANGE_HMNZD2 = factor(case_when(W1_INCMRANGE_HMNZD2 %in% (1:5) ~ 1,
                                                      TRUE ~ W1_INCMRANGE_HMNZD2),
                                     levels=c(1, 6:9),
                                     labels =c("Less than $35k",
                                               "$35k to less than $75k",
                                               "$75k to less than $100k",
                                               "$100k to less than $125k",
                                               "$125k or more")),
                 Study = as.character(Study))  %>%
          
          mutate_at(binary.vars[!binary.vars=="Study"],  to_num)  %>% 
          
          mutate(
            w1_total_ACE_count = rowSums(across(all_of(all_ACE_items_w1))),
            w1w4_total_ACE_count = rowSums(across(all_of(all_ACE_items_w1w4)))
          )
    
    #Check income recode:
       table(dat_all_imp_1$W1_INCMRANGE_HMNZD, dat_all_imp_1$W1_INCMRANGE_HMNZD2, exclude=NULL)
    
    
    ##--Create factor scores for each imputation-------
          for (i in 1:20) {
            #i<-1
            dat<-dat_all_imp_1[dat_all_imp_1$`.imp`==i,]
            
            #Wave 1 items only
            mod1<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + w1_ACE_see_domviol + 
                              w1_ACE_fam_substance + w1_ACE_par_jobloss + w1_ACE_par_jail + 
                              w1_ACE_fam_illness
                  
                          w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried'
            
            ACE_CFA1<-cfa(model=mod1, data=dat, ordered=T)
            dat$ACE_fac1<-predict(ACE_CFA1, newdata=dat) %>% as.numeric()
            dat$ACE_fac1_z<-scale(dat$ACE_fac1) %>% as.numeric()
            dat$ACE_fac1_z_gtmedian<-ifelse(dat$ACE_fac1>median(dat$ACE_fac1),1,0)
            
            
            #Wave 1 and new Wave 4 items
            mod2<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + w1_ACE_see_domviol + 
                              w1_ACE_fam_substance + w1_ACE_par_jobloss + w1_ACE_par_jail + 
                              w1_ACE_fam_illness + w4_ACE_par_physabuse + w4_ACE_hh_mentalill
                  
                          w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried'
            
            ACE_CFA2<-cfa(model=mod2, data=dat, ordered=T)
            dat$ACE_fac2<-predict(ACE_CFA2, newdata=dat) %>% as.numeric()
            dat$ACE_fac2_z<-scale(dat$ACE_fac2) %>% as.numeric()
            
            if (i==1){dat_all_imp_2<-bind_rows(dat_all_imp_1[dat_all_imp_1$`.imp`==0,],dat)} else {
              dat_all_imp_2<-bind_rows(dat_all_imp_2,dat)
            }
            
          } 

###################    
# Creating long data to get n waves and max study time
################### 

      #Making a vector of variables for long pivot
      vars <- c(
        "SENAS_exec_poolz",
        "SENAS_vrmem_poolz",
        "SENAS_telephone",
        "INTERVIEW_AGE")

      cols <-
        paste0("W", apply(expand_grid(seq(1, 4), vars) %>% arrange(vars), 1, paste,
                          collapse = "_"))

      #pivot long for variables that vary by wave
      imp_long <- dat_all_imp_2 %>%
        dplyr::select(STUDYID, .imp, tidyselect::all_of(cols)) %>%
        pivot_longer(
          cols = !c(STUDYID, .imp),
          names_to = c("wave", ".value"),
          names_pattern = ".(.)_(.*)"
        ) %>% filter(!is.na(INTERVIEW_AGE) & !(is.na(SENAS_exec_poolz) & is.na(SENAS_vrmem_poolz)) )
      #Note to revisit removing missing age with new data release

      # combining baseline data onto long dataset
      imp_long2 <-
        left_join(imp_long,
                  dat_all_imp_2,
                  by = c('STUDYID', '.imp')) %>%
        group_by(STUDYID, .imp) %>%
        mutate(
          #making an interview age variable that is easier to manage across restricted
          #datasets
          INTERVIEW_AGE_c75 = (INTERVIEW_AGE - 75),

          #getting age at first interview and centering
          first_visit = case_when(wave == min(wave, na.rm = T) ~ 1,
                                  TRUE ~ 0),
          W1_INTERVIEW_AGE_c75 = (W1_INTERVIEW_AGE - 75),

          #calculating studytime
          study_time = INTERVIEW_AGE - W1_INTERVIEW_AGE,
          max_study_time = max(study_time, na.rm = T),

          #recoding mode effects as 1/0 instead of 1/2
          phone = as.factor(case_when(
            SENAS_telephone == 'Y' ~ 1,
            SENAS_telephone %in% c('N','') ~ 0
          )),
          n_waves = n()

        ) %>%
        ungroup()


      #Calculate offset for balanced W1 and W2 data
          dat_offsets<-imp_long2 %>% filter(wave %in% c(1:2),
                                            !is.na(W2_SENAS_exec_poolz),
                                            !is.na(W2_SENAS_vrmem_poolz),
                                            .imp==0,
                                            SENAS_telephone!="Y")
    
          table(dat_offsets$SENAS_telephone, dat_offsets$wave, exclude=NULL)
    
          dat_off2<-dat_offsets %>% filter(wave==2) %>% mutate(mixid=paste0(Study,STUDYID))
    
          dat_off1<-dat_offsets %>% filter(wave==1) %>%
            mutate(mixid=paste0(Study,STUDYID)) %>%
            filter(mixid %in% (dat_off2$mixid))
    
          dat_offsets<-bind_rows(dat_off1, dat_off2)
    
    
          #first with KHANDLE as ref
          exec_mod_k<-lmer( SENAS_exec_poolz ~ INTERVIEW_AGE_c75 +
                              w1_female + W1_D_RACE_SUMMARY + w1_edu_yrs_cert +
                              first_visit + Study + first_visit*Study +
                              (1 |  mixid),data=dat_offsets)
    
          temp<-coef(summary(exec_mod_k)) %>% data.frame()
          temp$var<-rownames(temp)
    
          if (temp$`Pr...t..`[temp$var=="first_visit"] <0.05) {
            exec_offset_khandle<-round(temp$Estimate[temp$var=="first_visit"],2)
          } else     {exec_offset_khandle<-0}
    
    
          vrmem_mod_k<-lmer(SENAS_vrmem_poolz ~ INTERVIEW_AGE_c75 +
                              w1_female + W1_D_RACE_SUMMARY + w1_edu_yrs_cert +
                              first_visit + Study + first_visit*Study +
                              (1 |  STUDYID),data=dat_offsets)
          temp<-coef(summary(vrmem_mod_k)) %>% data.frame()
          temp$var<-rownames(temp)
    
          if (temp$`Pr...t..`[temp$var=="first_visit"] <0.05) {
            vrmem_offset_khandle<-round(temp$Estimate[temp$var=="first_visit"],2)
          } else     {vrmem_offset_khandle<-0}
    
          #Change ref for study to get STAR with CI
          dat_offsets$Study<-factor(dat_offsets$Study, levels=c("STAR", "KHANDLE"))
          exec_mod_s<-lmer(                 SENAS_exec_poolz ~ INTERVIEW_AGE_c75 +
                                              w1_female + W1_D_RACE_SUMMARY + w1_edu_yrs_cert +
                                              first_visit + Study + first_visit*Study +
                                              (1 |  STUDYID),data=dat_offsets)
    
          temp<-coef(summary(exec_mod_s)) %>% data.frame()
          temp$var<-rownames(temp)
    
          if (temp$`Pr...t..`[temp$var=="first_visit"] <0.05) {
            exec_offset_star<-round(temp$Estimate[temp$var=="first_visit"],2)
          } else     {exec_offset_star<-0}
    
    
          vrmem_mod_s<-lmer(SENAS_vrmem_poolz ~ INTERVIEW_AGE_c75 +
                              w1_female + W1_D_RACE_SUMMARY + w1_edu_yrs_cert +
                              first_visit + Study + first_visit*Study +
                              (1 |  STUDYID),data=dat_offsets)
    
          temp<-coef(summary(vrmem_mod_s)) %>% data.frame()
          temp$var<-rownames(temp)
    
          if (temp$`Pr...t..`[temp$var=="first_visit"] <0.05) {
            vrmem_offset_star<-round(temp$Estimate[temp$var=="first_visit"],2)
          } else     {vrmem_offset_star<-0}
    
    
          #Include offsets in the long dataset
          imp_long2<-imp_long2 %>% mutate(
            #deriving offset variable for first visit
            offset_exec = case_when(first_visit == 1 & Study == "STAR" ~ exec_offset_star,
                                    first_visit == 1 & Study == "KHANDLE" ~ exec_offset_khandle,
                                    first_visit != 1 ~ 0),
            offset_vrmem = case_when(first_visit == 1 & Study == "STAR"  ~ vrmem_offset_star,
                                     first_visit == 1 & Study == "KHANDLE" ~ vrmem_offset_khandle,
                                     first_visit != 1 ~ 0))


          #Check if people are missing both outcomes 
          nrow(imp_long2 %>% filter(is.na(SENAS_exec_poolz) & is.na(SENAS_vrmem_poolz) )) #0 rows
          nrow(imp_long2 %>% filter(is.na(SENAS_exec_poolz) )) # 0 rows
          nrow(imp_long2 %>% filter(is.na(SENAS_vrmem_poolz) )) # 504 rows (24/imputation)
          
      #----turning tall dataframe back into mids object----
      #Note: You will NEED a unique identifier AND the original dataset in the stacked
      #dataset for this code to work!
          imp_long_final <- imp_long2 %>%
          mutate(new_id = paste0(STUDYID, wave, .imp))
      
          length(unique(imp_long_final$new_id))
          #144837 unique id's for imputation; should match total num obs
          
    
          dat_all_imp_final_df_long <-
            imp_long_final[order(imp_long_final$.imp,
                                 imp_long_final$new_id),]
          
          dat_all_imp_final_mids_long <-
            as.mids(dat_all_imp_final_df_long,
                    .imp = ".imp",
                    .id = "new_id")
      
      #Extract one row per person to merge n_waves and max_study_time onto wide data
          dat_merge_nwav<-dat_all_imp_final_df_long %>% filter(.imp==0) %>%
            group_by(STUDYID) %>%
            select(STUDYID, n_waves, max_study_time) %>% slice_head()


###################    
# Finalizing wide post-imputation data (df and MIDS)
################### 
          
    #Merge on n waves and max_study_time          
      dat_all_imp_final_df<-left_join(dat_all_imp_2, dat_merge_nwav, by="STUDYID") %>% 
                                mutate(new_id = paste0(STUDYID, .imp))
    
    dat_all_imp_final_mids<-as.mids(dat_all_imp_final_df, .imp = ".imp",
                                    .id = "new_id")
    
    
    dat_mri_imp_final_df<-dat_all_imp_final_df %>% filter(., MRI_sample==1)
    dat_mri_imp_final_mids<-filter(dat_all_imp_final_mids, MRI_sample==1)
    
    dat_pet_imp_final_df<-dat_all_imp_final_df %>% filter(., PET_sample==1)
    dat_pet_imp_final_mids<-filter(dat_all_imp_final_mids, PET_sample==1)


###################    
# Saving post-imputation data
################### 
    #save wide cog files 
    save(dat_all_imp_final_df, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_df.Rdata")
    save(dat_all_imp_final_mids, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mids.Rdata")
    
    #save wide mri files
    save(dat_mri_imp_final_df, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mri_df.Rdata")
    save(dat_mri_imp_final_mids, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mri_mids.Rdata")
    
    #save wide pet files 
    save(dat_pet_imp_final_df, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_pet_df.Rdata")
    save(dat_pet_imp_final_mids, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_pet_mids.Rdata")
    
    #save long cog files 
    save(dat_all_imp_final_df_long, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_df_long.Rdata")
    save(dat_all_imp_final_mids_long, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_mids_long.Rdata")
