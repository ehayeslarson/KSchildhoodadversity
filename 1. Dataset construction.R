library(haven)
library(readxl)
library(tidyverse)
library(rcompanion)

#This is a switch for whether the data being read in is PHI data or not that the KP analyst needs to update before running:
PHIdata<-F

#load data
K_raw<-read_sas("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/khandle_all_waves_20230912.sas7bdat")
K_mri<-read_excel("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/khandle_T1_analysis_20230628_age.xlsx")
K_pet<-read_excel("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/khandle_PET_analysis_20230628_age.xlsx")
  
S_raw<-read_sas("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/star_all_waves_20220309.sas7bdat")
S_mri<-read_excel("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/k-star_T1_analysis_052322_age.xlsx")

#Update age variable names if PHI data
if(PHIdata){
  K_raw$W1_INTERVIEW_AGE<-K_raw$W1_INTERVIEW_AGE_PHI
  K_raw$W2_INTERVIEW_AGE<-K_raw$W2_INTERVIEW_AGE_PHI
  K_raw$W3_INTERVIEW_AGE<-K_raw$W3_INTERVIEW_AGE_PHI
  K_raw$W4_INTERVIEW_AGE<-K_raw$W4_INTERVIEW_AGE_PHI
  
  S_raw$W1_INTERVIEW_AGE<-S_raw$W1_INTERVIEW_AGE_PHI
  S_raw$W2_INTERVIEW_AGE<-S_raw$W2_INTERVIEW_AGE_PHI
  S_raw$W3_INTERVIEW_AGE<-S_raw$W3_INTERVIEW_AGE_PHI
  
  #Placeholders for if there are PHI versions of imaging age variables
  #K_mri$Age_at_Date<-K_mri$Age_at_Date_PHI
  #K_pet$Age_at_Date<-K_pet$Age_at_Date_PHI
  #S_mri$Age_at_Date<-S_mri$Age_at_Date_PHI
  }


#Merge all khandle data, sorted by studyid and date and keeping first scan for each person
K_mri$STUDYID<-K_mri$subject
K_pet$STUDYID<-K_pet$subject
K_raw$STUDYID<-paste0("K",K_raw$STUDYID)

K_mri2<-K_mri %>% select(-subject, -Study, -Status) %>% 
  rename(age_at_mri=Age_at_Date, MRI_precovid=Date_Precovid) %>% 
  group_by(STUDYID) %>% arrange(age_at_mri) %>% filter(row_number()==1) %>% ungroup()

K_pet2<-K_pet %>% select(-subject, -Study, -Status) %>% 
  rename(age_at_pet=Age_at_Date, PET_precovid=Date_Precovid) %>% 
  group_by(STUDYID) %>% arrange(age_at_pet) %>% filter(row_number()==1) %>% ungroup()

K<-left_join(K_raw, K_mri2, by="STUDYID") %>% 
  left_join(.,K_pet2,by="STUDYID") %>% mutate(Study="KHANDLE")
      
#merge all STAR data
S_mri$STUDYID<-S_mri$Subject
S_raw$STUDYID<-paste0("KS",S_raw$STUDYID)


S_mri2<-S_mri %>% select(-Subject, -Study, -Status) %>% 
  rename(age_at_mri=Age_at_Date, MRI_precovid=Date_Precovid)

S<-left_join(S_raw, S_mri2, by="STUDYID") %>% mutate(Study="STAR",
                                                     W1_INCMRANGE_HMNZD = case_when(W1_INCOME_RANGE %in% c(1:5) ~ W1_INCOME_RANGE,
                                                                                                 W1_INCOME_RANGE %in% c(6:9) ~ 6,
                                                                                                 W1_INCOME_RANGE == 10 ~ 7,
                                                                                                 W1_INCOME_RANGE == 11 ~ 8,
                                                                                                 W1_INCOME_RANGE %in% c(12:13) ~ 9
)) %>% 
  rename(W2_SENAS_telephone=W2_SENAS_Telephone, W3_SENAS_telephone=W3_SENAS_Telephone) %>% 
  mutate(W1_SENAS_telephone="N") #Creating a W1 telephone variable to harmonize with KHANDLE

#Raw variables to keep

admin<-c("Study", "STUDYID")

chd_items<-c("W1_CHILDHX_EVENTS_YN_1", #/*PARENTS SEP OR DIVORCED*/
               "W1_CHILDHX_EVENTS_YN_2", #/*PARENTS REMARRIED*/
               "W1_CHILDHX_EVENTS_YN_3", #/*WITNESS DOMESTIC VIOLENCE*/
               "W1_CHILDHX_EVENTS_YN_4", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
               "W1_CHILDHX_EVENTS_YN_5", #/*LOSS OF JOB BY PARENT*/
               "W1_CHILDHX_EVENTS_YN_6", #/*PARENT TO JAIL*/
               "W1_CHILDHX_EVENTS_YN_7", #/*SERIOUS ILLNESS OF FAM MEMBER*/
               "W1_CHILDHX_EVENTS_YN_8", #/*DEATH OF MOTHER*/
               "W1_CHILDHX_EVENTS_YN_9" #/*DEATH OF FATHER*/
             )

# adt_items<-c("W1_HEAD_INJURY_EXPLOSION", 
#              "W1_HEAD_INJURY_HOSP1", 
#              "W1_HEAD_INJURY_HOSP2", 
#              "W1_HEAD_INJURY_HOSP3", 
#              "W1_HEAD_INJURY_HOSP4", 
#              "W1_SPOUSE_DIED", 
#              #"W1_FAMILY_ILLNESS", DERIVED VARIABLE from:
#                     "W1_EMP_DIDNT_WORK",
#                     "W1_REASON_NOT_WORK_3",
#              "W1_DIVORCE")

covariates<-c("W1_INTERVIEW_AGE","W2_INTERVIEW_AGE","W3_INTERVIEW_AGE",
              "W1_SENAS_telephone", "W2_SENAS_telephone", "W3_SENAS_telephone", 
              
              "W1_D_GENDER", "W1_D_RACE_SUMMARY", "W1_MARITAL_STATUS",
              
              "W1_MATERNAL_EDUCATION", "W1_PATERNAL_EDUCATION", 
 #             "W1_MATERNAL_EDUCATION_TEXT", "W1_PATERNAL_EDUCATION_TEXT", #add back once not PHI
              
              "W1_US_STATE", "W1_COUNTRY_BORN",
             # "W1_PA_HVY_WRK", "W1_PA_VIG_EX", "W1_PA_VIG_HSE",
              
              "W1_GROWINGUP_FINANCE",
              "W1_GROWINGUP_GOHUNGRY",
              "W1_LADDER1", 
 
              "W1_INCMRANGE_HMNZD",
 
              "W1_EDU_EDUCATION", "W1_EDU_EDUCATION_TEXT", 
                "W1_EDU_LONGCERT", "W1_EDU_TRNCERT", 
              "W1_HEALTH"
 )

              
outcomes<-c("W1_SENAS_exec", "W1_SENAS_vrmem",
            "W2_SENAS_exec", "W2_SENAS_vrmem",
            "W3_SENAS_exec", "W3_SENAS_vrmem")     
  
imaging<-c("Cerebrum_tcv", "Cerebrum_tcb", "Total_hippo", "Total_gray", "Total_white", 
           "Total_wmh", "Total_brain",   "Frontal_Cortical", "Occipital_Cortical", 
           "Parietal_Cortical", "Temporal_Cortical",
          "age_at_mri", "MRI_precovid")


khandle_only<-c("Landau_All_FSR", "age_at_pet", "PET_precovid", #PET data
                "W4_INTERVIEW_AGE", "W1_RES_1_4_STATE_RGN",
                "W4_SENAS_exec", "W4_SENAS_vrmem","W4_SENAS_telephone",
                
                "W1_CHILDHX_EVENTS_YN_10",# /*PHYS ABUSE BY PARENT*/
                "W1_CHILDHX_EVENTS_YN_11", # /*HOUSEHOLD MENTAL ILLNESS*/    
                
                "W4_CHILDHX_EVENTS_YN_1", #/*PARENTS SEP OR DIVORCED*/
                "W4_CHILDHX_EVENTS_YN_2", #/*PARENTS REMARRIED*/
                "W4_CHILDHX_EVENTS_YN_3", #/*WITNESS DOMESTIC VIOLENCE*/
                "W4_CHILDHX_EVENTS_YN_4", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                "W4_CHILDHX_EVENTS_YN_5", #/*LOSS OF JOB BY PARENT*/
                "W4_CHILDHX_EVENTS_YN_6", #/*PARENT TO JAIL*/
                "W4_CHILDHX_EVENTS_YN_7", #/*SERIOUS ILLNESS OF FAM MEMBER*/
                "W4_CHILDHX_EVENTS_YN_8", #/*DEATH OF MOTHER*/
                "W4_CHILDHX_EVENTS_YN_9", #/*DEATH OF FATHER*/
                "W4_CHILDHX_EVENTS_YN_10", # /*PHYS ABUSE BY PARENT*/
                "W4_CHILDHX_EVENTS_YN_11" # /*HOUSEHOLD MENTAL ILLNESS*/

                # "W4_SQX_TLE_ATTCK",
                # "W4_SQX_TLE_CHDIE",
                # "W4_SQX_TLE_CMBT",
                # "W4_SQX_TLE_DSTR",
                # "W4_SQX_TLE_FMADD",
                # "W4_SQX_TLE_FMILL",
                # "W4_SQX_TLE_ILL"
            ) #anything related to wave 4
                          

#Filter to raw variables we want to keep and combine data from KHANDLE (cycle 1 cohort) and STAR
K2<-K %>% filter(COHORT==1)  %>% 
  select (all_of(c(admin, chd_items, outcomes, covariates, imaging, khandle_only)))

S2<-S %>% select (all_of(c(admin, chd_items, outcomes, covariates, imaging)))

#Define median between-wave intervals
W1W2interval_K<-K2 %>% filter(W1_INTERVIEW_AGE<89.99, W2_INTERVIEW_AGE<89.99) %>% 
                        mutate(W1W2interval = W2_INTERVIEW_AGE-W1_INTERVIEW_AGE) %>% 
                          summarise(mean(W1W2interval))  %>% as.numeric()
W1W2interval_S<-S2 %>% filter(W1_INTERVIEW_AGE<89.99, W2_INTERVIEW_AGE<89.99) %>% 
                        mutate(W1W2interval = W2_INTERVIEW_AGE-W1_INTERVIEW_AGE) %>% 
                        summarise(mean(W1W2interval))  %>% as.numeric()
W2W3interval_K<-K2 %>% filter(W2_INTERVIEW_AGE<89.99, W3_INTERVIEW_AGE<89.99) %>% 
                        mutate(W2W3interval = W3_INTERVIEW_AGE-W2_INTERVIEW_AGE) %>% 
                        summarise(mean(W2W3interval))  %>% as.numeric()
W2W3interval_S<-S2 %>% filter(W2_INTERVIEW_AGE<89.99, W3_INTERVIEW_AGE<89.99) %>% 
                        mutate(W2W3interval = W3_INTERVIEW_AGE-W2_INTERVIEW_AGE) %>% 
                        summarise(mean(W2W3interval))  %>% as.numeric()
W3W4interval_K<-K2 %>% filter(W3_INTERVIEW_AGE<89.99, W4_INTERVIEW_AGE<89.99) %>% 
                        mutate(W3W4interval = W4_INTERVIEW_AGE-W3_INTERVIEW_AGE) %>% 
                        summarise(mean(W3W4interval))  %>% as.numeric()
W1W3interval_K<-K2 %>% filter(W1_INTERVIEW_AGE<89.99, W3_INTERVIEW_AGE<89.99) %>% 
                        mutate(W1W3interval = W3_INTERVIEW_AGE-W1_INTERVIEW_AGE) %>% 
                        summarise(mean(W1W3interval))  %>% as.numeric()
W1W3interval_S<-S2 %>% filter(W1_INTERVIEW_AGE<89.99, W3_INTERVIEW_AGE<89.99) %>% 
                        mutate(W1W3interval = W3_INTERVIEW_AGE-W1_INTERVIEW_AGE) %>% 
                        summarise(mean(W1W3interval))  %>% as.numeric()
W1W4interval_K<-K2 %>% filter(W1_INTERVIEW_AGE<89.99, W4_INTERVIEW_AGE<89.99) %>% 
                        mutate(W1W4interval = W4_INTERVIEW_AGE-W1_INTERVIEW_AGE) %>% 
                        summarise(mean(W1W4interval))  %>% as.numeric()
W2W4interval_K<-K2 %>% filter(W2_INTERVIEW_AGE<89.99, W4_INTERVIEW_AGE<89.99) %>% 
                        mutate(W2W4interval = W4_INTERVIEW_AGE-W2_INTERVIEW_AGE) %>% 
                        summarise(mean(W2W4interval))  %>% as.numeric()

#Keep people of 4 larger R/E groups, non-missing exec and non-missing vrmem measure at W1, 
#and fix top-coded ages (topcoded at W1==90, else add medians above.)
all_dat<-bind_rows(K2, S2) %>% filter(W1_D_RACE_SUMMARY %in% 
                                        c("Asian", "Black", "LatinX", "White"), 
                                      !is.na(W1_SENAS_exec) & !is.na(W1_SENAS_vrmem)) 


#If de-ID data, fix top-coded ages (topcoded at W1==90, else add medians above.)
if(!PHIdata){all_dat <- all_dat%>%
                          mutate(W1_INTERVIEW_AGE = case_when(W1_INTERVIEW_AGE >89.99 ~ 90,
                                                              TRUE ~ W1_INTERVIEW_AGE),
                                 W2_INTERVIEW_AGE = case_when(W2_INTERVIEW_AGE >89.99  & 
                                                                !is.na(W1_INTERVIEW_AGE) & 
                                                                Study=="KHANDLE" ~ (W1_INTERVIEW_AGE + W1W2interval_K),
                                                              W2_INTERVIEW_AGE >89.99 & 
                                                                !is.na(W1_INTERVIEW_AGE) & 
                                                                Study=="STAR" ~ (W1_INTERVIEW_AGE + W1W2interval_S),
                                                              TRUE ~ W2_INTERVIEW_AGE),
                                 W3_INTERVIEW_AGE = case_when(W3_INTERVIEW_AGE >89.99 & 
                                                                !is.na(W2_INTERVIEW_AGE) &  
                                                                Study=="KHANDLE" ~ (W2_INTERVIEW_AGE + W2W3interval_K),
                                                              W3_INTERVIEW_AGE >89.99 & 
                                                                !is.na(W2_INTERVIEW_AGE) & 
                                                                Study=="STAR" ~ (W2_INTERVIEW_AGE + W2W3interval_S),
                                                              W3_INTERVIEW_AGE >89.99 & 
                                                                is.na(W2_INTERVIEW_AGE) &  
                                                                Study=="KHANDLE" ~ (W1_INTERVIEW_AGE + W1W3interval_K),
                                                              W3_INTERVIEW_AGE >89.99 & 
                                                                is.na(W2_INTERVIEW_AGE) & 
                                                                Study=="STAR" ~ (W1_INTERVIEW_AGE + W1W3interval_S),
                                                              TRUE ~ W3_INTERVIEW_AGE),
                                 W4_INTERVIEW_AGE = case_when(W4_INTERVIEW_AGE >89.99 & 
                                                                !is.na(W3_INTERVIEW_AGE) & 
                                                                Study=="KHANDLE" ~ (W3_INTERVIEW_AGE + W3W4interval_K),
                                                              W4_INTERVIEW_AGE >89.99 & 
                                                                is.na(W3_INTERVIEW_AGE) & 
                                                                !is.na(W2_INTERVIEW_AGE) & 
                                                                Study=="KHANDLE" ~ (W2_INTERVIEW_AGE + W2W4interval_K),
                                                              W4_INTERVIEW_AGE >89.99 & 
                                                                is.na(W3_INTERVIEW_AGE) & 
                                                                is.na(W2_INTERVIEW_AGE) & 
                                                                Study=="KHANDLE" ~ (W1_INTERVIEW_AGE + W1W4interval_K),
                                                              TRUE ~ W4_INTERVIEW_AGE))
}
  


#Data cleaning

#Define baseline means and SD for z-scorign cognitive variables
W1_SENAS_exec_mean<-mean(all_dat$W1_SENAS_exec)
W1_SENAS_exec_sd<-sd(all_dat$W1_SENAS_exec)

W1_SENAS_vrmem_mean<-mean(all_dat$W1_SENAS_vrmem)
W1_SENAS_vrmem_sd<-sd(all_dat$W1_SENAS_vrmem)

#Exposure variables
#Reecode to 1, 0 and rename more descriptively
all_chd_items<-c("w1_ACE_par_sepdiv", #/*PARENTS SEP OR DIVORCED*/
                 "w1_ACE_par_remarried", #/*PARENTS REMARRIED*/
                 "w1_ACE_see_domviol", #/*WITNESS DOMESTIC VIOLENCE*/
                 "w1_ACE_fam_substance", #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                 "w1_ACE_par_jobloss", #/*LOSS OF JOB BY PARENT*/
                 "w1_ACE_par_jail", #/*PARENT TO JAIL*/
                 "w1_ACE_fam_illness", #/*SERIOUS ILLNESS OF FAM MEMBER*/
                 "w1_ACE_mom_death", #/*DEATH OF MOTHER*/
                 "w1_ACE_dad_death", #/*DEATH OF FATHER*/
                 "w1_ACE_par_physabuse",# /*PHYS ABUSE BY PARENT*/
                 "w1_ACE_hh_mentalill", # /*HOUSEHOLD MENTAL ILLNESS*/    
                 
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

#Recode 1/2 to 1/0 for MI, and derive US born, southern birth, and education years

all_dat2 <- all_dat %>% 
            mutate(w1_ACE_par_sepdiv    = W1_CHILDHX_EVENTS_YN_1, #/*PARENTS SEP OR DIVORCED*/
                   w1_ACE_par_remarried = W1_CHILDHX_EVENTS_YN_2,  #/*PARENTS REMARRIED*/
                   w1_ACE_see_domviol   = W1_CHILDHX_EVENTS_YN_3, #/*WITNESS DOMESTIC VIOLENCE*/
                   w1_ACE_fam_substance = W1_CHILDHX_EVENTS_YN_4,  #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                   w1_ACE_par_jobloss   = W1_CHILDHX_EVENTS_YN_5, #/*LOSS OF JOB BY PARENT*/
                   w1_ACE_par_jail      = W1_CHILDHX_EVENTS_YN_6, #/*PARENT TO JAIL*/
                   w1_ACE_fam_illness   = W1_CHILDHX_EVENTS_YN_7, #/*SERIOUS ILLNESS OF FAM MEMBER*/
                   w1_ACE_mom_death     = W1_CHILDHX_EVENTS_YN_8, #/*DEATH OF MOTHER*/
                   w1_ACE_dad_death     = W1_CHILDHX_EVENTS_YN_9, #/*DEATH OF FATHER*/
                   w1_ACE_par_physabuse = W1_CHILDHX_EVENTS_YN_10, # /*PHYS ABUSE BY PARENT*/
                   w1_ACE_hh_mentalill  = W1_CHILDHX_EVENTS_YN_11, # /*HOUSEHOLD MENTAL ILLNESS*/    
                   
                   w4_ACE_par_sepdiv    = W4_CHILDHX_EVENTS_YN_1, #/*PARENTS SEP OR DIVORCED*/
                   w4_ACE_par_remarried = W4_CHILDHX_EVENTS_YN_2,  #/*PARENTS REMARRIED*/
                   w4_ACE_see_domviol   = W4_CHILDHX_EVENTS_YN_3, #/*WITNESS DOMESTIC VIOLENCE*/
                   w4_ACE_fam_substance = W4_CHILDHX_EVENTS_YN_4,  #/*SUBSTANCE ABUSE BY FAM MEMBER*/
                   w4_ACE_par_jobloss   = W4_CHILDHX_EVENTS_YN_5, #/*LOSS OF JOB BY PARENT*/
                   w4_ACE_par_jail      = W4_CHILDHX_EVENTS_YN_6, #/*PARENT TO JAIL*/
                   w4_ACE_fam_illness   = W4_CHILDHX_EVENTS_YN_7, #/*SERIOUS ILLNESS OF FAM MEMBER*/
                   w4_ACE_mom_death     = W4_CHILDHX_EVENTS_YN_8, #/*DEATH OF MOTHER*/
                   w4_ACE_dad_death     = W4_CHILDHX_EVENTS_YN_9, #/*DEATH OF FATHER*/
                   w4_ACE_par_physabuse = W4_CHILDHX_EVENTS_YN_10, # /*PHYS ABUSE BY PARENT*/
                   w4_ACE_hh_mentalill  = W4_CHILDHX_EVENTS_YN_11, # /*HOUSEHOLD MENTAL ILLNESS*/    
                   )  %>%   

            mutate_at(all_chd_items,  ~ if_else(. == 1, 1, 
                                                ifelse(. == 2, 0, 
                                                       ifelse(. %in% c(77, 88, 99), NA, .)
                      ))) %>%

  #Covariates 
  mutate(w1_female = case_when(W1_D_GENDER==2 ~ 1,
                               W1_D_GENDER==1 ~ 0,
                               TRUE ~ NA),
         
         W1_MATERNAL_EDUCATION = case_when(W1_MATERNAL_EDUCATION %in% c(66,77,88,99) ~ 0,
                                           TRUE ~ W1_MATERNAL_EDUCATION),
         W1_PATERNAL_EDUCATION = case_when(W1_PATERNAL_EDUCATION %in% c(66,77,88,99) ~ 0,
                                            TRUE ~ W1_PATERNAL_EDUCATION),
         
                
  ) %>% #recode missings
  mutate_at(covariates,  ~ ifelse(. %in% c(77, 88, 99), NA, .)) %>%
  
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
         w1_usborn = case_when((W1_COUNTRY_BORN %in% c(1,77,88,99) & W1_US_STATE != "") ~ 1,
                               W1_COUNTRY_BORN %in% c(2,3,4,5,6,7,8,9,10,11,12,
                                                      13,14,15,16,17,18,19,20,
                                                      21,22,23,24,25,26,27) ~ 0,
                               TRUE ~ NA),
         w1_southern_birth = case_when((W1_COUNTRY_BORN %in% c(1,77,88,99) &
                                          W1_US_STATE %in% c("US-DE", "US-DC", "US-FL",
                                                             "US-GA", "US-MD", "US-NC",
                                                             "US-SC", "US-VA", "US-WV",
                                                             "US-AL", "US-KY", "US-MS",
                                                             "US-TN", "US-AR", "US-LA",
                                                             "US-OK", "US-TX")) ~ 1,
                                       (W1_COUNTRY_BORN %in% c(1,77,88,99) & W1_US_STATE != "" &
                                          W1_US_STATE != 88 & W1_US_STATE != 99) ~ 0,
                                       W1_COUNTRY_BORN %in% c(2,3,4,5,6,7,8,9,10,11,12,
                                                              13,14,15,16,17,18,19,20,
                                                              21,22,23,24,25,26,27) ~ 0,
                                       TRUE ~ NA,
         ),
         w1_chd_okfinancially = case_when(W1_GROWINGUP_FINANCE %in% c(1,2) ~ 1,
                                          W1_GROWINGUP_FINANCE %in% c(3,4) ~ 0,
                                          TRUE ~ NA),
         w1_chd_everhungry = case_when(W1_GROWINGUP_GOHUNGRY == 1 ~ 0,
                                       W1_GROWINGUP_GOHUNGRY %in% c(2,3,4,5) ~ 1,
                                       TRUE ~ NA),
         w1_chd_commstanding = case_when(W1_LADDER1 %in% c(1:10) ~ W1_LADDER1,
                                         TRUE ~ NA),
           #----****total years of education----
           #code is from CWE
           edu_yrs = case_when(
             !is.na(W1_EDU_EDUCATION_TEXT) ~ W1_EDU_EDUCATION_TEXT,
             W1_EDU_EDUCATION == 1 ~ 13,
             W1_EDU_EDUCATION == 2 ~ 14,
             W1_EDU_EDUCATION == 3 ~ 16,
             W1_EDU_EDUCATION == 4 ~ 18,
             W1_EDU_EDUCATION == 5 ~ 20,
             TRUE ~ NA
           ),
           
           cert_flag = case_when(W1_EDU_TRNCERT == 2 &
                                   W1_EDU_LONGCERT == 4 ~ 1,
                                 TRUE ~ 0),
           
           w1_edu_yrs_cert = case_when(
             edu_yrs <= 12 & !is.na(cert_flag) ~ edu_yrs + cert_flag,
             TRUE ~ edu_yrs
           ),
         
 #Z-score cognitive outcomes to pooled sample        
         W1_SENAS_exec_poolz = scale(W1_SENAS_exec, 
                                      center=W1_SENAS_exec_mean, 
                                      scale=W1_SENAS_exec_sd) %>% as.numeric(),
         
         W2_SENAS_exec_poolz = scale(W2_SENAS_exec, 
                                     center=W1_SENAS_exec_mean, 
                                     scale=W1_SENAS_exec_sd) %>% as.numeric(),
         
         W3_SENAS_exec_poolz = scale(W3_SENAS_exec, 
                                     center=W1_SENAS_exec_mean, 
                                     scale=W1_SENAS_exec_sd) %>% as.numeric(),
         W4_SENAS_exec_poolz = scale(W4_SENAS_exec, 
                                     center=W1_SENAS_exec_mean, 
                                     scale=W1_SENAS_exec_sd) %>% as.numeric(),
         
         W1_SENAS_vrmem_poolz = scale(W1_SENAS_vrmem, 
                                     center=W1_SENAS_vrmem_mean, 
                                     scale=W1_SENAS_vrmem_sd) %>% as.numeric(),
         
         W2_SENAS_vrmem_poolz = scale(W2_SENAS_vrmem, 
                                     center=W1_SENAS_vrmem_mean, 
                                     scale=W1_SENAS_vrmem_sd) %>% as.numeric(),
         
         W3_SENAS_vrmem_poolz = scale(W3_SENAS_vrmem, 
                                     center=W1_SENAS_vrmem_mean, 
                                     scale=W1_SENAS_vrmem_sd) %>% as.numeric(),
         W4_SENAS_vrmem_poolz = scale(W4_SENAS_vrmem, 
                                     center=W1_SENAS_vrmem_mean, 
                                     scale=W1_SENAS_vrmem_sd) %>% as.numeric()
         
  )
         
#Check all variable cleaning
table(all_dat2$W1_CHILDHX_EVENTS_YN_1, all_dat2$w1_ACE_par_sepdiv, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_2, all_dat2$w1_ACE_par_remarried, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_3, all_dat2$w1_ACE_see_domviol, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_4, all_dat2$w1_ACE_fam_substance, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_5, all_dat2$w1_ACE_par_jobloss, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_6, all_dat2$w1_ACE_par_jail, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_7, all_dat2$w1_ACE_fam_illness, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_8, all_dat2$w1_ACE_mom_death, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_9, all_dat2$w1_ACE_dad_death, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_10, all_dat2$w1_ACE_par_physabuse, exclude=NULL)
table(all_dat2$W1_CHILDHX_EVENTS_YN_11, all_dat2$w1_ACE_hh_mentalill, exclude=NULL)

table(all_dat2$W4_CHILDHX_EVENTS_YN_1, all_dat2$w4_ACE_par_sepdiv, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_2, all_dat2$w4_ACE_par_remarried, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_3, all_dat2$w4_ACE_see_domviol, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_4, all_dat2$w4_ACE_fam_substance, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_5, all_dat2$w4_ACE_par_jobloss, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_6, all_dat2$w4_ACE_par_jail, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_7, all_dat2$w4_ACE_fam_illness, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_8, all_dat2$w4_ACE_mom_death, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_9, all_dat2$w4_ACE_dad_death, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_10, all_dat2$w4_ACE_par_physabuse, exclude=NULL)
table(all_dat2$W4_CHILDHX_EVENTS_YN_11, all_dat2$w4_ACE_hh_mentalill, exclude=NULL)


#Covariates
table(all_dat2$W1_D_GENDER, all_dat2$w1_female, exclude=NULL)
table(all_dat2$W1_D_RACE_SUMMARY, exclude=NULL)
table(all_dat2$W1_MARITAL_STATUS, exclude=NULL)
table(all_dat2$W1_MATERNAL_EDUCATION, exclude=NULL)
table(all_dat2$W1_PATERNAL_EDUCATION, exclude=NULL)
table(all_dat2$W1_US_STATE, exclude=NULL)
table(all_dat2$W1_COUNTRY_BORN, exclude=NULL)
table(all_dat2$W1_GROWINGUP_FINANCE, exclude=NULL)
table(all_dat2$W1_GROWINGUP_GOHUNGRY, exclude=NULL)
table(all_dat2$W1_LADDER1, exclude=NULL)
table(all_dat2$W1_INCMRANGE_HMNZD, exclude=NULL)


table(all_dat2$W1_MARITAL_STATUS, all_dat2$w1_married, exclude=NULL) #Check data dictionary
table(all_dat2$W1_MATERNAL_EDUCATION, all_dat2$w1_mom_edu_gt12, exclude=NULL)
table(all_dat2$W1_PATERNAL_EDUCATION, all_dat2$w1_dad_edu_gt12, exclude=NULL)
table(all_dat2$w1_mom_edu_gt12, all_dat2$w1_dad_edu_gt12, all_dat2$w1_momordad_edu_gt12, exclude=NULL)
table(all_dat2$W1_COUNTRY_BORN, all_dat2$w1_usborn, exclude=NULL)
table(all_dat2$W1_US_STATE, all_dat2$W1_COUNTRY_BORN, all_dat2$w1_usborn, exclude=NULL)
table(all_dat2$W1_US_STATE, all_dat2$w1_usborn, all_dat2$w1_southern_birth, exclude=NULL)


table(all_dat2$W1_GROWINGUP_FINANCE, all_dat2$w1_chd_okfinancially, exclude=NULL)
table(all_dat2$W1_GROWINGUP_GOHUNGRY, all_dat2$w1_chd_everhungry, exclude=NULL)
table(all_dat2$W1_LADDER1, all_dat2$w1_chd_commstanding, exclude=NULL)


#Cognitive variables
summary(all_dat2$W1_SENAS_exec_poolz)
sd(all_dat2$W1_SENAS_exec_poolz)

summary(all_dat2$W1_SENAS_vrmem_poolz)
sd(all_dat2$W1_SENAS_vrmem_poolz)

summary(all_dat2$W2_SENAS_exec_poolz)
summary(all_dat2$W3_SENAS_exec_poolz)
summary(all_dat2$W4_SENAS_exec_poolz)

summary(all_dat2$W2_SENAS_vrmem_poolz)
summary(all_dat2$W3_SENAS_vrmem_poolz)
summary(all_dat2$W4_SENAS_vrmem_poolz)

#Clean imaging variables:
vols<-c("Cerebrum_tcb", "Total_hippo", 
        "Total_gray", "Total_white", "Total_wmh", "Total_brain", 
        "Frontal_Cortical", "Occipital_Cortical", "Parietal_Cortical", "Temporal_Cortical"
)

MRIsample<-all_dat2 %>% filter(!is.na(Cerebrum_tcv) & !is.na(Total_wmh))
PETsample<-all_dat2 %>% filter(!is.na(Landau_All_FSR)) %>% 
                        select(Study, STUDYID, Landau_All_FSR) %>% 
                        mutate(PET_sample=1,
                                 Abeta_pos = case_when(Landau_All_FSR >= 1.1 ~ 1,
                                                       Landau_All_FSR < 1.1 ~ 0,
                                                       TRUE ~ NA))


for (i in vols){
  # i<-"Cerebrum_tcb"
  temp<-lm(get(i) ~ Cerebrum_tcv, data=MRIsample)
  summary(temp)
  
  #Get residuals
  MRIsample[,paste0(i,"_resid")]<-MRIsample[,i]-predict(temp, newdata=MRIsample)
  
  #Blom transform residuals 
  MRIsample[,paste0(i,"_residblom")]<-blom(MRIsample[,paste0(i,"_resid")], method="blom")
  
  #Z-score transform residuals 
  tempmean<-mean(MRIsample[,paste0(i,"_resid"), drop=T])
  tempSD<-sd(MRIsample[,paste0(i,"_resid"), drop=T])
  
  MRIsample[,paste0(i,"_residzscore")]<-(MRIsample[,paste0(i,"_resid")]-tempmean)/tempSD
  
  #Log transform residuals
  tempmin<-min(MRIsample[,paste0(i,"_resid")])
  MRIsample[,paste0(i,"_residlog")]<-log((MRIsample[,paste0(i,"_resid")]-tempmin+0.01))
  
}

MRIsample$Total_wmh_log<-log(MRIsample$Total_wmh)


MRImerge<-MRIsample %>% select(STUDYID, Study, Cerebrum_tcv, all_of(c(vols, 
                                                                  paste0(vols, "_resid"), 
                                                                  paste0(vols, "_residblom"), 
                                                                  paste0(vols, "_residzscore"), 
                                                                  paste0(vols, "_residlog"))), 
                           Total_wmh_log) %>% mutate(MRI_sample=1)



all_dat3<-left_join(all_dat2 %>% select(-all_of(vols), -Cerebrum_tcv, -Landau_All_FSR), 
                    MRImerge, by=c("Study", "STUDYID")) %>% 
          left_join(.,PETsample, by=c("Study", "STUDYID")) %>% 
            mutate(MRI_sample = case_when(is.na(MRI_sample) ~ 0,
                                          T ~ MRI_sample),
                   PET_sample =case_when(is.na(PET_sample) ~ 0,
                                         T ~ PET_sample)
            
          )




#Update variable names
all_dat_preimp<-all_dat3 %>% select(all_of(c(admin, 

                                             #exposures
                                             all_chd_items)), 
                                   
                                   #age and cognitive outcomes
                                   W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE, W4_INTERVIEW_AGE,
                                   W1_SENAS_telephone, W2_SENAS_telephone, W3_SENAS_telephone, W4_SENAS_telephone,
                                   all_of(outcomes), W4_SENAS_exec, W4_SENAS_vrmem,
                                   W1_SENAS_exec_poolz, W2_SENAS_exec_poolz, W3_SENAS_exec_poolz, W4_SENAS_exec_poolz,
                                   W1_SENAS_vrmem_poolz, W2_SENAS_vrmem_poolz, W3_SENAS_vrmem_poolz, W4_SENAS_vrmem_poolz,
                                   
                                   #covariates/descriptives
                                   W1_D_RACE_SUMMARY, 
                                   w1_female, W1_MARITAL_STATUS, W1_INCMRANGE_HMNZD, 
                                   W1_EDU_EDUCATION, W1_EDU_EDUCATION_TEXT, w1_edu_yrs_cert,
                                   W1_MATERNAL_EDUCATION,  W1_PATERNAL_EDUCATION, 
                                   W1_US_STATE, W1_COUNTRY_BORN,
                                   W1_HEALTH,
                                   W1_GROWINGUP_FINANCE,  W1_GROWINGUP_GOHUNGRY, W1_LADDER1,
                                   w1_married, w1_mom_edu_gt12, w1_dad_edu_gt12, 
                                   w1_momordad_edu_gt12, w1_usborn, w1_southern_birth, w1_chd_okfinancially,
                                   w1_chd_everhungry, w1_chd_commstanding, 
                                   
                                   #imaging
                                   Cerebrum_tcv, all_of(c(vols, 
                                                          paste0(vols, "_resid"), 
                                                          paste0(vols, "_residblom"), 
                                                          paste0(vols, "_residzscore"), 
                                                          paste0(vols, "_residlog"))), Total_wmh_log,
                                  age_at_mri, MRI_precovid,
                                  Landau_All_FSR, Abeta_pos, age_at_pet, PET_precovid, 
                                  MRI_sample, PET_sample)


save(all_dat_preimp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/preimputation.Rdata")
