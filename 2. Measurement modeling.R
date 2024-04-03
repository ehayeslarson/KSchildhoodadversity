#Comparing W1 and W4 chd variables
library(psych)
library(lavaan)
library(haven)
library(tidyverse)
library(openxlsx)


#load data and define variables
load("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/preimputation.Rdata")
K_raw<-read_sas("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Raw/khandle_all_waves_20230912.sas7bdat")
output_path <- "~/Library/CloudStorage/Box-Box/EHL K99/Code/KHANDLESTAR/ACEs reorg/output/"

ACE_w1_vars<-c("w1_ACE_par_sepdiv", 
                "w1_ACE_par_remarried", 
                "w1_ACE_see_domviol",
                "w1_ACE_fam_substance", 
                "w1_ACE_par_jobloss", 
                "w1_ACE_par_jail",
                "w1_ACE_fam_illness", 
                "w1_ACE_mom_death", 
                "w1_ACE_dad_death")

ACE_w1_vars2<-c("w1_ACE_par_sepdiv", 
               "w1_ACE_par_remarried", 
               "w1_ACE_see_domviol",
               "w1_ACE_fam_substance", 
               "w1_ACE_par_jobloss", 
               "w1_ACE_par_jail",
               "w1_ACE_fam_illness") #no parental death

ACE_w4_vars<-c("w4_ACE_par_sepdiv", 
                "w4_ACE_par_remarried", 
                "w4_ACE_see_domviol",
                "w4_ACE_fam_substance", 
                "w4_ACE_par_jobloss", 
                "w4_ACE_par_jail",
                "w4_ACE_fam_illness",
                "w4_ACE_mom_death", 
                "w4_ACE_dad_death")

ACE_w4_vars2<-c("w4_ACE_par_sepdiv", 
               "w4_ACE_par_remarried", 
               "w4_ACE_see_domviol",
               "w4_ACE_fam_substance", 
               "w4_ACE_par_jobloss", 
               "w4_ACE_par_jail",
               "w4_ACE_fam_illness") #no parental death


#CFA in Wave 1 complete data

  #restrict to complete data on all W1 ACEs
  baseline_cc<-all_dat_preimp[complete.cases(all_dat_preimp[,ACE_w1_vars]),]

  #Split sample
  set.seed(12345)
  
  rand_samp<-sample(baseline_cc$STUDYID, size=nrow(baseline_cc)/2, replace=F)
  
  train_data<-baseline_cc[baseline_cc$STUDYID %in% rand_samp,]
  test_data<-baseline_cc[!baseline_cc$STUDYID %in% rand_samp,]

  
  #Run CFA in train data
      #First with all 9 vars
      mod1<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + 
                              w1_ACE_see_domviol + w1_ACE_fam_substance + 
                              w1_ACE_par_jobloss + w1_ACE_par_jail + 
                              w1_ACE_fam_illness + w1_ACE_mom_death + 
                              w1_ACE_dad_death'
    
      ACE_CFA1<-cfa(mod1, data=train_data, ordered=ACE_w1_vars)
      summary(ACE_CFA1, fit.measures=T, standardized=T) 
        # mom and dad death don't really load. 
        #CFI = 0.903, RMSEA = 0.072
      
      #Second excluding parental death variables (not household dysfunction)
      mod2<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + 
                              w1_ACE_see_domviol + w1_ACE_fam_substance + 
                              w1_ACE_par_jobloss + w1_ACE_par_jail + 
                              w1_ACE_fam_illness'
  
      ACE_CFA2<-cfa(mod2, data=train_data, ordered=ACE_w1_vars2)
      summary(ACE_CFA2, fit.measures=T, standardized=T)
        #All vars load. 
        #CFI=0.943, RMSEA = 0.075
      modificationindices(ACE_CFA2) #Need to add w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried
      
      #Third adds correlation between sep/div and remarried.
      mod3<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + 
                              w1_ACE_see_domviol + w1_ACE_fam_substance + 
                              w1_ACE_par_jobloss + w1_ACE_par_jail + 
                              w1_ACE_fam_illness
    
            w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried'
      
      ACE_CFA3<-cfa(mod3, data=train_data, ordered=ACE_w1_vars2)
      summary(ACE_CFA3, fit.measures=T, standardized=T)
        #All vars load.
        #CFI =0.997, RMSEA = 0.017 -- excellent fit!
      modificationindices(ACE_CFA3)
        #All MIs <5
  
  #Test final model in testing data
      ACE_CFA3_test<-cfa(mod3, data=test_data, ordered=ACE_w1_vars2)
      summary(ACE_CFA3_test, fit.measures=T, standardized=T)
        #CFI = 1.000, RMSEA = 0.000
      
        
#Now check measurement adding in Wave 4 measures using complete W1 and W4 data


      #restrict to KHANDLE participants who completed both waves:
      w4complete<-K_raw %>% filter(!is.na(W4_COMPLETED_AT)) %>% select(STUDYID) %>% mutate(STUDYID = paste0("K",STUDYID))
      
      khandle_w1w4<-all_dat_preimp %>% filter(Study=="KHANDLE", STUDYID %in% 
                                                w4complete$STUDYID) %>% unique() %>% 
                                        select(Study, STUDYID, 
                                               all_of(c(ACE_w1_vars,ACE_w4_vars))) %>% 
                                                 filter(complete.cases(.))
      
      #Check correspondence between W1 and W4 measures
      par_sepdiv<-table(khandle_w1w4$w1_ACE_par_sepdiv,khandle_w1w4$w4_ACE_par_sepdiv)
      par_remarried<-table(khandle_w1w4$w1_ACE_par_remarried,khandle_w1w4$w4_ACE_par_remarried)
      see_domviol<-table(khandle_w1w4$w1_ACE_see_domviol,khandle_w1w4$w4_ACE_see_domviol)
      fam_substance<-table(khandle_w1w4$w1_ACE_fam_substance,khandle_w1w4$w4_ACE_fam_substance)
      par_jobloss<-table(khandle_w1w4$w1_ACE_par_jobloss,khandle_w1w4$w4_ACE_par_jobloss)
      par_jail<-table(khandle_w1w4$w1_ACE_par_jail,khandle_w1w4$w4_ACE_par_jail)
      fam_illness<-table(khandle_w1w4$w1_ACE_fam_illness,khandle_w1w4$w4_ACE_fam_illness)
      mom_death<-table(khandle_w1w4$w1_ACE_mom_death,khandle_w1w4$w4_ACE_mom_death)
      dad_death<-table(khandle_w1w4$w1_ACE_dad_death,khandle_w1w4$w4_ACE_dad_death)
      
      k1<-cohen.kappa(par_sepdiv)
      k2<-cohen.kappa(par_remarried)
      k3<-cohen.kappa(see_domviol)
      k4<-cohen.kappa(fam_substance)
      k5<-cohen.kappa(par_jobloss)
      k6<-cohen.kappa(par_jail)
      k7<-cohen.kappa(fam_illness)
      k8<-cohen.kappa(mom_death)
      k9<-cohen.kappa(dad_death)
      
      all_kappas<-rbind(k1, k2, k3, k4, k5, k6, k7, k8, k9) %>% data.frame()

      #Naming variables
      ACE_vars<-c("ACE_par_sepdiv", "ACE_par_remarried", "ACE_see_domviol",
                      "ACE_fam_substance", "ACE_par_jobloss", "ACE_par_jail",
                      "ACE_fam_illness", "ACE_mom_death", "ACE_dad_death")
      
      all_kappas<-cbind(ACE_vars, all_kappas) %>% 
                  select(ACE_vars, n.obs, kappa, var.kappa) %>% 
                  mutate(kappa = sprintf('%.3f', kappa),
                         var.kappa = sprintf('%.3f', var.kappa))
      all_kappas     


    #Checking if CFA for ACEs run in W1 data still holds in sample with W1 and W4 data
    mod3_w1<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + w1_ACE_see_domviol + 
                w1_ACE_fam_substance + w1_ACE_par_jobloss + w1_ACE_par_jail + 
                w1_ACE_fam_illness
    
            w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried'
    
    ACE_CFA3_w1<-cfa(model=mod3_w1, data=khandle_w1w4, ordered=ACE_w1_vars2)
    summary(ACE_CFA3_w1, fit.measures=T, standardized=T)
    khandle_w1w4$mod3_w1<-predict(ACE_CFA3_w1)

    #Now predict on w4 data using w1 model fit. 
    khandle_w1w4_rename<-khandle_w1w4 %>% select(-all_of(ACE_w1_vars2)) %>% 
      rename(w1_ACE_par_sepdiv = w4_ACE_par_sepdiv , 
             w1_ACE_par_remarried = w4_ACE_par_remarried, 
             w1_ACE_see_domviol = w4_ACE_see_domviol,
             w1_ACE_fam_substance = w4_ACE_fam_substance, 
             w1_ACE_par_jobloss = w4_ACE_par_jobloss, 
             w1_ACE_par_jail = w4_ACE_par_jail,
             w1_ACE_fam_illness = w4_ACE_fam_illness)
    
    khandle_w1w4$mod3_w4_modw1<-predict(ACE_CFA3_w1, newdata=khandle_w1w4_rename)
    
    CFA3_w1w4_cor<-format(round(cor(khandle_w1w4$mod3_w1, khandle_w1w4$mod3_w4_modw1),digits=2), nsmall=2)#0.72 
    CFA3_w1w4_cor


#Now try modeling the W1 variables with extra W4 measures (phys abuse, fam mental illness)

khandle_w1w4_2<-all_dat_preimp %>% filter(Study=="KHANDLE", STUDYID %in% 
                                          w4complete$STUDYID) %>% unique() %>% 
                                  select(Study, STUDYID, all_of(c(ACE_w1_vars2,ACE_w4_vars2)), 
                               w4_ACE_par_physabuse, w4_ACE_hh_mentalill,) %>% filter(complete.cases(.))

#create test/train data:
set.seed(12345)
rand_samp2<-sample(khandle_w1w4_2$STUDYID, size=nrow(khandle_w1w4_2)/2, replace=F)

train_data2<-khandle_w1w4_2[khandle_w1w4_2$STUDYID %in% rand_samp,]
test_data2<-khandle_w1w4_2[!khandle_w1w4_2$STUDYID %in% rand_samp,]

    #Try model with just one factor--add W4 measures to final CFA from W1 only
    mod4<-'hhchallenges =~ w1_ACE_par_sepdiv + w1_ACE_par_remarried + w1_ACE_see_domviol + 
                w1_ACE_fam_substance + w1_ACE_par_jobloss + w1_ACE_par_jail + 
                w1_ACE_fam_illness + w4_ACE_par_physabuse + w4_ACE_hh_mentalill
    
            w1_ACE_par_sepdiv ~~ w1_ACE_par_remarried'
    
    ACE_CFA4<-cfa(mod4, data=train_data, ordered=T)
    summary(ACE_CFA4, fit.measures=T, standardized=T)
      #CFI=1.00, RMSEA = 0.000
    modificationindices(ACE_CFA4)
      #All MI <5


    #Testing
    ACE_CFA4_test<-cfa(mod4, data=test_data, ordered=T)
    summary(ACE_CFA4_test, fit.measures=T, standardized=T)


#Correlation between W1 only and models with W1 and W4, and model with parent death
khandle_w1w4_2$mod3<-predict(cfa(model=mod3, data=khandle_w1w4_2, ordered=ACE_w1_vars2))
khandle_w1w4_2$mod4<-predict(cfa(model=mod4, data=khandle_w1w4_2, ordered=T))

fac1_fac2_cor<-format(round(cor(khandle_w1w4_2$mod3, khandle_w1w4_2$mod4),digits=2), nsmall=2) #Correlation is 0.95
fac1_fac2_cor



#Things we need to export:

w1_CFA_final<-rbind(
  (data.frame(inspect(ACE_CFA3_test,what="std")$lambda) %>% mutate(item=rownames(.))),
  (data.frame(hhchallenges=fitMeasures(ACE_CFA3_test, c("cfi.scaled","rmsea.scaled")), 
             item=c("cfi_scaled","rmsea_scaled")))
) %>% select(item, hhchallenges) %>% mutate(hhchallenges = sprintf('%.3f', hhchallenges))

w1w4_CFA_final<-rbind(
  (data.frame(inspect(ACE_CFA4_test,what="std")$lambda) %>% mutate(item=rownames(.))),
  (data.frame(hhchallenges=fitMeasures(ACE_CFA4_test, c("cfi.scaled","rmsea.scaled")), 
              item=c("cfi_scaled","rmsea_scaled")))
) %>% select(item, hhchallenges) %>% mutate(hhchallenges = sprintf('%.3f', hhchallenges))

CFA_output<-list(w1_CFA_final=w1_CFA_final,
  w1_w4kappa=all_kappas,
  w1_w4corr_w1mod=CFA3_w1w4_cor, 
  w1w4_CFA_final= w1w4_CFA_final,
  fac1_fac2_cor=fac1_fac2_cor)

write.xlsx(
  CFA_output,
  paste0(output_path, "CFA_output.xlsx")
)
