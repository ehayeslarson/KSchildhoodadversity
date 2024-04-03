library(haven)
library(readxl)
library(tidyverse)
library(mice)
library(openxlsx)


#load data and define output path
    load("/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/preimputation.Rdata")
    output_path <- "~/Library/CloudStorage/Box-Box/EHL K99/Code/KHANDLESTAR/ACEs reorg/output/"

###################    
#Set up and run imputations
###################    
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
    #Summarize missingness in variables to be imputed:
    vars.to.impute<-c("W1_INTERVIEW_AGE", 
                      "w1_female", "W1_D_RACE_SUMMARY", "W1_MARITAL_STATUS", 
                      "W1_MATERNAL_EDUCATION",  "W1_PATERNAL_EDUCATION",
                      "w1_usborn", "w1_southern_birth", "w1_edu_yrs_cert", "W1_HEALTH",
                      "W1_INCMRANGE_HMNZD",
                      "W1_GROWINGUP_FINANCE",  "W1_GROWINGUP_GOHUNGRY", "W1_LADDER1",
                      all_chd_items)
    
    missingsummary<-data.frame(varname=vars.to.impute, pctmiss=NA)
    row.names(missingsummary)<-vars.to.impute
    for (i in vars.to.impute){
      #i<-vars.to.impute[2]
      missingsummary[i,"pctmiss"]<-100*sum(is.na(all_dat_preimp[,i]))/nrow(all_dat_preimp)
      print(i)
      print(table(all_dat_preimp[,i], exclude=NULL))
    }
    
    missingsummary<-missingsummary[order(missingsummary$pctmiss),]  %>% mutate(pctmiss = sprintf('%.1f', pctmiss))
    missingsummary
    
    #Output missingness summary for supplemental table
    write.xlsx(
      missingsummary,
      paste0(output_path, "Miss_summary.xlsx")
    )

    #Assess missingness in auxiliary vars, which are included to help with imputations. 
    #Also including outcome variables, but won't use imputed values for these (and there are none for W1). 
    #don't use imaging outcomes because too much missingness by desgin to be worth imputing
        aux.vars<-c("STUDYID", "Study", "W1_SENAS_exec_poolz", "W1_SENAS_vrmem_poolz")
    
        auxmissingsummary<-data.frame(varname=aux.vars, pctmiss=NA)
        row.names(auxmissingsummary)<-aux.vars
        for (i in aux.vars){
          auxmissingsummary[i,"pctmiss"]<-100*sum(is.na(all_dat_preimp[,i]))/nrow(all_dat_preimp)
        }
        
        auxmissingsummary<-auxmissingsummary[order(auxmissingsummary$pctmiss),]  
        auxmissingsummary

      #Combine to get vector of all variables that will go in imputation model, 
      #Ordered by increasing missingness in vars.to.impute, and subsequenty, aux.vars. 
        all.vars<-c(paste(missingsummary$varname), paste(auxmissingsummary$varname))
        length(all.vars)
        all.vars

    #prep data by dropping vars we don't need, ordering by missingness
      in.data<-all_dat_preimp[,all.vars]
  
    #Set variable types
      continuous.vars<-c("W1_INTERVIEW_AGE", "W1_SENAS_exec_poolz", "W1_SENAS_vrmem_poolz",
                         "W1_LADDER1", "w1_edu_yrs_cert")
      binary.vars<-c(all_chd_items, "w1_female", "Study", "w1_usborn", "w1_southern_birth")
      ordinal.vars<-c("W1_MATERNAL_EDUCATION", "W1_PATERNAL_EDUCATION", 
                      "W1_GROWINGUP_FINANCE", "W1_GROWINGUP_GOHUNGRY", "W1_HEALTH",
                      "W1_INCMRANGE_HMNZD")
      categorical.vars<-c("W1_D_RACE_SUMMARY", "W1_MARITAL_STATUS", "STUDYID" )
      
    #Check that have all variables, no duplicates
      length(c(continuous.vars, binary.vars, ordinal.vars, categorical.vars))
      sum(duplicated(c(continuous.vars, binary.vars, ordinal.vars, categorical.vars)))    
    
    #Set variable classes by type
      in.data1 <- as.data.frame(lapply(in.data[,continuous.vars], as.numeric))
      in.data2 <- as.data.frame(lapply(in.data[,c(binary.vars, categorical.vars)], as.factor))
      in.data3 <- as.data.frame(lapply(in.data[,ordinal.vars], as.ordered))
      
      in.data<-cbind(in.data1, in.data2, in.data3) %>% select(all_of(all.vars)) #reorders by missingness
    
    #recheck classes
      classsum<-data.frame(varname=all.vars, class=NA)
      for (i in all.vars){
        classsum[classsum$varname==i,"class"]<-(class(in.data[,i])[1])
      }
      classsum

    #Initiate imputations
      ini<-mice(in.data, maxit=0, defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)
      
      ini$method
      meth<-ini$method
      meth
      
      ini$predictorMatrix
      pred<-ini$predictorMatrix
      
      pred[,"STUDYID"]<-0 #Removing studyid from use for prediction because studyid is unique  
      
      pred[grepl("^w4",rownames(pred)),"Study"]<-0 
      #Study (STAR) is collinear with missingness on W4 data since STAR has no W4
      
      nimp<-20
  
      #Run imputations
          imp_pmm_all<-mice(in.data, m=nimp, maxit=10, pred=pred, meth=meth, 
                        defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)
      
      #examine diagnostics
          events<-imp_pmm_all$loggedEvents #None
          plot(imp_pmm_all)

          dat_all_imp <- complete(imp_pmm_all, action = 'long',
                                  include = TRUE)

          
save(dat_all_imp, file="/Users/eleanorhayes-larson/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation.Rdata")
          