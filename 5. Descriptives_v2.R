# Author: Eleanor Hayes-Larson
# Purpose: generate Table 1 data (with and without MI) for each sample (cognitve, MRI, PET)

library(gtsummary)
library(tidyverse)
library(magrittr)
library(survey)
library(labelled)
library(openxlsx)

load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/postimputation_df.Rdata")
output_path <- "~/Library/CloudStorage/Box-Box/EHL K99/Code/KHANDLESTAR/ACEs reorg/output/"

# set data formatting ----
dat_all_imp_final_df2 <- dat_all_imp_final_df  %>% mutate(n_waves=as.numeric(n_waves)) %>%
  set_variable_labels(
    Study = 'Cohort',
    w1_total_ACE_count = 'Number of ACEs reported',
    W1_INTERVIEW_AGE = 'Age at baseline (years)',
    W1_SENAS_exec_poolz = 'Wave 1 executive function (z-score)',
    W1_SENAS_vrmem_poolz = 'Wave 1 verbal memory (z-score)',
    n_waves = 'Waves of data',
    max_study_time = 'Follow-up time (years)',
    w1_female = 'Female',
    W1_D_RACE_SUMMARY = 'Race/ethnicity',
    w1_momordad_edu_gt12 = 'Parent with over 12 years of education',
    w1_usborn = 'US born',
    w1_southern_birth = "Southern birth",
    w1_chd_okfinancially = "Financially average or well-off in childhood",
    w1_chd_everhungry = "Ever insufficient money for food in childhood",
    w1_chd_commstanding = "Family community standing in childhood (1=lowest, 10=highest)",
    w1_edu_yrs_cert = 'Years of education',
    w1_married = "Married/partnered",
    W1_INCMRANGE_HMNZD2 = "Annual household income",
    age_at_mri = "Age at MRI scan",
    age_at_pet = "Age at PET scan",
    Abeta_pos = "Amyloid-positive"
    
  ) %>% 
  purrr::modify_if(labelled::is.labelled, labelled::to_factor)

var_order <- c(
  "Study", "W1_INTERVIEW_AGE", "w1_total_ACE_count", "w1_female", "W1_D_RACE_SUMMARY", "w1_usborn", 
  "w1_southern_birth", "w1_momordad_edu_gt12", "w1_chd_okfinancially",
  "w1_chd_everhungry", "w1_chd_commstanding", "w1_edu_yrs_cert", 
  "w1_married", "W1_INCMRANGE_HMNZD2",
  "W1_SENAS_exec_poolz", "W1_SENAS_vrmem_poolz"
)

# pre-imputation ----

dat_tab1_cogsamp <- dat_all_imp_final_df2 %>% filter(`.imp`==0)

## cog sample: not stratified ----

tab1_cogsamp <- dat_tab1_cogsamp %>%  
  tbl_summary(missing = "ifany", 
              missing_text="Missing",
              by=NULL,
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(1,1)),
              include = c(all_of(var_order),"n_waves", "max_study_time")) %>%
  modify_header(label = "**Variable**", 
                all_stat_cols() ~ paste0("Cognitive data sample","<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns=label, undo = T) %>%
  bold_labels() 

tab1_cogsamp

## MRI sample: not stratified ----
tab1_mrisamp <- dat_tab1_cogsamp %>% filter(MRI_sample==1) %>% 
  tbl_summary(missing = "ifany", 
              missing_text="Missing",
              by=NULL,
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(1,1)),
              include = c(all_of(var_order), age_at_mri))%>%
  modify_header(label = "**Variable**", 
                all_stat_cols() ~ paste0("MRI data sample","<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns=label, undo = T) %>%
  bold_labels() 

tab1_mrisamp


## PET sample: not stratified ----
tab1_petsamp <- dat_tab1_cogsamp %>% filter(PET_sample==1) %>% 
  tbl_summary(missing = "ifany",
              missing_text="Missing",
              by=NULL,
              type = list(all_continuous() ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean} ({sd})"),
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(1,1)),
              include = c(all_of(var_order), age_at_pet, Abeta_pos))%>%
  modify_header(label = "**Variable**", 
                all_stat_cols() ~ paste0("Amyloid PET data sample","<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns=label, undo = T) %>%
  bold_labels() 

tab1_petsamp

## combine into one table ----
tab1 <- tbl_merge(tbls = list(tab1_cogsamp, tab1_mrisamp, tab1_petsamp), tab_spanner = FALSE)
tab1

tab1_df<-tab1 %>% as_tibble()

write.xlsx(tab1_df,
  paste0(output_path, "table1_allsamp_pre_MI.xlsx")
)

#Check N >=65 at baseline
nrow(dat_tab1_cogsamp[dat_tab1_cogsamp$W1_INTERVIEW_AGE>=65,])

# post-imputation ----
# not stratified and stratified at median exposure (ACE_fac1_z_gtmedian)

dat_tab1_cogsamp_imp <- dat_all_imp_final_df2 %>% 
  filter(`.imp` != 0) %>% 
  mutate(wt = 1 / max(.imp),
         ACE_fac1_z_gtmedian=factor(ACE_fac1_z_gtmedian, levels = c(0,1),
                                    labels = c("Less than median ACEs", "Greater than median ACEs")))

# group variables by type
catvars <-
  c(
    'Study',
    'w1_total_ACE_count',
    'w1_female',
    'W1_D_RACE_SUMMARY',
    'w1_momordad_edu_gt12',
    'w1_usborn',
    'w1_southern_birth',
    'w1_chd_okfinancially',
    'w1_chd_everhungry',
    'w1_married',
    'W1_INCMRANGE_HMNZD2',
    'n_waves'
  )

contvars <-
  c(
    'W1_INTERVIEW_AGE',
    'W1_SENAS_exec_poolz',
    'W1_SENAS_vrmem_poolz',
    #'interview_age_phi',
    'w1_edu_yrs_cert',
    'w1_chd_commstanding',
    'max_study_time'
    # 'age_at_mri', 
    # 'age_at_pet'
  )

catvars <- catvars[order(match(catvars, var_order))]
contvars <- contvars[order(match(contvars, var_order))]

## function to get mean and sd for imputed continuous variables ----

imp_cts_var_tbl1 <- function(df, cts_vars, grp, digits = 1) {
   # df <- dat_tab1_cogsamp_imp
   # # cts_vars is a vector of variables to summarize
   # cts_vars <- contvars
   # # grp is a vector of variables to stratify by
   # grp <- "ACE_fac1_z_gtmedian"
   # digits <- 1
  
  # extract labels 
  labels <- var_label(df) %>% 
    bind_rows() %>% t() %>% 
    as_tibble(rownames = "variable") %>% 
    setNames(c("variable", "label")) %>% 
    filter(variable %in% c(cts_vars, grp))
  
  grpout <- df %>% 
    group_by(across(all_of(grp))) %>% 
    group_keys()
  
  overallout <- tibble("Overall")
  colnames(overallout) <- grp
  
  for (var in cts_vars) {
    #var<-"W1_INTERVIEW_AGE"
    temp <- df %>% 
      group_by(.imp) %>% 
      summarise(mean_imp = mean(get(var)), var_imp = var(get(var))) %>% 
      ungroup() %>% 
      summarise(mean = mean(mean_imp), 
                sd = mean(var_imp) %>% sqrt()) %>% 
      mutate(out = paste0(format(round(mean, digits), nsmall = digits), 
                          " (", format(round(sd, digits), nsmall = digits), ")"))
    
    overallout[var] <- temp$out
    
    temp <- df %>% 
      group_by(across(all_of(c(".imp", grp)))) %>% 
      summarise(mean_imp = mean(get(var)), var_imp = var(get(var))) %>% 
      ungroup() %>% 
      group_by(across(all_of(grp))) %>% 
      summarise(mean = mean(mean_imp), 
                sd = mean(var_imp) %>% sqrt()) %>% 
      ungroup() %>% 
      mutate(out = paste0(format(round(mean, digits), nsmall = digits), 
                          " (", format(round(sd, digits), nsmall = digits), ")"))
    grpout[var] <- temp$out
  }
  
  rbind(grpout, overallout) %>% 
    t() %>% as_tibble(rownames = "variable") %>% 
    left_join(labels, by = "variable") %>% 
    select(label, V1, V2, V3) %>% 
    return()
}

## cog sample ----

tab1_cogsamp_imp_cat <- dat_tab1_cogsamp_imp %>% 
  svydesign(id = ~ 1, weights = ~ wt, data = .) %>% 
  tbl_svysummary(
    by = ACE_fac1_z_gtmedian, 
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    include = all_of(catvars)
  ) %>%
  add_overall(last = TRUE) %>% 
  # modify_header(label = "**Variable**",
  #               all_stat_cols() ~ paste0("MRI data sample", "<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns = label, undo = T) %>%
  bold_labels() 
#Note, we get error warnings using tbl_svysummary about the weight scaling. 
#These are package version-dependent and do not affect results; they can be ignored. 

tab1_cogsamp_imp_cont <- dat_tab1_cogsamp_imp %>% 
  imp_cts_var_tbl1(contvars, "ACE_fac1_z_gtmedian")

write.xlsx(
  list("categorical" = tab1_cogsamp_imp_cat %>% as_tibble(),
       "continuous" = tab1_cogsamp_imp_cont), 
  paste0(output_path, "table1_cogsamp_post_MI.xlsx")
)

## MRI sample ----

tab1_mrisamp_imp_cat <- dat_tab1_cogsamp_imp %>% 
  filter(MRI_sample == 1) %>% 
  svydesign(id = ~ 1, weights = ~ wt, data = .) %>% 
  tbl_svysummary(
    by = ACE_fac1_z_gtmedian, 
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    include = all_of(catvars)
  ) %>%
  add_overall(last = TRUE) %>% 
  # modify_header(label = "**Variable**",
  #               all_stat_cols() ~ paste0("MRI data sample", "<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns = label, undo = T) %>%
  bold_labels() 
#Note, we get error warnings using tbl_svysummary about the weight scaling. 
#These are package version-dependent and do not affect results; they can be ignored. 

# why is there missingness in age at MRI scan ???
tab1_mrisamp_imp_cont <- dat_tab1_cogsamp_imp %>% 
  filter(MRI_sample == 1) %>% 
  imp_cts_var_tbl1(c(contvars, "age_at_mri"), "ACE_fac1_z_gtmedian")

write.xlsx(
  list("categorical" = tab1_mrisamp_imp_cat %>% as_tibble(),
       "continuous" = tab1_mrisamp_imp_cont), 
  paste0(output_path, "table1_mrisamp_post_MI.xlsx")
)

## PET sample ----

tab1_petsamp_imp_cat <- dat_tab1_cogsamp_imp %>% 
  filter(PET_sample == 1) %>% 
  svydesign(id = ~ 1, weights = ~ wt, data = .) %>% 
  tbl_svysummary(
    by = ACE_fac1_z_gtmedian, 
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1)),
    include = all_of(catvars)
  ) %>%
  add_overall(last = TRUE) %>% 
  # modify_header(label = "**Variable**",
  #               all_stat_cols() ~ paste0("MRI data sample", "<br> N={N}")) %>%
  add_stat_label(location = "row") %>%
  modify_column_indent(columns = label, undo = T) %>%
  bold_labels() 
#Note, we get error warnings using tbl_svysummary about the weight scaling. 
#These are package version-dependent and do not affect results; they can be ignored. 

tab1_petsamp_imp_cont <- dat_tab1_cogsamp_imp %>% 
  filter(PET_sample == 1) %>% 
  imp_cts_var_tbl1(c(contvars, "age_at_pet"), "ACE_fac1_z_gtmedian")

write.xlsx(
  list("categorical" = tab1_petsamp_imp_cat %>% as_tibble(),
       "continuous" = tab1_petsamp_imp_cont), 
  paste0(output_path, "table1_petsamp_post_MI.xlsx")
)
