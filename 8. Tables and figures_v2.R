
# figures and tables 

library(tidyverse)
library(openxlsx)

# load all results and format data ----

# write results to 
output_path <- "~/Library/CloudStorage/Box-Box/EHL K99/Code/KHANDLESTAR/ACEs reorg/output/"

# main results
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_pet.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_suvr.Rdata")

# subsamples of cog results
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_mrisamp.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_petsamp.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_cog_ge65samp.Rdata")

# subsamples of mri results
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri_petsamp.Rdata")
load(file = "~/Library/CloudStorage/Box-Box/EHL K99/Data/Clean/all_res_mri_ge65samp.Rdata")

clean <- function(data, digits = 3, mri = FALSE, pet = FALSE) {
  
  if (mri) {
    data <- data %>% 
      mutate(
        outcome_raw = str_detect(model, "ICV"), 
        outcome_label = case_when(
          str_detect(outcome, "Cerebrum_tcb") ~ "Total cerebrum volume", 
          str_detect(outcome, "Total_brain") ~ "Total brain volume", 
          str_detect(outcome, "Total_hippo") ~ "Hippocampal volume", 
          str_detect(outcome, "Total_gray") ~ "Total gray matter volume", 
          str_detect(outcome, "Total_white") ~ "Total white matter volume", 
          str_detect(outcome, "Total_wmh") ~ "Total cerebrum white matter hyperintensity volume", 
          str_detect(outcome, "Parietal_Cortical") ~ "Parietal lobe gray matter volume", 
          str_detect(outcome, "Temporal_Cortical") ~ "Temporal lobe gray matter volume", 
          str_detect(outcome, "Frontal_Cortical") ~ "Frontal lobe gray matter volume", 
          str_detect(outcome, "Occipital_Cortical") ~ "Occipital lobe gray matter volume",
          TRUE ~ NA_character_
        ) %>% 
          factor(levels = region_levels)
      ) 
  } 
  
  data <- data %>% 
    rename(lower = `2.5 %`, upper = `97.5 %`) %>% 
    mutate(
      model = case_when(
        str_detect(model, "Crude") ~ "Model 1",
        str_detect(model, "Adjusted1") ~ "Model 2",
        str_detect(model, "Adjusted") ~ "Model 3",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Model 1", "Model 2", "Model 3"))
    )
  
  if (pet) {
    data <- data %>% 
      mutate(
        across(c(RR, LCI_RR, UCI_RR), 
               function(x) format(round(x, digits), nsmall = digits) %>% str_trim(),
               .names = "{.col}_r"),
        out = paste0(RR_r, " (", LCI_RR_r, ",", UCI_RR_r, ")")
      )
  } else {
    data <- data %>% 
      mutate(
        across(c(estimate, lower, upper), 
               function(x) format(round(x, digits), nsmall = digits) %>% str_trim(),
               .names = "{.col}_r"),
        out = paste0(estimate_r, " (", lower_r, ",", upper_r, ")")
      )
  }
  return(data)
}

all_res_cog <- all_res_cog %>% clean()
all_res_cog_mrisamp <- all_res_cog_mrisamp %>% clean()
all_res_cog_petsamp <- all_res_cog_petsamp %>% clean()
all_res_cog_ge65samp <- all_res_cog_ge65samp %>% clean()
all_res_pet <- all_res_pet %>% clean(digits = 2, pet = TRUE)
all_res_suvr <- all_res_suvr %>% clean()

# order the regions here
region_levels <- c(
  "Total cerebrum volume",
  "Total gray matter volume", 
  "Total white matter volume", 
  "Parietal lobe gray matter volume", 
  "Temporal lobe gray matter volume", 
  "Frontal lobe gray matter volume", 
  "Occipital lobe gray matter volume",
  "Hippocampal volume", 
  "Total brain volume",
  "Total cerebrum white matter hyperintensity volume"
  )

all_res_mri <- all_res_mri %>% clean(mri = TRUE)
all_res_mri_petsamp <- all_res_mri_petsamp %>% clean(mri = TRUE)
all_res_mri_ge65samp <- all_res_mri_ge65samp %>% clean(mri = TRUE)


# main results ----

## cog results table ----
tabcog <- all_res_cog %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"), 
    time == "Study",
    str_detect(term, exposure)
  ) %>% 
  select(exposure, outcome, term, model, out) %>% 
  pivot_wider(
    id_cols = c(exposure, outcome, term),
    names_from = model, 
    values_from = out
  ) 

### MRI subsample ----
tabcog_mri <- all_res_cog_mrisamp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"), 
    time == "Study",
    str_detect(term, exposure)
  ) %>% 
  select(exposure, outcome, term, model, out) %>% 
  pivot_wider(
    id_cols = c(exposure, outcome, term),
    names_from = model, 
    values_from = out
  )

### PET subsample ----
tabcog_pet <- all_res_cog_petsamp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"), 
    time == "Study",
    str_detect(term, exposure)
  ) %>% 
  select(exposure, outcome, term, model, out) %>% 
  pivot_wider(
    id_cols = c(exposure, outcome, term),
    names_from = model, 
    values_from = out
  )

### ge65 subsample ----
tabcog_ge65 <- all_res_cog_ge65samp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"), 
    time == "Study",
    str_detect(term, exposure)
  ) %>% 
  select(exposure, outcome, term, model, out) %>% 
  pivot_wider(
    id_cols = c(exposure, outcome, term),
    names_from = model, 
    values_from = out
  )
  
write.xlsx(
  list("main cog results"= tabcog, 
       "mri sample" = tabcog_mri,
       "pet sample" = tabcog_pet, 
       "ge65 sample" = tabcog_ge65),
  paste0(output_path, "cog_results_study_time.xlsx")
)

## mri results figure 1 ----

dodge_by <- 0.5
barwidth <- 0.5

results <- all_res_mri %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "residzscore"),
    str_detect(term, exposure), 
    !str_detect(outcome, "wmh")
  ) %>% 
  filter(outcome_label %in% region_levels[1:8])

# plot 
results %>% 
  ggplot(aes(x = outcome_label, y = estimate, 
             color = model, group = model)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point(position = position_dodge(width = dodge_by)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = barwidth, 
    position = position_dodge(width = dodge_by)
  ) + 
  labs(x = "Brain region", y = "Estimate", color = "Model") + 
  theme_bw() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  )

ggsave(
  paste0(output_path, "fig_mri.pdf"),
  height = 5, width = 7, units = "in"
)

# tabular results
tabmri <- results %>% 
  pivot_wider(
    id_cols = c(outcome_label),
    names_from = model, 
    values_from = out
  ) %>% 
  arrange(outcome_label)

### PET subsample ----
results <- all_res_mri_petsamp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "residzscore"),
    !str_detect(outcome, "wmh"),
    str_detect(term, exposure)
  ) %>% 
  filter(outcome_label %in% region_levels[1:8])

# plot 
results %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  ggplot(aes(x = outcome_label, y = estimate, 
             color = model, group = model)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point(position = position_dodge(width = dodge_by)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = barwidth, 
    position = position_dodge(width = dodge_by)
  ) + 
  labs(x = "Brain region", y = "Estimate", color = "Model") + 
  theme_bw() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  )

ggsave(
  paste0(output_path, "fig_mri_petsamp.pdf"),
  height = 5, width = 7, units = "in"
)

# tabular results
tabmri_pet <- results %>% 
  pivot_wider(
    id_cols = c(outcome_label),
    names_from = model, 
    values_from = out,
  ) %>% 
  arrange(outcome_label)

### ge65 subsample ----
results <- all_res_mri_ge65samp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "residzscore"),
    !str_detect(outcome, "wmh"),
    str_detect(term, exposure)
  ) %>% 
  filter(outcome_label %in% region_levels[1:8]) 

# plot 
results %>% 
  ggplot(aes(x = outcome_label, y = estimate, 
             color = model, group = model)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point(position = position_dodge(width = dodge_by)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = barwidth, 
    position = position_dodge(width = dodge_by)
  ) + 
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) + 
  labs(x = "Brain region", y = "Estimate", color = "Model") + 
  theme_bw() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  )

ggsave(
  paste0(output_path, "fig_mri_ge65samp.pdf"),
  height = 5, width = 7, units = "in"
)

# tabular results
tabmri_ge65 <- results %>% 
  pivot_wider(
    id_cols = c(outcome_label),
    names_from = model, 
    values_from = out
  ) %>% 
  arrange(outcome_label)

write.xlsx(
  list("main mri results"= tabmri, 
       "pet sample" = tabmri_pet, 
       "ge65 sample" = tabmri_ge65),
  paste0(output_path, "mri_results_study_time.xlsx")
)


## numerical results ----

### total wmh log results ----
tabwmh <- all_res_mri %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "Total_wmh_log"), 
    str_detect(term, exposure)
  ) %>%
  select(exposure, outcome, term, model, out) 

# subsamples
tabwmh_pet <- all_res_mri_petsamp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "Total_wmh_log"), 
    str_detect(term, exposure)
  ) %>%
  select(exposure, outcome, term, model, out) 

tabwmh_ge65 <- all_res_mri_ge65samp %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "Total_wmh_log"), 
    str_detect(term, exposure)
  ) %>%
  select(exposure, outcome, term, model, out) 
  
### amyloid positive results ----
tabamyloid <- all_res_pet %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(term, exposure)
  ) %>%
  select(exposure, outcome, term, model, out) 

### amyloid burden results ----
tabsuvr <- all_res_suvr %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(term, exposure)
  ) %>%
  select(exposure, outcome, term, model, out) 


write.xlsx(
  list(
    "main wmh results" = tabwmh,
    "wmh results pet sample" = tabwmh_pet, 
    "wmh results ge65 sample" = tabwmh_ge65,
    "amyloid results" = tabamyloid,
    "suvr results" = tabsuvr
  ),
  paste0(output_path, "numerical_results.xlsx")
)


# sensitivity analysis results ----

## results table for age models ----
all_res_cog %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"), 
    time == "Age",
    str_detect(term, exposure)
  ) %>% 
  select(outcome, term, model, out) %>% 
  pivot_wider(
    id_cols = c(outcome, term),
    names_from = model, 
    values_from = out
  ) %>% 
  write.xlsx(
    paste0(output_path, "cog_results_age_models.xlsx")
  )



## figure 1: alternative outcomes ----

dodge_by <- 0.5
barwidth <- 0.5


### outcomes ending with _resid ----
resid_results <- all_res_mri %>% 
  filter(
    exposure == "ACE_fac1_z", 
    str_detect(outcome, "resid$"),
    !str_detect(outcome, "wmh"),
    str_detect(term, exposure)
  )

## plot 
resid_results %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  ggplot(aes(x = outcome_label, y = estimate, 
             color = model, group = model)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point(position = position_dodge(width = dodge_by)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = barwidth, 
    position = position_dodge(width = dodge_by)
  ) + 
  scale_y_continuous(limits = c(-2, 8), breaks = seq(-2, 8, 2)) +
  labs(x = "Brain region", y = "Estimate", color = "Model") + 
  theme_bw() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  )

ggsave(
  paste0(output_path, "fig_mri_resid.pdf"),
  height = 5, width = 7, units = "in"
)

## tabular results
resid_results %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  pivot_wider(
    id_cols = c(outcome_label),
    names_from = model, 
    values_from = out
  ) %>% 
  write.xlsx(
    paste0(output_path, "mri_results_resid.xlsx")
  )


### raw outcomes ----

# the model variable is missing for some model results using 
# raw outcomes

raw_results <- all_res_mri %>% 
  filter(
    exposure == "ACE_fac1_z", 
    outcome_raw, 
    !str_detect(outcome, "wmh"), 
    str_detect(term, exposure)
  )

## plot 
raw_results %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  ggplot(aes(x = outcome_label, y = estimate, 
             color = model, group = model)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point(position = position_dodge(width = dodge_by)) + 
  geom_errorbar(
    aes(ymin = lower, ymax = upper), 
    width = barwidth, 
    position = position_dodge(width = dodge_by)
  ) + 
  scale_y_continuous(limits = c(-2, 8), breaks = seq(-2, 8, 2)) +
  labs(x = "brain region", y = "Estimate", color = "Model") + 
  theme_bw() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

ggsave(
  paste0(output_path, "fig_mri_raw.pdf"),
  height = 5, width = 7, units = "in"
)

## tabular results
raw_results %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  pivot_wider(
    id_cols = c(outcome_label),
    names_from = model, 
    values_from = out
  ) %>% 
  write.xlsx(
    paste0(output_path, "mri_results_raw.xlsx")
  )

## individual ACE results ----

# reorder here if needed
ACE_levels <- c("w1_ACE_par_sepdiv", "w1_ACE_par_remarried", "w1_ACE_see_domviol",
                "w1_ACE_fam_substance", "w1_ACE_par_jobloss", "w1_ACE_par_jail",
                "w1_ACE_fam_illness", "w4_ACE_par_physabuse", "w4_ACE_hh_mentalill")
ACE_labels <- c("Parental separation/divorce",
                "Parent remarried", 
                "Saw domestic violence",
                "Family substance abuse",
                "Parental job loss",
                "Parent went to jail",
                "Serious illness in family",
                "Physical abuse by parent",
                "Mental illness in household")

### cog results ----

all_res_cog %>% 
  filter(
    str_detect(exposure, "w(1|4)_ACE_"),
    outcome %in% c("SENAS_exec_poolz", "SENAS_vrmem_poolz"),
    time == "Study",
    model == "Model 3",
    str_detect(term, exposure)
  ) %>% 
  mutate(
    exposure = factor(exposure, levels = ACE_levels, labels = ACE_labels)
  ) %>% 
  select(
    term, exposure, outcome, model, out
  ) %>% 
  # there are different ways to pivot this table
  # right now I have one column for each outcome
  pivot_wider(
    id_cols = c(term, exposure), 
    names_from = outcome, values_from = out
  ) %>% 
  write.xlsx(
    paste0(output_path, "cog_results_individual_ACEs.xlsx")
  )

### mri results ----

all_res_mri %>% 
  filter(
    str_detect(exposure, "w(1|4)_ACE_"),
    str_detect(outcome, "residzscore"),
    !str_detect(outcome, "wmh"), 
    model == "Model 3",
    str_detect(term, exposure)
  ) %>% 
  mutate(
    exposure = factor(exposure, levels = ACE_levels, labels = ACE_labels)
  ) %>% 
  filter(outcome_label %in% region_levels[1:8]) %>% 
  ggplot(aes(x = outcome_label, y = estimate, color = outcome_label)) + 
  geom_hline(yintercept = 0, color = "grey40") + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = barwidth) + 
  scale_y_continuous(limits = c(-0.5, 0.55), breaks = seq(-0.5, 0.5, 0.25)) +
  labs(x = NULL, y = "Estimate", color = "Brain region") + 
  facet_wrap(~ exposure, nrow = 3) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave(
  paste0(output_path, "fig_mri_individual_ACEs.pdf"),
  height = 6, width = 10, units = "in"
)

### pet results ----

all_res_pet %>%
  filter(
    str_detect(exposure, "w(1|4)_ACE_"),
    model == "Model 3",
    str_detect(term, exposure)
  ) %>%
  mutate(
    exposure = factor(exposure, levels = ACE_levels, labels = ACE_labels)
  ) %>% 
  ggplot(aes(x = exposure, y = RR)) +
  geom_hline(yintercept = 1, color = "grey40") +
  geom_point() +
  geom_errorbar(aes(ymin = LCI_RR, ymax = UCI_RR), width = barwidth) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  labs(x = "ACE", y = "Risk Ratio", color = "Brain region") +
  theme_bw() +
  # coord_flip()
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 1.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

ggsave(
  paste0(output_path, "fig_pet_individual_ACEs.pdf"),
  height = 5, width = 5, units = "in"
)

# ggsave(
#   paste0(output_path, "fig_pet_individual_ACEs_flipped.pdf"),
#   height = 4, width = 6, units = "in"
# )
