## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", out.width = "70%")
library(knitr)  # loading to gain access to inclue_graphics()
library(here)

source(here("scripts", "VADA_R_part_1_purled.R"))

library(tidyverse)
library(broom)
library(here)

predictor_list <- 
  c("age_cat", "gender", "race_cat", 
    "specialty", "admission", "primary_diag", 
    "discharge", "time_in_hospital", 
    "change", "hba1c")

diab <- diabetes %>% 
  rename(hba1c = A1Cresult) %>%
  select(patient_nbr, readmit_30, one_of(predictor_list)) %>%
  filter(gender != "Unknown/Invalid")

## ----tea_party, echo = FALSE, out.width = "60%"--------------------------
include_graphics(here("illus", "tea_party_interaction.png"))

## ----fit_interaction-----------------------------------------------------
fit_inter <- diab %>% 
  glm(readmit_30 ~ time_in_hospital * discharge, family = binomial, data = .)
fit_inter %>% tidy() %>% transmute(term, estimate = round(estimate, 5), p.value = round(p.value, 5))

## ----fig_model_inter, out.width = "60%"----------------------------------
expand(diab, discharge, time_in_hospital) %>% # creates a tbl with all combinations
  augment(fit_inter, newdata = ., type.predict = "link") %>% # get predicted values
  ggplot(aes(x = time_in_hospital, y = .fitted, colour = discharge)) + 
  geom_line() + xlab("days in hosp") + ylab("log-odds of readmission")

## ----fig_model_inter_prob, echo = FALSE, out.width = "60%"---------------
expand(diab, discharge, time_in_hospital) %>% # creates a tbl with all combinations
  augment(fit_inter, newdata = ., type.predict = "response") %>% # get predicted values
  ggplot(aes(x = time_in_hospital, y = .fitted, colour = discharge)) + 
  geom_line() + xlab("days in hosp") + ylab("prob of readmission")

## ----interp_inter_2------------------------------------------------------
fit_inter_2 <- diab %>% 
  glm(readmit_30 ~ -1 + discharge + discharge:time_in_hospital, family = binomial, data = .)
fit_inter_2 %>% tidy() %>% transmute(term, estimate = round(estimate, 5), p.value = round(p.value, 5))

## ----soln----------------------------------------------------------------
glm(readmit_30 ~ discharge*gender, data = diab) %>% tidy() %>% 
  transmute(term, OR = round(exp(estimate), 4), p.value = round(p.value, 4))

fit_dg <- tidy(glm(readmit_30 ~ discharge*gender, data = diab))
fit_dg %>% mutate(estimate = round(estimate, 3), std.error = round(std.error, 3), 
  statistic = round(statistic, 3), p.value = round(p.value, 3))

## ----soln_2--------------------------------------------------------------
#calculate the odds ratios for each gender taking into account the interaction
fit_dg <- fit_dg %>% mutate(odds_ratio = exp(estimate))

#odds ratio for other/home discharge for Females
OR_F <- fit_dg[2, 6]

#odds ratio for other/home discharge for Males
OR_M <- fit_dg[2, 6]*fit_dg[4, 6]
OR_F
OR_M

## ----soln_3--------------------------------------------------------------
glm(readmit_30 ~ -1 + gender + gender:discharge, data = diab) %>% tidy() %>% 
  transmute(term, OR = round(exp(estimate), 4), p.value = round(p.value, 4))

## ----fig_model_inter_prob_2,, out.width = "55%"--------------------------
expand(diab, discharge, time_in_hospital) %>% # creates a tbl with all combinations
  augment(fit_inter, newdata = ., type.predict = "response") %>% # get predicted values
  ggplot(aes(x = time_in_hospital, y = .fitted, colour = discharge)) + 
  geom_line() + xlab("days in hosp") + ylab("prob of readmission") + ylim(0,1)

