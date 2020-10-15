## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", out.width = "70%")
library(knitr)  # loading to gain access to inclue_graphics()
library(here)
library(tidyverse)
library(modelr)
library(broom)
library(here)

#source(here("scripts", "VADA_R_part_1_purled.R"))
diabetes <- read_csv(here("data", "diabetes_data.csv"))

predictor_list <- 
  c("age", "gender", "race", 
    "specialty", "admission", "primary_diag", 
    "discharge", "time_in_hospital", 
    "change", "hba1c")

diab <- diabetes %>% 
  select(patient_nbr, readmit_30, one_of(predictor_list))

## ----mlr_1---------------------------------------------------------------
diab %>% model_matrix(
  formula = readmit_30 ~ (age + gender + race + specialty + admission + 
  primary_diag + discharge + time_in_hospital + change + hba1c)^2) %>% dim()

## ----make_train----------------------------------------------------------
set.seed(2001)
training_sample <- diab %>% sample_frac(0.10) %>% pull(patient_nbr)

train <- diab %>% filter(patient_nbr %in% training_sample)

## ----forward_1, cache = TRUE---------------------------------------------
fit_train_0 <- glm(readmit_30 ~ 1, train, family = "binomial")

add1(fit_train_0, ~ age + gender + race + specialty + admission + 
  primary_diag + discharge + time_in_hospital + change + hba1c)

## ----forward_2, cache = TRUE---------------------------------------------
fit_train_1 <- update(fit_train_0, . ~ . + discharge)  # or specify using glm()
glance(fit_train_0)$AIC; glance(fit_train_1)$AIC
add1(fit_train_1, . ~ . + age + gender + race + specialty + admission + 
  primary_diag + discharge + time_in_hospital + change + hba1c)

## ----forward_3, cache = TRUE---------------------------------------------
fit_train_2 <- update(fit_train_1, . ~ . + time_in_hospital)
glance(fit_train_1)$AIC; glance(fit_train_2)$AIC
add1(fit_train_2, . ~ . + age + gender + race + specialty + admission + 
  primary_diag + discharge + time_in_hospital + change + hba1c)

## ----forward_4, cache = TRUE---------------------------------------------
fit_train_3 <- update(fit_train_2, . ~ . + primary_diag)
glance(fit_train_2)$AIC; glance(fit_train_3)$AIC
add1(fit_train_3, . ~ . + age + gender + race + specialty + admission + 
  primary_diag + discharge + time_in_hospital + change + hba1c)

## ----null----------------------------------------------------------------
add1(fit_train_0, 
  . ~ (age + gender + race + specialty + admission + primary_diag + 
      discharge + time_in_hospital + change + hba1c)^2)

## ----forward_5, cache = TRUE---------------------------------------------
fit_train_fwd <- step(fit_train_0, 
  . ~ (age + gender + race + specialty + admission + primary_diag + 
      discharge + time_in_hospital + change + hba1c)^2, 
  direction = "forward")

## ----anova_fwd-----------------------------------------------------------
anova(fit_train_fwd)

## ----back, cache = TRUE--------------------------------------------------
fit_train_full <- glm(
  readmit_30 ~ (age + gender + race + specialty + admission + 
      primary_diag + discharge + time_in_hospital + change + hba1c)^2, 
  family = binomial, data = train)

fit_train_back <- step(fit_train_full, direction = "backward")

## ----anova_back, cache = TRUE--------------------------------------------
anova(fit_train_back)

## ----step, cache = TRUE--------------------------------------------------
fit_train_sw<- step(fit_train_0, 
  . ~ (age + gender + race + specialty + admission + primary_diag + 
      discharge + time_in_hospital + change + hba1c)^2, 
  direction = "both")

## ----anova_step----------------------------------------------------------
anova(fit_train_sw)

