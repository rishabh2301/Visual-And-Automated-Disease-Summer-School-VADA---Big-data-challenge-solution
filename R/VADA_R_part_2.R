## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", out.width = "70%")
library(knitr)  # loading to gain access to include_graphics()
library(here)


## ----loading, message = FALSE--------------------------------------------
library(tidyverse)
library(broom)
library(here)

diabetes_raw <- read_csv("diabetes_data.csv")

predictor_list <- 
  c("age", "gender", "race", 
    "specialty", "admission", "primary_diag", 
    "discharge", "time_in_hospital", 
    "change", "hba1c")

diab <- diabetes_raw %>% select(patient_nbr, readmit_30, one_of(predictor_list))

## ----summary-------------------------------------------------------------
nrow(diab)  

diab %$% table(readmit_30) %>% addmargins(1)

diab %$% table(gender, readmit_30) %>% addmargins(2)

## ----summary_2-----------------------------------------------------------
diab %$% table(race, readmit_30) %>% addmargins(2)

diab %$% table(age, readmit_30) %>% addmargins()

## ----summary_3-----------------------------------------------------------
diab %$% table(discharge, readmit_30) %>% addmargins()

## ---- echo = FALSE, lin_pred_fail----------------------------------------
mtcars %>% select(x = mpg , success = vs) %>% 
  ggplot(aes(x = x, y = success)) + geom_point() + geom_smooth(method = "lm")

## ---- echo = FALSE, out.width = "50%"------------------------------------
tibble(t = seq(-6, 6, length = 51), p = 1/(1+exp(-t)), x2 = -t) %>%
  ggplot(aes(x = t)) + geom_line(aes(y = p), colour = "darkblue", size = 1.5) + 
  geom_line(aes(y = 1 - p), colour = "darkgreen", size = 1.5) +
  geom_text(x = 4, y = 0.2, label = "negative slope", colour = "darkgreen") +
  geom_text(x = 4, y = 0.9, label = "positive slope", colour = "darkblue") +
  theme_bw()

## ----logistic_success, echo = FALSE, out.width = "60%"-------------------
mtcars %>% select(x = mpg , success = vs) %>% 
  ggplot(aes(x = x, y = success)) + geom_point() + 
  stat_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_text(x = 12, y = 0.2, label = "predicted > 0\nconf int > 0",
    colour = "dark red") +
  geom_text(x = 30, y = 0.8, label = "predicted < 1\nconf int < 1",
    colour = "dark red")

## ----diab_fig_1, out.width = "60%"---------------------------------------
diab %>% 
  ggplot(aes(x = time_in_hospital, y = readmit_30)) + geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

## ----diab_fig_2, out.width = "60%"---------------------------------------
diab %>% sample_frac(0.05) %>%  # use only a small fraction of the data
  ggplot(aes(x = time_in_hospital, y = readmit_30)) + 
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

## ----diab_fig_3, eval = FALSE--------------------------------------------
## diab %>% sample_frac(0.05) %>%  # use only a small fraction of the data
##   ggplot(aes(x = time_in_hospital, y = readmit_30)) +
##   geom_jitter(width = 0.1, height = 0.05, alpha = 0.2) +
##   geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

## ----fit_age_1-----------------------------------------------------------
fit_age <- glm(readmit_30 ~ factor(age), family = "binomial", data = diab)
summary(fit_age)

## ----fit_age_2-----------------------------------------------------------
anova(fit_age, test = "LRT")

## ---- cache = TRUE-------------------------------------------------------
coef_age <- tidy(fit_age); coef_age
p_age <- anova(fit_age, test = "LRT")[["Pr(>Chi)"]][2]
p_age

## ---- cache = TRUE-------------------------------------------------------
fit_gender <- glm(readmit_30 ~ gender, family = "binomial", data = diab)
# summary(fit_gender)
coef_gender <- tidy(fit_gender); coef_gender
p_gender <- anova(fit_gender, test = "LRT")[["Pr(>Chi)"]][2]; p_gender

## ---- cache = TRUE-------------------------------------------------------
fit_race <- glm(readmit_30 ~ race, family = "binomial", data = diab)
# summary(fit_race)
coef_race <- tidy(fit_race); coef_race
p_race <- anova(fit_race, test = "LRT")[["Pr(>Chi)"]][2]; p_race

## ---- cache = TRUE-------------------------------------------------------
fit_specialty <- glm(readmit_30 ~ specialty, family = "binomial", data = diab)
coef_specialty <- tidy(fit_specialty)
p_specialty <-  anova(fit_specialty, test = "LRT")[["Pr(>Chi)"]][2]

fit_admission <- glm(readmit_30 ~ admission, family = "binomial", data = diab)
coef_admission <- tidy(fit_admission)
p_admission <-  anova(fit_admission, test = "LRT")[["Pr(>Chi)"]][2]

fit_primary_diag <- glm(readmit_30 ~ primary_diag, family = "binomial", data = diab)
coef_primary_diag <- tidy(fit_primary_diag)
p_primary_diag <-  anova(fit_primary_diag, test = "LRT")[["Pr(>Chi)"]][2]

## ---- cache = TRUE-------------------------------------------------------
fit_discharge <- glm(readmit_30 ~ discharge, family = "binomial", data = diab)
coef_discharge <- tidy(fit_discharge)
p_discharge <-  anova(fit_discharge, test = "LRT")[["Pr(>Chi)"]][2]

fit_time <- glm(readmit_30 ~ time_in_hospital, family = "binomial", data = diab)
coef_time <- tidy(fit_time)
p_time <- anova(fit_discharge, test = "LRT")[["Pr(>Chi)"]][2]

## ---- cache = TRUE-------------------------------------------------------
fit_change <- glm(readmit_30 ~ change, family = "binomial", data = diab)
coef_change <- tidy(fit_change)
p_change <- anova(fit_change, test = "LRT")[["Pr(>Chi)"]][2]

fit_hba1c <- glm(readmit_30 ~ hba1c, family = "binomial", data = diab)
# summary(fit_hba1c)
coef_hba1c <- tidy(fit_hba1c)
p_hba1c <- anova(fit_hba1c, test = "LRT")[["Pr(>Chi)"]][2]

## ---- cache = TRUE-------------------------------------------------------
# bind them together by row, then drop the intercept in each 
# model; they aren't a concern right now
single_pred_coef <- bind_rows(coef_age, coef_gender, coef_race, 
  coef_specialty, coef_admission, coef_primary_diag, 
  coef_discharge, coef_time, coef_change, coef_hba1c, 
  .id = "predictor") %>%
  mutate(predictor = as.numeric(predictor))

single_pred_overall <- 
  tibble(predictor = 1:10, 
    p.overall = c(p_age, p_gender, p_race,
    p_specialty, p_admission, p_primary_diag, 
    p_discharge, p_time, p_change, p_hba1c)) 

## ---- cache = TRUE-------------------------------------------------------
single_pred <- single_pred_coef %>% 
  left_join(single_pred_overall) %>%
  select(predictor, term, estimate, std.error, p.value, p.overall) %>%
  mutate(
    estimate = if_else(term == "(Intercept)", NA_real_, estimate),
    std.error = if_else(term == "(Intercept)", NA_real_, std.error),
    p.value = if_else(term == "(Intercept)", p.overall, p.value),
    term = if_else(term == "(Intercept)", 
      str_c(predictor_list[predictor], " overall"), term))

## ---- cache = TRUE-------------------------------------------------------
single_pred <- single_pred %>% 
  mutate(odds_ratio = round(exp(estimate), 4),
    or_lower = round(exp(estimate - 1.96*std.error), 4),
    or_upper = round(exp(estimate + 1.96*std.error), 4), 
    p.value = round(p.value, 4)) %>% 
  select(term, odds_ratio, or_lower, or_upper, p.value) %>%
  mutate(sig = case_when(
    p.value < 0.01 ~ "**",  p.value < 0.05 ~ "*",
    p.value < 0.10 ~ "+",   p.value < 0.20 ~ ".",
    TRUE ~ ""))
#View(single_pred)

