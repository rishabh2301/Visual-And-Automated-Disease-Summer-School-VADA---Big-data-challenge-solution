## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", out.width = "70%")
library(knitr)  # loading to gain access to include_graphics()
library(kableExtra) # for some options when displaying tables
library(here)
library(tidyverse)
library(magrittr)
library(modelr)
library(broom)
library(here)
library(pROC)
library(plotROC)

```{r}
set.seed(101)
data<-rider_2
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
````
#source(here("scripts", "VADA_R_part_1_purled.R"))
diabetes <- read_csv(here("data", "diabetes_data.csv"))

predictor_list <- 
  c("age", "gender", "race", 
    "specialty", "admission", "primary_diag", 
    "discharge", "time_in_hospital", 
    "change", "hba1c")

diab <- diabetes %>% 
  select(patient_nbr, readmit_30, one_of(predictor_list))

set.seed(2001)
training_sample <- diab %>% sample_frac(0.10) %>% pull(patient_nbr)

train <- diab %>% filter(patient_nbr %in% training_sample)

## ----pred_0, eval = FALSE, out.width = "55%"-----------------------------
## fit_train_main <- glm(readmit_30 ~ age + gender + race + specialty + admission +
##     primary_diag + discharge + time_in_hospital + change + hba1c, data = train)
## augment(fit_train_main) %>%  mutate(Readmitted = c("No", "Yes")[readmit_30 + 1]) %>%
##   ggplot(aes(x = .fitted, fill = Readmitted)) + geom_density(alpha = 0.2)

## ----pred_1, echo = FALSE, out.width = "55%"-----------------------------
fit_train_main <- glm(readmit_30 ~ age + gender + race + specialty + admission + 
    primary_diag + discharge + time_in_hospital + change + hba1c, data = train)
augment(fit_train_main) %>% mutate(Readmitted = c("No", "Yes")[readmit_30 + 1]) %>%
  ggplot(aes(x = .fitted, fill = Readmitted)) + geom_density(alpha = 0.2)

## ----pred_2, echo = FALSE, out.width = "55%"-----------------------------
augment(fit_train_main) %>% mutate(Readmitted = c("No", "Yes")[readmit_30 + 1]) %>%
  ggplot(aes(x = .fitted, fill = Readmitted)) + geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0.09, colour = "steelblue", size = 1.2)

## ----pred_3--------------------------------------------------------------
perf_09 <- augment(fit_train_main) %>% 
  mutate(pred_readmit = if_else(.fitted < 0.09, 0,1)) %$%
  table(pred_readmit, readmit_30)
perf_09 %>% addmargins(1:2)

## ----illus_FP_FN, echo = FALSE, out.width = "60%"------------------------
include_graphics(here("illus", "false_positive_and_false_negative.png"))

## ----roc_simple----------------------------------------------------------
example_df <- tibble(x = c(10, 11, 13, 14, 15, 16, 16, 17, 18, 19, 20),
  y = c(0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1)) 

glm(y ~ x, family = "binomial", data = example_df) %>%
  augment(type.predict = "response") %>% select(x, y, .fitted) %>% 
  mutate(TP = if_else(.fitted > 0.4 & y == 1, 1, 0),
    FP = if_else(.fitted > 0.4 & y == 0, 1, 0)) %>% round(3)

## ----roc_example, out.width = "60%"--------------------------------------
glm(y ~ x, family = "binomial", data = example_df) %>% augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = y, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens")

## ----roc_time_in_hospital_1, out.width = "60%"---------------------------
roc_plot <- glm(readmit_30 ~ time_in_hospital, family = "binomial", data = train) %>%
  augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens")
roc_plot

## ----roc_time_in_hospital_2, out.width = "55%"---------------------------
roc_plot <- glm(readmit_30 ~ time_in_hospital, family = "binomial", data = train) %>%
  augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens"); roc_plot + annotate("text", 
    x = 0.75, y = 0.2, label = str_c("AUC=", round(calc_auc(roc_plot)$AUC, 3)))

## ----roc_discharge, out.width = "55%"------------------------------------
roc_plot <- glm(readmit_30 ~ discharge, family = "binomial", data = train) %>%
  augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens"); roc_plot + annotate("text", 
    x = 0.75, y = 0.2, label = str_c("AUC=", round(calc_auc(roc_plot)$AUC, 3)))

## ----fit_train_fwd, include = FALSE, cache = TRUE------------------------
fit_train_0 <- glm(readmit_30 ~ 1, train, family = "binomial") 
fit_train_fwd <- step(fit_train_0, 
  . ~ (age + gender + race + specialty + admission + primary_diag + 
      discharge + time_in_hospital + change + hba1c)^2, 
  direction = "forward")

## ----roc_fit_train_fwd, out.width = "55%"--------------------------------
roc_plot_train_fwd <- fit_train_fwd %>%
  augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens"); roc_plot_train_fwd + annotate("text", 
    x = 0.75, y = 0.2, label = str_c("AUC=", round(calc_auc(roc_plot_train_fwd)$AUC, 2)))

## ----fit_train_main, include = FALSE, cache = TRUE-----------------------
fit_train_main <-glm(readmit_30 ~ age + gender + race + specialty + admission + 
      primary_diag + discharge + time_in_hospital + change + hba1c, 
  family = binomial, data = train)

## ----roc_fit_train_main, out.width = "55%"-------------------------------
roc_plot_train_main <- fit_train_main %>%
  augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens"); roc_plot_train_main + annotate("text", 
    x = 0.75, y = 0.2, label = str_c("AUC=", round(calc_auc(roc_plot_train_main)$AUC, 2)))

## ----fit_train_full, include = FALSE, cache = TRUE-----------------------
fit_train_full <-glm(readmit_30 ~ (age + gender + race + specialty + admission + 
      primary_diag + discharge + time_in_hospital + change + hba1c)^2, 
  family = binomial, data = train)

## ----roc_fit_train_full, echo = TRUE, out.width = "55%"------------------
roc_plot_train_full <- fit_train_full %>% augment(type.predict = "response") %>%
  ggplot() + geom_roc(aes(d = readmit_30, m = .fitted), n.cuts = 11, labelround = 2) +
  style_roc(xlab = "1 - spec", ylab = "sens"); roc_plot_train_full + annotate("text", 
    x = 0.75, y = 0.2, label = str_c("AUC=", round(calc_auc(roc_plot_train_full)$AUC, 2)))

## ----roc_compare_1, cache = TRUE-----------------------------------------
# we are using predict() here instead of augment().  Same effect.
roc_data_train <- train %>% select(patient_nbr, readmit_30) %>%
  mutate(fit_train_full = predict(fit_train_full, train, type = "response"),
    fit_train_main = predict(fit_train_main, train, type = "response"),
    fit_train_fwd = predict(fit_train_fwd, train, type = "response"))

## ----roc_compare_1_5, eval = FALSE, cache = TRUE, out.width = "55%"------
## roc_data_train %>% gather(model, pred_prob, -patient_nbr, -readmit_30) %>%
##   ggplot(aes(d = readmit_30, m = pred_prob, colour = model)) +
##   geom_roc(n.cuts = 0) + style_roc(xlab = "1 - spec", ylab = "sens") +
##   annotate("text", x = 0.75, y = c(0.2, 0.15, 0.10),
##     label = str_c("AUC=", round(
##       c(calc_auc(roc_plot_train_full)$AUC,
##         calc_auc(roc_plot_train_main)$AUC,
##         calc_auc(roc_plot_train_fwd)$AUC), 2)),colour = c("red", "darkgreen", "blue"))

## ----roc_compare_2, echo = FALSE, cache = TRUE, out.width = "65%"--------
roc_data_train %>% gather(model, pred_prob, -patient_nbr, -readmit_30) %>%
  ggplot(aes(d = readmit_30, m = pred_prob, colour = model)) + 
  geom_roc(n.cuts = 0) + style_roc(xlab = "1 - spec", ylab = "sens") + 
  annotate("text", x = 0.75, y = c(0.2, 0.15, 0.10), 
    label = str_c("AUC=", round(
      c(calc_auc(roc_plot_train_full)$AUC, 
        calc_auc(roc_plot_train_main)$AUC,
        calc_auc(roc_plot_train_fwd)$AUC), 2)),colour = c("red", "darkgreen", "blue"))

## ----illus_cv, echo = FALSE, out.width = "60%"---------------------------
include_graphics(here("illus", "train-test_split.png"))

## ----roc_test_0, cache = TRUE--------------------------------------------
test <- diab %>% filter(not(patient_nbr %in% training_sample))

roc_data_test <- test %>% select(patient_nbr, readmit_30) %>%
  mutate(fit_train_full = predict(fit_train_full, test, type = "response"),
    fit_train_main = predict(fit_train_main, test, type = "response"),
    fit_train_fwd = predict(fit_train_fwd, test, type = "response"))

## ----roc_test_1, eval = FALSE, cache = TRUE, out.width = "60%"-----------
## roc_plot_test_fwd <- bind_rows(
##   roc_data_test %>% select(readmit_30, fit_train_fwd) %>% mutate(dataset = "test"),
##   roc_data_train %>% select(readmit_30, fit_train_fwd) %>% mutate(dataset = "train")) %>%
##   ggplot() +
##   geom_roc(aes(d = readmit_30, m = fit_train_fwd, colour = dataset), n.cuts = 0) +
##   style_roc(xlab = "1 - spec", ylab = "sens")
## roc_plot_test_fwd + annotate("text",
##     x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=",
##       round(calc_auc(roc_plot_test_fwd)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----roc_test_1_1, echo = FALSE, cache = TRUE, out.width = "60%"---------
roc_plot_test_fwd <- bind_rows(
  roc_data_test %>% select(readmit_30, fit_train_fwd) %>% mutate(dataset = "test"),
  roc_data_train %>% select(readmit_30, fit_train_fwd) %>% mutate(dataset = "train")) %>%
  ggplot() + 
  geom_roc(aes(d = readmit_30, m = fit_train_fwd, colour = dataset), n.cuts = 0) +
  style_roc(xlab = "1 - spec", ylab = "sens")
roc_plot_test_fwd + annotate("text", 
    x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=", 
      round(calc_auc(roc_plot_test_fwd)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----roc_test_3, eval = FALSE, cache = TRUE, out.width = "60%"-----------
## roc_plot_test_main <- bind_rows(
##   roc_data_test %>% select(readmit_30, fit_train_main) %>% mutate(dataset = "test"),
##   roc_data_train %>% select(readmit_30, fit_train_main) %>% mutate(dataset = "train")) %>%
##   ggplot() +
##   geom_roc(aes(d = readmit_30, m = fit_train_main, colour = dataset), n.cuts = 0) +
##   style_roc(xlab = "1 - spec", ylab = "sens")
## roc_plot_test_main + annotate("text",
##     x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=",
##       round(calc_auc(roc_plot_test_main)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----roc_test_3_1, echo = FALSE, cache = TRUE, out.width = "60%"---------
roc_plot_test_main <- bind_rows(
  roc_data_test %>% select(readmit_30, fit_train_main) %>% mutate(dataset = "test"),
  roc_data_train %>% select(readmit_30, fit_train_main) %>% mutate(dataset = "train")) %>%
  ggplot() + 
  geom_roc(aes(d = readmit_30, m = fit_train_main, colour = dataset), n.cuts = 0) +
  style_roc(xlab = "1 - spec", ylab = "sens")
roc_plot_test_main + annotate("text", 
    x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=", 
      round(calc_auc(roc_plot_test_main)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----roc_test_4, eval = FALSE, cache = TRUE, out.width = "60%"-----------
## roc_plot_test_full <- bind_rows(
##   roc_data_test %>% select(readmit_30, fit_train_full) %>% mutate(dataset = "test"),
##   roc_data_train %>% select(readmit_30, fit_train_full) %>% mutate(dataset = "train")) %>%
##   ggplot() +
##   geom_roc(aes(d = readmit_30, m = fit_train_full, colour = dataset), n.cuts = 0) +
##   style_roc(xlab = "1 - spec", ylab = "sens")
## roc_plot_test_full + annotate("text",
##     x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=",
##       round(calc_auc(roc_plot_test_full)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----roc_test_4_1, echo = FALSE, cache = TRUE, out.width = "60%"---------
roc_plot_test_full <- bind_rows(
  roc_data_test %>% select(readmit_30, fit_train_full) %>% mutate(dataset = "test"),
  roc_data_train %>% select(readmit_30, fit_train_full) %>% mutate(dataset = "train")) %>%
  ggplot() + 
  geom_roc(aes(d = readmit_30, m = fit_train_full, colour = dataset), n.cuts = 0) +
  style_roc(xlab = "1 - spec", ylab = "sens")
roc_plot_test_full + annotate("text", 
    x = 0.75, y = c(0.2, 0.15), label = str_c("AUC=", 
      round(calc_auc(roc_plot_test_full)$AUC, 2)), colour = c("#F8766D", "#00BFC4"))

## ----optimism, echo = FALSE----------------------------------------------
optimism <- as_tibble(round(
    rbind(calc_auc(roc_plot_test_fwd)$AUC,
      calc_auc(roc_plot_test_main)$AUC,
      calc_auc(roc_plot_test_full)$AUC), 3)) %>% 
  rename(`AUC in test` = V1, `AUC in train` = V2) %>%
  cbind(Model = c("fit_train_fwd", "fit_train_main", "fit_train_full")) %>%
  mutate(Optimism = `AUC in train` - `AUC in test`) %>%
  select(Model, `AUC in train`, `AUC in test`, Optimism)
optimism %>% kable(digits = 3)

## ---- illus_cv_1, echo = FALSE-------------------------------------------
include_graphics(here("illus", "CV_splits.png"))

## ----illus_cv_2, echo = FALSE--------------------------------------------
include_graphics(here("illus", "CV-fold_1.png"))

## ---- illus_cv_3, echo = FALSE-------------------------------------------
include_graphics(here("illus", "CV-fold_2.png"))

## ----cv_in_r_1-----------------------------------------------------------
logistic_model_main <- function(df){
  glm(readmit_30 ~ age + gender + race + specialty + admission + 
    primary_diag + discharge + change + hba1c + time_in_hospital, 
    family = "binomial", data = df)
}

## ----cv_in_r_2, cache = TRUE---------------------------------------------
fit_full_cv <- diab %>% 
  crossv_kfold(k = 10) %>%
  mutate(fitted_model = map(train, logistic_model_main))

## ----cv_in_r_3, cache = TRUE---------------------------------------------
fit_full_cv_perf <- fit_full_cv %>% 
  mutate(fitted = map2(fitted_model, test, ~ augment(.x, newdata = .y, type.predict = "response"))) %>%
  #unnest(fitted, pred_prob) %>% 
  unnest(fitted) %>%
  select(.id, readmit_30, .fitted)

## ----cv_in_r_4, cache = TRUE---------------------------------------------
cv_auc_full <- fit_full_cv_perf %>% 
  group_by(.id) %>%
  summarize(AUC = roc(readmit_30, .fitted)$auc) %>%
  select(AUC) %>% summarize(mean(AUC)) %>% unlist() %>% unname()
round(cv_auc_full, 3)

## ----cv_in_r_5, cache = TRUE---------------------------------------------
fit_full_all <- diab %>% 
  glm(readmit_30 ~ age + gender + race + specialty + admission + 
    primary_diag + discharge + change + hba1c + time_in_hospital, 
    family = "binomial", data = .)
augment(fit_full_all, type.predict = "response") %$%   # Note the "exposition" operator %$%
  roc(readmit_30, .fitted)$auc %>% round(3)

