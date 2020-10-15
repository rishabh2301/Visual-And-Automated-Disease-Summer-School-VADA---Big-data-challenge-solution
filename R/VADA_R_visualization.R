## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(knitr)  # loading to gain access to include_graphics()
library(here)
library(tidyverse)
library(modelr)
library(broom)
library(here)
library(pROC)
library(plotROC)

diabetes_raw <- read_csv(here("data", "diabetes_data.csv"))

diab <- diabetes_raw %>%
    filter(gender != "Unknown/Invalid")

#need all predictors to be factors
diab <- diab %>% mutate(race=factor(race),
                        specialty=factor(specialty),
                        admission=factor(admission),
                        primary_diag=factor(primary_diag),
                        discharge=factor(discharge),
                        gender=factor(gender), 
                        age=factor(age),
                        hba1c=factor(hba1c),
                        change=factor(change))

## ----models1-------------------------------------------------------------
#fit main effects model using glm
fit_all <- glm(readmit_30 ~ discharge + gender + race + admission + specialty + 
                 time_in_hospital + age + primary_diag + hba1c + change, 
               family = "binomial", data=diab)

#model with all interactions
fit_allint <- glm(readmit_30 ~ (discharge + gender + race + admission + specialty + 
                                  time_in_hospital + age + primary_diag + hba1c + 
                                  change)^2, 
                  family = "binomial", data=diab)

## ----models2-------------------------------------------------------------
#model from forward selection
step_forward <- glm(formula = readmit_30 ~ discharge + gender + race + admission + 
                      specialty + time_in_hospital + age + primary_diag + hba1c + 
                      change + time_in_hospital:primary_diag + discharge:primary_diag + 
                      specialty:age + discharge:time_in_hospital + discharge:race + 
                      admission:age + gender:primary_diag + admission:primary_diag + 
                      time_in_hospital:hba1c + specialty:change + gender:age + 
                      gender:admission + discharge:specialty + specialty:time_in_hospital + 
                      gender:hba1c + discharge:gender, family = "binomial", data = diab)

## ----predicted_values1, warning=FALSE------------------------------------
#Main effects model
predict_all <- predict(fit_all, type="response")
model <- rep("Main effects", times=69981)
all_table <- data.frame(diab$readmit_30, predicted_values=predict_all, model)

#Model with all two-way interactions
predict_allint <- predict(fit_allint, type="response")
model <- rep("All interactions", rep=69981)
allint_table <- data.frame(diab$readmit_30, predicted_values=predict_allint, model)

#Model from forward selection
predict_forwardint <- predict(step_forward, type="response")
model <- rep("Forward selection", rep=69981)
forwardint_table <- data.frame(diab$readmit_30, predicted_values=predict_forwardint, model)

#Put all predicted values into one data frame
all_models <- bind_rows(all_table, allint_table, forwardint_table)

## ----AUC1----------------------------------------------------------------
#calculate the AUC and 95% confidence interval
roc_all<-roc(diab$readmit_30, predict_all, ci=TRUE)
roc_all

## ----AUC2----------------------------------------------------------------
#calculate the AUC and 95% confidence interval
roc_allint<-roc(diab$readmit_30, predict_allint, ci=TRUE)
roc_allint

## ----AUC3----------------------------------------------------------------
#calculate the AUC and 95% confidence interval
roc_forwardint<-roc(diab$readmit_30, predict_forwardint, ci=TRUE)
roc_forwardint

## ----ROC1----------------------------------------------------------------
#now lets plot the three curves
r1<-ggplot(all_models, aes(d=diab.readmit_30, m=predicted_values, colour=model)) + 
  geom_roc(labelround = 4, n.cuts=0)  + 
  scale_colour_manual(values=c("blue","red", "darkgreen")) +
  labs(y="Sensitivity", x="1-Specificity")

auc1<-round(calc_auc(r1)$AUC, 4);

roc <- r1 + annotate("text",  x=0.60, y = 0.25, label=paste("AUC main effects: ", 
                                           auc1[3], ", 95% CI = 0.6020-0.6163")) +
  annotate("text",  x=0.60, y = 0.20, label=paste("AUC forward selection: ", 
                                           auc1[2], ", 95% CI = 0.6161-0.6303")) +
  annotate("text",  x=0.60, y = 0.15, label=paste("AUC all interactions: ", 
                                           auc1[1], ", 95% CI = 0.6292-0.6433"))


## ----ROC2, echo=FALSE----------------------------------------------------
roc

## ----save----------------------------------------------------------------
ggsave("ROC curves.pdf", roc)

## ----plotexample, echo=FALSE---------------------------------------------
include_graphics(here("illus", "Mosiac_dot_plot.png"))

## ----echo=FALSE----------------------------------------------------------
#Look at the odds ratios for main effects model
#exp(cbind("Odds ratio" = coef(fit_all), confint.default(fit_all, level = 0.95)))

## ----predictions1--------------------------------------------------------
p_other <- predict(fit_all, data.frame(discharge="Other", gender="Female", race="Caucasian",
                                       admission="Emergency room", 
                                       specialty="Family/general practice", 
                                       time_in_hospital=10, age="[30, 60)", 
                                       primary_diag="Diabetes", hba1c=">8", 
                                       change="Ch"), type="response")
p_other

## ----predictions2--------------------------------------------------------
p_home <- predict(fit_all, data.frame(discharge="Discharge to home", gender="Female", 
                                      race="Caucasian", admission="Emergency room", 
                                      specialty="Family/general practice", 
                                      time_in_hospital=10, age="[30, 60)", 
                                      primary_diag="Diabetes", hba1c=">8", change="Ch"),
                  type="response")
p_home 

## ----predictions3--------------------------------------------------------
#number of these individuals predicted to have readmission out of 100 of the same people
readmission_other <- rep(c("No readmission", "Readmission"), times=c(86, 14))
readmission_home <- rep(c("No readmission", "Readmission"), times=c(91, 9))

#create a grid to have 100 points
x <- rep(1:10, times=10)
y <- rep(1:10, each=10)

df <- data.frame(x, y, readmission_other, readmission_home)

## ----predictions4--------------------------------------------------------
p1 <- ggplot(df, aes(x=x, y=y, color=readmission_other)) + 
  geom_point(aes(size=2),  group=readmission_other) +
  theme_void() +
  scale_colour_discrete(name = "Readmitted",
                        breaks=c("Readmission", "No readmission"),
                        labels=c("Yes", "No")) +
  scale_size(guide = 'none') +
  ggtitle("Discharge to other") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = margin(0, 10, 0, 0, unit="pt"))

## ----predictions5, echo=FALSE--------------------------------------------
p1

## ----predictions6--------------------------------------------------------
p2 <- ggplot(df, aes(x=x, y=y, color=readmission_home)) + 
  geom_point(aes(size=2),  group=readmission_home) +
  theme_void() +
  scale_colour_discrete(name = "Readmitted",
                        breaks=c("Readmission", "No readmission"),
                        labels=c("Yes", "No")) +
  scale_size(guide = 'none') +
  ggtitle("Discharge to home") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = margin(0, 0, 0, 10, unit="pt"))

## ----predictions7, echo=FALSE--------------------------------------------
p2

## ----predictions8, echo=FALSE, warning=FALSE, message=FALSE--------------
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

## ----personograph1, message=FALSE----------------------------------------
library(personograph)
list("Readmit if Home" = p_home, "Add Readmit if Other" = p_other-p_home, 
     "Not Readmit" = 1-p_other)


## ----personograph2, eval=FALSE-------------------------------------------
## personograph(rev(list("Readmit if Home" = p_home,
##                       "Add Readmit if Other" = p_other-p_home,
##                       "Not Readmit" = 1-p_other)),
##             colors = c("Readmit if Home" = "red",
##                        "Add Readmit if Other" = "yellow",
##                        "Not Readmit" = "green"))
## 

## ----personograph3, echo=FALSE-------------------------------------------
personograph(rev(list("Readmit if Home" = p_home, "Add Readmit if Other" = p_other-p_home, "Not Readmit" = 1-p_other)), 
colors = c("Readmit if Home" = "red", "Add Readmit if Other" = "yellow", "Not Readmit" = "green"))


## ----nomogram1, warning=FALSE, message=FALSE-----------------------------
#create a nomogram for the main effects model
library(rms)

#fit main effects model using lrm since the nomogram function needs a model from lrm function
fit_lrm <- lrm(readmit_30 ~ discharge + gender + race + admission + specialty + 
                 time_in_hospital + age + primary_diag + hba1c + change, data=diab)
dd <- datadist(diab)
options(datadist="dd")
#summary(fit_lrm)

## ----nomogram2, eval=FALSE-----------------------------------------------
## plot(nomogram(fit_lrm, fun = function(x)1/(1+exp(-x)), funlabel="Risk of readmission",
##               lp=FALSE))

## ----nomogram3, echo=FALSE, fig.height=7---------------------------------
plot(nomogram(fit_lrm, fun = function(x)1/(1+exp(-x)), funlabel="Risk of readmission",
              lp=FALSE))

## ----nomogram4, eval=FALSE-----------------------------------------------
## #fit a colour version of the nomogram
## library(VRPM)
## colplot(fit_all)

## ----nomogram5, echo=FALSE-----------------------------------------------
include_graphics(here("illus", "Colour_nomogram.png"))

## ----nomogram6, eval=FALSE-----------------------------------------------
## #fit a interactive version of a nomogram
## library(DynNom)
## DynNom(fit_all, diab, clevel=0)

