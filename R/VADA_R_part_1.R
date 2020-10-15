## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)  # loading to gain access to inclue_graphics()

## ----notes, include=FALSE------------------------------------------------
# last mod: 2018-06-23

# TO DO/discuss
# + Do we want to load only minimal packages?  Or show additional "related ones"? 
#   e.g., readr may not be necessary here.

# library(readr)
#library("tidyverse", lib.loc="C:/Users/umeinar6/Documents/R/library")

## ----libraries, echo = TRUE, message = FALSE-----------------------------
library(here)       # simplifies file references
library(tidyverse)  # basis for the modern R "dialect"" we use
library(magrittr)   # member of tidyverse, allows %$% and others

## ---- echo = FALSE, out.width = "70%"------------------------------------
include_graphics(here("illus","wrangling.png"))

## ---- echo = FALSE, out.width = "90%"------------------------------------
include_graphics(here("illus", "tidyverse-logo.png"))

## ------------------------------------------------------------------------
mtcars

## ------------------------------------------------------------------------
as_tibble(mtcars)

## ------------------------------------------------------------------------
mtcars %>% rownames_to_column("model") 

## ------------------------------------------------------------------------
mtcars %>% filter(mpg > 20) %>%
  mutate(`engine size` = 
      if_else(disp > median(disp), "large", "small")) %$%
  table(`engine size`, cyl)

## ---- eval = FALSE-------------------------------------------------------
## mtcars_20 <- mtcars[mtcars$mpg > 20, ]  # or base R subset()
## mtcars_20$engine_size <-
##   ifelse(mtcars_20$disp > median(mtcars_20$disp), "large", "small")
## table(mtcars_20$engine_size, mtcars_20$cyl)
## # or, you can use with(); e.g. with(mtcars_20, table(engine_size, cyl))

## ---- message=FALSE, cache = TRUE----------------------------------------
#diabetes_raw <- read_csv("C:/Users/umeinar6/Documents/VADA 2018/dataset_diabetes/diabetic_data.csv")
diabetes_raw <- read_csv(here("data", "diabetic_data.csv"))

## ------------------------------------------------------------------------
diabetes_raw

## ---- eval = FALSE-------------------------------------------------------
## # how many records per pt?
## diabetes_raw %>% count(patient_nbr)
## 
## # how many pts with n records?
## diabetes_raw %>% count(patient_nbr) %>% count(n)

## ---- message = FALSE----------------------------------------------------
# load the combined info
IDs_mapping <- read_csv(here("data", "IDs_mapping.csv"), col_names = FALSE)

## ------------------------------------------------------------------------
admission_type_mapping <- IDs_mapping %>% 
  filter(row_number() %in% 2:9) %>% 
  mutate(admission_type_id = as.numeric(X1)) %>%
  select(admission_type_id, 
    admission_type = X2)     # or could use rename() function instead

discharge_disposition_mapping <- IDs_mapping %>% 
  filter(row_number() %in% 12:41) %>% 
  mutate(discharge_disposition_id = as.numeric(X1)) %>%
  select(discharge_disposition_id, 
    discharge_disposition = X2)  

admission_source_mapping <- IDs_mapping %>% 
  filter(row_number() %in% 44:68) %>% 
  mutate(admission_source_id = as.numeric(X1)) %>%
  select(admission_source_id, 
    admission_source = X2) 

## ---- message = FALSE----------------------------------------------------
# ID 10 is neonates... maybe not right to excl. -- though
# there are only 6 pts in that group. Others??
discharge_id_to_exclude <- c(10, 11, 13, 14, 19, 20, 21)

diabetes <- diabetes_raw %>% 
  left_join(admission_type_mapping) %>%
  left_join(discharge_disposition_mapping) %>%
  left_join(admission_source_mapping) %>%
  filter(not(discharge_disposition_id %in% discharge_id_to_exclude))

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
   # distinct keeps only first rec per pt, .keep_all retains all variables
  distinct(patient_nbr, .keep_all = TRUE) 
nrow(diabetes)   # paper reports 69,984 encounters as final dataset

## ------------------------------------------------------------------------
diabetes <- diabetes %>% na_if("?")    # convert "?" to R's NA

## ---- eval = FALSE-------------------------------------------------------
## diabetes <- diabetes %>% mutate(
##   age = if_else(age == "?", NA_character_, age))
##   x = ...)  # and so on, with possibly different appraoch for each var.

## ------------------------------------------------------------------------
diabetes %$% table(readmitted, useNA="always") 

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  mutate(readmit_30 = ifelse(readmitted == "<30", 1, 0))

diabetes %$% table(readmitted, readmit_30, useNA = "always") %>% addmargins(1)

## ------------------------------------------------------------------------
diabetes %$% table(gender, readmit_30, useNA = "always") %>% addmargins(2)

## ------------------------------------------------------------------------
diabetes %$% table(race, useNA="always") 

diabetes <- diabetes %>% 
  mutate(race = case_when(
    race %in% c("Asian", "Hispanic", "Other") ~ "Other", 
    is.na(race) ~ "Missing",   # creates a valid category for missing race
    TRUE ~ race))              # all other cases

## ------------------------------------------------------------------------
diabetes %$% table(race, race, useNA = "always")

# fix the ordering by change race to a factor
diabetes <- diabetes %>% 
  mutate(race = factor(race, 
    levels = c("AfricanAmerican", "Caucasian", "Other", "Missing")))

## ------------------------------------------------------------------------
diabetes %$% table(race, readmit_30) %>% addmargins(2)

## ---- out.width = "60%"--------------------------------------------------
diabetes %>% ggplot(aes(x = age, y = readmit_30)) + 
  stat_summary(geom = "point", fun.y = mean) + ggtitle("Approx. Fig. 2")

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  mutate(age = case_when(
    age %in% c("[0-10)", "[10-20)", "[20-30)") ~ "[0, 30)", 
    age %in% c("[30-40)", "[40-50)", "[50-60)") ~ "[30, 59)",
    TRUE ~ "[60, 100)"))
diabetes %$% table(age, readmit_30, useNA="always") %>% addmargins()

## ------------------------------------------------------------------------
diabetes %$% table(weight, useNA="always")

## ------------------------------------------------------------------------
diabetes %$% table(weight, readmit_30, useNA="always") %>% 
  addmargins(1) %>% prop.table(margin = 1) %>% round(3)

## ------------------------------------------------------------------------
length(diabetes %$% table(discharge_disposition_id, useNA="always"))

diabetes <- diabetes %>% 
  mutate(discharge = if_else(discharge_disposition_id %in% c(1, 8), 
    "Discharge to home", "Other"))

diabetes %$% table(discharge, readmit_30, useNA = "always") %>% addmargins()

## ------------------------------------------------------------------------
length(diabetes %$% table(admission_source_id, useNA="always"))

diabetes <- diabetes %>% 
  mutate(admission = case_when(
    admission_source_id %in% c(1, 2) ~ "Physician/clinic referral",
    admission_source_id == 7 ~ "Emergency room",
    TRUE ~ "Other"),
    admission = fct_relevel(admission, 
      "Emergency room", "Physician/clinic referral", "Other"))

## ------------------------------------------------------------------------
diabetes %$% table(admission, readmit_30, useNA="always")

## ------------------------------------------------------------------------
length(diabetes %$% table(medical_specialty, useNA="always"))

## ------------------------------------------------------------------------
diabetes <- diabetes %>% mutate(
  specialty = case_when(
    medical_specialty == "InternalMedicine" ~ "Internal medicine",
    medical_specialty %in% c("Cardiology", "Cardiology-Pediatric") ~ "Cardiology",
    str_sub(medical_specialty, 1, 4) == "Surg" ~ "Surgery",
    medical_specialty == "Family/GeneralPractice" ~ "Family/general practice", 
    is.na(medical_specialty) ~ "Missing", 
    TRUE ~ "Other"))

## ------------------------------------------------------------------------
diabetes %$% table(specialty, readmit_30, useNA="always") %>% addmargins(2)

## ------------------------------------------------------------------------
diabetes <- diabetes %>% mutate(
  # fct_relevel from forcats package - permits manual specification of order
  specialty = fct_relevel(specialty,
    "Internal medicine", "Cardiology", "Surgery", 
    "Family/general practice", "Missing", "Other"))

## ------------------------------------------------------------------------
diabetes %$% table(A1Cresult, useNA="always") # we don't need ">7" distinction

diabetes %$% table(diabetes$change, useNA="always") # seems okay

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  mutate(HbA1c_med = case_when(
      A1Cresult==">8" & change=="Ch" ~ "Result high & meds changed",
      A1Cresult==">8" & change=="No" ~ "Result high & meds not changed",
      A1Cresult=="None" ~ "No test",
      TRUE ~ "Normal"))

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  # fct_relevel from for_cats package
  mutate(HbA1c_med = fct_relevel(HbA1c_med, "No test", 
    "Result high & meds changed", "Result high & meds not changed",
    "Normal")) 
diabetes %$% table(HbA1c_med, readmit_30, useNA = "always") %>% addmargins(2)

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  mutate(diag_1_num = if_else(str_sub(diag_1, 1, 1) %in% c("V", "E"), 0, 
      as.numeric(diag_1)))

## ------------------------------------------------------------------------
summary(is.na(diabetes$diag_1))  # Yes - there are 10 obs missing diag_1

## ------------------------------------------------------------------------
diabetes <- diabetes %>% 
  mutate(
    primary_diag = case_when(
      diag_1_num %>% between(390, 459) | diag_1_num == 785 ~ "Circulatory",
      diag_1_num %>% between(460, 519) | diag_1_num == 786 ~ "Respiratory",
      diag_1_num %>% between(520, 579) | diag_1_num == 787 ~ "Digestive",
      floor(diag_1_num) == 250 ~ "Diabetes", 
      diag_1_num %>% between(800, 999) ~ "Injury & Poison",
      diag_1_num %>% between(710, 739) ~ "Musculoskeletal",
      diag_1_num %>% between(580, 629) | diag_1_num == 788 ~ "Genitourinary", 
      diag_1_num %>% between(140, 239) ~ "Neoplasms", 
      TRUE ~ "Other"),    # TRUE will catch the "V" and "E" diagnoses 
    primary_diag = fct_relevel(primary_diag, 
      c("Circulatory","Diabetes", "Respiratory", "Digestive", 
        "Injury & Poison", "Musculoskeletal", 
        "Genitourinary", "Neoplasms", "Other")))

## ------------------------------------------------------------------------
diabetes %$% table(primary_diag, readmit_30, useNA = "always") %>% addmargins(2)

## ------------------------------------------------------------------------
diabetes %$% summary(time_in_hospital) # looks good!

