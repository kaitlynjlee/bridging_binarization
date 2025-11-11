library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(future)
library(future.apply)


here::i_am("analyze_data.R")

data_births_exposure_active_wells <- read_csv(paste0(here(),
                                                     "/data/data_births_exposure_active_wells_2007_2015.csv"),
                                              col_types = "cdc")
data_births_path <- "R:\\CDPH birth data (2000-2020)\\Clean data\\"

start_year <- 2010

birth_data_raw <- read_csv(paste0(data_births_path,
                                  "birthdata_", 
                                  start_year,
                                  ".csv"), col_types = paste0(rep("c",110), collapse = ""))

birth_data <- birth_data_raw %>% 
  filter(!is.na(lmp) & !is.na(child_dob) & ! is.na(X) & !is.na(Y)) %>% 
  select(id,
         lmp,
         X,
         Y,
         year,
         child_male,
         child_dob, 
         mat_age, 
         education, 
         race_eth, 
         pc_index, 
         nulliparous,
         sga,
         birthweight_g,
         mult_births,
         cg_ab,
         gest_wks_ob,
         gest_days_lmp) %>% 
  mutate(birth_id         = as.factor(id),
         date_conception  = ymd(lmp),
         date_delivery    = ymd(child_dob),
         X = as.numeric(X),
         Y = as.numeric(Y),
         birthweight_g = as.numeric(birthweight_g),
         mult_births = as.numeric(mult_births),
         gest_wks_ob = as.numeric(gest_wks_ob),
         gest_days_lmp = as.numeric(gest_days_lmp)) %>% 
  filter(birthweight_g > 100, 
         birthweight_g < 9000,
         mult_births == 1,  
         cg_ab == "0",
         gest_wks_ob > 20,
         gest_wks_ob < 45,
         gest_days_lmp > 140,
         gest_days_lmp < 315) 


data_births_exposure <- data_births_exposure_active_wells %>% 
  left_join(birth_data,
            by = "birth_id")

rm(data_births_exposure_active_wells)
rm(birth_data_raw)

data_of_interest <- data_births_exposure %>% 
  select(birth_id,
         dist_nearest_well_m,
         year,
         child_male,
         child_dob, 
         mat_age, 
         education, 
         race_eth, 
         pc_index, 
         nulliparous,
         sga,
         birthweight_g) %>% 
  mutate(mat_age = as.numeric(mat_age),
         child_dob = ymd(child_dob),
         child_dob_month = month(child_dob),
         child_dob_year = year(child_dob),
         child_male = as.numeric(child_male),
         mat_age_bucket = case_when(mat_age < 20 ~ "<20",
                                    mat_age < 25 ~ "20-24",
                                    mat_age < 30 ~ "25-29",
                                    mat_age < 35 ~ "30-34",
                                    mat_age >= 35 ~ "35+",
                                    T ~ NA),
         mat_age_bucket = factor(mat_age_bucket,
                                 levels = c("<20", "20-24", "25-29", "30-34", "35+")),
         race_eth = case_when(race_eth == "amerind_nh" | race_eth == "multi" | race_eth == "other_nh" ~ "other",
                              T ~ race_eth),
         race_eth = factor(race_eth),
         education = factor(education, levels = c("lths", "hs_grad", "some_col", "col_grad", "grad_grad")),
         pc_index = factor(pc_index, levels = c("inadequate", "intermediate", "adequate", "adequate+")),
         nulliparous = as.numeric(nulliparous),
         sga = as.numeric(sga),
         birthweight_g = as.numeric(birthweight_g),
         lbw = ifelse(birthweight_g < 2500, 1, 0)) %>% 
  drop_na()

rm(data_births_exposure)

model_data <- data_of_interest %>% 
  mutate(threshold = as.numeric(dist_nearest_well_m > 1000))

subgroup <- model_data %>% 
  filter(race_eth == "hispanic_single",
         mat_age_bucket == "25-29",
         education == "lths",
         pc_index == "adequate",
         nulliparous == 0,
         child_dob_year == 2010, 
         child_dob_month == 9,
         child_male == 1)

ggplot(subgroup) +
  geom_histogram(aes(dist_nearest_well_m), binwidth = 250) +
  xlab("Distance to Nearest Well (m)") +
  theme_bw() + 
  xlim(-5, 10005)

ggplot(subgroup %>% 
         filter(threshold == 1)) +
  geom_histogram(aes(dist_nearest_well_m), binwidth = 250) +
  geom_vline(xintercept = 1000, color = "red") +
  xlab("Distance to Nearest Well (m)") +
  theme_bw() + 
  xlim(-5, 10005)