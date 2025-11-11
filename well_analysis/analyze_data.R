library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(future)
library(future.apply)

# AIPW Estimator ---------------------------------------------------------


aipw_est <- function(data,
                     covariates,
                     exposure,
                     outcome,
                     estimand = c("BATE","CAB")){
  estimand <- match.arg(estimand)
  
  
  prop_score_formula = paste0(
    paste0(exposure, "~"),
    paste0(covariates, collapse = "+")
  )
  outcome_formula = paste0(
    paste0(outcome, "~"),
    paste0(covariates, collapse = "+"),
    "+",
    paste0(covariates, "*", exposure, collapse ="+")
  )
  
  prop_score_model = glm(data = data, formula = as.formula(prop_score_formula), 
                         family = binomial(link = "logit"))
  outcome_model = glm(data = data, formula = as.formula(outcome_formula), 
                      family = binomial(link = "logit"))
  
  prop_score = predict(prop_score_model, data, type = "response")
  
  data0 <- data
  data0[[exposure]] <- 0
  data1 <- data
  data1[[exposure]] <- 1
  
  y_hat = predict.glm(outcome_model, newdata = data, type = "response")
  y_0 = predict.glm(outcome_model, newdata = data0, type = "response")
  y_1 = predict.glm(outcome_model, newdata = data1, type = "response")
  y <- data[[outcome]]
  A <- data[[exposure]]
  
  if(estimand == "BATE"){
    mean(((A/prop_score) - (1-A)/(1-prop_score))*(y-y_hat) + 
           y_1 - y_0)
  } else if(estimand == "CAB"){
    mean((A/prop_score - 1)*(y-y_hat) + 
           y_1 - y_hat)
  }
}

aipw_boot_future <- function(data, 
                             covariates, 
                             exposure, 
                             outcome,
                             estimand = c("BATE","CAB"),
                             alpha = 0.05,
                             B = 500, 
                             seed = 1548,
                             max_workers = 16,
                             cap_threads = TRUE) {
  set.seed(seed)
  estimand <- match.arg(estimand)
  
  stopifnot(all(c(exposure, outcome, covariates) %in% names(data)))
  
  if (cap_threads) {
    Sys.setenv(OMP_NUM_THREADS = "1",
               MKL_NUM_THREADS = "1",
               OPENBLAS_NUM_THREADS = "1")  
  }
  
  old_plan <- plan()
  on.exit(plan(old_plan), add = TRUE)
  
  workers <- min(max_workers, availableCores()-4)
  plan(multisession, workers = workers)
  
  n_rows <- as.integer(nrow(data))
  
  boots <- future_replicate(
    B,
    {
      n_rows_local <- n_rows
      idx <- sample.int(n = n_rows_local, size = n_rows_local, replace = TRUE)
      aipw_est(data[idx, , drop = FALSE], covariates, exposure, outcome, estimand)
    },
    future.seed = TRUE
  )
  
  est <- aipw_est(data, covariates, exposure, outcome, estimand)
  se  <- sd(boots)
  
  list(
    estimate = est,
    se = se,
    ci_percentile = quantile(boots, c(alpha/2, 1-(alpha/2)), names = FALSE)
  )
}



aipw_boot_strat <- function(data,
                            covariates, exposure, outcome,
                            estimand = c("BATE","CAB"),
                            B = 500,
                            seed = 123) {
  
  estimand <- match.arg(estimand)
  set.seed(seed)
  
  theta_hat <- aipw_est(data, covariates, exposure, outcome, estimand)
  
  A <- data[[exposure]]
  idx1 <- which(A == 1)
  idx0 <- which(A == 0)
  
  n1 <- length(idx1)
  n0 <- length(idx0)
  
  boots <- replicate(B, {
    s1 <- sample(idx1, n1, replace = TRUE)
    s0 <- sample(idx0, n0, replace = TRUE)
    d_b <- data[c(s1, s0), , drop = FALSE]
    aipw_est(d_b, covariates, exposure, outcome, estimand)
  })
  
  se <- stats::sd(boots)
  
  list(
    estimand = estimand,
    estimate = theta_hat,
    se = se
  )
}


# Analyze data ------------------------------------------------------------


here::i_am("analyze_data.R")

data_births_exposure_active_wells <- read_csv(paste0(here(),
                                                     "/data/data_births_exposure_active_wells_2007_2015.csv"),
                                              col_types = "cdc")
data_births_path <- "R:\\CDPH birth data (2000-2020)\\Clean data\\"

start_year <- 2007
end_year <- 2015

birth_data_raw <- read_csv(paste0(data_births_path,
                                  "birthdata_", 
                                  start_year,
                                  ".csv"), col_types = paste0(rep("c",110), collapse = ""))

for(year in (start_year+1):end_year){
  birth_data_raw <- birth_data_raw %>% 
    bind_rows(read_csv(paste0(data_births_path,
                              "birthdata_", 
                              year,
                              ".csv"), 
                       col_types = paste0(rep("c",110), collapse = ""))
    )
}

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

covariates <- c("child_male", "child_dob_month", "child_dob_year", "mat_age_bucket",
                "education","race_eth","pc_index","nulliparous")
exposure <- "threshold"
outcome <- "lbw"

CAB = aipw_boot_future(model_data, covariates, exposure, outcome, estimand = "CAB", B = 500)
BATE = aipw_boot_future(model_data, covariates, exposure, outcome, estimand = "BATE", B = 500)


aipw_est(model_data,  
         c("child_dob_month", "child_dob_year", 
           "education","mat_age_bucket","race_eth","pc_index","nulliparous"), 
         exposure, outcome, estimand = "BATE")

write_csv(as.data.frame(CAB),
          paste0(here(),
                 "/output/CAB.csv"))
write_csv(as.data.frame(BATE),
          paste0(here(),
                 "/output/BATE.csv"))

future::plan(sequential)
