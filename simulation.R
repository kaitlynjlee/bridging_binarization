library(tidyverse)
library(magrittr)
library(car)
library(zeallot)
set.seed(491)

# Set up directory --------------------------------------------------------
here::i_am("plots.R")
save_dir <- here::here("simulations")
# Functions ---------------------------------------------------------------

# DGP

w <- function(rep, p = 0.5){
  (rbinom(rep, 1, 0.5))
}

x <- function(rep, mean = 0, sd = 1, w = 0){
  
  rnorm(rep, mean = mean, sd = sd) + 2*w
}

y <- function(x, w = 0){
  x^3 + sin(x) + rnorm(1) + 100*w
}


# policy restricting X >= 6
new_x_greater <- function(pre_x, threshold, mean = 0, sd = 1, w = 0){
  map2_dbl(pre_x, w_cov, function(x, w){
    while(x < threshold){
      x <- rnorm(1, mean, sd) + 2*w
    }
    return(x)
  })
}



# policy restricting X < 6
new_x_less <- function(pre_x, threshold, mean = 0, sd = 1, w = 0){
  map2_dbl(pre_x, w, function(x, w){
    while(x >= threshold){
      x <- rnorm(1, mean, sd) + 2*w
    }
    return(x)
  })
}

# threshold setting X < 0.25 to X = 0.25
threshold_x <- function(pre_x, threshold){
  map2_dbl(pre_x, threshold, function(x, threshold){
    if(x < threshold){
      x <- threshold
    }
    return(x)
  })
}


# Calculate true psi with covariate ---------------------------------------


w_cov <- w(1000000)
mean <- 5
threshold <- 6


data_cov <- tibble(pre_x = x(1000000, mean = mean, w=w_cov), 
                   pre_y = y(pre_x, w_cov),
                   cov_w = w_cov) %>% 
  mutate(post_x_greater = new_x_greater(pre_x, threshold, mean = mean, w=cov_w), 
         post_y_greater = y(post_x_greater, cov_w),
         post_x_less = new_x_less(pre_x, threshold, mean = mean, w=cov_w), 
         post_y_less = y(post_x_less, cov_w),
         post_x_threshold = threshold_x(pre_x, threshold),
         post_y_threshold = y(post_x_threshold, cov_w),
         id = row_number())

# calculate true psi based on n = 1000000
true_psi_cov = mean(data_cov$post_y_greater) - mean(data_cov$post_y_less)
psi_threshold_cov = mean(data_cov$post_y_threshold)  -  mean(data_cov$pre_y)
psi_greater_minus_pre_cov = mean(data_cov$post_y_greater) - mean(data_cov$pre_y)


# IPW ---------------------------------------------------------------------


# IPW functions -----------------------------------------------------------

ipw_est <- function(data){
  propensity_score_est <- glm(thres ~ pre_w,
                              family = binomial(link = "logit"),
                              data = data)
  ipw_data <- data %>% 
    mutate(prop = predict(propensity_score_est, data, type = "response"),
           ipw = thres/prop + ((1-thres)/(1-prop)))
  
  
  nrows <- nrow(ipw_data)
  
  psi_hat <- ipw_data %$% 
    {mean(thres*pre_y/prop)- mean((1-thres)*pre_y/(1-prop))}
  
  psi_hat_peb <- ipw_data %$% 
    {mean(thres*pre_y/prop) - mean(pre_y)}
  
  return(c(psi_hat, psi_hat_peb))
}

run_sim_cov_ipw <- function(n, threshold){
  
  data <- tibble(pre_w = w(n),
                 pre_x = x(n, mean = mean, w = pre_w), 
                 pre_y = y(pre_x, w = pre_w),
                 thres = pre_x > threshold,
                 demeaned_w = pre_w - mean(pre_w))
  
  ipw_est_out <- ipw_est(data)

  bootstrap <- replicate(500,
                         {new_sample <- slice_sample(data, n=n, replace = T)
                         ipw_est(new_sample)
                         })

  bootstrap_se <- apply(bootstrap, 1, sd)

  psi_hat <- ipw_est_out[1]
  se_est <- bootstrap_se[1]

  psi_hat_peb <- ipw_est_out[2]
  se_est_peb <- bootstrap_se[2]

  return(c(psi_hat, se_est, psi_hat_peb, se_est_peb))
}

ipw_study <- function(n_samp, reps){
    sim <- sapply(rep(n_samp[1], reps),
                  run_sim_cov_ipw, 
                  threshold = threshold) %>% 
            t() %>% 
            as_tibble()
  
    names(sim) <- c("psi_hat", "se_est", "psi_hat_alt", "se_est_alt")
  
    tibble(estimand = "BATE",
           estimator = "ipw",
           n_samp = n_samp,
         est = mean(sim$psi_hat),
         bias = est - true_psi_cov,
         perc_bias = bias/est,
         simulated_se = sd(sim$psi_hat),
         est_se = mean(sim$se_est)) %>% 
    bind_rows(
      tibble(estimand = "PEB",
             estimator = "ipw",
             n_samp = n_samp,
             est = mean(sim$psi_hat_alt),
             bias = est - psi_greater_minus_pre_cov,
             perc_bias = bias/est,
             simulated_se = sd(sim$psi_hat_alt),
             est_se = mean(sim$se_est_alt))
    )
}

# Regression --------------------------------------------------------------

# Variance functions ------------------------------------------------------



# B matrix
B_matrix = function(data, resid){
  theta1 <- mean(data$thres)
  theta2 <- mean(data$thres*data$demeaned_w)
  
  Bn = data %>%
    mutate(resid = resid) %>%
    mutate(theta1 = thres - theta1,
           theta2 = thres*demeaned_w - theta2,
           theta3 = resid,
           theta4 = resid*thres,
           theta5 = resid*demeaned_w,
           theta6 = resid*demeaned_w*thres, .keep='none') %>%
    data.matrix %>%
    {t(.) %*% .}
  
  return(Bn)
}

# A matrix
A_matrix <- function(data){
  An = data %>%
    mutate(theta1 = 0,
           theta2 = 0,
           theta3 = 1,
           theta4 = thres,
           theta5 = demeaned_w,
           theta6 = thres*demeaned_w, .keep='none') %>%
    data.matrix() %>%
    {t(.) %*% .}
  An[1,1] = nrow(data)
  An[2,2] = nrow(data)

  return(An)
  
}

# Return variance-covariance estimates
var_cov <- function(data, resid){
  a_matrix <- A_matrix(data)
  b_matrix <- B_matrix(data, resid)

  to_return <- solve(a_matrix)%*%b_matrix%*%t(solve(a_matrix)) * nrow(data)
  return(to_return)
}




# Functions to make generate regression estimates -----------------------

reg_est <- function(data){
  model_1 <- lm(pre_y ~ thres * demeaned_w ,
                data = data)
  resid <- model_1$residuals

  
  #BATE
  psi_hat <- model_1$coefficients[2]
  
  cov_matrix_est_n <- var_cov(data, resid)
  sd_hat <- hccm(model_1)[2,2] %>% sqrt()
  theta1 <- mean(data$thres)
  theta2 <- mean(data$thres*data$demeaned_w)
  theta4 <- model_1$coefficients[2]
  theta6 <- model_1$coefficients[4]
  
  g_prime <- c(-theta4,
               -theta6,
               0,
               (1-theta1),
               0,
               -theta2)

  psi_hat_peb <-  (1-theta1)*theta4-(theta2*theta6)
  
  var_hat_peb <- t(g_prime)%*%cov_matrix_est_n%*%g_prime
  sd_hat_peb = sqrt(var_hat_peb/nrow(data))
  
  return(c(psi_hat, 
           sd_hat, 
           psi_hat_peb, 
           sd_hat_peb))
}

run_sim_cov_reg <- function(n, threshold, mean = 0, sd = 1){

  data <- tibble(error = rnorm(n),
                 pre_w = w(n),
                 pre_x = x(n, mean = mean, w = pre_w), 
                 pre_y = y(pre_x, w = pre_w),
                 thres = pre_x >= threshold,
                 demeaned_w = pre_w - mean(pre_w))
  
  reg_est_out <- reg_est(data)
  
  return(reg_est_out)
}

reg_study <- function(n_samp, reps){
  sim <- sapply(rep(n_samp[1], reps),
                run_sim_cov_reg, 
                threshold = threshold,
                mean = mean) %>% 
    t() %>% 
    as_tibble()
  
  names(sim) <- c("psi_hat", "se_est", "psi_hat_alt", "se_est_alt")
  
  tibble(estimand = "BATE",
         estimator = "reg",
         n_samp = n_samp,
         est = mean(sim$psi_hat),
         bias = est - true_psi_cov,
         perc_bias = bias/est,
         simulated_se = sd(sim$psi_hat),
         est_se = mean(sim$se_est)) %>% 
    bind_rows(
      tibble(estimand = "PEB",
             estimator = "reg",
             n_samp = n_samp,
             est = mean(sim$psi_hat_alt),
             bias = est - psi_greater_minus_pre_cov,
             perc_bias = bias/est,
             simulated_se = sd(sim$psi_hat_alt),
             est_se = mean(sim$se_est_alt))
    )
}


# Run studies -------------------------------------------------------------

reps = 2000
n_samp = c(150, 300, 500)

sim_results_ipw <- map(n_samp,
                       ipw_study, 
                       reps = reps) %>% 
  bind_rows()

sim_results_reg <- map(n_samp,
                       reg_study, 
                       reps = reps) %>% 
  bind_rows()


# Save results ------------------------------------------------------------

write.csv(sim_results_ipw, paste0(here::here(),"/simulations/sim_results_ipw.csv"))
write.csv(sim_results_reg, paste0(here::here(),"/simulations/sim_results_reg.csv"))
