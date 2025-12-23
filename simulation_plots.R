library(tidyverse)
library(magrittr)
library(car)
library(zeallot)
set.seed(491)

# Set up directory --------------------------------------------------------
here::i_am("simulation_plots.R")
save_dir <- here::here("simulations")
save_dir_poster <- here::here("poster")
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

# threshold setting X < 6 to X = 6
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
psi_threshold_cov = mean(data_cov$post_y_threshold) - mean(data_cov$pre_y)
psi_greater_minus_pre_cov = mean(data_cov$post_y_greater) - mean(data_cov$pre_y)


# plots -------------------------------------------------------------------
to_plot_cov <- data_cov %>% 
  select(pre_x, post_x_greater, post_x_less, post_x_threshold, cov_w) %>% 
  pivot_longer(1:4, names_to = "dataset", values_to = "x") %>% 
  mutate(intervention = factor(dataset, levels = c("pre_x",
                                                   "post_x_greater",
                                                   "post_x_less",
                                                   "post_x_threshold"),
                               labels = c("Observed A",
                                          "T = 1",
                                          "T = 0",
                                          "Piled up"))) 


mtp_comparisons <- to_plot_cov %>% 
  filter(intervention != "Piled up") %>% 
  ggplot() +
  geom_density(aes(x = x, colour = intervention, linetype = intervention), linewidth = 1) +
  facet_wrap(vars(cov_w)) +
  xlab("A") +
  scale_color_manual(values = c("darkgrey", "#d73027", "#4575b4"), 
                     labels = c("Observed A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  geom_vline(aes(xintercept = threshold), color = "#1a9850", linetype = 1, linewidth = 1) +
  scale_linetype_manual(values = c(1, 2, 2), guide = "none") +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = 0,
        shape = 22,
        size = 6,
        fill = c("darkgrey", "#d73027", "#4575b4")
      )
    )
  ) +
  theme_classic() + 
  theme(text = element_text(size = 25)) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) 

t_1_verus_pile_up <- to_plot_cov %>% 
  filter(intervention != "T = 0") %>% 
  ggplot() +
  geom_density(aes(x = x, colour = intervention, linetype = intervention)) +
  facet_wrap(vars(cov_w)) +
  xlab("A") +
  theme_classic() + 
  scale_color_manual(values = c("darkgrey", "red", "darkviolet"), 
                     labels = c("Observed A", bquote(tilde(A)[1]), "Piled up A"),
                     name = "") + 
  scale_linetype_manual(values=c(1,2,2), guide = "none")

t_1_verus_pile_up_zoomed <- to_plot_cov %>% 
  filter(intervention != "T = 0") %>% 
  ggplot() +
  geom_density(aes(x = x, colour = intervention, linetype = intervention)) + 
  facet_wrap(vars(cov_w)) +
  xlab("A") +
  theme_classic() + 
  scale_color_manual(values = c("darkgrey", "red", "darkviolet"),
                     labels = c("Status quo A", bquote(tilde(A)[1]), "Piled up A"),
                     name = "") + 
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  ylim(0, 1.25)


ggsave("mtp_comparisons.png",
       mtp_comparisons,
       path = save_dir,
       dpi = 1500,
       width = 10,
       height = 6)

ggsave("t_1_verus_pile_up.png",
       t_1_verus_pile_up,
       path = save_dir,
       dpi = 1500,
       width = 9,
       height = 4)

ggsave("t_1_verus_pile_up_zoomed.png",
       t_1_verus_pile_up_zoomed,
       path = save_dir,
       dpi = 1500,
       width = 9,
       height = 4)

