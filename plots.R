library(tidyverse)

here::i_am("plots.R")
save_dir <- here::here("poster_plots")

set.seed(1680)


# normal dgp --------------------------------------------------------------


data = data.frame(values = rnorm(150, 5, 3),
                  id = 1:150) %>% 
  mutate(above = values >= 7)


gray_plot <- ggplot(data) +
  geom_histogram(aes(x = values), bins = 100, color = "azure4", fill = "azure4") +
  xlab("Treatment values") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 25))

gray_plot_line <- ggplot(data) +
  geom_histogram(aes(x = values), bins = 100, color = "azure4", fill = "azure4") +
  xlab("Treatment values") +
  geom_vline(aes(xintercept = 7), color = "#1a9850", linewidth = 1.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 25))

colored_plot <- ggplot(data) +
  geom_histogram(aes(x = values,
                     fill = above),
                 bins = 100,
                 show.legend = FALSE) +
  geom_vline(aes(xintercept = 7), color = "#1a9850", linewidth = 1.5) +
  xlab("Treatment values") +
  theme_bw() +
  scale_fill_manual(values=c("#d73027", "#4575b4")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size = 25))



ggsave("gray_hist.png",
       gray_plot,
       path = save_dir,
       dpi = 750,
       width = 8,
       heigh = 8)

ggsave("gray_plot_line.png",
       gray_plot_line,
       path = save_dir,
       dpi = 750,
       width = 8,
       heigh = 8)

ggsave("colored_plot.png",
       colored_plot,
       path = save_dir,
       dpi = 750,
       width = 8,
       heigh = 8)



# relative self-selection -------------------------------------------------

# Exponential example -----------------------------------------------------

rbern = \(n,p=0.5) rbinom(n,1,p)
rate = 0.8
cut_off <- 1.25

pdf_a1_exp <- function(a, cut_off){
  map_dbl(a, function(a){
    if(a < cut_off)
      return(0)
    else{
      prob_in_a <- 1-pexp(cut_off, rate)
      dexp(a, rate)/prob_in_a
  }})

}

pdf_a0_exp <- function(a, cut_off){
  map_dbl(a, function(a){
    if(a >= cut_off)
      return(0)
    else{
      prob_in_a <- pexp(cut_off, rate)
      return(dexp(a, rate)/prob_in_a)
    }})
  
}

to_plot_exp <- tibble(x = seq(0,8, 0.001),
                  A = dexp(x, rate),
                  A_1 = pdf_a1_exp(x, cut_off),
                  A_0 = pdf_a0_exp(x, cut_off)) %>% 
  pivot_longer(c(2:4), names_to = "dataset", values_to = "a") %>% 
  mutate(intervention = factor(dataset, levels = c("A", "A_1", "A_0"),
                               labels = c("Status quo A",
                                          "T = 1",
                                          "T = 0")))



threshold_plot_exp <- to_plot_exp %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention),
            linewidth = 1) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "#d73027", "#4575b4"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  geom_vline(aes(xintercept = cut_off), color = "#1a9850", linetype = 1, linewidth = 1) +
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  theme_classic() +
  theme(text = element_text(size = 25)) +
  xlim(0, 6) +
  ylab("density") +
  xlab("A")

ggsave("threshold_plot_exp.png",
       threshold_plot_exp,
       path = save_dir,
       dpi = 750,
       width = 8,
       heigh = 8)

threshold_plot_exp_pre <- to_plot_exp %>% 
  filter(intervention == "Status quo A") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention),
            linewidth = 1) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "#d73027", "#4575b4"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  theme_classic() +
  theme(text = element_text(size = 25),
        legend.position = "none") +
  xlim(0, 6) +
  ylab("density") +
  xlab("A")

threshold_plot_exp_cutoff <- to_plot_exp %>% 
  filter(intervention == "Status quo A") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention),
            linewidth = 1) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "#d73027", "#4575b4"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  geom_vline(aes(xintercept = cut_off), color = "#1a9850", linetype = 1,
             linewidth = 1) +
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  theme_classic() +
  theme(text = element_text(size = 25),
        legend.position = "none") +
  xlim(0, 6) +
  ylab("density") +
  xlab("A")

threshold_plot_exp_a1 <- to_plot_exp %>% 
  filter(intervention != "T = 0") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention),
            linewidth = 1) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "#d73027", "#4575b4"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  scale_linetype_manual(values=c(1,1), guide = "none") + 
  theme_classic() +
  theme(text = element_text(size = 25)) +
  xlim(0, 6) +
  ylab("density") +
  xlab("A")

threshold_plot_exp_a0 <- to_plot_exp %>% 
  filter(intervention != "T = 1") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention),
            linewidth = 1) +
  xlab("X") +
  scale_color_manual(values = c("azure4",  "#4575b4"),  
                     labels = c("Status quo A", bquote(tilde(A)[0])),
                     name = "") + 
  scale_linetype_manual(values=c(1,1), guide = "none") + 
  theme_classic() +
  theme(text = element_text(size = 25)) +
  xlim(0, 6) +
  ylab("density") +
  xlab("A")

ggsave("threshold_plot_exp_pre.png",
       threshold_plot_exp_pre,
       path = save_dir,
       dpi = 750,
       width = 10,
       heigh = 8)

ggsave("threshold_plot_exp_cutoff.png",
       threshold_plot_exp_cutoff,
       path = save_dir,
       dpi = 750,
       width = 10,
       heigh = 8)

ggsave("threshold_plot_exp_a1.png",
       threshold_plot_exp_a1,
       path = save_dir,
       dpi = 750,
       width = 10,
       heigh = 8)

ggsave("threshold_plot_exp_a0.png",
       threshold_plot_exp_a0,
       path = save_dir,
       dpi = 750,
       width = 10,
       heigh = 8)

# uniform -----------------------------------------------------------------
rbern = \(n,p=0.5) rbinom(n,1,p)
dbern = \(n,p=0.5) dbinom(n,1,p)

cut_off <- 0.3

pdf_a1_unif <- function(a, cut_off){
  map_dbl(a, function(a){
    if(a < cut_off)
      return(0)
    else{
      prob_in_a <- 1-punif(cut_off)
      return(dunif(a )/prob_in_a)
    }})
  
}

pdf_a0_unif <- function(a, cut_off){
  map_dbl(a, function(a){
    if(a >= cut_off)
      return(0)
    else{
      prob_in_a <- punif(cut_off)
      return(dunif(a)/prob_in_a)
    }})
  
}
to_plot_unif <- tibble(x = seq(-0.1,1.2, 0.001),
                  A = dunif(x),
                  A_1 = pdf_a1_unif(x, cut_off),
                  A_0 = pdf_a0_unif(x, cut_off)) %>% 
  pivot_longer(c(2:4), names_to = "dataset", values_to = "a") %>% 
  mutate(intervention = factor(dataset, levels = c("A", "A_1", "A_0"),
                               labels = c("Status quo A",
                                          "T = 1",
                                          "T = 0")))



threshold_plot_unif <- to_plot_unif %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "red", "blue"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  geom_vline(aes(xintercept = cut_off), color = "lightblue", linetype = 1) +
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  theme_classic() +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")

threshold_plot_unif_pre <- to_plot_unif %>% 
  dplyr::filter(intervention == "Status quo A") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey"),  
                     labels = c("Status quo A"),
                     name = "") + 
  scale_linetype_manual(values=c(1), guide = "none") + 
  theme_classic() +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")

threshold_plot_unif_cutoff <- to_plot_unif %>% 
  dplyr::filter(intervention == "Status quo A") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey"),  
                     labels = c("Status quo A"),
                     name = "") + 
  geom_vline(aes(xintercept = cut_off), color = "lightblue", linetype = 1) +
  scale_linetype_manual(values=c(1), guide = "none") + 
  theme_classic() +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")

threshold_plot_unif <- to_plot_unif %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "red", "blue"),  
                     labels = c("Status quo A", bquote(tilde(A)[1]), bquote(tilde(A)[0])),
                     name = "") + 
  geom_vline(aes(xintercept = cut_off), color = "lightblue", linetype = 1) +
  scale_linetype_manual(values=c(1,2,2), guide = "none") + 
  theme_classic() +
  theme(legend.text=element_text(size=rel(1))) +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")


threshold_plot_unif_a1 <- to_plot_unif %>% 
  filter(intervention != "T = 0") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "red"),  
                     labels = c("Status quo A", bquote(tilde(A)[1])),
                     name = "") + 
  scale_linetype_manual(values=c(1,1), guide = "none") + 
  theme_classic() +
  theme(legend.text=element_text(size=rel(1))) +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")

threshold_plot_unif_a0 <- to_plot_unif %>% 
  filter(intervention != "T = 1") %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "blue"),  
                     labels = c("Status quo A", bquote(tilde(A)[0])),
                     name = "") + 
  scale_linetype_manual(values=c(1,1), guide = "none") + 
  theme_classic() +
  theme(legend.text=element_text(size=rel(1))) +
  xlim(-0.1, 1.1) +
  ylab("density") +
  xlab("A")

ggsave("threshold_plot_unif.png",
       threshold_plot_unif,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)

ggsave("threshold_plot_unif_pre.png",
       threshold_plot_unif_pre,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)

ggsave("threshold_plot_unif_cutoff.png",
       threshold_plot_unif_cutoff,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)

ggsave("threshold_plot_unif_a1.png",
       threshold_plot_unif_a1,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)

ggsave("threshold_plot_unif_a0.png",
       threshold_plot_unif_a0,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)


# MTP examples: shift -----------------------------------------------------


# Exponential example -----------------------------------------------------

mean = 5
shift = 2


to_plot_shift <- tibble(x = seq(1,11, 0.001),
                      A = dnorm(x, mean = mean),
                      A_1 = dnorm(x, mean+shift)) %>% 
  pivot_longer(c(2:3), names_to = "dataset", values_to = "a") %>% 
  mutate(intervention = factor(dataset, levels = c("A", "A_1"),
                               labels = c("Status quo A",
                                          "Shift: A+2")))



plot_shift <- to_plot_shift %>% 
  ggplot() +
  geom_line(aes(x=x, y=a, colour = intervention, linetype = intervention)) +
  xlab("X") +
  scale_color_manual(values = c("darkgrey", "red"), name = "") + 
  scale_linetype_manual(values=c(1,2), guide = "none") + 
  theme_classic() +
  ylab("density") +
  xlab("A")

ggsave("plot_shift.png",
       plot_shift,
       path = save_dir,
       dpi = 750,
       width = 5,
       heigh = 4)
