library(tidyverse)

# Define true model
mu_true <- 3
sigma_true <- 0.3

# Generate sample
n <- 50000
obs_data <- rnorm(n, mu_true, sigma_true)

# Functions
mu_star <- mean(obs_data)
sigma2_star <- 1/n * sum((obs_data - mu_star)^2)

likelihood_normal <- function(dvec, mu, sigma2){
  - sum((dvec - mu)^2) / (2 * sigma2) - length(dvec) / 2 * log(sigma2)
}

# Plot
mu_rg <- seq(-0.5, 6, by = 0.1)
sigma2 <- seq(0.1, 10, by = 0.1)

viz_df <- expand.grid(mu = mu_rg, sigma2 = sigma2) %>%
  as.data.frame %>%
  rowwise() %>%
  mutate(likelihood = likelihood_normal(obs_data, mu, sigma2))

ggplot(viz_df, aes(x = mu, y = sigma2)) +
  geom_tile(aes(fill = likelihood))

which.max(viz_df$likelihood)

viz_df[39, ]


library(plotly)

viz_mat <- viz_df %>% pivot_wider(names_from = mu, values_from = likelihood)  %>% dplyr::select(-sigma2) %>% as.matrix


plot_ly(x = mu_rg, y = sigma2, z = viz_mat, 
  type="surface")

get_mean_variance <- function(n){
  y_samp <- rnorm(n, 50, 5)
  y_mean <- mean(y_samp)
  y_sigma <- 1/n * sum((y_samp - y_mean)^2)
  return(c(y_mean, y_sigma))
}

rep_vec <- 1:10000
names(rep_vec) <- 1:10000

samp_df <- map_dfr(rep_vec, ~ get_mean_variance(100)) %>% 
  t %>%
  as.data.frame

ggplot(samp_df, aes(V1, V2)) +
  geom_point()

mean(samp_df$V2)
var(samp_df$V1)
var(samp_df$V2)
25^2
