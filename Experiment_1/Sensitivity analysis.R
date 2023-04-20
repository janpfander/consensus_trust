# Sensitivity analysis (re-write the script so that number is in there as a control)

library(tidyverse); library(purrr); library(broom); 
set.seed(1000)

# draw a sample
create_data <- function(n_subj, n_low, n_high, beta_0, beta_1, 
                        tau_0, tau_1, rho, sigma) { 
  
  items <- data.frame(
    item_id = seq_len(n_low + n_high),
    convergence = rep(c("low", "high"), c(n_low, n_high)),
    X_i = rep(c(-0.5, 0.5), c(n_low, n_high)))
  
  # variance-covariance matrix for random effects by participant
  cov_mx  <- matrix(
    c(tau_0^2,             rho * tau_0 * tau_1,
      rho * tau_0 * tau_1, tau_1^2            ),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_id = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items)  %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           competence = beta_0 + T_0s + (beta_1 + T_1s) * X_i + e_si) %>%
    select(subj_id, item_id, convergence, X_i, competence)
}

# estimate model based on sample
est_model <- function(n_subj, n_low, n_high, beta_0, beta_1, tau_0, tau_1, rho, sigma)
{ # residual (standard deviation)
  
  data_sim <- create_data(n_subj, n_low, n_high, beta_0, beta_1, tau_0, tau_1, rho, sigma)
  model_sim <- lmer(competence ~ X_i + (1 + X_i | subj_id),
                    data_sim)
  
  tidied <- broom.mixed::tidy(model_sim)
  sig <- tidied$p.value[2] < .05
  
  return(sig)
}


# iterate sampling and estimation process 
iterate <- function(n_subj, n_low, n_high, beta_0, beta_1, tau_0, tau_1, rho, sigma, iterations) 
{ # number of iterations
  
  results <-  1:iterations %>%
    map_dbl(function(x) {
      # To keep track of progress
      if (x %% 100 == 0) {print(paste("iteration number ", x))}
      
      # Run our model and return the result
      return(est_model(n_subj, n_low, n_high, beta_0, beta_1, tau_0, tau_1, rho, sigma))
    })
  
  # We want to know statistical power, 
  # i.e., the proportion of significant results
  return(mean(results))
}

# Let's find the minimum sample size
mss <- tibble(n_subj = c(30, 50, 70, 80, 90, 100))
mss$power <- c(30, 50, 70, 80, 90, 100) %>%
  # Before we had to do function(x) here, but now the argument we're 
  # passing is the first argument of iterate(), i.e. "n_subj" so we don't need it
  map_dbl(iterate,
          n_low  =  4,   # number of low convergence stimuli
          n_high =  4,   # number of high convergence stimuli
          beta_0     = 4.13,   # grand mean
          beta_1     =  1.23,   # effect of convergence
          tau_0      = 0.64,   # by-subject random intercept sd
          tau_1      =  0.99,   # by-subject random slope sd
          rho        = 0.11,   # correlation between intercept and slope by-subject
          sigma      = 0.955,  # residual (standard deviation)
          iterations = 1000)
# Look for the first N with power above 90%
mss

# plot results
ggplot(mss, 
       aes(x = n_subj, y = power)) +
  geom_line(color = 'red', size = 1.5) + 
  # add a horizontal line at 90%
  geom_hline(aes(yintercept = .9), linetype = 'dashed') + 
  # Prettify!
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sample Size', y = 'Power')

###########################
# quick power tryout
power <- iterate(
  n_subj = 20,
  n_low  =  4,   # number of low convergence stimuli
  n_high =  4,   # number of high convergence stimuli
  beta_0     = 4.13,   # grand mean
  beta_1     =  1.23,   # effect of convergence
  tau_0      = 0.64,   # by-subject random intercept sd
  tau_1      =  0.99,   # by-subject random slope sd
  rho        = 0.11,   # correlation between intercept and slope by-subject
  sigma      = 0.955,  # residual (standard deviation)
  iterations = 1000)
#############################

# Let's find the minimum detectable effect
power_levels <- c()
effects_to_try <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.2)

for (i in 1:length(effects_to_try)) {
  
  # keep track of progress
  print(paste("effect to try: ", effects_to_try[i]))
  
  # iterate
  power_levels[i] <- iterate(
    n_subj = 200,
    n_low  =  4,   # number of low convergence stimuli
    n_high =  4,   # number of high convergence stimuli
    beta_0     = 4.13,   # grand mean
    beta_1     =  effects_to_try[i],   # effect of convergence
    tau_0      = 0.64,   # by-subject random intercept sd
    tau_1      =  0.99,   # by-subject random slope sd
    rho        = 0.11,   # correlation between intercept and slope by-subject
    sigma      = 0.955,  # residual (standard deviation)
    iterations = 1000)
}

# Where do we cross 80%?
power_results <- tibble(effect = effects_to_try,
                        power = power_levels)
power_results

# plot results
ggplot(power_results, 
       aes(x = effect, y = power)) +
  geom_line(color = 'red', size = 1.5) + 
  # add a horizontal line at 90%
  geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
  # Prettify!
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Mixed model effect size of convergence', y = 'Power')