# round numbers from models
rounded_numbers <- function(x) mutate_if(x, is.numeric, round, 3)

# get model outputs ready for inline text reporting
text_ready <- function(model_output) {
  
  result <- tidy(model_output, conf.int = TRUE) %>% 
    filter(effect == "fixed") %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ sprintf("p = %.3f", p.value)
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>% 
    mutate(
      term = ifelse(term == "(Intercept)", "intercept", term),
      ci = glue::glue("[{conf.low}, {conf.high}]"), 
      # if there is an interaction (recognized by ":"), name it just interaction
      term = str_replace(term, ".*:.*", "interaction")
    ) %>% 
    select(term, estimate, std.error, ci, p.value) %>% 
    split(.$term)
  
  return(result)
}

# Plot results from simulation

# Plot competence distributions
plot_competence_distributions <- function(data) {
  # Your data frame
  d <- data %>% 
    group_by(competence) %>% 
    reframe(alpha = unique(alpha), beta = unique(beta))
  
  # Function to plot the different competence distributions 
  plot_competence_distribution <- function(alpha, beta) {
    ggplot() +
      geom_function(
        fun = ~dbeta(.x, shape1 = alpha, shape2 = beta)
      ) +
      labs(title = paste0("alpha = ", alpha, ", beta = ", beta), 
           y = "Density", x = "Competence") +
      plot_theme
  }
  
  # Specify the values of alpha and beta from the data
  alpha_values <- d$alpha
  beta_values <- d$beta 
  
  # Create a list of ggplot objects
  plot_list <- map2(alpha_values, beta_values, plot_competence_distribution)
  
  # Arrange and display the plots
  competence_plot <- ggarrange(plotlist = plot_list) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob("Competence distributions", 
                                                    face = "bold",
                                                    color = "darkgrey",
                                                    size = 14))
  
  
  return(competence_plot) 
}

# Categorical scenario

# Vary competence
# group by constellations
plot_competence_vary_categorical <- function(data, variable = options, outcome = "Accuracy", 
                                 return = "simulations") {
  
  d <- data
  
  # Extract variable name as a string
  variable_name <- deparse(substitute(variable))
  
  # retrieve info from choice of variable
  if (variable_name == "options") {
    x_label = "Number of Choice Options"
    caption = paste0("Sample size of informants: ", d$sample[1],
                     "; Iterations per sample size: ", d$total_iterations[1], 
                     "; Population size per iteration: ", d$population[1])
  }
  if (variable_name == "sample") {
    x_label = "Size of informant groups"
    caption = paste0("Number of choice options: ", d$options[1],
                     "; Iterations per sample size: ", d$total_iterations[1], 
                     "; Population size per iteration: ", d$population[1])
  }
  
  # make constellation a factor with the right levels
  d$constellation <- fct_relevel(d$constellation, "minority(ies)", "dissensus", "majority", "consensus")
  
  d <- d %>% 
    group_by(constellation, {{variable}}, competence) %>% 
    summarise(competence_mean = mean(average_relative_competence),
              accuracy_mean = mean(average_accuracy),
              accuracy_sd = sd(average_accuracy),
              competence_sd = sd(average_relative_competence)
    )
  
  # Assign outcome variable
  if(outcome == "Accuracy") {
    d$outcome <- d$accuracy_mean
    d$outcome_sd <- d$accuracy_sd
  }
  
  if(outcome == "Competence") {
    d$outcome <- d$competence_mean
    d$outcome_sd <- d$competence_sd
  }
  
  # plot for accuracy
  plot_outcome <- ggplot(d,
                         aes(x = {{variable}}, y = outcome,
                             color = constellation)) +
    # geom_half_violin(position = position_nudge(x = -.2),
    #                  adjust=2, alpha = .5,
    #                  side = "l") +
    geom_pointrange(aes(ymin = outcome - outcome_sd, 
                        ymax = outcome + outcome_sd), 
                    size = 0.1) +
    geom_line() +
    # Add nice labels
    labs(x = x_label, y = outcome, caption = caption) +
    scale_color_viridis_d(option = "plasma", begin = 0.1, 
                          limits = rev(levels(d$constellation)),
                          direction = -1
    ) +
    facet_wrap(~competence) +
    plot_theme + 
    theme(legend.position = "top")
  
  # Plot competence distributions
  # Your data frame
  d <- data %>% 
    group_by(competence) %>% 
    reframe(alpha = unique(alpha), beta = unique(beta))
  
  # Function to plot the different competence distributions 
  plot_competence_distributions <- function(alpha, beta) {
    ggplot() +
      geom_function(
        fun = ~dbeta(.x, shape1 = alpha, shape2 = beta)
      ) +
      labs(title = paste0("alpha = ", alpha, ", beta = ", beta), 
           y = "Density", x = "Competence") +
      plot_theme
  }
  
  # Specify the values of alpha and beta from the data
  alpha_values <- d$alpha
  beta_values <- d$beta 
  
  # Create a list of ggplot objects
  plot_list <- map2(alpha_values, beta_values, plot_competence_distributions)
  
  # Arrange and display the plots
  competence_plot <- ggarrange(plotlist = plot_list) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob("Competence distributions", 
                                                    face = "bold",
                                                    color = "darkgrey",
                                                    size = 14))
  
  if(return == "competence distributions") {
    return(competence_plot)
  } else {
    return(plot_outcome) 
  }
} 

# main plot by size of relative majority
plot_competence_vary_relative_majority_categorical <- function(data, variable = options, outcome = "Accuracy") {
  
  d <- data
  
  # Extract variable name as a string
  variable_name <- deparse(substitute(variable))
  
  # retrieve info from choice of variable
  if (variable_name == "options") {
    caption = paste0("Sample size of informants: ", d$sample[1],
                     "; Iterations per sample size: ", d$total_iterations[1], 
                     "; Population size per iteration: ", d$population[1])
  }
  if (variable_name == "sample") {
    caption = paste0("Number of choice options: ", d$options[1],
                     "; Iterations per sample size: ", d$total_iterations[1], 
                     "; Population size per iteration: ", d$population[1])
  }
  
  d <- d %>% 
    group_by(relative_majority, {{variable}}, competence) %>%
    summarise(competence_mean = mean(average_relative_competence),
              accuracy_mean = mean(average_accuracy),
              accuracy_sd = sd(average_accuracy),
              competence_sd = sd(average_relative_competence)
    )
  
  # Assign outcome variable
  if(outcome == "Accuracy") {
    d$outcome <- d$accuracy_mean
    d$outcome_sd <- d$accuracy_sd
  }
  
  if(outcome == "Competence") {
    d$outcome <- d$competence_mean
    d$outcome_sd <- d$competence_sd
  }
  
  plot_outcome <- ggplot(d, 
                         aes(x = relative_majority, y = outcome,
                             color = as.factor({{variable}}))) +
    geom_point() +
    geom_line() +
    # Add nice labels
    labs(x = "Relative majority \n (share of informants agreeing on an option)", 
         y = outcome, caption = caption, 
         color = variable_name) +
    scale_color_viridis_d(option = "plasma", begin = 0.1, 
                          #limits = rev(levels(d$constellation)),
                          direction = -1
    ) +
    facet_wrap(~competence) +
    plot_theme + 
    theme(legend.position = "top")
  
  return(plot_outcome) 
  
} 

# Numerical scenario

# plot a single one sample size
plot_competence_single_sample <- function(data, outcome = "accuracy") {
  
  d <- data
  
  # extract simulation info
  caption = paste0("Sample size of informants: ", d$sample[1],
                   "; Iterations per sample size: ", d$total_iterations[1], 
                   "; Population size per iteration: ", d$population[1])
  
  
  plot_accuracy <- ggplot(d, aes(x = convergence, y = accuracy_mean)) +
    geom_hex(alpha = 0.9) +
    #geom_smooth(method = "lm", se = FALSE) +
    stat_smooth(geom='line', method = "lm", alpha=0.7, se=FALSE, color = "darkgrey") +
    # Add nice labels
    labs(x = "Convergence \n(SDs of samples)", 
         y = "Accuracy \n(squared distance to true mean)",
         caption = caption) +
    scale_fill_viridis_c(option = "plasma", begin = 0.1) +
    facet_wrap(~competence) +
    plot_theme + 
    theme(legend.position = "top", 
          legend.key.width = unit(1.5, "cm"), 
          legend.key.height = unit(0.3, "cm"))
  
  plot_competence <- ggplot(d, aes(x = convergence, y = competence_mean)) +
    geom_hex(alpha = 0.9) +
    #geom_smooth(method = "lm", se = FALSE) +
    stat_smooth(geom='line', method = "lm", alpha=0.7, se=FALSE, color = "darkgrey") +
    # Add nice labels
    labs(x = "Convergence \n(SDs of samples)", 
         y = "Competence \n(SD for data generating function)",
         caption = caption) +
    scale_fill_viridis_c(option = "plasma", begin = 0.1) +
    facet_wrap(~competence) +
    plot_theme + 
    theme(legend.position = "top", 
          legend.key.width = unit(1.5, "cm"), 
          legend.key.height = unit(0.3, "cm"))
  
  if (outcome == "accuracy") {
    
    return(plot_accuracy)
  }
  
  if (outcome == "competence") {
    
    return(plot_competence)
  }
}

# plot a list by sample size
plot_competence_vary_numeric <- function(data, ...) {
  
  # get different samples as vector
  samples <- data %>% 
    reframe(unique(as.character(sample))) %>% pull()
  
  
  # empty list
  plot_list <- list()
  
  # make plots fro each moderator
  for (i in samples) {
    
    plot_data <- data %>%
      filter(sample == i)
    
    sample_plot <- plot_competence_single_sample(data = plot_data, ...)
    
    plot_list[[i]] <- sample_plot
  }
  
  return(plot_list)
}







