---
title: "Model for numerical choice scenarios (experiments 1 to 3)"
output: 
  html_document: 
    keep_md: yes
date: "2023-04-08"
---








```r
# ensure this script returns the same results on each run
set.seed(8726)
```

## Explaining the model


```r
# Imagine a situation in which answers range from 1000 to 2000
range <- 1000
# and a population of n = 999
population <- 999
# and we observe samples of 3
sample <- 3
```


### Define competence and its distribution 

We define competence as the value of the standard deviation from which one's answer is drawn. We suppose that answers for all individuals are drawn from normal distributions. Each individual has their own normal distribution. All normal distributions are centered around the true answer, but they differ in their standard deviations. The higher the competence, the lower the standard deviation, i.e. the more certain a guess drawn from the normal distribution will be close to the true answer. 

We (arbitrarily) set the *lowest* competence to the range of possible values, in our case 2000 - 1000 = 1000. We set the *highest* competence to 1/1000 of the range of possible values, in our case 1. 


```r
# Define the x-axis values
x <- seq(1000, 2000, length.out = 1000)

# Define the PDFs for the two distributions
high_competence_pdf <- dnorm(x, mean = 1500, sd = 1)
low_competence_pdf <- dnorm(x, mean = 1500, sd = 1000)

# Create the plot
ggplot() +
  geom_line(aes(x, high_competence_pdf, color = "Highest Competence Individual \n (SD = 1, Mean = 1500)"), size = 1) +
  geom_line(aes(x, low_competence_pdf, color = "Lowest Competence Individual \n (SD = 1000, Mean = 1500)"), size = 1) +
  labs(x = "Competence Level", y = "Density", color = "Data generating function for") +
  ggtitle("Competence Level Distributions") +
  scale_color_manual(values = c("Highest Competence Individual \n (SD = 1, Mean = 1500)" = "blue", 
                                "Lowest Competence Individual \n (SD = 1000, Mean = 1500)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](model_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

We suppose that these competence levels are drawn from a random uniform distribution. That is, in a population, each level of competence is equally likely to occur. 

In order to be able to compare competence measures across different ranges, we want to scale it to reach from 0 (minimal competence) to 1 (maximal competence). 


```r
# 1: Distribution of competence
data <- data.frame(competence = runif(population, 
                                      min = range/1000, 
                                      max = range)) %>% 
  # Scale competence to reach from 0 to 1 (instead of 1 to 1000).
  # Reverse the scale such that 1 becomes maximal competence (instead of 0).
  mutate(competence_scaled = 1 - ((competence - 1) / (1000-1)))


# generated population
ggplot(data, aes(x = competence_scaled)) +
  geom_histogram() + 
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) + 
  annotate("text", x = 0.9, y = 100, label = paste0("n = ", population), color = "red") +
  labs(title = "(sampled) Population of competence drawn from \n uniform population distribution", 
       y = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](model_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
### Generate a choice for each individual based on their competence. 

The numbers that we generate from the normal distributions are tuncated such that they all lie within the pre-defined range (1000 to 2000)


```r
# 2: Draw individual answers based on competence levels 
mean <- 1500

data$answer <- data$competence %>% 
  purrr::map_dbl(function(x){ 
    
    sd <- x
    
    answer = truncnorm::rtruncnorm(1, mean = mean, sd = sd, a = 1000, b = 2000)
  }
  )

ggplot(data, aes(x = answer)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](model_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Measure accuracy

We measure accuracy as the (absolute value of) distance between the chosen answer and the true answer for each individual.

We let this distance be negative, such that higher values represent higher accuracy. 


```r
# 3: measure accuracy
data <- data %>% 
  mutate(accuracy = -1 * abs(mean - answer))

ggplot(data, aes(x = accuracy)) +
  geom_histogram() +

ggplot(data, aes(x = competence_scaled, y = accuracy)) +
  geom_point()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](model_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Draw samples, calculate convergence & compare average outcomes

### Randomly draw samples from the population


```r
# 4: randomly assign samples in population
data <- data %>% mutate(sample_id = rep(1:(population/sample), sample))
```

### Calculate convergence 

We define convergence as the standard deviation of the answers within a sample (multiplied by -1 so that higher values correspond to greater convegence)


```r
# 5: calculate convergence 
data <- data %>% 
  # identify how often a one type of answer occurs in one group
  group_by(sample_id) %>% 
  mutate(convergence = -1 * sd(answer)) %>% 
  ungroup()

ggplot(data, aes(x = convergence)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](model_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Compute average accuracy/competence per sample.


```r
# 6: calculate accuracy and competence per sample

# compute the summary statistics for each sample
results <- data %>% 
  group_by(sample_id, convergence) %>% 
  summarize(competence_mean = mean(competence_scaled),
            accuracy_mean = mean(accuracy)) %>% 
  # store simulation information
  mutate(population = population, 
         sample = sample)
```

```
## `summarise()` has grouped output by 'sample_id'. You can override using the
## `.groups` argument.
```


```r
accuracy_plot <- ggplot(results, aes(x = convergence, y = accuracy_mean)) +
  geom_point() +
  # Add nice labels
    labs(x = "Convergence \n(SDs within samples of three)", 
         y = "Accuracy \n(average distance to true mean)") +
  plot_theme 

competence_plot <- ggplot(results, aes(x = convergence, y = competence_mean)) +
  geom_point() +
  # Add nice labels
    labs(x = "Convergence \n(SDs within samples of three)", 
         y = "Competence \n(SD for data generating function, transposed)") +
  plot_theme 

accuracy_plot + competence_plot
```

![](model_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Functions

### Data generating functions

#### Single population





#### Various populations







#### Vary sample size

This function allows us to investigate how accuracy and competence change with varying the sample size. 

The simulations that this function executes will take quite some time. Therefore, we do not want to run it every time we render this document. Instead we want to store the output of the power simulation in a `.csv` file, and have an integrated "stop" mechanism to prevent execution when that file already exists. To achieve this, we make `file_name` a mandatory argument. If a file with that name already exists, the function will not be executed.





### Plot functions

#### a) Plot various populations

First, we want a function that plots results obtained by the `simulate_various_populations` function. 



#### b) Plot varying sample & options

Second, we want a function that plots results obtained by the `vary_sample()` function. 



## Simulation

### Generate data


```r
n <- c(3, 10, 20, 50)

# run simulation and store results in .csv files
vary_sample(population = 999,
            min_range = 1000, max_range = 2000,
            competence_ratio = 1000,
            iterations = 100,
            file_name = "model_vary_3_10_20_50.csv",
            n = n)

# read simulated data from .csv files
data <- read_csv("data/model_vary_3_10_20_50.csv")
```

### Plot


```r
# plot results
plot_results_vary(data)
```

```
## $`3`
```

![](model_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```
## 
## $`10`
```

![](model_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```
## 
## $`20`
```

![](model_files/figure-html/unnamed-chunk-22-3.png)<!-- -->

```
## 
## $`50`
```

![](model_files/figure-html/unnamed-chunk-22-4.png)<!-- -->

### Analyze & Compare to participant data

It's not clear to me yet what we would want to compare. 

In the experiment, we generated guesses from two different distribution widths: one with SD = 20 and another with SD = 150. In the model, this corresponds to the competence. However, we did not measure the observed SD of the guesses (which is the measure of convergence I rely on in the models). 

Below, what I did was predict accuracy and competence for convergence levels of 20 and 150 based on a regression run on the model data. We could compare that to the participant data - but it's not quite the same thing...


```
## 
## Call:
## lm(formula = competence_mean ~ convergence + sample + convergence * 
##     sample, data = regression_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3.15628 -0.57816  0.01201  0.59079  2.60270 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          4.514e+00  1.109e-02 406.902  < 2e-16 ***
## convergence          2.333e-03  4.546e-05  51.334  < 2e-16 ***
## sample10             2.810e-01  4.603e-02   6.105 1.03e-09 ***
## convergence:sample10 1.002e-03  1.884e-04   5.320 1.04e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8877 on 43196 degrees of freedom
## Multiple R-squared:  0.0643,	Adjusted R-squared:  0.06424 
## F-statistic: 989.5 on 3 and 43196 DF,  p-value: < 2.2e-16
```

```
## Warning: 'tidy.numeric' is deprecated.
## See help("Deprecated")

## Warning: 'tidy.numeric' is deprecated.
## See help("Deprecated")
```

```
##   convergence sample predicted_accuracy predicted_competence
## 1         -20      3           5.967740             4.467172
## 2        -150      3           5.072650             4.163824
## 3         -20     10           6.869570             4.728143
## 4        -150     10           5.521129             4.294510
```




