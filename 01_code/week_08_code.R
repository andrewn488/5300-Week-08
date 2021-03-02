# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 2/27/2021
# Week 8 class materials (Choose your own adventure: Experiments)
# Resources:
# https://nickch-k.github.io/EconometricsSlides/Week_08/Power_Simulations.html


# how to analyze experiments. Analyze the data:
lm(outcome ~ treatment, data = experimentdata)

## DETECTING ATTRITION ##
# Create a binary variable telling us there's attrition, 
  # which we might see by not having an outcome for that individual
experimentdata <- experimentdata %>%
  mutate(attrition = is.na(outcome))

# Check whether it's higher/lower in the treated/untreated group:
lm(attrition ~ treatment, data = experimentdata)

# Check whether it's different in treated/untreated for a specific group - 
  # this could be some pre-experiment demographic group like gender, or could be 
  # some early outcome, like "people who rated their experience lower after the 
  # first week of the experiment"
lm(attrition ~ treatment*gender, data = experimentdata)

## CHECKING FOR BALANCE ## 

# If we see more differences than we expect (say, a lot more than 1/20 comparisons 
  # are different using a 95% test) that may indicate problems with randomization
# If we see any differences, we may want to add them as controls to the analysis 
  # if the difference is meaningfully large not just statistically significant
library(vtable)
sumtable(experimentdata, group = 'treatment', group.test = TRUE)

# BALANCE tables:
data(mtcars)
sumtable(mtcars, group = 'am', group.test = TRUE)

# 2SLS - 2 Stage Least Squares:
library(estimatr)
iv_robust(outcome ~ treatment | assignment, data = experimentdata)

# 2SLS: 
library(tidyverse)
library(jtools)
tb <- tibble(assignment = sample(c(TRUE,FALSE),1000, replace = TRUE)) %>%
  # Start with assignment working
  mutate(treatment = assignment) %>%
  # Randomly reassign 20% of people not based on treatment at all
  mutate(treatment = case_when(
    sample(c(TRUE,FALSE),1000,prob = c(.8,.2), replace = TRUE) ~ assignment,
    TRUE ~ sample(c(TRUE,FALSE),1000, replace = TRUE))) %>%
  # True effect is 2
  mutate(outcome = 2*treatment + 3*rnorm(1000))
# Intent-to-treat
itt <- lm_robust(outcome ~ treatment, data = tb)
# 2sls to adjust for compliance
twosls <- iv_robust(outcome ~ treatment | assignment, data = tb)
# Do the scaling by hand (since this is a simple case)
treatment_increase <- (tb %>% filter(assignment) %>% 
                         pull(treatment) %>% 
                         mean()) - (tb %>% filter(!assignment) %>% 
                                      pull(treatment) %>% mean())
assignment_effect <- (tb %>% filter(assignment) %>% 
                        pull(outcome) %>% 
                        mean()) - (tb %>% filter(!assignment) %>% 
                                     pull(outcome) %>% mean())
scaled <- assignment_effect/treatment_increase
c(assignment_effect, treatment_increase, scaled)

# look at results: 
export_summs(itt, twosls, statistics = c(N = 'nobs'))

## POWER Simulations ##
# Step 1: Make up data with the properties we want.
# Make a tibble (or data.frame) to contain our data
tib <- tibble(
  # Start by creating the variables that don't depend on anything else
  # the 1000 is how many observations we're going to have. THe 0 and 1 are the start and end of the uniform distribution
  X = runif(1000, 0, 1)
) %>%
  # Then mutate in any variables that depend on earlier variables
  # Don't forget to add additional noise!
  # The *true effect* of X on Y is .2
  mutate(Y = .2*X + rnorm(1000, mean = 0, sd = 3))

# Step 2: perform analysis we shall perform
library(estimatr)
model <- lm_robust(Y ~ X, data = tib)

# Step 3: pull out the results we want
library(broom)
tidy(model)

# Here we go!
tidy(model)$p.value[2]

# And if we're just interested in significance, say at the 95% level...
sig <- tidy(model)$p.value[2] <= .05
sig

# Step 4: Repeat!
# for loop: 
for (i in 1:2000) {
  # Have to re-create the data EVERY TIME or it will just be the same data over and over
  tib <- tibble(
    X = runif(1000, 0, 1)
  ) %>%
    mutate(Y = .2*X + rnorm(1000, mean = 0, sd = 3))
  
  # Run the analysis
  model <- lm_robust(Y ~ X, data = tib)
  
  # Get the results
  coef_on_X <- coef(model)[2]
  print(coef_on_X)
  sig <- tidy(model)$p.value[2] <= .05
  print(sig)
}

# Step 5: Store the results

coef_results <- c()
sig_results <- c()

for (i in 1:2000) {
  # Have to re-create the data EVERY TIME or it will just be the same data over and over
  tib <- tibble(
    X = runif(1000, 0, 1)
  ) %>%
    mutate(Y = .2*X + rnorm(1000, mean = 0, sd = 3))
  
  # Run the analysis
  model <- lm_robust(Y ~ X, data = tib)
  
  # Get the results
  coef_results[i] <- coef(model)[2]
  sig_results[i] <- tidy(model)$p.value[2] <= .05
}

# Step 6: Examine the results: 
mean(sig_results)

ggplot(mapping = aes(coef_results)) + 
  geom_density()

results_tibble <- tibble(coef = coef_results,
                         sig = sig_results)
ggplot(results_tibble, aes(x = coef)) + 
  geom_density() + 
  # Prettify!
  theme_minimal() + 
  labs(x = 'Coefficient', y = 'Density') +
  scale_x_discrete(labels = c('Insignificant','Significant'))

# Step 7: Fiddle! 
my_power_function <- function(x) {
  coef_results <- c()
  sig_results <- c()
  
  for (i in 1:2000) {
    # Have to re-create the data EVERY TIME or it will just be the same data over and over
    tib <- tibble(
      X = runif(1000, 0, 1)
    ) %>%
      mutate(Y = .2*X + rnorm(1000, mean = 0, sd = 3))
    
    # Run the analysis
    model <- lm_robust(Y ~ X, data = tib)
    
    # Get the results
    coef_results[i] <- coef(model)[2]
    sig_results[i] <- tidy(model)$p.value[2] <= .05
  }
}

# more functions: 
my_power_function <- function(effect, sample_size) {
  coef_results <- c()
  sig_results <- c()
  
  for (i in 1:500) {
    # Have to re-create the data EVERY TIME or it will just be the same data over and over
    tib <- tibble(
      X = runif(sample_size, 0, 1)
    ) %>%
      mutate(Y = effect*X + rnorm(sample_size, mean = 0, sd = 3))
    
    # Run the analysis
    model <- lm_robust(Y ~ X, data = tib)
    
    # Get the results
    coef_results[i] <- coef(model)[2]
    sig_results[i] <- tidy(model)$p.value[2] <= .05
  }
}

my_power_function <- function(effect, sample_size) {
  sig_results <- c()
  
  for (i in 1:500) {
    # Have to re-create the data EVERY TIME or it will just be the same data over and over
    tib <- tibble(
      X = runif(sample_size, 0, 1)
    ) %>%
      mutate(Y = effect*X + rnorm(sample_size, mean = 0, sd = 3))
    
    # Run the analysis
    model <- lm_robust(Y ~ X, data = tib)
    
    # Get the results
    sig_results[i] <- tidy(model)$p.value[2] <= .05
  }
  
  sig_results %>%
    mean() %>%
    return()
}

# Now we can just call the function, setting effect and sample_size to whatever we want, 
  # and get the power back! Let’s check it with the values we had before and make sure we’re in the same range:
my_power_function(.2, 1000)

# if Sample Size really is 1000: 
power_levels <- c()

effects_to_try <- c(.4, .8, 1.2, 1.6, 2)

for (i in 1:5) {
  power_levels[i] <- my_power_function(effects_to_try[i], 1000)
}

# Where do we cross 80%?
power_results <- tibble(effect = effects_to_try,
                        power = power_levels)
power_results

ggplot(power_results, 
       aes(x = effect, y = power)) +
  geom_line(color = 'red', size = 1.5) + 
  # add a horizontal line at 90%
  geom_hline(aes(yintercept = .8), linetype = 'dashed') + 
  # Prettify!
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Linear Effect Size', y = 'Power')
