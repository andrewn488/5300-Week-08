# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 2/27/2021
# Week 8 class materials (Choose your own adventure: Experiments)


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
