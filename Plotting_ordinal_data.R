# Load libraries
library(tidyverse)
library(ordinal)
library(MASS)
library(sjPlot)
library(effects)
library(ggeffects)
library(ggpubr)


# For additional tests
library(DescTools)
library(generalhoslem)
library(brant)

# Load data
survey <- read.csv("Glass_2021_survey_processed.csv")

# Inspect dataset
str(survey)
head(survey)

# 1. Density plot
ggplot(survey, aes(x = rating, fill = verb)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~verb) +
  theme_minimal() +
  labs(title = "Density of Ratings by Verb and Routine",
       x = "Rating", y = "Density")

# 2. Stacked bar chart
survey %>%
  count(verb, rating) %>%
  group_by(verb) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = verb, y = prop, fill = factor(rating))) +
  geom_col() +
  theme_minimal() +
  labs(title = "Distribution of Ratings by Verb",
       x = "Verb", y = "Proportion", fill = "Rating")

# 3. Violin plot
ggplot(survey, aes(x = verb, y = rating, fill = verb)) +
  geom_violin() +
 # geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Ratings by Verb",
       x = "Verb", y = "Rating")

# 4. Heat map
survey %>%
  count(verb, routine, rating) %>%
  ggplot(aes(x = verb, y = routine, fill = n)) +
  geom_tile() +
  facet_wrap(~rating) +
  theme_minimal() +
  labs(title = "Heat Map of Ratings by Verb and Routine",
       x = "Verb", y = "Routine", fill = "Count")

# 5. Diverging stacked bar chart
midpoint <- median(survey$rating)
survey %>%
  mutate(rating_centered = rating - midpoint) %>%
  count(verb, rating_centered) %>%
  group_by(verb) %>%
  mutate(prop = n / sum(n),
         prop = ifelse(rating_centered < 0, -prop, prop)) %>%
  ggplot(aes(x = verb, y = prop, fill = factor(rating_centered))) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Diverging Stacked Bar Chart of Ratings by Verb",
       x = "Verb", y = "Proportion", fill = "Rating (Centered)")

library(ggridges)

# Basic density ridge plot
ggplot(survey, aes(x = rating, y = verb, fill = ParticipantID)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Ratings by Verb",
       x = "Rating", y = "Verb")

# Density ridge plot with jittered points
ggplot(survey, aes(x = rating, y = verb, fill = verb)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    scale = 0.9
  ) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Ratings by Verb with Individual Ratings",
       x = "Rating", y = "Verb")

# Density ridge plot faceted by routine
ggplot(survey, aes(x = rating, y = verb, fill = verb)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  facet_wrap(~routine) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Ratings by Verb and Routine",
       x = "Rating", y = "Verb")

# Density ridge plot with color gradient
ggplot(survey, aes(x = rating, y = verb, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Rating", option = "C") +
  theme_ridges() +
  labs(title = "Distribution of Ratings by Verb",
       x = "Rating", y = "Verb")
